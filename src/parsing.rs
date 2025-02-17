use crate::{
	lexing::{Lexer, LexingError, Token},
	Document, GrdfDocument,
};
use decoded_char::DecodedChar;
use iref::IriBuf;
use locspan::{Span, Spanned};
use rdf_types::{Id, Literal, LiteralType, LocalTerm, Quad, RdfQuad, XSD_STRING};
use std::fmt;

#[derive(Debug)]
pub enum Error<E = LexingError> {
	Lexer(E),
	Unexpected(Option<Token>, Span),
}

impl<E: fmt::Display> fmt::Display for Error<E> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Unexpected(None, _) => write!(f, "unexpected end of file"),
			Self::Unexpected(Some(token), _) => write!(f, "unexpected {token}"),
			Self::Lexer(e) => e.fmt(f),
		}
	}
}

impl<E: 'static + std::error::Error> std::error::Error for Error<E> {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			Self::Lexer(e) => Some(e),
			_ => None,
		}
	}
}

pub struct Parser<L> {
	lexer: L,
	position: usize,
	pending: Option<Token>,
}

impl<L> Parser<L> {
	pub fn new(lexer: L) -> Self {
		Self {
			lexer,
			position: 0,
			pending: None,
		}
	}
}

impl<L, E> Parser<L>
where
	L: Iterator<Item = Result<Token, E>>,
{
	fn next_token(&mut self) -> Result<Option<Token>, Error<E>> {
		let token = match self.pending.take() {
			Some(token) => Some(token),
			None => self.lexer.next().transpose().map_err(Error::Lexer)?,
		};

		if let Some(token) = &token {
			self.position = token.span().end
		}

		Ok(token)
	}

	#[allow(clippy::type_complexity)]
	fn peek(&mut self) -> Result<Option<&Token>, Error<E>> {
		if self.pending.is_none() {
			self.pending = self.lexer.next().transpose().map_err(Error::Lexer)?;
		}

		Ok(self.pending.as_ref())
	}

	#[allow(clippy::type_complexity)]
	fn parse_literal(
		&mut self,
		value: String,
		mut span: Span,
	) -> Result<(Literal, Span), Error<E>> {
		match self.peek()? {
			Some(Token::LangTag(_, _)) => {
				let Some(Token::LangTag(tag, tag_span)) = self.next_token()? else {
					unreachable!()
				};

				span.append(tag_span);

				Ok((Literal::new(value, LiteralType::LangString(tag)), span))
			}
			Some(Token::Carets(_)) => {
				self.next_token()?;

				match self.next_token()? {
					Some(Token::Iri(iri, iri_span)) => {
						span.append(iri_span);
						Ok((Literal::new(value, LiteralType::Any(iri)), span))
					}
					Some(token) => {
						let span = token.span();
						Err(Error::Unexpected(Some(token), span))
					}
					None => Err(Error::Unexpected(None, self.position.into())),
				}
			}
			_ => Ok((
				Literal::new(value, LiteralType::Any(XSD_STRING.to_owned())),
				span,
			)),
		}
	}

	fn parse_iri(&mut self) -> Result<(IriBuf, Span), Error<E>> {
		match self.next_token()? {
			Some(Token::Iri(iri, span)) => Ok((iri, span)),
			Some(token) => {
				let span = token.span();
				Err(Error::Unexpected(Some(token), span))
			}
			None => Err(Error::Unexpected(None, self.position.into())),
		}
	}

	fn parse_id(&mut self) -> Result<(Id, Span), Error<E>> {
		match self.next_token()? {
			Some(Token::Iri(iri, span)) => Ok((Id::Iri(iri), span)),
			Some(Token::BlankNodeLabel(label, span)) => Ok((Id::BlankId(label), span)),
			Some(token) => {
				let span = token.span();
				Err(Error::Unexpected(Some(token), span))
			}
			None => Err(Error::Unexpected(None, self.position.into())),
		}
	}

	fn parse_id_opt(&mut self) -> Result<Option<(Id, Span)>, Error<E>> {
		match self.peek()? {
			Some(Token::Dot(_)) => Ok(None),
			_ => self.parse_id().map(Some),
		}
	}

	fn parse_term(&mut self) -> Result<(LocalTerm, Span), Error<E>> {
		match self.next_token()? {
			Some(Token::Iri(iri, span)) => Ok((LocalTerm::iri(iri), span)),
			Some(Token::BlankNodeLabel(label, span)) => Ok((LocalTerm::Anonymous(label), span)),
			Some(Token::StringLiteral(value, span)) => {
				let (literal, span) = self.parse_literal(value, span)?;
				Ok((LocalTerm::literal(literal), span))
			}
			Some(token) => {
				let span = token.span();
				Err(Error::Unexpected(Some(token), span))
			}
			None => Err(Error::Unexpected(None, self.position.into())),
		}
	}

	fn parse_term_opt(&mut self) -> Result<Option<(LocalTerm, Span)>, Error<E>> {
		match self.peek()? {
			Some(Token::Dot(_)) => Ok(None),
			_ => self.parse_term().map(Some),
		}
	}

	fn parse_dot(&mut self) -> Result<Span, Error<E>> {
		match self.next_token()? {
			Some(Token::Dot(span)) => Ok(span),
			Some(token) => {
				let span = token.span();
				Err(Error::Unexpected(Some(token), span))
			}
			None => Err(Error::Unexpected(None, self.position.into())),
		}
	}

	fn parse_quad(&mut self) -> Result<(RdfQuad, QuadSpans), Error<E>> {
		let (s, s_span) = self.parse_id()?;
		let (p, p_span) = self.parse_iri()?;
		let (o, o_span) = self.parse_term()?;
		let (g, g_span) = split_option(self.parse_id_opt()?);
		let dot_span = self.parse_dot()?;
		let span = s_span.union(dot_span);
		Ok((
			Quad(s, p, o, g),
			(span, Quad(s_span, p_span, o_span, g_span)),
		))
	}

	fn parse_grdf_quad(&mut self) -> Result<(Quad, QuadSpans), Error<E>> {
		let (s, s_span) = self.parse_term()?;
		let (p, p_span) = self.parse_term()?;
		let (o, o_span) = self.parse_term()?;
		let (g, g_span) = split_option(self.parse_term_opt()?);
		let dot_span = self.parse_dot()?;
		let span = s_span.union(dot_span);
		Ok((
			Quad(s, p, o, g),
			(span, Quad(s_span, p_span, o_span, g_span)),
		))
	}

	fn parse_document(&mut self) -> Result<(Vec<RdfQuad>, CodeMap), Error<E>> {
		let mut quads = Vec::new();
		let mut spans = Vec::new();

		while self.peek()?.is_some() {
			let (quad, span) = self.parse_quad()?;
			quads.push(quad);
			spans.push(span);
		}

		Ok((quads, spans))
	}

	fn parse_grdf_document(&mut self) -> Result<(Vec<Quad>, CodeMap), Error<E>> {
		let mut quads = Vec::new();
		let mut spans = Vec::new();

		while self.peek()?.is_some() {
			let (quad, span) = self.parse_grdf_quad()?;
			quads.push(quad);
			spans.push(span);
		}

		Ok((quads, spans))
	}
}

fn split_option<A, B>(value: Option<(A, B)>) -> (Option<A>, Option<B>) {
	match value {
		Some((a, b)) => (Some(a), Some(b)),
		None => (None, None),
	}
}

pub type QuadSpans = (Span, Quad<Span>);

pub type CodeMap = Vec<QuadSpans>;

pub trait Parse: Sized {
	#[allow(clippy::type_complexity)]
	fn parse_with<L, E>(parser: &mut Parser<L>) -> Result<(Self, CodeMap), Error<E>>
	where
		L: Iterator<Item = Result<Token, E>>;

	#[inline(always)]
	fn parse<C, E>(chars: C) -> Result<(Self, CodeMap), Error<LexingError<E>>>
	where
		C: Iterator<Item = Result<DecodedChar, E>>,
	{
		let mut parser = Parser::new(Lexer::new(chars));
		Self::parse_with(&mut parser)
	}

	#[inline(always)]
	fn parse_infallible<C>(chars: C) -> Result<(Self, CodeMap), Error>
	where
		C: Iterator<Item = DecodedChar>,
	{
		Self::parse(chars.map(Ok))
	}

	#[inline(always)]
	fn parse_utf8<C, E>(chars: C) -> Result<(Self, CodeMap), Error<LexingError<E>>>
	where
		C: Iterator<Item = Result<char, E>>,
	{
		Self::parse(decoded_char::FallibleUtf8Decoded::new(chars))
	}

	#[inline(always)]
	fn parse_utf8_infallible<C>(chars: C) -> Result<(Self, CodeMap), Error>
	where
		C: Iterator<Item = char>,
	{
		Self::parse_infallible(decoded_char::Utf8Decoded::new(chars))
	}

	#[inline(always)]
	fn parse_utf16<C, E>(chars: C) -> Result<(Self, CodeMap), Error<LexingError<E>>>
	where
		C: Iterator<Item = Result<char, E>>,
	{
		Self::parse(decoded_char::FallibleUtf16Decoded::new(chars))
	}

	#[inline(always)]
	fn parse_utf16_infallible<C>(chars: C) -> Result<(Self, CodeMap), Error>
	where
		C: Iterator<Item = char>,
	{
		Self::parse_infallible(decoded_char::Utf16Decoded::new(chars))
	}

	#[inline(always)]
	fn parse_str(string: &str) -> Result<(Self, CodeMap), Error> {
		Self::parse_utf8_infallible(string.chars())
	}
}

impl Parse for Document {
	fn parse_with<L, E>(parser: &mut Parser<L>) -> Result<(Self, CodeMap), Error<E>>
	where
		L: Iterator<Item = Result<Token, E>>,
	{
		parser.parse_document()
	}
}

impl Parse for GrdfDocument {
	fn parse_with<L, E>(parser: &mut Parser<L>) -> Result<(Self, CodeMap), Error<E>>
	where
		L: Iterator<Item = Result<Token, E>>,
	{
		parser.parse_grdf_document()
	}
}
