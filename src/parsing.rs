use crate::{
	lexing::{self, Token, Tokens},
	Lexer, StringLiteral,
};
use decoded_char::DecodedChar;
use iref::IriBuf;
use locspan::{Meta, Span};
use std::fmt;

#[derive(Debug)]
pub enum Error<E> {
	Lexer(E),
	Unexpected(Option<Token>),
}

pub type MetaError<E, M> = Meta<Box<Error<E>>, M>;

impl<E: fmt::Display> fmt::Display for Error<E> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Unexpected(None) => write!(f, "unexpected end of file"),
			Self::Unexpected(Some(token)) => write!(f, "unexpected {}", token),
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

pub struct Parser<L, F> {
	lexer: L,
	metadata_builder: F,
}

impl<L, F> Parser<L, F> {
	pub fn new(lexer: L, metadata_builder: F) -> Self {
		Self {
			lexer,
			metadata_builder,
		}
	}
}

impl<L: Tokens, F: FnMut(Span) -> M, M> Parser<L, F> {
	fn next(&mut self) -> Result<Meta<Option<Token>, Span>, MetaError<L::Error, M>> {
		self.lexer
			.next()
			.map_err(|Meta(e, span)| Meta(Box::new(Error::Lexer(e)), (self.metadata_builder)(span)))
	}

	#[allow(clippy::type_complexity)]
	fn peek(&mut self) -> Result<Meta<Option<&Token>, Span>, MetaError<L::Error, M>> {
		self.lexer
			.peek()
			.map_err(|Meta(e, span)| Meta(Box::new(Error::Lexer(e)), (self.metadata_builder)(span)))
	}

	fn begin(&mut self) -> Result<Span, MetaError<L::Error, M>> {
		self.lexer
			.begin()
			.map_err(|Meta(e, span)| Meta(Box::new(Error::Lexer(e)), (self.metadata_builder)(span)))
	}

	fn last_span(&self) -> Span {
		self.lexer.last()
	}

	fn build_metadata(&mut self, span: Span) -> M {
		(self.metadata_builder)(span)
	}

	#[allow(clippy::type_complexity)]
	fn parse_literal(
		&mut self,
		Meta(string, string_span): Meta<StringLiteral, Span>,
	) -> Result<Meta<crate::Literal<M>, M>, MetaError<L::Error, M>> {
		let mut span = string_span;
		match self.peek()? {
			Meta(Some(Token::LangTag(_)), tag_span) => {
				let tag = match self.next()? {
					Meta(Some(Token::LangTag(tag)), _) => tag,
					_ => panic!("expected lang tag"),
				};

				span.append(tag_span);
				Ok(Meta(
					crate::Literal::LangString(
						Meta(string, self.build_metadata(string_span)),
						Meta(tag, self.build_metadata(tag_span)),
					),
					self.build_metadata(span),
				))
			}
			Meta(Some(Token::Carets), _) => {
				self.next()?;
				match self.next()? {
					Meta(Some(Token::Iri(iri)), iri_span) => {
						span.append(iri_span);
						Ok(Meta(
							crate::Literal::TypedString(
								Meta(string, self.build_metadata(string_span)),
								Meta(iri, self.build_metadata(iri_span)),
							),
							self.build_metadata(span),
						))
					}
					Meta(unexpected, span) => Err(Meta(
						Box::new(Error::Unexpected(unexpected)),
						self.build_metadata(span),
					)),
				}
			}
			_ => Ok(Meta(
				crate::Literal::String(Meta(string, self.build_metadata(string_span))),
				self.build_metadata(span),
			)),
		}
	}
}

pub trait Parse<M>: Sized {
	#[allow(clippy::type_complexity)]
	fn parse_with<L, F>(parser: &mut Parser<L, F>) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M;

	#[inline(always)]
	fn parse<C, F, E>(
		chars: C,
		metadata_builder: F,
	) -> Result<Meta<Self, M>, MetaError<lexing::Error<E>, M>>
	where
		C: Iterator<Item = Result<DecodedChar, E>>,
		F: FnMut(Span) -> M,
	{
		let mut parser = Parser::new(Lexer::new(chars), metadata_builder);
		Self::parse_with(&mut parser)
	}

	#[inline(always)]
	fn parse_infallible<C, F>(
		chars: C,
		metadata_builder: F,
	) -> Result<Meta<Self, M>, MetaError<lexing::Error, M>>
	where
		C: Iterator<Item = DecodedChar>,
		F: FnMut(Span) -> M,
	{
		Self::parse(chars.map(Ok), metadata_builder)
	}

	#[inline(always)]
	fn parse_utf8<C, F, E>(
		chars: C,
		metadata_builder: F,
	) -> Result<Meta<Self, M>, MetaError<lexing::Error<E>, M>>
	where
		C: Iterator<Item = Result<char, E>>,
		F: FnMut(Span) -> M,
	{
		Self::parse(
			decoded_char::FallibleUtf8Decoded::new(chars),
			metadata_builder,
		)
	}

	#[inline(always)]
	fn parse_utf8_infallible<C, F>(
		chars: C,
		metadata_builder: F,
	) -> Result<Meta<Self, M>, MetaError<lexing::Error, M>>
	where
		C: Iterator<Item = char>,
		F: FnMut(Span) -> M,
	{
		Self::parse_infallible(decoded_char::Utf8Decoded::new(chars), metadata_builder)
	}

	#[inline(always)]
	fn parse_utf16<C, F, E>(
		chars: C,
		metadata_builder: F,
	) -> Result<Meta<Self, M>, MetaError<lexing::Error<E>, M>>
	where
		C: Iterator<Item = Result<char, E>>,
		F: FnMut(Span) -> M,
	{
		Self::parse(
			decoded_char::FallibleUtf16Decoded::new(chars),
			metadata_builder,
		)
	}

	#[inline(always)]
	fn parse_utf16_infallible<C, F>(
		chars: C,
		metadata_builder: F,
	) -> Result<Meta<Self, M>, MetaError<lexing::Error, M>>
	where
		C: Iterator<Item = char>,
		F: FnMut(Span) -> M,
	{
		Self::parse_infallible(decoded_char::Utf16Decoded::new(chars), metadata_builder)
	}

	#[inline(always)]
	fn parse_str<F>(
		string: &str,
		metadata_builder: F,
	) -> Result<Meta<Self, M>, MetaError<lexing::Error, M>>
	where
		F: FnMut(Span) -> M,
	{
		Self::parse_utf8_infallible(string.chars(), metadata_builder)
	}
}

impl<M: Clone> Parse<M> for IriBuf {
	fn parse_with<L, F>(parser: &mut Parser<L, F>) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		match parser.next()? {
			Meta(Some(Token::Iri(iri)), span) => Ok(Meta(iri, parser.build_metadata(span))),
			Meta(unexpected, span) => Err(Meta(
				Box::new(Error::Unexpected(unexpected)),
				parser.build_metadata(span),
			)),
		}
	}
}

impl<M: Clone> Parse<M> for crate::Subject {
	fn parse_with<L, F>(parser: &mut Parser<L, F>) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		match parser.next()? {
			Meta(Some(Token::Iri(iri)), span) => {
				Ok(Meta(Self::Iri(iri), parser.build_metadata(span)))
			}
			Meta(Some(Token::BlankNodeLabel(label)), span) => {
				Ok(Meta(Self::Blank(label), parser.build_metadata(span)))
			}
			Meta(unexpected, span) => Err(Meta(
				Box::new(Error::Unexpected(unexpected)),
				parser.build_metadata(span),
			)),
		}
	}
}

impl<M: Clone> Parse<M> for crate::Literal<M> {
	fn parse_with<L, F>(parser: &mut Parser<L, F>) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		match parser.next()? {
			Meta(Some(Token::StringLiteral(string)), span) => {
				parser.parse_literal(Meta(string, span))
			}
			Meta(unexpected, span) => Err(Meta(
				Box::new(Error::Unexpected(unexpected)),
				parser.build_metadata(span),
			)),
		}
	}
}

impl<M: Clone> Parse<M> for crate::Object<M> {
	fn parse_with<L, F>(parser: &mut Parser<L, F>) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		match parser.next()? {
			Meta(Some(Token::Iri(iri)), span) => {
				Ok(Meta(Self::Iri(iri), parser.build_metadata(span)))
			}
			Meta(Some(Token::BlankNodeLabel(label)), span) => {
				Ok(Meta(Self::Blank(label), parser.build_metadata(span)))
			}
			Meta(Some(Token::StringLiteral(string)), string_span) => {
				let Meta(lit, loc) = parser.parse_literal(Meta(string, string_span))?;
				Ok(Meta(Self::Literal(lit), loc))
			}
			Meta(unexpected, span) => Err(Meta(
				Box::new(Error::Unexpected(unexpected)),
				parser.build_metadata(span),
			)),
		}
	}
}

impl<M: Clone> Parse<M>
	for crate::Quad<
		Meta<crate::Subject, M>,
		Meta<crate::IriBuf, M>,
		Meta<crate::Object<M>, M>,
		Meta<crate::GraphLabel, M>,
	>
{
	fn parse_with<L, F>(parser: &mut Parser<L, F>) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		let mut span = parser.begin()?;
		let subject = crate::Subject::parse_with(parser)?;
		let predicate = IriBuf::parse_with(parser)?;
		let object = crate::Object::parse_with(parser)?;
		let graph = match parser.next()? {
			Meta(Some(Token::Dot), _) => None,
			opt_token => {
				let graph_label = match opt_token {
					Meta(Some(Token::Iri(iri)), span) => {
						Meta(crate::GraphLabel::Iri(iri), parser.build_metadata(span))
					}
					Meta(Some(Token::BlankNodeLabel(label)), span) => {
						Meta(crate::GraphLabel::Blank(label), parser.build_metadata(span))
					}
					Meta(unexpected, span) => {
						return Err(Meta(
							Box::new(Error::Unexpected(unexpected)),
							parser.build_metadata(span),
						))
					}
				};

				match parser.next()? {
					Meta(Some(Token::Dot), _) => Some(graph_label),
					Meta(unexpected, span) => {
						return Err(Meta(
							Box::new(Error::Unexpected(unexpected)),
							parser.build_metadata(span),
						))
					}
				}
			}
		};

		span.append(parser.last_span());
		Ok(Meta(
			crate::Quad(subject, predicate, object, graph),
			parser.build_metadata(span),
		))
	}
}

impl<M: Clone> Parse<M>
	for crate::Quad<
		Meta<crate::Term<M>, M>,
		Meta<crate::Term<M>, M>,
		Meta<crate::Term<M>, M>,
		Meta<crate::Term<M>, M>,
	>
{
	fn parse_with<L, F>(parser: &mut Parser<L, F>) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		let mut span = parser.begin()?;
		let subject = crate::Term::parse_with(parser)?;
		let predicate = crate::Term::parse_with(parser)?;
		let object = crate::Term::parse_with(parser)?;
		let graph = match parser.next()? {
			Meta(Some(Token::Dot), _) => None,
			opt_token => {
				let graph_label = match opt_token {
					Meta(Some(Token::Iri(iri)), span) => {
						Meta(crate::Term::Iri(iri), parser.build_metadata(span))
					}
					Meta(Some(Token::BlankNodeLabel(label)), span) => {
						Meta(crate::Term::Blank(label), parser.build_metadata(span))
					}
					Meta(Some(Token::StringLiteral(string)), string_span) => {
						let Meta(lit, meta) = parser.parse_literal(Meta(string, string_span))?;
						Meta(crate::Term::Literal(lit), meta)
					}
					Meta(unexpected, span) => {
						return Err(Meta(
							Box::new(Error::Unexpected(unexpected)),
							parser.build_metadata(span),
						))
					}
				};

				match parser.next()? {
					Meta(Some(Token::Dot), _) => Some(graph_label),
					Meta(unexpected, span) => {
						return Err(Meta(
							Box::new(Error::Unexpected(unexpected)),
							parser.build_metadata(span),
						))
					}
				}
			}
		};

		span.append(parser.last_span());
		Ok(Meta(
			crate::Quad(subject, predicate, object, graph),
			parser.build_metadata(span),
		))
	}
}

impl<M: Clone> Parse<M> for crate::Document<M> {
	fn parse_with<L, F>(parser: &mut Parser<L, F>) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		let mut quads = Vec::new();
		let mut span = parser.begin()?;

		loop {
			match parser.peek()? {
				Meta(Some(_), _) => {
					quads.push(crate::Quad::parse_with(parser)?);
				}
				Meta(None, end) => {
					span.append(end);
					break;
				}
			}
		}

		Ok(Meta(quads, parser.build_metadata(span)))
	}
}

impl<M: Clone> Parse<M> for crate::GrdfDocument<M> {
	fn parse_with<L, F>(parser: &mut Parser<L, F>) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		let mut quads = Vec::new();
		let mut span = parser.begin()?;

		loop {
			match parser.peek()? {
				Meta(Some(_), _) => {
					quads.push(crate::Quad::parse_with(parser)?);
				}
				Meta(None, end) => {
					span.append(end);
					break;
				}
			}
		}

		Ok(Meta(quads, parser.build_metadata(span)))
	}
}
