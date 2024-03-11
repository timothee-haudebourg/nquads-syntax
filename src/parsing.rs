use crate::{
	lexing::{self, Token, Tokens},
	Lexer,
};
use decoded_char::DecodedChar;
use iref::{Iri, IriBuf};
use locspan::{Meta, Span};
use rdf_types::{Id, Literal, LiteralType};
use static_iref::iri;
use std::fmt;

#[derive(Debug)]
pub enum Error<E> {
	Lexer(E),
	Unexpected(Option<Token>),
}

pub type MetaError<E, Span> = Meta<Box<Error<E>>, Span>;

impl<E: fmt::Display> fmt::Display for Error<E> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Unexpected(None) => write!(f, "unexpected end of file"),
			Self::Unexpected(Some(token)) => write!(f, "unexpected {token}"),
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
}

impl<L> Parser<L> {
	pub fn new(lexer: L) -> Self {
		Self { lexer }
	}
}

const XSD_STRING: &Iri = iri!("http://www.w3.org/2001/XMLSchema#string");

impl<L: Tokens> Parser<L> {
	fn next(&mut self) -> Result<Meta<Option<Token>, Span>, MetaError<L::Error, Span>> {
		self.lexer
			.next()
			.map_err(|Meta(e, span)| Meta(Box::new(Error::Lexer(e)), span))
	}

	#[allow(clippy::type_complexity)]
	fn peek(&mut self) -> Result<Meta<Option<&Token>, Span>, MetaError<L::Error, Span>> {
		self.lexer
			.peek()
			.map_err(|Meta(e, span)| Meta(Box::new(Error::Lexer(e)), span))
	}

	fn begin(&mut self) -> Result<Span, MetaError<L::Error, Span>> {
		self.lexer
			.begin()
			.map_err(|Meta(e, span)| Meta(Box::new(Error::Lexer(e)), span))
	}

	fn last_span(&self) -> Span {
		self.lexer.last()
	}

	#[allow(clippy::type_complexity)]
	fn parse_literal(
		&mut self,
		Meta(string, string_span): Meta<String, Span>,
	) -> Result<Meta<Literal, Span>, MetaError<L::Error, Span>> {
		let mut span = string_span;
		match self.peek()? {
			Meta(Some(Token::LangTag(_)), tag_span) => {
				let tag = match self.next()? {
					Meta(Some(Token::LangTag(tag)), _) => tag,
					_ => panic!("expected lang tag"),
				};

				span.append(tag_span);
				Ok(Meta(
					Literal::new(string, LiteralType::LangString(tag)),
					span,
				))
			}
			Meta(Some(Token::Carets), _) => {
				self.next()?;
				match self.next()? {
					Meta(Some(Token::Iri(iri)), iri_span) => {
						span.append(iri_span);
						Ok(Meta(Literal::new(string, LiteralType::Any(iri)), span))
					}
					Meta(unexpected, span) => {
						Err(Meta(Box::new(Error::Unexpected(unexpected)), span))
					}
				}
			}
			_ => Ok(Meta(
				Literal::new(string, LiteralType::Any(XSD_STRING.to_owned())),
				span,
			)),
		}
	}
}

pub trait Parse: Sized {
	#[allow(clippy::type_complexity)]
	fn parse_with<L>(parser: &mut Parser<L>) -> Result<Meta<Self, Span>, MetaError<L::Error, Span>>
	where
		L: Tokens;

	#[inline(always)]
	fn parse<C, E>(chars: C) -> Result<Meta<Self, Span>, MetaError<lexing::Error<E>, Span>>
	where
		C: Iterator<Item = Result<DecodedChar, E>>,
	{
		let mut parser = Parser::new(Lexer::new(chars));
		Self::parse_with(&mut parser)
	}

	#[inline(always)]
	fn parse_infallible<C>(chars: C) -> Result<Meta<Self, Span>, MetaError<lexing::Error, Span>>
	where
		C: Iterator<Item = DecodedChar>,
	{
		Self::parse(chars.map(Ok))
	}

	#[inline(always)]
	fn parse_utf8<C, E>(chars: C) -> Result<Meta<Self, Span>, MetaError<lexing::Error<E>, Span>>
	where
		C: Iterator<Item = Result<char, E>>,
	{
		Self::parse(decoded_char::FallibleUtf8Decoded::new(chars))
	}

	#[inline(always)]
	fn parse_utf8_infallible<C>(
		chars: C,
	) -> Result<Meta<Self, Span>, MetaError<lexing::Error, Span>>
	where
		C: Iterator<Item = char>,
	{
		Self::parse_infallible(decoded_char::Utf8Decoded::new(chars))
	}

	#[inline(always)]
	fn parse_utf16<C, E>(chars: C) -> Result<Meta<Self, Span>, MetaError<lexing::Error<E>, Span>>
	where
		C: Iterator<Item = Result<char, E>>,
	{
		Self::parse(decoded_char::FallibleUtf16Decoded::new(chars))
	}

	#[inline(always)]
	fn parse_utf16_infallible<C>(
		chars: C,
	) -> Result<Meta<Self, Span>, MetaError<lexing::Error, Span>>
	where
		C: Iterator<Item = char>,
	{
		Self::parse_infallible(decoded_char::Utf16Decoded::new(chars))
	}

	#[inline(always)]
	fn parse_str(string: &str) -> Result<Meta<Self, Span>, MetaError<lexing::Error, Span>> {
		Self::parse_utf8_infallible(string.chars())
	}
}

impl Parse for IriBuf {
	fn parse_with<L>(parser: &mut Parser<L>) -> Result<Meta<Self, Span>, MetaError<L::Error, Span>>
	where
		L: Tokens,
	{
		match parser.next()? {
			Meta(Some(Token::Iri(iri)), span) => Ok(Meta(iri, span)),
			Meta(unexpected, span) => Err(Meta(Box::new(Error::Unexpected(unexpected)), span)),
		}
	}
}

impl Parse for crate::Subject {
	fn parse_with<L>(parser: &mut Parser<L>) -> Result<Meta<Self, Span>, MetaError<L::Error, Span>>
	where
		L: Tokens,
	{
		match parser.next()? {
			Meta(Some(Token::Iri(iri)), span) => Ok(Meta(Self::Iri(iri), span)),
			Meta(Some(Token::BlankNodeLabel(label)), span) => Ok(Meta(Self::Blank(label), span)),
			Meta(unexpected, span) => Err(Meta(Box::new(Error::Unexpected(unexpected)), span)),
		}
	}
}

impl Parse for Literal {
	fn parse_with<L>(parser: &mut Parser<L>) -> Result<Meta<Self, Span>, MetaError<L::Error, Span>>
	where
		L: Tokens,
	{
		match parser.next()? {
			Meta(Some(Token::StringLiteral(string)), span) => {
				parser.parse_literal(Meta(string, span))
			}
			Meta(unexpected, span) => Err(Meta(Box::new(Error::Unexpected(unexpected)), span)),
		}
	}
}

impl Parse for crate::Object {
	fn parse_with<L>(parser: &mut Parser<L>) -> Result<Meta<Self, Span>, MetaError<L::Error, Span>>
	where
		L: Tokens,
	{
		match parser.next()? {
			Meta(Some(Token::Iri(iri)), span) => Ok(Meta(Self::Id(Id::Iri(iri)), span)),
			Meta(Some(Token::BlankNodeLabel(label)), span) => {
				Ok(Meta(Self::Id(Id::Blank(label)), span))
			}
			Meta(Some(Token::StringLiteral(string)), string_span) => {
				let Meta(lit, loc) = parser.parse_literal(Meta(string, string_span))?;
				Ok(Meta(Self::Literal(lit), loc))
			}
			Meta(unexpected, span) => Err(Meta(Box::new(Error::Unexpected(unexpected)), span)),
		}
	}
}

impl Parse for crate::Quad {
	fn parse_with<L>(parser: &mut Parser<L>) -> Result<Meta<Self, Span>, MetaError<L::Error, Span>>
	where
		L: Tokens,
	{
		let mut span = parser.begin()?;
		let subject = crate::Subject::parse_with(parser)?;
		let predicate = IriBuf::parse_with(parser)?;
		let object = crate::Object::parse_with(parser)?;
		let graph = match parser.next()? {
			Meta(Some(Token::Dot), _) => None,
			opt_token => {
				let graph_label = match opt_token {
					Meta(Some(Token::Iri(iri)), span) => Meta(crate::GraphLabel::Iri(iri), span),
					Meta(Some(Token::BlankNodeLabel(label)), span) => {
						Meta(crate::GraphLabel::Blank(label), span)
					}
					Meta(unexpected, span) => {
						return Err(Meta(Box::new(Error::Unexpected(unexpected)), span))
					}
				};

				match parser.next()? {
					Meta(Some(Token::Dot), _) => Some(graph_label),
					Meta(unexpected, span) => {
						return Err(Meta(Box::new(Error::Unexpected(unexpected)), span))
					}
				}
			}
		};

		span.append(parser.last_span());
		Ok(Meta(
			crate::Quad::new(subject, predicate, object, graph),
			span,
		))
	}
}

impl Parse for crate::GrdfQuad {
	fn parse_with<L>(parser: &mut Parser<L>) -> Result<Meta<Self, Span>, MetaError<L::Error, Span>>
	where
		L: Tokens,
	{
		let mut span = parser.begin()?;
		let subject = crate::Term::parse_with(parser)?;
		let predicate = crate::Term::parse_with(parser)?;
		let object = crate::Term::parse_with(parser)?;
		let graph = match parser.next()? {
			Meta(Some(Token::Dot), _) => None,
			opt_token => {
				let graph_label = match opt_token {
					Meta(Some(Token::Iri(iri)), span) => Meta(crate::Term::Id(Id::Iri(iri)), span),
					Meta(Some(Token::BlankNodeLabel(label)), span) => {
						Meta(crate::Term::Id(Id::Blank(label)), span)
					}
					Meta(Some(Token::StringLiteral(string)), string_span) => {
						let Meta(lit, meta) = parser.parse_literal(Meta(string, string_span))?;
						Meta(crate::Term::Literal(lit), meta)
					}
					Meta(unexpected, span) => {
						return Err(Meta(Box::new(Error::Unexpected(unexpected)), span))
					}
				};

				match parser.next()? {
					Meta(Some(Token::Dot), _) => Some(graph_label),
					Meta(unexpected, span) => {
						return Err(Meta(Box::new(Error::Unexpected(unexpected)), span))
					}
				}
			}
		};

		span.append(parser.last_span());
		Ok(Meta(
			crate::GrdfQuad::new(subject, predicate, object, graph),
			span,
		))
	}
}

impl Parse for crate::Document {
	fn parse_with<L>(parser: &mut Parser<L>) -> Result<Meta<Self, Span>, MetaError<L::Error, Span>>
	where
		L: Tokens,
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

		Ok(Meta(quads, span))
	}
}

impl Parse for crate::GrdfDocument {
	fn parse_with<L>(parser: &mut Parser<L>) -> Result<Meta<Self, Span>, MetaError<L::Error, Span>>
	where
		L: Tokens,
	{
		let mut quads = Vec::new();
		let mut span = parser.begin()?;

		loop {
			match parser.peek()? {
				Meta(Some(_), _) => {
					quads.push(crate::GrdfQuad::parse_with(parser)?);
				}
				Meta(None, end) => {
					span.append(end);
					break;
				}
			}
		}

		Ok(Meta(quads, span))
	}
}
