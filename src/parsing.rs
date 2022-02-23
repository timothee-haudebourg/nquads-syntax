use crate::{
	lexing::{Token, Tokens},
	StringLiteral,
};
use iref::IriRefBuf;
use locspan::{Loc, MapLocErr};
use std::fmt;

#[derive(Debug)]
pub enum Error<E> {
	Lexer(E),
	Unexpected(Option<Token>),
}

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

pub trait Parse<F>: Sized {
	#[allow(clippy::type_complexity)]
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>>;
}

impl<F: Clone> Parse<F> for IriRefBuf {
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		match lexer.next().map_loc_err(Error::Lexer)? {
			Loc(Some(Token::IriRef(iri_ref)), loc) => Ok(Loc(iri_ref, loc)),
			Loc(unexpected, loc) => Err(Loc(Error::Unexpected(unexpected), loc)),
		}
	}
}

impl<F: Clone> Parse<F> for crate::Subject {
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		match lexer.next().map_loc_err(Error::Lexer)? {
			Loc(Some(Token::IriRef(iri_ref)), loc) => Ok(Loc(Self::IriRef(iri_ref), loc)),
			Loc(Some(Token::BlankNodeLabel(label)), loc) => Ok(Loc(Self::Blank(label), loc)),
			Loc(unexpected, loc) => Err(Loc(Error::Unexpected(unexpected), loc)),
		}
	}
}

#[allow(clippy::type_complexity)]
fn parse_literal<F: Clone, L: Tokens<F>>(
	lexer: &mut L,
	string: StringLiteral,
	string_loc: locspan::Location<F>,
) -> Result<Loc<crate::Literal<F>, F>, Loc<Error<L::Error>, F>> {
	match lexer.peek().map_loc_err(Error::Lexer)? {
		Loc(Some(Token::LangTag(_)), tag_loc) => {
			let tag = match lexer.next().map_loc_err(Error::Lexer)? {
				Loc(Some(Token::LangTag(tag)), _) => tag,
				_ => panic!("expected lang tag"),
			};

			let mut loc = string_loc.clone();
			loc.span_mut().append(tag_loc.span());
			Ok(Loc(
				crate::Literal::LangString(Loc(string, string_loc), Loc(tag, tag_loc)),
				loc,
			))
		}
		Loc(Some(Token::Carets), _) => {
			lexer.next().map_loc_err(Error::Lexer)?;
			match lexer.next().map_loc_err(Error::Lexer)? {
				Loc(Some(Token::IriRef(iri_ref)), iri_ref_loc) => {
					let mut loc = string_loc.clone();
					loc.span_mut().append(iri_ref_loc.span());
					Ok(Loc(
						crate::Literal::TypedString(
							Loc(string, string_loc),
							Loc(iri_ref, iri_ref_loc),
						),
						loc,
					))
				}
				Loc(unexpected, loc) => Err(Loc(Error::Unexpected(unexpected), loc)),
			}
		}
		_ => Ok(Loc(
			crate::Literal::String(Loc(string, string_loc.clone())),
			string_loc,
		)),
	}
}

impl<F: Clone> Parse<F> for crate::Literal<F> {
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		match lexer.next().map_loc_err(Error::Lexer)? {
			Loc(Some(Token::StringLiteral(string)), string_loc) => {
				parse_literal(lexer, string, string_loc)
			}
			Loc(unexpected, loc) => Err(Loc(Error::Unexpected(unexpected), loc)),
		}
	}
}

impl<F: Clone> Parse<F> for crate::Object<F> {
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		match lexer.next().map_loc_err(Error::Lexer)? {
			Loc(Some(Token::IriRef(iri_ref)), loc) => Ok(Loc(Self::IriRef(iri_ref), loc)),
			Loc(Some(Token::BlankNodeLabel(label)), loc) => Ok(Loc(Self::Blank(label), loc)),
			Loc(Some(Token::StringLiteral(string)), string_loc) => {
				let Loc(lit, loc) = parse_literal(lexer, string, string_loc)?;
				Ok(Loc(Self::Literal(lit), loc))
			}
			Loc(unexpected, loc) => Err(Loc(Error::Unexpected(unexpected), loc)),
		}
	}
}

impl<F: Clone> Parse<F> for crate::GraphLabel {
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		Self::from_opt_token(lexer.next().map_loc_err(Error::Lexer)?)
	}
}

impl crate::GraphLabel {
	fn from_opt_token<E, F>(
		token: Loc<Option<Token>, F>,
	) -> Result<Loc<Self, F>, Loc<Error<E>, F>> {
		match token {
			Loc(Some(Token::IriRef(iri_ref)), loc) => Ok(Loc(Self::IriRef(iri_ref), loc)),
			Loc(Some(Token::BlankNodeLabel(label)), loc) => Ok(Loc(Self::Blank(label), loc)),
			Loc(unexpected, loc) => Err(Loc(Error::Unexpected(unexpected), loc)),
		}
	}
}

impl<F: Clone> Parse<F> for crate::Quad<F> {
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		let subject = crate::Subject::parse(lexer)?;
		let predicate = IriRefBuf::parse(lexer)?;
		let object = crate::Object::parse(lexer)?;
		let (graph, loc) = match lexer.next().map_loc_err(Error::Lexer)? {
			Loc(Some(Token::Dot), _) => (None, subject.location().clone().with(object.span())),
			opt_token => {
				let graph_label = crate::GraphLabel::from_opt_token(opt_token)?;
				let loc = subject.location().clone().with(graph_label.span());
				match lexer.next().map_loc_err(Error::Lexer)? {
					Loc(Some(Token::Dot), _) => (Some(graph_label), loc),
					Loc(unexpected, loc) => return Err(Loc(Error::Unexpected(unexpected), loc)),
				}
			}
		};

		Ok(Loc(
			crate::Quad {
				subject,
				predicate,
				object,
				graph,
			},
			loc,
		))
	}
}

impl<F: Clone> Parse<F> for crate::Document<F> {
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		let mut quads = Vec::new();
		let mut loc: Option<locspan::Location<F>> = None;

		let loc = loop {
			match lexer.peek().map_loc_err(Error::Lexer)? {
				Loc(Some(_), quad_loc) => {
					quads.push(crate::Quad::parse(lexer)?);
					loc = match loc {
						Some(loc) => Some(loc.with(quad_loc.span())),
						None => Some(quad_loc),
					};
				}
				Loc(None, end_loc) => {
					break match loc {
						Some(loc) => loc,
						None => end_loc,
					}
				}
			}
		};

		Ok(Loc(crate::Document { quads }, loc))
	}
}
