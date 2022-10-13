use crate::{
	lexing::{Token, Tokens},
	StringLiteral,
};
use iref::IriBuf;
use locspan::{Loc, Location, Meta};
use std::fmt;

#[derive(Debug)]
pub enum Error<E> {
	Lexer(E),
	Unexpected(Option<Token>),
}

pub type BoxedError<E, F> = Box<Loc<Error<E>, F>>;

impl<E> Error<E> {
	fn from_lexer<F>(Meta(e, loc): Loc<E, F>) -> BoxedError<E, F> {
		Box::new(Meta(Self::Lexer(e), loc))
	}
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
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, BoxedError<L::Error, F>>;
}

impl<F: Clone> Parse<F> for IriBuf {
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, BoxedError<L::Error, F>> {
		match lexer.next().map_err(Error::from_lexer)? {
			Meta(Some(Token::Iri(iri)), loc) => Ok(Loc(iri, loc)),
			Meta(unexpected, loc) => Err(Box::new(Loc(Error::Unexpected(unexpected), loc))),
		}
	}
}

impl<F: Clone> Parse<F> for crate::Subject {
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, BoxedError<L::Error, F>> {
		match lexer.next().map_err(Error::from_lexer)? {
			Meta(Some(Token::Iri(iri)), loc) => Ok(Loc(Self::Iri(iri), loc)),
			Meta(Some(Token::BlankNodeLabel(label)), loc) => Ok(Loc(Self::Blank(label), loc)),
			Meta(unexpected, loc) => Err(Box::new(Loc(Error::Unexpected(unexpected), loc))),
		}
	}
}

#[allow(clippy::type_complexity)]
fn parse_literal<F: Clone, L: Tokens<F>>(
	lexer: &mut L,
	string: StringLiteral,
	string_loc: Location<F>,
) -> Result<Loc<crate::Literal<Location<F>>, F>, BoxedError<L::Error, F>> {
	match lexer.peek().map_err(Error::from_lexer)? {
		Meta(Some(Token::LangTag(_)), tag_loc) => {
			let tag = match lexer.next().map_err(Error::from_lexer)? {
				Meta(Some(Token::LangTag(tag)), _) => tag,
				_ => panic!("expected lang tag"),
			};

			let mut loc = string_loc.clone();
			loc.span_mut().append(tag_loc.span());
			Ok(Loc(
				crate::Literal::LangString(Loc(string, string_loc), Loc(tag, tag_loc)),
				loc,
			))
		}
		Meta(Some(Token::Carets), _) => {
			lexer.next().map_err(Error::from_lexer)?;
			match lexer.next().map_err(Error::from_lexer)? {
				Meta(Some(Token::Iri(iri)), iri_loc) => {
					let mut loc = string_loc.clone();
					loc.span_mut().append(iri_loc.span());
					Ok(Loc(
						crate::Literal::TypedString(Loc(string, string_loc), Loc(iri, iri_loc)),
						loc,
					))
				}
				Meta(unexpected, loc) => Err(Box::new(Loc(Error::Unexpected(unexpected), loc))),
			}
		}
		_ => Ok(Loc(
			crate::Literal::String(Loc(string, string_loc.clone())),
			string_loc,
		)),
	}
}

impl<F: Clone> Parse<F> for crate::Literal<Location<F>> {
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, BoxedError<L::Error, F>> {
		match lexer.next().map_err(Error::from_lexer)? {
			Meta(Some(Token::StringLiteral(string)), string_loc) => {
				parse_literal(lexer, string, string_loc)
			}
			Meta(unexpected, loc) => Err(Box::new(Loc(Error::Unexpected(unexpected), loc))),
		}
	}
}

impl<F: Clone> Parse<F> for crate::Object<Location<F>> {
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, BoxedError<L::Error, F>> {
		match lexer.next().map_err(Error::from_lexer)? {
			Meta(Some(Token::Iri(iri)), loc) => Ok(Loc(Self::Iri(iri), loc)),
			Meta(Some(Token::BlankNodeLabel(label)), loc) => Ok(Loc(Self::Blank(label), loc)),
			Meta(Some(Token::StringLiteral(string)), string_loc) => {
				let Meta(lit, loc) = parse_literal(lexer, string, string_loc)?;
				Ok(Loc(Self::Literal(lit), loc))
			}
			Meta(unexpected, loc) => Err(Box::new(Loc(Error::Unexpected(unexpected), loc))),
		}
	}
}

impl<F: Clone> Parse<F>
	for crate::Quad<
		Loc<crate::Subject, F>,
		Loc<crate::IriBuf, F>,
		Loc<crate::Object<Location<F>>, F>,
		Loc<crate::GraphLabel, F>,
	>
{
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, BoxedError<L::Error, F>> {
		let subject = crate::Subject::parse(lexer)?;
		let predicate = IriBuf::parse(lexer)?;
		let object = crate::Object::parse(lexer)?;
		let (graph, loc) = match lexer.next().map_err(Error::from_lexer)? {
			Meta(Some(Token::Dot), _) => (None, subject.location().clone().with(object.span())),
			opt_token => {
				let graph_label = match opt_token {
					Meta(Some(Token::Iri(iri)), loc) => Loc(crate::GraphLabel::Iri(iri), loc),
					Meta(Some(Token::BlankNodeLabel(label)), loc) => {
						Loc(crate::GraphLabel::Blank(label), loc)
					}
					Meta(unexpected, loc) => return Err(Box::new(Loc(Error::Unexpected(unexpected), loc))),
				};

				let loc = subject.location().clone().with(graph_label.span());
				match lexer.next().map_err(Error::from_lexer)? {
					Meta(Some(Token::Dot), _) => (Some(graph_label), loc),
					Meta(unexpected, loc) => return Err(Box::new(Loc(Error::Unexpected(unexpected), loc))),
				}
			}
		};

		Ok(Loc(crate::Quad(subject, predicate, object, graph), loc))
	}
}

impl<F: Clone> Parse<F>
	for crate::Quad<
		Loc<crate::Term<Location<F>>, F>,
		Loc<crate::Term<Location<F>>, F>,
		Loc<crate::Term<Location<F>>, F>,
		Loc<crate::Term<Location<F>>, F>,
	>
{
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, BoxedError<L::Error, F>> {
		let subject = crate::Term::parse(lexer)?;
		let predicate = crate::Term::parse(lexer)?;
		let object = crate::Term::parse(lexer)?;
		let (graph, loc) = match lexer.next().map_err(Error::from_lexer)? {
			Meta(Some(Token::Dot), _) => (None, subject.location().clone().with(object.span())),
			opt_token => {
				let graph_label = match opt_token {
					Meta(Some(Token::Iri(iri)), loc) => Loc(crate::Term::Iri(iri), loc),
					Meta(Some(Token::BlankNodeLabel(label)), loc) => {
						Loc(crate::Term::Blank(label), loc)
					}
					Meta(Some(Token::StringLiteral(string)), string_loc) => {
						let Meta(lit, lit_loc) = parse_literal(lexer, string, string_loc)?;
						Loc(crate::Term::Literal(lit), lit_loc)
					}
					Meta(unexpected, loc) => return Err(Box::new(Loc(Error::Unexpected(unexpected), loc))),
				};

				let loc = subject.location().clone().with(graph_label.span());
				match lexer.next().map_err(Error::from_lexer)? {
					Meta(Some(Token::Dot), _) => (Some(graph_label), loc),
					Meta(unexpected, loc) => return Err(Box::new(Loc(Error::Unexpected(unexpected), loc))),
				}
			}
		};

		Ok(Loc(crate::Quad(subject, predicate, object, graph), loc))
	}
}

impl<F: Clone> Parse<F> for crate::Document<F> {
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, BoxedError<L::Error, F>> {
		let mut quads = Vec::new();
		let mut loc: Option<Location<F>> = None;

		let loc = loop {
			match lexer.peek().map_err(Error::from_lexer)? {
				Meta(Some(_), quad_loc) => {
					quads.push(crate::Quad::parse(lexer)?);
					loc = match loc {
						Some(loc) => Some(loc.with(quad_loc.span())),
						None => Some(quad_loc),
					};
				}
				Meta(None, end_loc) => {
					break match loc {
						Some(loc) => loc,
						None => end_loc,
					}
				}
			}
		};

		Ok(Loc(quads, loc))
	}
}

impl<F: Clone> Parse<F> for crate::GrdfDocument<F> {
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, BoxedError<L::Error, F>> {
		let mut quads = Vec::new();
		let mut loc: Option<Location<F>> = None;

		let loc = loop {
			match lexer.peek().map_err(Error::from_lexer)? {
				Meta(Some(_), quad_loc) => {
					quads.push(crate::Quad::parse(lexer)?);
					loc = match loc {
						Some(loc) => Some(loc.with(quad_loc.span())),
						None => Some(quad_loc),
					};
				}
				Meta(None, end_loc) => {
					break match loc {
						Some(loc) => loc,
						None => end_loc,
					}
				}
			}
		};

		Ok(Loc(quads, loc))
	}
}
