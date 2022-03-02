use iref::IriBuf;
use locspan::Loc;

pub mod lexing;
pub mod parsing;

pub use lexing::Lexer;
pub use parsing::Parse;
pub use rdf_types::loc::{Literal, Object};
pub use rdf_types::{BlankIdBuf, GraphLabel, Quad, StringLiteral, Subject};

/// Located quad.
pub type LocQuad<F> =
	Loc<Quad<Loc<Subject, F>, Loc<IriBuf, F>, Loc<Object<F>, F>, Loc<GraphLabel, F>>, F>;

/// N-Quads document.
pub type Document<F> = Vec<LocQuad<F>>;
