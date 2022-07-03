use iref::IriBuf;
use locspan::{Location, Span};

pub mod lexing;
pub mod parsing;

pub use lexing::Lexer;
pub use parsing::Parse;
pub use rdf_types::meta::{Literal, Object, Term};
pub use rdf_types::{BlankIdBuf, GraphLabel, Quad, StringLiteral, Subject};

/// Located quad.
pub type LocQuad<F, S = Span> = rdf_types::meta::MetaRdfQuad<Location<F, S>>;

/// Located gRDF quad.
pub type LocGrdfQuad<F, S = Span> = rdf_types::meta::MetaGrdfQuad<Location<F, S>>;

/// N-Quads document.
pub type Document<F, S = Span> = Vec<LocQuad<F, S>>;

/// gRDF N-Quads document.
pub type GrdfDocument<F, S = Span> = Vec<LocGrdfQuad<F, S>>;
