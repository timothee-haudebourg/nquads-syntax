use iref::IriBuf;

pub mod lexing;
pub mod parsing;

pub use lexing::Lexer;
pub use parsing::Parse;
pub use rdf_types::meta::{Literal, Object, Term};
use rdf_types::meta::{MetaGrdfQuad, MetaRdfQuad};
pub use rdf_types::{BlankIdBuf, GraphLabel, Quad, StringLiteral, Subject};

/// N-Quads document.
pub type Document<M> = Vec<MetaRdfQuad<M>>;

/// gRDF N-Quads document.
pub type GrdfDocument<M> = Vec<MetaGrdfQuad<M>>;
