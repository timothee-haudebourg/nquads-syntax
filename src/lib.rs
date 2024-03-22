//! N-Quads is a line-based, plain text format for encoding an RDF dataset.
//! This library provides a [W3C Recommendation](https://www.w3.org/TR/n-quads/)
//! compliant parser that keeps track of the position of each syntax node in the
//! source file using the [locspan](https://crates.io/crates/locspan) library.
use iref::IriBuf;

pub mod lexing;
pub mod parsing;

pub use lexing::Lexer;
use locspan::{Meta, Span};
pub use parsing::Parse;
pub use rdf_types::{BlankIdBuf, GraphLabel, Subject};
use rdf_types::{Id, Object, Term};

pub type Quad =
	rdf_types::Quad<Meta<Id, Span>, Meta<IriBuf, Span>, Meta<Object, Span>, Meta<GraphLabel, Span>>;

pub type GrdfQuad = rdf_types::Quad<Meta<Term, Span>>;

/// N-Quads document.
pub type Document = Vec<Meta<Quad, Span>>;

/// gRDF N-Quads document.
pub type GrdfDocument = Vec<Meta<GrdfQuad, Span>>;

/// Strips all the metadata from the given quad.
#[allow(clippy::type_complexity)]
pub fn strip_quad<S, P, O, G, M>(
	quad: rdf_types::Quad<Meta<S, M>, Meta<P, M>, Meta<O, M>, Meta<G, M>>,
) -> rdf_types::Quad<S, P, O, G> {
	quad.map_all(Meta::into_value, Meta::into_value, Meta::into_value, |g| {
		g.map(Meta::into_value)
	})
}
