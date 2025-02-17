//! N-Quads is a line-based, plain text format for encoding an RDF dataset.
//! This library provides a [W3C Recommendation](https://www.w3.org/TR/n-quads/)
//! compliant parser that keeps track of the position of each syntax node in the
//! source file using the [locspan](https://crates.io/crates/locspan) library.
pub mod lexing;
mod parsing;

pub use lexing::LexingError;
pub use parsing::*;
pub use rdf_types::BlankIdBuf;
use rdf_types::{Quad, RdfQuad};

/// N-Quads document.
pub type Document = Vec<RdfQuad>;

/// gRDF N-Quads document.
pub type GrdfDocument = Vec<Quad>;

pub fn document_from_str(string: &str) -> Result<(Document, CodeMap), parsing::Error> {
	Document::parse_str(string)
}

pub fn grdf_document_from_str(string: &str) -> Result<(GrdfDocument, CodeMap), parsing::Error> {
	GrdfDocument::parse_str(string)
}
