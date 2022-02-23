use iref::IriRefBuf;
use locspan::Loc;

pub mod lexing;
pub mod parsing;

pub use parsing::Parse;
pub use rdf_types::loc::Literal;
pub use rdf_types::{BlankIdBuf, StringLiteral};

/// Subject.
#[derive(Clone, Debug)]
pub enum Subject {
	IriRef(IriRefBuf),
	Blank(BlankIdBuf),
}

/// Object.
#[derive(Clone, Debug)]
pub enum Object<F> {
	IriRef(IriRefBuf),
	Blank(BlankIdBuf),
	Literal(Literal<F>),
}

/// Graph Label.
#[derive(Clone, Debug)]
pub enum GraphLabel {
	IriRef(IriRefBuf),
	Blank(BlankIdBuf),
}

/// RDF Quad.
#[derive(Clone, Debug)]
pub struct Quad<F> {
	pub subject: Loc<Subject, F>,
	pub predicate: Loc<IriRefBuf, F>,
	pub object: Loc<Object<F>, F>,
	pub graph: Option<Loc<GraphLabel, F>>,
}

/// N-Quads document.
#[derive(Clone, Debug)]
pub struct Document<F> {
	pub quads: Vec<Loc<Quad<F>, F>>,
}

impl<F> Document<F> {
	pub fn iter(&self) -> std::slice::Iter<Loc<Quad<F>, F>> {
		self.quads.iter()
	}
}

impl<'a, F> IntoIterator for &'a Document<F> {
	type Item = &'a Loc<Quad<F>, F>;
	type IntoIter = std::slice::Iter<'a, Loc<Quad<F>, F>>;

	fn into_iter(self) -> Self::IntoIter {
		self.quads.iter()
	}
}

impl<F> IntoIterator for Document<F> {
	type Item = Loc<Quad<F>, F>;
	type IntoIter = std::vec::IntoIter<Loc<Quad<F>, F>>;

	fn into_iter(self) -> Self::IntoIter {
		self.quads.into_iter()
	}
}
