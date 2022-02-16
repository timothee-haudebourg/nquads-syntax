use iref::IriRefBuf;
use langtag::LanguageTagBuf;
use locspan::Loc;

pub mod lexing;
mod parsing;

/// Subject.
pub enum Subject {
	IriRef(IriRefBuf),
	Blank(String),
}

/// Literal value.
pub enum Literal<F> {
	String(Loc<String, F>),
	Typed(Loc<String, F>, Loc<IriRefBuf, F>),
	LangString(Loc<String, F>, Loc<LanguageTagBuf, F>),
}

/// Object.
pub enum Object<F> {
	IriRef(IriRefBuf),
	Blank(String),
	Literal(Literal<F>),
}

/// Graph Label.
pub enum GraphLabel {
	IriRef(IriRefBuf),
	Blank(String),
}

/// RDF Quad.
pub struct Quad<F> {
	pub subject: Loc<Subject, F>,
	pub predicate: Loc<IriRefBuf, F>,
	pub object: Loc<Object<F>, F>,
	pub graph: Option<Loc<GraphLabel, F>>,
}

/// N-Quads document.
pub struct Document<F> {
	pub quads: Vec<Loc<Quad<F>, F>>,
}
