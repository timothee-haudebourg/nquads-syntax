use iref::IriRefBuf;
use langtag::LanguageTagBuf;
use locspan::Loc;

pub mod lexing;
mod parsing;

/// Subject.
#[derive(Clone, Debug)]
pub enum Subject {
	IriRef(IriRefBuf),
	Blank(String),
}

/// Literal value.
#[derive(Clone, Debug)]
pub enum Literal<F> {
	String(Loc<String, F>),
	TypedString(Loc<String, F>, Loc<IriRefBuf, F>),
	LangString(Loc<String, F>, Loc<LanguageTagBuf, F>),
}

/// Object.
#[derive(Clone, Debug)]
pub enum Object<F> {
	IriRef(IriRefBuf),
	Blank(String),
	Literal(Literal<F>),
}

/// Graph Label.
#[derive(Clone, Debug)]
pub enum GraphLabel {
	IriRef(IriRefBuf),
	Blank(String),
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
