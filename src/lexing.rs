use iref::IriRefBuf;
use langtag::LanguageTagBuf;
use locspan::{Loc, Location, Span};
use std::fmt;
use std::iter::Peekable;

/// Decoded character, with its encoded byte length.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct DecodedChar {
	c: char,
	len: usize,
}

impl DecodedChar {
	/// Creates a new decoded character.
	#[inline(always)]
	pub fn new(c: char, len: usize) -> DecodedChar {
		Self { c, len }
	}

	/// Unwraps and returns the character.
	#[inline(always)]
	pub fn unwrap(self) -> char {
		self.c
	}

	/// Returns the original encoded byte length of the character.
	#[inline(always)]
	#[allow(clippy::len_without_is_empty)]
	pub fn len(&self) -> usize {
		self.len
	}
}

impl From<DecodedChar> for char {
	fn from(e: DecodedChar) -> Self {
		e.c
	}
}

impl std::ops::Deref for DecodedChar {
	type Target = char;

	fn deref(&self) -> &char {
		&self.c
	}
}

/// Stream of token, with lookahead.
pub trait Tokens<F> {
	type Error: fmt::Debug;

	#[allow(clippy::type_complexity)]
	fn peek(&mut self) -> Result<Loc<Option<&Token>, F>, Loc<Self::Error, F>>;

	#[allow(clippy::type_complexity)]
	fn next(&mut self) -> Result<Loc<Option<Token>, F>, Loc<Self::Error, F>>;
}

/// Lexing error.
#[derive(Debug)]
pub enum Error<E> {
	InvalidLangTag,
	InvalidCodepoint(u32),
	InvalidIriRef(iref::Error, String),
	Unexpected(Option<char>),
	Stream(E),
}

impl<E: fmt::Display> fmt::Display for Error<E> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::InvalidLangTag => write!(f, "invalid language tag"),
			Self::InvalidCodepoint(c) => write!(f, "invalid character code point {:x}", c),
			Self::InvalidIriRef(e, iri_ref) => write!(f, "invalid IRI reference <{}>: {}", iri_ref, e),
			Self::Unexpected(None) => write!(f, "unexpected end of file"),
			Self::Unexpected(Some(c)) => write!(f, "unexpected character `{}`", c),
			Self::Stream(e) => e.fmt(f)
		}
	}
}

impl<E: 'static + std::error::Error> std::error::Error for Error<E> {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			Self::InvalidIriRef(e, _) => Some(e),
			Self::Stream(e) => Some(e),
			_ => None
		}
	}
}

/// Token.
#[derive(Debug)]
pub enum Token {
	LangTag(LanguageTagBuf),
	IriRef(IriRefBuf),
	StringLiteral(String),
	BlankNodeLabel(String),
	Dot,
	Carets,
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::LangTag(tag) => write!(f, "language tag `{}`", tag),
			Self::IriRef(iri_ref) => write!(f, "IRI reference <{}>", iri_ref),
			Self::StringLiteral(string) => write!(f, "string literal \"{}\"", DisplayStringLiteral(string)),
			Self::BlankNodeLabel(label) => write!(f, "blank node label `{}`", label),
			Self::Dot => write!(f, "dot `.`"),
			Self::Carets => write!(f, "carets `^^`")
		}
	}
}

/// Wrapper to display string literals.
pub struct DisplayStringLiteral<'a>(pub &'a str);

impl<'a> fmt::Display for DisplayStringLiteral<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for c in self.0.chars() {
			match c {
				'"' => write!(f, "\\u0022"),
				'\\' => write!(f, "\\u005c"),
				'\n' => write!(f, "\\n"),
				'\r' => write!(f, "\\r"),
				'\t' => write!(f, "\\t"),
				'\u{08}' => write!(f, "\\b"),
				'\u{0c}' => write!(f, "\\f"),
				c => c.fmt(f)
			}?
		}

		Ok(())
	}
}

/// Lexer position.
struct Position<F> {
	file: F,
	span: Span,
	last_span: Span,
}

impl<F: Clone> Position<F> {
	fn current(&self) -> Location<F> {
		Location::new(self.file.clone(), self.span)
	}

	fn current_span(&self) -> Span {
		self.span
	}

	fn end(&self) -> Location<F> {
		Location::new(self.file.clone(), self.span.end())
	}

	fn last(&self) -> Location<F> {
		Location::new(self.file.clone(), self.last_span)
	}

	fn last_span(&self) -> Span {
		self.last_span
	}
}

/// Lexer.
///
/// Changes a character iterator into a `Token` iterator.
pub struct Lexer<F, E, C: Iterator<Item = Result<DecodedChar, E>>> {
	chars: Peekable<C>,
	pos: Position<F>,
	lookahead: Option<Loc<Token, F>>,
}

impl<F, E, C: Iterator<Item = Result<DecodedChar, E>>> Lexer<F, E, C> {
	pub fn new(file: F, chars: Peekable<C>) -> Self {
		Self {
			chars,
			pos: Position {
				file,
				span: Span::default(),
				last_span: Span::default(),
			},
			lookahead: None,
		}
	}
}

impl<F: Clone, E, C: Iterator<Item = Result<DecodedChar, E>>> Lexer<F, E, C> {
	fn peek_char(&mut self) -> Result<Option<char>, Loc<Error<E>, F>> {
		match self.chars.peek() {
			None => Ok(None),
			Some(Ok(c)) => Ok(Some(c.unwrap())),
			Some(Err(_)) => self.next_char(),
		}
	}

	fn next_char(&mut self) -> Result<Option<char>, Loc<Error<E>, F>> {
		match self.chars.next() {
			None => Ok(None),
			Some(Ok(c)) => {
				self.pos.span.push(c.len());
				self.pos.last_span.clear();
				self.pos.last_span.push(c.len());
				Ok(Some(c.unwrap()))
			}
			Some(Err(e)) => Err(Loc(Error::Stream(e), self.pos.end())),
		}
	}

	fn expect_char(&mut self) -> Result<char, Loc<Error<E>, F>> {
		self.next_char()?
			.ok_or_else(|| Loc(Error::Unexpected(None), self.pos.end()))
	}

	fn skip_whitespaces(&mut self) -> Result<(), Loc<Error<E>, F>> {
		while let Some(c) = self.peek_char()? {
			if c.is_whitespace() {
				self.next_char()?;
			} else if c == '#' {
				self.next_comment()?;
			} else {
				break;
			}
		}

		self.pos.span.clear();
		Ok(())
	}

	/// Parses the rest of a comment, after the first `#` character.
	///
	/// Comments in N-Quads take the form of `#`,
	/// outside an IRIREF or STRING_LITERAL_QUOTE,
	/// and continue to the end of line (EOL) or end of file
	/// if there is no end of line after the comment marker.
	fn next_comment(&mut self) -> Result<(), Loc<Error<E>, F>> {
		loop {
			if matches!(self.next_char()?, None | Some('\n')) {
				break Ok(());
			}
		}
	}

	/// Parses the rest of a lang tag, after the first `@` character.
	fn next_langtag(&mut self) -> Result<Loc<LanguageTagBuf, F>, Loc<Error<E>, F>> {
		let mut tag = String::new();

		loop {
			match self.peek_char()? {
				None => {
					if tag.is_empty() {
						return Err(Loc(Error::InvalidLangTag, self.pos.current()));
					} else {
						break;
					}
				}
				Some(c) => {
					if c.is_ascii_alphabetic() {
						tag.push(self.expect_char()?);
					} else {
						return Err(Loc(Error::Unexpected(Some(c)), self.pos.last()));
					}
				}
			}
		}

		while let Some('-') = self.peek_char()? {
			loop {
				match self.peek_char()? {
					None => {
						if tag.is_empty() {
							return Err(Loc(Error::InvalidLangTag, self.pos.current()));
						} else {
							break;
						}
					}
					Some(c) => {
						if c.is_ascii_alphanumeric() {
							tag.push(self.expect_char()?);
						} else {
							return Err(Loc(Error::Unexpected(Some(c)), self.pos.last()));
						}
					}
				}
			}
		}

		match LanguageTagBuf::new(tag.into_bytes()) {
			Ok(tag) => Ok(Loc(tag, self.pos.current())),
			Err(_) => Err(Loc(Error::InvalidLangTag, self.pos.current())),
		}
	}

	/// Parses an IRI reference, starting after the first `<` until the closing `>`.
	fn next_iriref(&mut self) -> Result<Loc<IriRefBuf, F>, Loc<Error<E>, F>> {
		let mut iriref = String::new();

		loop {
			match self.next_char()? {
				Some('>') => break,
				Some('\\') => {
					let span = self.pos.last_span();
					let c = match self.next_char()? {
						Some('u') => self.next_uchar(span, 4)?,
						Some('U') => self.next_uchar(span, 8)?,
						unexpected => {
							return Err(Loc(Error::Unexpected(unexpected), self.pos.last()))
						}
					};

					iriref.push(c)
				}
				Some(c) => {
					if matches!(
						c,
						'\u{00}'..='\u{20}' | '<' | '>' | '"' | '{' | '}' | '|' | '^' | '`' | '\\'
					) {
						return Err(Loc(Error::Unexpected(Some(c)), self.pos.last()));
					}

					iriref.push(c)
				}
				None => return Err(Loc(Error::Unexpected(None), self.pos.end())),
			}
		}

		match IriRefBuf::from_string(iriref) {
			Ok(iriref) => Ok(Loc(iriref, self.pos.current())),
			Err((e, string)) => Err(Loc(Error::InvalidIriRef(e, string), self.pos.current())),
		}
	}

	fn next_uchar(&mut self, mut span: Span, len: u8) -> Result<char, Loc<Error<E>, F>> {
		let mut codepoint = 0;

		for _ in 0..len {
			let c = self.expect_char()?;
			match c.to_digit(16) {
				Some(d) => codepoint = codepoint << 4 | d,
				None => return Err(Loc(Error::Unexpected(Some(c)), self.pos.last())),
			}
		}

		span.set_end(self.pos.current_span().end());
		match char::try_from(codepoint) {
			Ok(c) => Ok(c),
			Err(_) => Err(Loc(
				Error::InvalidCodepoint(codepoint),
				Location::new(self.pos.file.clone(), span),
			)),
		}
	}

	/// Parses a string literal, starting after the first `"` until the closing `"`.
	fn next_string_literal(&mut self) -> Result<Loc<String, F>, Loc<Error<E>, F>> {
		let mut string = String::new();

		loop {
			match self.next_char()? {
				Some('"') => break,
				Some('\\') => {
					let span = self.pos.last_span();
					let c = match self.next_char()? {
						Some('u') => self.next_uchar(span, 4)?,
						Some('U') => self.next_uchar(span, 8)?,
						Some('t') => '\t',
						Some('b') => '\u{08}',
						Some('n') => '\n',
						Some('r') => '\r',
						Some('f') => '\u{0c}',
						Some('\'') => '\'',
						Some('"') => '"',
						Some('\\') => '\\',
						unexpected => {
							return Err(Loc(Error::Unexpected(unexpected), self.pos.last()))
						}
					};

					string.push(c)
				}
				Some(c) => {
					if matches!(c, '\n' | '\r') {
						return Err(Loc(Error::Unexpected(Some(c)), self.pos.last()));
					}

					string.push(c)
				}
				None => return Err(Loc(Error::Unexpected(None), self.pos.end())),
			}
		}

		Ok(Loc(string, self.pos.current()))
	}

	/// Parses a blank node label, starting after the first `_`.
	fn next_blank_node_label(&mut self) -> Result<Loc<String, F>, Loc<Error<E>, F>> {
		match self.next_char()? {
			Some(':') => {
				let mut label = String::new();
				label.push('_');
				label.push(':');
				match self.next_char()? {
					Some(c) if c.is_ascii_digit() || is_pn_chars_u(c) => {
						label.push(c);
						let mut last_is_pn_chars = true;
						loop {
							match self.peek_char()? {
								Some(c) if is_pn_chars(c) => {
									label.push(self.expect_char()?);
									last_is_pn_chars = true
								}
								Some('.') => {
									label.push(self.expect_char()?);
									last_is_pn_chars = false;
								}
								_ if last_is_pn_chars => break,
								unexpected => {
									return Err(Loc(Error::Unexpected(unexpected), self.pos.last()))
								}
							}
						}

						Ok(Loc(label, self.pos.current()))
					}
					unexpected => Err(Loc(Error::Unexpected(unexpected), self.pos.last())),
				}
			}
			unexpected => Err(Loc(Error::Unexpected(unexpected), self.pos.last())),
		}
	}

	pub fn consume(&mut self) -> Result<Loc<Option<Token>, F>, Loc<Error<E>, F>> {
		self.skip_whitespaces()?;
		match self.next_char()? {
			Some('@') => Ok(self.next_langtag()?.map(|t| Some(Token::LangTag(t)))),
			Some('<') => Ok(self.next_iriref()?.map(|t| Some(Token::IriRef(t)))),
			Some('"') => Ok(self
				.next_string_literal()?
				.map(|t| Some(Token::StringLiteral(t)))),
			Some('_') => Ok(self
				.next_blank_node_label()?
				.map(|t| Some(Token::BlankNodeLabel(t)))),
			Some('.') => Ok(Loc(Some(Token::Dot), self.pos.current())),
			Some('^') => match self.next_char()? {
				Some('^') => Ok(Loc(Some(Token::Carets), self.pos.current())),
				unexpected => Err(Loc(Error::Unexpected(unexpected), self.pos.last())),
			},
			None => Ok(Loc(None, self.pos.end())),
			unexpected => Err(Loc(Error::Unexpected(unexpected), self.pos.last())),
		}
	}

	#[allow(clippy::type_complexity)]
	pub fn peek(&mut self) -> Result<Loc<Option<&Token>, F>, Loc<Error<E>, F>> {
		if self.lookahead.is_none() {
			if let locspan::Loc(Some(token), loc) = self.consume()? {
				self.lookahead = Some(Loc::new(token, loc));
			}
		}

		match &self.lookahead {
			Some(locspan::Loc(token, loc)) => Ok(Loc::new(Some(token), loc.clone())),
			None => Ok(Loc::new(None, self.pos.end())),
		}
	}

	#[allow(clippy::type_complexity, clippy::should_implement_trait)]
	pub fn next(&mut self) -> Result<Loc<Option<Token>, F>, Loc<Error<E>, F>> {
		match self.lookahead.take() {
			Some(locspan::Loc(token, loc)) => Ok(Loc::new(Some(token), loc)),
			None => self.consume(),
		}
	}
}

impl<F: Clone, E: fmt::Debug, C: Iterator<Item = Result<DecodedChar, E>>> Tokens<F>
	for Lexer<F, E, C>
{
	type Error = Error<E>;

	fn peek(&mut self) -> Result<Loc<Option<&Token>, F>, Loc<Error<E>, F>> {
		self.peek()
	}

	fn next(&mut self) -> Result<Loc<Option<Token>, F>, Loc<Error<E>, F>> {
		self.next()
	}
}

fn is_pn_chars_base(c: char) -> bool {
	matches!(c, 'A'..='Z' | 'a'..='z' | '\u{00c0}'..='\u{00d6}' | '\u{00d8}'..='\u{00f6}' | '\u{00f8}'..='\u{02ff}' | '\u{0370}'..='\u{037d}' | '\u{037f}'..='\u{1fff}' | '\u{200c}'..='\u{200d}' | '\u{2070}'..='\u{218f}' | '\u{2c00}'..='\u{2fef}' | '\u{3001}'..='\u{d7ff}' | '\u{f900}'..='\u{fdcf}' | '\u{fdf0}'..='\u{fffd}' | '\u{10000}'..='\u{effff}')
}

fn is_pn_chars_u(c: char) -> bool {
	is_pn_chars_base(c) || matches!(c, '_' | ':')
}

fn is_pn_chars(c: char) -> bool {
	is_pn_chars_u(c)
		|| matches!(c, '-' | '0'..='9' | '\u{00b7}' | '\u{0300}'..='\u{036f}' | '\u{203f}'..='\u{2040}')
}
