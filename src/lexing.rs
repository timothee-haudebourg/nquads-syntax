use super::BlankIdBuf;
use decoded_char::DecodedChar;
use iref::IriBuf;
use langtag::LangTagBuf;
use locspan::{ErrAt, Meta, Span};
use std::{fmt, iter::Peekable};

/// Fallible tokens iterator with lookahead.
pub trait Tokens {
	type Error;

	#[allow(clippy::type_complexity)]
	fn peek(&mut self) -> Result<Meta<Option<&Token>, Span>, Meta<Self::Error, Span>>;

	#[allow(clippy::type_complexity)]
	fn next(&mut self) -> Result<Meta<Option<Token>, Span>, Meta<Self::Error, Span>>;

	/// Begin a new span.
	///
	/// Skips white spaces and return an empty span at the cursor position.
	fn begin(&mut self) -> Result<Span, Meta<Self::Error, Span>>;

	/// Returns the span of the last parsed token.
	fn last(&self) -> Span;
}

/// Lexing error.
#[derive(Debug)]
pub enum Error<E = std::convert::Infallible> {
	InvalidLangTag,
	InvalidCodepoint(u32),
	InvalidIriRef(String),
	Unexpected(Option<char>),
	Stream(E),
}

impl<E: fmt::Display> fmt::Display for Error<E> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::InvalidLangTag => write!(f, "invalid language tag"),
			Self::InvalidCodepoint(c) => write!(f, "invalid character code point {c:x}"),
			Self::InvalidIriRef(iri_ref) => {
				write!(f, "invalid IRI reference <{iri_ref}>")
			}
			Self::Unexpected(None) => write!(f, "unexpected end of file"),
			Self::Unexpected(Some(c)) => write!(f, "unexpected character `{c}`"),
			Self::Stream(e) => e.fmt(f),
		}
	}
}

impl<E: 'static + std::error::Error> std::error::Error for Error<E> {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			Self::Stream(e) => Some(e),
			_ => None,
		}
	}
}

/// Token.
#[derive(Debug)]
pub enum Token {
	LangTag(LangTagBuf),
	Iri(IriBuf),
	StringLiteral(String),
	BlankNodeLabel(BlankIdBuf),
	Dot,
	Carets,
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::LangTag(tag) => write!(f, "language tag `{tag}`"),
			Self::Iri(iri) => write!(f, "IRI <{iri}>"),
			Self::StringLiteral(string) => {
				write!(f, "string literal \"{}\"", DisplayStringLiteral(string))
			}
			Self::BlankNodeLabel(label) => write!(f, "blank node label `{label}`"),
			Self::Dot => write!(f, "dot `.`"),
			Self::Carets => write!(f, "carets `^^`"),
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
				c => c.fmt(f),
			}?
		}

		Ok(())
	}
}

/// Characters iterator.
struct Chars<C: Iterator>(Peekable<C>);

impl<E, C: Iterator<Item = Result<DecodedChar, E>>> Chars<C> {
	fn peek(&mut self) -> Result<Option<DecodedChar>, Error<E>> {
		match self.0.peek() {
			None => Ok(None),
			Some(Ok(c)) => Ok(Some(*c)),
			Some(Err(_)) => self.next(),
		}
	}

	fn next(&mut self) -> Result<Option<DecodedChar>, Error<E>> {
		self.0.next().transpose().map_err(Error::Stream)
	}
}

/// Lexer position.
#[derive(Default)]
struct Position {
	span: Span,
	last_span: Span,
}

impl Position {
	fn current(&self) -> Span {
		self.span
	}

	fn end(&self) -> Span {
		self.span.end().into()
	}

	fn last(&self) -> Span {
		self.last_span
	}
}

/// Lexer.
///
/// Changes a character iterator into a `Token` iterator.
pub struct Lexer<C: Iterator<Item = Result<DecodedChar, E>>, E> {
	chars: Chars<C>,
	pos: Position,
	lookahead: Option<Meta<Token, Span>>,
}

impl<C: Iterator<Item = Result<DecodedChar, E>>, E> Lexer<C, E> {
	pub fn new(chars: C) -> Self {
		Self {
			chars: Chars(chars.peekable()),
			pos: Position::default(),
			lookahead: None,
		}
	}
}

impl<E, C: Iterator<Item = Result<DecodedChar, E>>> Lexer<C, E> {
	fn peek_decoded_char(&mut self) -> Result<Option<DecodedChar>, Meta<Error<E>, Span>> {
		self.chars.peek().err_at(|| self.pos.end())
	}

	fn peek_char(&mut self) -> Result<Option<char>, Meta<Error<E>, Span>> {
		self.peek_decoded_char()
			.map(|c| c.map(DecodedChar::into_char))
	}

	fn next_char(&mut self) -> Result<Option<char>, Meta<Error<E>, Span>> {
		match self.chars.next().err_at(|| self.pos.end())? {
			Some(c) => {
				self.pos.span.push(c.len());
				self.pos.last_span.clear();
				self.pos.last_span.push(c.len());
				Ok(Some(*c))
			}
			None => Ok(None),
		}
	}

	fn expect_char(&mut self) -> Result<char, Meta<Error<E>, Span>> {
		self.next_char()?
			.ok_or_else(|| Meta(Error::Unexpected(None), self.pos.end()))
	}

	fn skip_whitespaces(&mut self) -> Result<(), Meta<Error<E>, Span>> {
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
	fn next_comment(&mut self) -> Result<(), Meta<Error<E>, Span>> {
		loop {
			if matches!(self.next_char()?, None | Some('\n')) {
				break Ok(());
			}
		}
	}

	/// Parses the rest of a lang tag, after the first `@` character.
	fn next_langtag(&mut self) -> Result<Meta<LangTagBuf, Span>, Meta<Error<E>, Span>> {
		let mut tag = String::new();

		loop {
			match self.peek_char()? {
				None => {
					if tag.is_empty() {
						return Err(Meta(Error::InvalidLangTag, self.pos.current()));
					} else {
						break;
					}
				}
				Some(c) => {
					if c.is_ascii_alphabetic() {
						tag.push(self.expect_char()?);
					} else if c.is_whitespace() || c == '-' {
						if tag.is_empty() {
							return Err(Meta(Error::InvalidLangTag, self.pos.current()));
						} else {
							break;
						}
					} else {
						self.next_char()?;
						return Err(Meta(Error::Unexpected(Some(c)), self.pos.last()));
					}
				}
			}
		}

		let mut empty_subtag = true;
		if let Some('-') = self.peek_char()? {
			tag.push(self.expect_char()?);
			loop {
				match self.peek_char()? {
					Some('-') if !empty_subtag => tag.push(self.expect_char()?),
					Some(c) if c.is_ascii_alphanumeric() => {
						empty_subtag = false;
						tag.push(self.expect_char()?)
					}
					Some(c) => {
						if c.is_whitespace() {
							if empty_subtag {
								return Err(Meta(Error::InvalidLangTag, self.pos.current()));
							} else {
								break;
							}
						} else {
							self.next_char()?;
							return Err(Meta(Error::Unexpected(Some(c)), self.pos.last()));
						}
					}
					None => {
						if empty_subtag {
							return Err(Meta(Error::InvalidLangTag, self.pos.current()));
						} else {
							break;
						}
					}
				}
			}
		}

		match LangTagBuf::new(tag) {
			Ok(tag) => Ok(Meta(tag, self.pos.current())),
			Err(_) => Err(Meta(Error::InvalidLangTag, self.pos.current())),
		}
	}

	/// Parses an IRI, starting after the first `<` until the closing `>`.
	fn next_iri(&mut self) -> Result<Meta<IriBuf, Span>, Meta<Error<E>, Span>> {
		let mut iri = String::new();

		loop {
			match self.next_char()? {
				Some('>') => break,
				Some('\\') => {
					let span = self.pos.last();
					let c = match self.next_char()? {
						Some('u') => self.next_uchar(span, 4)?,
						Some('U') => self.next_uchar(span, 8)?,
						unexpected => {
							return Err(Meta(Error::Unexpected(unexpected), self.pos.last()))
						}
					};

					iri.push(c)
				}
				Some(c) => {
					if matches!(
						c,
						'\u{00}'..='\u{20}' | '<' | '>' | '"' | '{' | '}' | '|' | '^' | '`' | '\\'
					) {
						return Err(Meta(Error::Unexpected(Some(c)), self.pos.last()));
					}

					iri.push(c)
				}
				None => return Err(Meta(Error::Unexpected(None), self.pos.end())),
			}
		}

		match IriBuf::new(iri) {
			Ok(iri) => Ok(Meta(iri, self.pos.current())),
			Err(e) => Err(Meta(Error::InvalidIriRef(e.0), self.pos.current())),
		}
	}

	fn next_uchar(&mut self, mut span: Span, len: u8) -> Result<char, Meta<Error<E>, Span>> {
		let mut codepoint = 0;

		for _ in 0..len {
			let c = self.expect_char()?;
			match c.to_digit(16) {
				Some(d) => codepoint = codepoint << 4 | d,
				None => return Err(Meta(Error::Unexpected(Some(c)), self.pos.last())),
			}
		}

		span.set_end(self.pos.current().end());
		match char::try_from(codepoint) {
			Ok(c) => Ok(c),
			Err(_) => Err(Meta(Error::InvalidCodepoint(codepoint), span)),
		}
	}

	/// Parses a string literal, starting after the first `"` until the closing `"`.
	fn next_string_literal(&mut self) -> Result<Meta<String, Span>, Meta<Error<E>, Span>> {
		let mut string = String::new();

		loop {
			match self.next_char()? {
				Some('"') => break,
				Some('\\') => {
					let span = self.pos.last();
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
							return Err(Meta(Error::Unexpected(unexpected), self.pos.last()))
						}
					};

					string.push(c)
				}
				Some(c) => {
					if matches!(c, '\n' | '\r') {
						return Err(Meta(Error::Unexpected(Some(c)), self.pos.last()));
					}

					string.push(c)
				}
				None => return Err(Meta(Error::Unexpected(None), self.pos.end())),
			}
		}

		Ok(Meta(string, self.pos.current()))
	}

	/// Parses a blank node label, starting after the first `_`.
	fn next_blank_node_label(&mut self) -> Result<Meta<BlankIdBuf, Span>, Meta<Error<E>, Span>> {
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
									return Err(Meta(
										Error::Unexpected(unexpected),
										self.pos.last(),
									))
								}
							}
						}

						Ok(Meta(
							unsafe { BlankIdBuf::new_unchecked(label) },
							self.pos.current(),
						))
					}
					unexpected => Err(Meta(Error::Unexpected(unexpected), self.pos.last())),
				}
			}
			unexpected => Err(Meta(Error::Unexpected(unexpected), self.pos.last())),
		}
	}

	pub fn consume(&mut self) -> Result<Meta<Option<Token>, Span>, Meta<Error<E>, Span>> {
		self.skip_whitespaces()?;
		match self.next_char()? {
			Some('@') => Ok(self.next_langtag()?.map(|t| Some(Token::LangTag(t)))),
			Some('<') => Ok(self.next_iri()?.map(|t| Some(Token::Iri(t)))),
			Some('"') => Ok(self
				.next_string_literal()?
				.map(|t| Some(Token::StringLiteral(t)))),
			Some('_') => Ok(self
				.next_blank_node_label()?
				.map(|t| Some(Token::BlankNodeLabel(t)))),
			Some('.') => Ok(Meta(Some(Token::Dot), self.pos.current())),
			Some('^') => match self.next_char()? {
				Some('^') => Ok(Meta(Some(Token::Carets), self.pos.current())),
				unexpected => Err(Meta(Error::Unexpected(unexpected), self.pos.last())),
			},
			None => Ok(Meta(None, self.pos.end())),
			unexpected => Err(Meta(Error::Unexpected(unexpected), self.pos.last())),
		}
	}

	#[allow(clippy::type_complexity)]
	pub fn peek(&mut self) -> Result<Meta<Option<&Token>, Span>, Meta<Error<E>, Span>> {
		if self.lookahead.is_none() {
			if let Meta(Some(token), span) = self.consume()? {
				self.lookahead = Some(Meta::new(token, span));
			}
		}

		match &self.lookahead {
			Some(Meta(token, span)) => Ok(Meta::new(Some(token), *span)),
			None => Ok(Meta::new(None, self.pos.end())),
		}
	}

	#[allow(clippy::type_complexity, clippy::should_implement_trait)]
	pub fn next(&mut self) -> Result<Meta<Option<Token>, Span>, Meta<Error<E>, Span>> {
		match self.lookahead.take() {
			Some(Meta(token, span)) => Ok(Meta::new(Some(token), span)),
			None => self.consume(),
		}
	}
}

impl<E, C: Iterator<Item = Result<DecodedChar, E>>> Tokens for Lexer<C, E> {
	type Error = Error<E>;

	fn peek(&mut self) -> Result<Meta<Option<&Token>, Span>, Meta<Error<E>, Span>> {
		self.peek()
	}

	fn next(&mut self) -> Result<Meta<Option<Token>, Span>, Meta<Error<E>, Span>> {
		self.next()
	}

	fn begin(&mut self) -> Result<Span, Meta<Error<E>, Span>> {
		self.skip_whitespaces()?;
		Ok(self.pos.current())
	}

	fn last(&self) -> Span {
		self.pos.last_span
	}
}

impl<E, C: Iterator<Item = Result<DecodedChar, E>>> Iterator for Lexer<C, E> {
	type Item = Result<Meta<Token, Span>, Meta<Error<E>, Span>>;

	fn next(&mut self) -> Option<Self::Item> {
		match self.next() {
			Ok(Meta(Some(token), loc)) => Some(Ok(Meta::new(token, loc))),
			Ok(Meta(None, _)) => None,
			Err(e) => Some(Err(e)),
		}
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
