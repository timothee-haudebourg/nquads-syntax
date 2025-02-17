use super::BlankIdBuf;
use decoded_char::DecodedChar;
use iref::IriBuf;
use langtag::LangTagBuf;
use locspan::{Span, Spanned};
use std::{fmt, iter::Peekable};

/// Lexing error.
#[derive(Debug)]
pub enum LexingError<E = std::convert::Infallible> {
	InvalidLangTag(Span),
	InvalidCodepoint(u32, Span),
	InvalidIriRef(String, Span),
	Unexpected(Option<char>, Span),
	Stream(E),
}

impl<E: fmt::Display> fmt::Display for LexingError<E> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::InvalidLangTag(_) => write!(f, "invalid language tag"),
			Self::InvalidCodepoint(c, _) => write!(f, "invalid character code point {c:x}"),
			Self::InvalidIriRef(iri_ref, _) => {
				write!(f, "invalid IRI reference <{iri_ref}>")
			}
			Self::Unexpected(None, _) => write!(f, "unexpected end of file"),
			Self::Unexpected(Some(c), _) => write!(f, "unexpected character `{c}`"),
			Self::Stream(e) => e.fmt(f),
		}
	}
}

impl<E: 'static + std::error::Error> std::error::Error for LexingError<E> {
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
	LangTag(LangTagBuf, Span),
	Iri(IriBuf, Span),
	StringLiteral(String, Span),
	BlankNodeLabel(BlankIdBuf, Span),
	Dot(Span),
	Carets(Span),
}

impl Spanned for Token {
	fn span(&self) -> Span {
		match self {
			Self::LangTag(_, s) => *s,
			Self::Iri(_, s) => *s,
			Self::StringLiteral(_, s) => *s,
			Self::BlankNodeLabel(_, s) => *s,
			Self::Dot(s) => *s,
			Self::Carets(s) => *s,
		}
	}
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::LangTag(tag, _) => write!(f, "language tag `{tag}`"),
			Self::Iri(iri, _) => write!(f, "IRI <{iri}>"),
			Self::StringLiteral(string, _) => {
				write!(f, "string literal \"{}\"", DisplayStringLiteral(string))
			}
			Self::BlankNodeLabel(label, _) => write!(f, "blank node label `{label}`"),
			Self::Dot(_) => write!(f, "dot `.`"),
			Self::Carets(_) => write!(f, "carets `^^`"),
		}
	}
}

/// Wrapper to display string literals.
pub struct DisplayStringLiteral<'a>(pub &'a str);

impl fmt::Display for DisplayStringLiteral<'_> {
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
	fn peek(&mut self) -> Result<Option<DecodedChar>, LexingError<E>> {
		match self.0.peek() {
			None => Ok(None),
			Some(Ok(c)) => Ok(Some(*c)),
			Some(Err(_)) => self.next(),
		}
	}

	fn next(&mut self) -> Result<Option<DecodedChar>, LexingError<E>> {
		self.0.next().transpose().map_err(LexingError::Stream)
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
		self.span.end.into()
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
	// lookahead: Option<Meta<Token, Span>>,
}

impl<C: Iterator<Item = Result<DecodedChar, E>>, E> Lexer<C, E> {
	pub fn new(chars: C) -> Self {
		Self {
			chars: Chars(chars.peekable()),
			pos: Position::default(),
		}
	}
}

impl<E, C: Iterator<Item = Result<DecodedChar, E>>> Lexer<C, E> {
	fn peek_decoded_char(&mut self) -> Result<Option<DecodedChar>, LexingError<E>> {
		self.chars.peek()
	}

	fn peek_char(&mut self) -> Result<Option<char>, LexingError<E>> {
		self.peek_decoded_char()
			.map(|c| c.map(DecodedChar::into_char))
	}

	fn next_char(&mut self) -> Result<Option<char>, LexingError<E>> {
		match self.chars.next()? {
			Some(c) => {
				self.pos.span.push(c.len());
				self.pos.last_span.clear();
				self.pos.last_span.push(c.len());
				Ok(Some(*c))
			}
			None => Ok(None),
		}
	}

	fn expect_char(&mut self) -> Result<char, LexingError<E>> {
		self.next_char()?
			.ok_or_else(|| LexingError::Unexpected(None, self.pos.last()))
	}

	fn skip_whitespaces(&mut self) -> Result<(), LexingError<E>> {
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
	fn next_comment(&mut self) -> Result<(), LexingError<E>> {
		loop {
			if matches!(self.next_char()?, None | Some('\n')) {
				break Ok(());
			}
		}
	}

	/// Parses the rest of a lang tag, after the first `@` character.
	fn next_langtag(&mut self) -> Result<(LangTagBuf, Span), LexingError<E>> {
		let mut tag = String::new();

		loop {
			match self.peek_char()? {
				None => {
					if tag.is_empty() {
						return Err(LexingError::InvalidLangTag(self.pos.current()));
					} else {
						break;
					}
				}
				Some(c) => {
					if c.is_ascii_alphabetic() {
						tag.push(self.expect_char()?);
					} else if c.is_whitespace() || c == '-' {
						if tag.is_empty() {
							return Err(LexingError::InvalidLangTag(self.pos.current()));
						} else {
							break;
						}
					} else {
						self.next_char()?;
						return Err(LexingError::Unexpected(Some(c), self.pos.last()));
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
								return Err(LexingError::InvalidLangTag(self.pos.current()));
							} else {
								break;
							}
						} else {
							self.next_char()?;
							return Err(LexingError::Unexpected(Some(c), self.pos.last()));
						}
					}
					None => {
						if empty_subtag {
							return Err(LexingError::InvalidLangTag(self.pos.current()));
						} else {
							break;
						}
					}
				}
			}
		}

		match LangTagBuf::new(tag) {
			Ok(tag) => Ok((tag, self.pos.current())),
			Err(_) => Err(LexingError::InvalidLangTag(self.pos.current())),
		}
	}

	/// Parses an IRI, starting after the first `<` until the closing `>`.
	fn next_iri(&mut self) -> Result<(IriBuf, Span), LexingError<E>> {
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
							return Err(LexingError::Unexpected(unexpected, self.pos.last()))
						}
					};

					iri.push(c)
				}
				Some(c) => {
					if matches!(
						c,
						'\u{00}'..='\u{20}' | '<' | '>' | '"' | '{' | '}' | '|' | '^' | '`' | '\\'
					) {
						return Err(LexingError::Unexpected(Some(c), self.pos.last()));
					}

					iri.push(c)
				}
				None => return Err(LexingError::Unexpected(None, self.pos.end())),
			}
		}

		match IriBuf::new(iri) {
			Ok(iri) => Ok((iri, self.pos.current())),
			Err(e) => Err(LexingError::InvalidIriRef(e.0, self.pos.current())),
		}
	}

	fn next_uchar(&mut self, mut span: Span, len: u8) -> Result<char, LexingError<E>> {
		let mut codepoint = 0;

		for _ in 0..len {
			let c = self.expect_char()?;
			match c.to_digit(16) {
				Some(d) => codepoint = codepoint << 4 | d,
				None => return Err(LexingError::Unexpected(Some(c), self.pos.last())),
			}
		}

		span.end = self.pos.current().end;

		match char::try_from(codepoint) {
			Ok(c) => Ok(c),
			Err(_) => Err(LexingError::InvalidCodepoint(codepoint, span)),
		}
	}

	/// Parses a string literal, starting after the first `"` until the closing `"`.
	fn next_string_literal(&mut self) -> Result<(String, Span), LexingError<E>> {
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
							return Err(LexingError::Unexpected(unexpected, self.pos.last()))
						}
					};

					string.push(c)
				}
				Some(c) => {
					if matches!(c, '\n' | '\r') {
						return Err(LexingError::Unexpected(Some(c), self.pos.last()));
					}

					string.push(c)
				}
				None => return Err(LexingError::Unexpected(None, self.pos.end())),
			}
		}

		Ok((string, self.pos.current()))
	}

	/// Parses a blank node label, starting after the first `_`.
	fn next_blank_node_label(&mut self) -> Result<(BlankIdBuf, Span), LexingError<E>> {
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
									return Err(LexingError::Unexpected(
										unexpected,
										self.pos.last(),
									))
								}
							}
						}

						Ok((
							unsafe { BlankIdBuf::new_unchecked(label) },
							self.pos.current(),
						))
					}
					unexpected => Err(LexingError::Unexpected(unexpected, self.pos.last())),
				}
			}
			unexpected => Err(LexingError::Unexpected(unexpected, self.pos.last())),
		}
	}
}

impl<E, C: Iterator<Item = Result<DecodedChar, E>>> Iterator for Lexer<C, E> {
	type Item = Result<Token, LexingError<E>>;

	fn next(&mut self) -> Option<Self::Item> {
		if let Err(e) = self.skip_whitespaces() {
			return Some(Err(e));
		}

		match self.next_char() {
			Ok(Some('@')) => match self.next_langtag() {
				Ok((tag, span)) => Some(Ok(Token::LangTag(tag, span))),
				Err(e) => Some(Err(e)),
			},
			Ok(Some('<')) => match self.next_iri() {
				Ok((iri, span)) => Some(Ok(Token::Iri(iri, span))),
				Err(e) => Some(Err(e)),
			},
			Ok(Some('"')) => match self.next_string_literal() {
				Ok((string, span)) => Some(Ok(Token::StringLiteral(string, span))),
				Err(e) => Some(Err(e)),
			},
			Ok(Some('_')) => match self.next_blank_node_label() {
				Ok((blank_id, span)) => Some(Ok(Token::BlankNodeLabel(blank_id, span))),
				Err(e) => Some(Err(e)),
			},
			Ok(Some('^')) => match self.next_char() {
				Ok(Some('^')) => Some(Ok(Token::Carets(self.pos.current()))),
				Ok(unexpected) => Some(Err(LexingError::Unexpected(unexpected, self.pos.last()))),
				Err(e) => Some(Err(e)),
			},
			Ok(Some('.')) => Some(Ok(Token::Dot(self.pos.current()))),
			Ok(Some(c)) => Some(Err(LexingError::Unexpected(Some(c), self.pos.last()))),
			Ok(None) => None,
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
