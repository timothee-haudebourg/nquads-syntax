use locspan::Loc;
use std::path::Path;
use nquads_syntax::Parse;

fn infallible(c: char) -> Result<char, std::convert::Infallible> { Ok(c) }

fn parse<P: AsRef<Path>>(path: P) {
	stderrlog::new().init().ok();
	match std::fs::read_to_string(&path) {
		Ok(buffer) => {
			let mut lexer = nquads_syntax::Lexer::new(
				(),
				nquads_syntax::lexing::Utf8Decoded::new(buffer.chars().map(infallible)).peekable(),
			);

			match nquads_syntax::Document::parse(&mut lexer) {
				Ok(_) => (), // success!
				Err(Loc(e, _)) => {
					log::error!("parse error: {}", e);
					panic!("parse error: {:?}", e)
				}
			}
		}
		Err(e) => {
			log::error!("unable to read file `{}`: {}", path.as_ref().display(), e);
			panic!("IO error: {:?}", e)
		}
	}
}

#[test]
fn p01() { parse("tests/positive/01.nq") }

#[test]
fn p02() { parse("tests/positive/02.nq") }

#[test]
fn p03() { parse("tests/positive/03.nq") }