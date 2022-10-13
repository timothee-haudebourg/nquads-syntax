use nquads_syntax::Parse;
use std::path::Path;

fn infallible(c: char) -> Result<char, std::convert::Infallible> {
	Ok(c)
}

fn parse<P: AsRef<Path>>(path: P) {
	stderrlog::new().init().ok();
	match std::fs::read_to_string(&path) {
		Ok(buffer) => {
			let mut lexer = nquads_syntax::Lexer::new(
				(),
				nquads_syntax::lexing::Utf8Decoded::new(buffer.chars().map(infallible)).peekable(),
			);

			match nquads_syntax::GrdfDocument::parse(&mut lexer) {
				Ok(_) => (), // success!
				Err(e) => {
					log::error!("parse error: {}", e.value());
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
fn pg01() {
	parse("tests/positive/grdf/01.nq")
}
