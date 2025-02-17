use nquads_syntax::Parse;
use std::path::Path;

fn parse<P: AsRef<Path>>(path: P) {
	stderrlog::new().init().ok();
	match std::fs::read_to_string(&path) {
		Ok(buffer) => {
			match nquads_syntax::Document::parse_str(&buffer) {
				Ok(_) => (), // success!
				Err(e) => {
					log::error!("parse error: {e}");
					panic!("parse error: {e:?}")
				}
			}
		}
		Err(e) => {
			log::error!("unable to read file `{}`: {}", path.as_ref().display(), e);
			panic!("IO error: {e:?}")
		}
	}
}

#[test]
fn p01() {
	parse("tests/positive/01.nq")
}

#[test]
fn p02() {
	parse("tests/positive/02.nq")
}

#[test]
fn p03() {
	parse("tests/positive/03.nq")
}

#[test]
fn p04() {
	parse("tests/positive/04.nq")
}
