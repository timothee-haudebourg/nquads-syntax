use nquads_syntax::Parse;
use std::path::Path;

fn parse<P: AsRef<Path>>(path: P) {
	stderrlog::new().init().ok();
	match std::fs::read_to_string(&path) {
		Ok(buffer) => {
			match nquads_syntax::GrdfDocument::parse_str(&buffer, |span| span) {
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
