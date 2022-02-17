# N-Quads parser for Rust

N-Quads is a line-based, plain text format for encoding an RDF dataset.
This library provides a [W3C Recommendation](https://www.w3.org/TR/n-quads/) compliant parser that keeps track of the position of each syntax node in the source file using the [locspan](https://crates.io/crates/locspan) library.