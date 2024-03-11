# N-Quads parser for Rust

[![Crate informations](https://img.shields.io/crates/v/nquads-syntax.svg?style=flat-square)](https://crates.io/crates/nquads-syntax)
[![License](https://img.shields.io/crates/l/nquads-syntax.svg?style=flat-square)](https://github.com/timothee-haudebourg/nquads-syntax#license)
[![Documentation](https://img.shields.io/badge/docs-latest-blue.svg?style=flat-square)](https://docs.rs/nquads-syntax)

<!-- cargo-rdme start -->

N-Quads is a line-based, plain text format for encoding an RDF dataset.
This library provides a [W3C Recommendation](https://www.w3.org/TR/n-quads/)
compliant parser that keeps track of the position of each syntax node in the
source file using the [locspan](https://crates.io/crates/locspan) library.

<!-- cargo-rdme end -->

## License

Licensed under either of

 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
