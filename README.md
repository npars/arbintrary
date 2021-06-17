# arbINTrary [![CI](https://github.com/npars/arbintrary/actions/workflows/ci.yaml/badge.svg)](https://github.com/npars/arbintrary/actions/workflows/ci.yaml) [![Crates.io](https://img.shields.io/crates/v/arbintrary.svg)](https://crates.io/crates/arbintrary)

A Proof of Concept implementation of generic integers using the const generics feature. Based on the [uX crate](https://crates.io/crates/ux)
and @programmerjake's [initial implementation](https://github.com/rust-lang/rfcs/pull/2581#issuecomment-730709707). 

When non-standard-width integers are required in an application, the norm is to use a larger container and make sure the value is within range after manipulation. ArbINTrary aims to take care of this once and for all by:
 - Providing `uint<0>`-`uint<128>` and `int<0>`-`int<128>` types that should behave as similar as possible to the built in rust types
     - The methods of the defined types are the same as for the built in types (far from all is implemented at this point but fill out an issue or create a PR if something essential for you is missing)
     - Overflow will panic in debug and wrap in release.

# License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or
  http://www.apache.org/licenses/LICENSE-2.0)

- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
