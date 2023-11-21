//! Difftastic is a syntactic diff tool.
//!
//! For usage instructions and advice on contributing, see [the
//! manual](http://difftastic.wilfred.me.uk/).
//!

// This tends to trigger on larger tuples of simple types, and naming
// them would probably be worse for readability.
#![allow(clippy::type_complexity)]
// == "" is often clearer when dealing with strings.
#![allow(clippy::comparison_to_empty)]
// It's common to have pairs foo_lhs and foo_rhs, leading to double
// the number of arguments and triggering this lint.
#![allow(clippy::too_many_arguments)]
// Has false positives on else if chains that sometimes have the same
// body for readability.
#![allow(clippy::if_same_then_else)]
// Purely stylistic, and ignores whether there are explanatory
// comments in the if/else.
#![allow(clippy::bool_to_int_with_if)]
// Good practice in general, but a necessary evil for Syntax. Its Hash
// implementation does not consider the mutable fields, so it is still
// correct.
#![allow(clippy::mutable_key_type)]

extern crate log;
extern crate pretty_env_logger;

use mimalloc::MiMalloc;

/// The global allocator used by difftastic.
///
/// Diffing allocates a large amount of memory, and `MiMalloc` performs
/// better.
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

/// The entrypoint.
fn main() {
    println!("Thanks for using baz-difftastic");
}
