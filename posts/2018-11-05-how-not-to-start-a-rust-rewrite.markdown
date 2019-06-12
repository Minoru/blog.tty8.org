---
title: How not to start a Rust rewrite
language: english
description: # No double quotes; end with a period
tags: programming, thoughts, newsboat-rust-rewrite
---

I never bragged about it here, but for the last three years or so, I have been
maintaining an RSS reader called [Newsboat][website]. It's written in C++, and
as you might've already guessed, we're porting it to Rust now. [Kind folks on
Reddit][reddit-comment] encouraged me to blog about it, so here goes.

In this port, I'll be using "we" quite a bit, since a lion's share of the work
is done not by me, but by a number of [awesome contributors][contributors].
Thank you for all the help, people!

# Using existing code as a library

Newsboat isn't small enough to rewrite in a few evenings, so the first order of
business was to hook Cargo into our existing Makefile. To check if it links all
right, we had to rewrite some small function. A "genius" idea came to my mind:
I'll rewrite `main()`, and use existing C++ code as a library!

There was a method to my madness. I thought that by putting Rust at the very
bottom of the call stack—making it the very first thing that's called when the
program starts—I'll ensure that Rust owns all the long-lived objects. That way,
I could start reaping the benefits of borrow checking right away.

This worked fine—but only on my machine. Since we were now using `rustc` to link
the final executable, we ran into problems with debug builds: `rustc` couldn't
find gcov symbols (or its Clang counterparts). On OS X, it was failing even
harder, because it couldn't find C++ standard library even if we passed `-lc++`.
I managed to fix GCC builds by linking with `gcov` library, but Clang and OS X
remained.

All this trouble made me re-evaluate my approach, and finally realize that it
didn't really matter who calls whom. We could just as easily keep `main()` in
C++, and have it immediately call into Rust to get all the same benefits. [A
prod from \@upsuper][upsuper-prod] definitely helped here.

I implemented that, and the only problem we've found since them is that we have
to manually link dynamic libraries required by Rust. Since final linking is done
by the C++ compiler, `rustc` simply doesn't get a chance to do the required
linking itself. So now, we always link with `pthread` and `dl`. On OS X, we also
pass `$(CXX)` a `-framework Security` flag, [to get the `_SecRandomCopyBytes`
symbol][sec-random-copy-bytes] required by `rand` crate.

# Diving head-first into FFI

With the build shenanigans out of the way, we could finally start porting some
code. I thought the main challenge with that would be converting functions'
arguments from one representation to another. To prepare, I even wrote a toy
thing that called a C function from Rust (unwittingly re-creating [Alex
Crichton's "Rust FFI examples"][rust-ffi-examples]). Easy as pie!

Turns out I prepared for the wrong thing. Thanks to [The Rust FFI
Omnibus][the-rust-ffi-omnibus], all the FFI stuff we needed so far could be
simply copy-pasted from the Web. However, since I put all our Rust code into
a single crate that compiled to a static library, we couldn't write integration
tests. Rust simply [does not link][rust-linkage] with static libraries produced
from crates, so it doesn't even create executables for the integration tests!

Luckily, [the StackOverflow question][ut-in-staticlib] about this very problem
provided a solution: split the code into "pure-Rust" and "FFI" crates. The
former can then be tested via Rust, and the latter can be tested e.g. by writing
a C++ application. We already had a test executable running our C++ tests, so
implementing this was as simple as moving some Rust code around.

Of course, all of this happened a few days prior to me learning about Cargo
workspaces, so I had to re-organize our crates *again* to deduplicate
dependencies and cut down on build times. Consider finishing The Book before you
start putting code into production, folks!

&nbsp;

That was the first of the series on rewriting Newsboat in Rust. In [the next
installment][next], I'll tell you how I ported a global mutable singleton, and rant
about Rust's unit testing framework. If you don't want to miss it,
[subscribe](/subscribe.html)—via RSS, of course :) Happy coding!

[website]: https://newsboat.org/ "Newsboat, an RSS reader"

[reddit-comment]:
    https://www.reddit.com/r/rust/comments/9j2702/newsboat_is_rewriting_from_c_to_rust/e6o8xo2/
    "newsboat is rewriting from C++ to rust: /r/rust"

[contributors]:
    https://github.com/newsboat/newsboat/graphs/contributors
    "Contributors to newsboat/newsboat — GitHub"

[upsuper-prod]:
    https://github.com/newsboat/newsboat/issues/287#issuecomment-424740754
    "Profiling builds fail on Linux when linking via rustc · Issue #287 · newsboat/newsboat"

[sec-random-copy-bytes]:
    https://travis-ci.org/newsboat/newsboat/jobs/450786799#L2491
    "Undefined symbols for architecture x86_64: \_SecRandomCopyBytes — Job #654.11 - newsboat/newsboat - Travis CI"

[rust-ffi-examples]:
    https://github.com/alexcrichton/rust-ffi-examples
    "alexcrichton/rust-ffi-examples: FFI examples written in Rust — GitHub"

[the-rust-ffi-omnibus]:
    http://jakegoulding.com/rust-ffi-omnibus/
    "The Rust FFI Omnibus"

[rust-linkage]:
    https://doc.rust-lang.org/reference/linkage.html
    "Linkage — The Rust Reference"

[ut-in-staticlib]:
    https://stackoverflow.com/questions/41241585/how-to-link-against-rust-crate-from-integration-tests-in-tests-folder-when-bui
    "How to link against Rust crate from integration tests in 'tests' folder
    when building static library? — StackOverflow"

[next]: /posts/2018-11-24-porting-our-logger-to-rust.html
    "Porting our logger to Rust — Debiania"
