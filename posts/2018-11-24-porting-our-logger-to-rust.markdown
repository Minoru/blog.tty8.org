---
title: Porting our logger to Rust
language: english
description: # No double quotes; end with a period
tags: programming, thoughts
---

As I mentioned in [an earlier post][prev], me and other contributors are porting
[Newsboat][newsboat] to Rust. We're working on utility functions right now, but
soon, it will be time to port more substantial chunks. To prepare, I ported our
logger class. Here's how that went.

The proper way to do logging in Rust is to add [`log`][log-crate] to your
Cargo.toml and be done with it. However, this couldn't work for Newsboat: we
have a custom log level called "user error", messages on which are not only
written to the main log, but are also copied to a special one. The `log` crate
doesn't allow for custom log levels, so we had to roll our own.

My plan was simple: write a single-threaded logger, put it behind a `Mutex`,
done. I had to immediately correct this when I learned that I can't create
a `static` with a `Mutex` inside; [`lazy_static!`][lazy_static] solved that.
Then it took me about a week to realize that this design will needlessly
serialize the threads, as they'll have to acquire the mutex just to check the
current log level. Back to the drawing board, then.

A better design, lifted from the `log` crate, is to put the current log level
into an atomic, and protect the rest with the mutex. That way the checks are
relatively cheap and concurrent, while actual logging is still serialized
properly.

Even with that new design, my Rust logger was still 1.5 times slower than the
old C++ one. That's when I really started making mistakes instead of progress.
I read up on the internals of `lazy_static!`, `Once`, and other primitives I was
using. I tried varying the design in hopes of making the log level check even
faster. I did learn about Rust a bit, and I sped up the timestamp formatting on
the Rust side, but I still couldn't make the new logger go as fast as the old
one.

And then it hit me. All the code was in Rust now, the only thing left on the C++
side was a thin wrapper over the C API that Rust provided. That wrapper was
a tad *too* thin, because it didn't check the current log level. All it did was
format the log message and call Rust, which would discard the message
immediately if the current log level wasn't high enough. In the worst case—i.e.
with the logging disabled—C++ wasted a ton of time formatting messages.

Once I started checking the level on the C++ side too, the performance of the
new logger became indistinguishable from that of the old one. Yay!

&nbsp;

I know I promised you a rant about Rust's unit testing framework, but it didn't
fit well with the tale of the logger. [Stay tuned](/subscribe.html) if you don't
want to miss it when I finally post it ;)

[prev]: /posts/2018-11-05-how-not-to-start-a-rust-rewrite.html
    "How not to start a Rust rewrite — Debiania"

[newsboat]: https://newsboat.org/ "Newsboat, an RSS reader"

[log-crate]: https://crates.io/crates/log
    "log - Cargo: packages for Rust"

[lazy_static]: https://crates.io/crates/lazy_static
    "lazy_static - Cargo: packages for Rust"
