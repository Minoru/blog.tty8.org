---
title: I underestimated the dangers of Rust FFI
language: english
description: How I ended up with a buffer overrun by (unwittingly) misusing
    std::ffi::CString.
tags: programming, newsboat-rust-rewrite
---

As a continuation of [Newsboat rewrite series][rewrite-series], here's an
example of just how attentive you have to be when writing FFI code.

FFI stands for "foreign function interface". It's a way for Rust code to call
other languages, and to be called from other languages as well. FFI relies on
C as a *lingua franca*, a common language that defines how things are laid out
in memory, and how functions are invoked.

Working with FFI is dangerous. You're responsible for upholding invariants that
are usually taken care of by Rust. You must get your types right, and you need
to make sure there is no way for you to overwrite someone else's memory.

Here's an abridged version of the code I was working on the other day (also
available on [Rust Playground][playground]):

<style>
div#badCode pre {
background-color: #89000034;
}

div#okCode pre {
background-color: #99990034;
}

div#goodCode pre {
background-color: #00890034;
}
</style>

<div id="badCode">
```rust
// This code is BROKEN — see below for a correct one.
use std::ffi::CString;

fn obtain_answer() -> Option<String> {
    let format = "The answer ... is: %llu"; // ❶
    const BUF_SIZE: usize = 1024;
    let buffer = Vec::<u8>::with_capacity(BUF_SIZE);
    unsafe {
        CString::new(buffer).ok().and_then(|buffer| {
            CString::new(format).ok().and_then(|format| {
                let buffer_ptr = buffer.into_raw();
                libc::snprintf( // ❷
                    buffer_ptr,
                    BUF_SIZE as libc::size_t,
                    format.as_ptr() as *const libc::c_char,
                    std::u64::MAX,
                );
                let buffer = CString::from_raw(buffer_ptr);
                buffer.into_string().ok() // ❸
            })
        })
    }
}
```
</div>

It takes a format string ❶, passes it into C function `snprintf` ❷ to replace
"%llu" with `std::u64::MAX`, and collects the result as a Rust `String` ❸.
Pretty innocuous, isn't it? And you'd expect that if there were a bug, it'd be
somewhere inside that `unsafe` block, right?

But the bug is right between `Vec::with_capacity` and the `unsafe` keyword.
Nothing there? Exactly!

This is not documented in `CString`'s docs in any way [yet][rust-doc-issue],
but its `new` method calls `Vec::into_boxed_slice`, which [sheds unused
capacity][into_boxed_slice-doc]. So here's what happens when the code is ran:

1. we create an empty `Vec` that has 1024 bytes reserved already;
2. we pass that `Vec` into `CString::new`, which appends a null byte (increasing
   vector size to 1, and decreasing reserve to 1023);
3. `CString::new` then calls `Vec::into_boxed_slice`, which allocates just
   enough space to hold that one byte, copies it there, and frees the original
   buffer of 1024 bytes;
4. we convert the `CString` into a pointer to its one-byte buffer using
   `into_raw`;
5. unlike Rust slices, C pointers don't store the size of the chunk they point
   to—that's why we also pass `BUF_SIZE` to `snprintf`. However, in our case
   this size no longer matches the size of the buffer (we pass 1024 but it's
   actually 1);
6. `snprintf` trusts us, and writes all over the 1023 bytes that follow our
   buffer.

This is a classic [buffer overrun][buffer_overrun], the staple on which numerous
bugs subsist. The behaviour of this code differs by platform, compiler version,
and what other code is ran afterwards. It will most probably work just fine—the
first time around, anyway. In the Playground, you'll get a timeout, coredump,
and a message saying "realloc(): invalid next size". On my Linux machine, I saw
tests hang in jemalloc (with pre-1.32 Rust, i.e. before Rust transitioned to the
system allocator).

# A solution that sorta works

Once found, the problem is easy to fix: resize the vector to `BUF_SIZE-1`,
leaving that one last byte unused for `CString::new` to insert the terminating
null byte. Use anything *but* zero for initialization value, as `new` checks for
them and will return an `Err` if vector contains any zeroes already.

<div id="okCode">
```rust
const BUF_SIZE: usize = 1024;
let buffer = Vec::<u8>::with_capacity(BUF_SIZE);
buffer.resize(BUF_SIZE - 1, 1);
unsafe {
    CString::new(buffer).ok().and_then(|buffer| {
        // `buffer` definitely has `BUF_SIZE` bytes in it
    })
}
```
</div>

But as [commenters on lobste.rs][lobsters-thread] point out, that's still
a wrong way to do it. `CString` is meant to own a C string that we loan to FFI;
it's *not* suited for buffers that we ask the FFI to fill. So let's finally get
to…

# The correct solution

Just use `Vec`. Preallocate as much space as you need, use FFI, reconstruct the
`Vec`, simultaneously resizing it to match how many bytes were written. [In
production][production] we'd also check if `snprintf` couldn't fit everything
into the buffer, and allocate a larger one if need be. But this is a blog post,
so let's skip that part for brevity:

<div id="goodCode">
```rust
let mut buffer = Vec::<u8>::with_capacity(BUF_SIZE);
let buffer_ptr = buffer.as_mut_ptr();
unsafe {
    // We're passing the buffer to C, so let's make
    // Rust forget about it for a while.
    mem::forget(buffer);

    let bytes_written = libc::snprintf(
        buffer_ptr as *mut libc::c_char,
        buf_size as libc::size_t,
        format_cstring.as_ptr() as *const libc::c_char,
        std::u64::MAX,
    ) as usize;

    let buffer = Vec::from_raw_parts(
        buffer_ptr,
        bytes_written,
        buf_size);
    Ok(String::from_utf8_lossy(&buffer).into_owned())
}
```
</div>

Avoid FFI.

[rewrite-series]: /posts/2018-11-05-how-not-to-start-a-rust-rewrite.html
    "How not to start a Rust rewrite — Debiania"

[playground]: https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=53a40ccb1a0f475e019b44895feac8fa
    "Rust Playground"

[rust-doc-issue]: https://github.com/rust-lang/rust/issues/60558
    "Document that CString::new sheds unused capacity from the buffer it's given — Issue #60558 — rust-lang/rust"

[into_boxed_slice-doc]: https://doc.rust-lang.org/std/vec/struct.Vec.html#method.into_boxed_slice
    "std::vec::Vec — Rust"

[buffer_overrun]: https://en.wikipedia.org/wiki/Buffer_overflow
    "Buffer overflow — Wikipedia"

[lobsters-thread]: https://lobste.rs/s/wfqc4w/i_underestimated_dangers_rust_ffi
    "I underestimates the dangers of FFI | Lobsters"

[production]: https://github.com/newsboat/newsboat/blob/1d439ede40dadc55e96fa2884c3e27cc9631f68a/rust/strprintf/src/lib.rs#L52-L58
    "rust/strprintf/src/lib.rs at 1d439ede40dadc55e96fa2884c3e27cc9631f68a · newsboat/newsboat · GitHub"
