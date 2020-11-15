---
title: Reviving hakyll-convert
language: english
description: hakyll-convert is a CLI tool that splits Blogger and WordPress
    backups into separate posts that Hakyll can understand. I just updated it to
    work with modern dependencies.
tags: hakyll, programming, stuff
---

_**Summary:** `hakyll-convert` is a CLI tool that splits Blogger and WordPress
backups into separate posts that Hakyll can understand. I just updated it to
work with modern dependencies._

I put this blog together with a static generator called [Hakyll][hakyll]. Before that,
I was using Google Blogger and wrote no fewer than 100 posts over there.
Naturally, I wanted to migrate all that content to the new place, but how?

That's where `hakyll-convert` saved me. I exported my posts from Blogger, ran
`hakyll-convert` on the resulting XML file — and voilà, I had a hundred files
with Hakyll-style "front matter" and my posts' HTML. Just copy that into my
"posts" directory, and I was done.

At the time, `hakyll-convert` couldn't rename the files according to
a user-provided template, so I submitted [a pull request][format-pr] to add that
feature. It turned out that the program's author, Eric Kow, [no longer had time
to maintain it][no-maintainer], so he asked me to take over. After a bit of
thought, [I agreed][agreed].

`hakyll-convert` is a small project, easy to keep up-to-date, and I didn't plan
to make any grand changes. I just wanted to be around when someone submits
a pull request or files an issue. Just keep the project alive and kicking.

Fast-forward a year. I received a [heads-up][] about an upcoming change in the
`feed` package. That promised some severe breakage for `hakyll-convert`, which
heavily relied on `feed`'s data types. Unfortunately, I was busy with a new job,
so I didn't have time to investigate and fix.

Another year passed. The new version of `feed` came out, breaking
`hakyll-convert` as expected — and I was still busy with something (don't even
remember what exactly). So much for "being around" and
"keeping the project alive and kicking".

Two and a half years later, I finally got around to fixing it. That turned out
to be easier than I thought: the thrust of the change was a migration from `xml`
package to `xml-types`, which boiled down to search-and-replace thanks to
Haskell's type system. I did introduce one logical error, but the tests caught
that.

Oh, and Blogger changed their format slightly in those intervening years, but
that [wasn't hard to fix][modern-blogger-fix] either.

And that's it. `hakyll-convert` is back now. You can install it from Hackage:

```
$ cabal install hakyll-convert
```

I also submitted [a PR][stackage-pr] to add it to Stackage Nightly, and so it
will make it into Stackage LTS 17 as well. Try it out, and please [open an
issue][new-issue] if something doesn't work!

[hakyll]: https://jaspervdj.be/hakyll/
    "Hakyll — A static website compiler library in Haskell"

[format-pr]: https://github.com/Minoru/hakyll-convert/pull/8
    "Implement output filename formatting by Minoru · Pull Request #8
    · Minoru/hakyll-convert"

[no-maintainer]: https://github.com/Minoru/hakyll-convert/pull/7#issuecomment-228756338
    "Build with Stackage LTS 5 by Minoru (comment) · Pull Request #7
    · Minoru/hakyll-convert"

[agreed]: https://github.com/Minoru/hakyll-convert/pull/8#issuecomment-229102864
    "Implement output filename formatting by Minoru (comment) · Pull Request #8
    · Minoru/hakyll-convert"

[heads-up]: https://github.com/Minoru/hakyll-convert/issues/15
    "feed dependency · Issue #15 · Minoru/hakyll-convert"

[modern-blogger-fix]: https://github.com/Minoru/hakyll-convert/pull/45/commits/855573cf51dd69f53928f7122512ddc7807fa16c
    "Fix URLs in modern Blogger backups · Minoru/hakyll-convert@855573c"

[stackage-pr]: https://github.com/commercialhaskell/stackage/pull/5744
    "Add hakyll-convert by Minoru · Pull Request #5744
    · commercialhaskell/stackage"

[new-issue]: https://github.com/Minoru/hakyll-convert/issues/new
    "New Issue · Minoru/hakyll-convert"
