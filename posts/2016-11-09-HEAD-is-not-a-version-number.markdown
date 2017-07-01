----
title: HEAD is not a version number
language: english
description: Don't use HEAD in your bug reports and issues because it might
    be(come) very hard to tell which commit you're referring to.
tags: programming, thoughts
----

For quite some time now I've been commenting on issues in one of my pet projects
using phrases like "the bug isn't reproducible with `HEAD`" or "both 2.9 and
`HEAD` exhibit the behaviour described". I'm also dogfooding a version built
directly from Git, so I filed reports saying that "%something% is broken in
`HEAD`". "HEAD" is shorter and—let's admit it—so much cooler than "the version
from the `master` branch", so why not, right?

I knew that `HEAD` moves all the time, of course, but I thought that the date on
the GitHub comment is enough to disambiguate that. Now I realize that it's not.

The thing is, Git only records the time the commits were *made*, but not the
time they were *pushed*. Furthermore, due to history rewriting, it's possible to
get a sequence of commits with no order to the dates whatsoever: a commit made
today can be followed by one made last Friday, followed by another that was made
yesterday. Add a possibility of race condition where you push new commits in
between user building the software and filing an issue, and bingo!—the "HEAD"
they mention in the report is *meaningless*.

And this is not a fault of Git's. This is just a wrong way to use it.

The old ones figured this all out a long time ago. Try building your favourite
software from the development repo, and chances are it'll report its version as
"2.10-cc3d3bf8" or something. Cause `HEAD` is fickle; it can't be treated as
a version number. I'm now going to implement the same technique in my own
project, and I already started making "HEAD" a hyperlink to a specific commit in
the comments I write. This post is just a final touch: a minute of reading that
will save you from wasting time making the same mistake. Cheers!
