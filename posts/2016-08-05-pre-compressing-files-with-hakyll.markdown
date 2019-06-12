---
title: Pre-compressing files with Hakyll
language: english
description: How to make Hakyll gzip your posts and stuff so that your
    webserver doesn't have to.
tags: howto, programming, hakyll
---

Usually, HTTP responses are compressed in real time, on the web server. That
incurs some overhead for each request, but is the only way to go when dealing
with dynamic content.

We in the static generation camp have some choice, though. Both
[Nginx][ngx_gzip_static] and [Apache][apache-precompressed-howto] can be
configured to serve pre-compressed files. Apart from eliminating the overhead,
this approach lets us use levels of compression that aren't feasible in
real-time setting, but are perfectly reasonable offline.

Faster *and* smaller responses. What's not to like?

[ngx_gzip_static]:
    http://nginx.org/en/docs/http/ngx_http_gzip_static_module.html
    "Nginx: Module ngx_http_gzip_static_module"

[apache-precompressed-howto]:
    http://blog.codegrill.org/2009/07/how-to-pre-compress-static-files-in.html
    "How To Serve Pre-Compressed Static Files in Apache"

Don't obsess over compression levels, though. For files smaller than 100Kb, ten
percent extra of compression translates into mere hundreds of bytes saved. Not
too impressive. (But do run your own benchmarks.)

Overall, this isn't the top optimization there is; yet, it's quite cheap and
accessible, so why not implement it?

$break$

Let's get our hands dirty, then. How shall we go about coding this? Well, we
might start with recognizing that we want each input file to produce two
outputs—one gzipped, the other not. That, in turn, immediately leads us to
[Hakyll's tutorial on versioning][hakyll-multiple-versions]. So if you have code
along the lines of:

[hakyll-multiple-versions]:
    https://jaspervdj.be/hakyll/tutorials/06-versions.html
    "Tutorial: Producing multiple versions of a single file"

```Haskell
match "posts/*" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" postCtx
```

…you should add a new block that will look like this:

```Haskell
match "posts/*" $ version "gzipped" $ do
    route   $ setExtension "html.gz"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= gzip  -- to be defined later
```

That will definitely work, but you probably aren't happy about code duplication
and the fact that Hakyll will now do the same work twice. Neither am I, so let's
press on!

Usually, duplication is eliminated with [snapshots][hakyll-snapshots], but if we
add `saveSnapshot` at the end of the first block and use `loadSnapshotBody` in
the second, Hakyll will give us a runtime error due to dependency cycle: gzipped
version of the item will depend on itself. Bummer!

[hakyll-snapshots]: https://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html
    "Tutorial: Snapshots, and how to produce an RSS/Atom feed"

The thing is, versions are part of identifiers. That's only logical: to
distinguish X from another X, you label one with "gzipped", and now it's easy to
tell them apart—one is just "X", another is "X (gzipped)". In Hakyll, that means
that when you're running, say, `loadSnapshotBody` from inside a block wrapped in
`version "gzipped"`, you'll be requesting a snapshot of *identifier that's
labeled "gzipped"*. That's what causes a dependency cycle.

Luckily for us, Hakyll exports functions for manipulating identifier's version.
So our second code block will now look as follows:

```Haskell
match "posts/*" $ version "gzipped" $ do
    route   $ setExtension "html.gz"
    compile $ do
        id <- getUnderlying
        body <- loadBody (setVersion Nothing id)
        makeItem body
            >>= gzip
```

As you can see, we're obtaining the current identifier (which is versioned
because of `version "gzipped"`) and modifying it so that it references the
unversioned item. Note that we must use `makeItem` there—had we tried to `gzip`
an item returned by `load`, we'd get a runtime error, because identifier of the
item we'd be returning won't have the appropriate version.

One caveat with the code above is that `loadBody` won't work for files compiled
with `copyFileCompiler` (because the latter doesn't really copy the contents of
the file into the cache, from which `loadBody` reads). For such files, we'll
have to use another approach:

```Haskell
match "images/*.svg" $ version "gzipped" $ do
    route   $ setExtension "svg.gz"
    compile $ getResourceBody
          >>= gzip
```

This code circumvents the problem by reading the file straight from the disk.

$break$

With versions sorted out, it's time to turn our attention to implementing
`gzip`. Luckily, this part is much simpler: Hakyll already provides a means for
running external programs. All we have to do is convert item's body from
`String` to lazy `ByteString` (on an assumption that it's UTF-8); the reason
being that the binary returned by compressor is not a textual string and might
not be representable with `String`:

```Haskell
gzip :: Item String -> Compiler (Item LBS.ByteString)
gzip = withItemBody
           (unixFilterLBS "gzip" ["--best"]
           . LBS.fromStrict
           . TE.encodeUtf8
           . T.pack)
```

And that's it. You can now go add that code into your site's config and
experience the major drawback of this solution, namely the fact that it requires
separate rules for different filename extensions. If you have a Markdown file
compiled into HTML and a bunch of SVG files that are just copied over, you'll
have to write two rules. If you find a way to scrap that boilerplate, please let
me know; my email is at the end of this post.

$break$

Whoa, you got through that meandering mess of an article! That makes two of us.
As a reward, I'm going to tell you how to use [Zopfli][zopfli] to gzip your
files. It's the best DEFLATE compressor out there, and using it goes against my
earlier advice of not obsessing over the byte count, but whatever; it's fun.

[zopfli]: https://github.com/google/zopfli "GitHub: google/zopfli"

So, Zopfli. The trouble with that compressor is that it's not a Unix filter—it
doesn't accept data on standard input. In order to use it, we have to write
item's body into a temporary file, compress that, then read the result back.
Fortunately, Zopfli supports writing the result into `stdout`; that allows us to
make do with safer of the functions provided by Hakyll. (If it wasn't the case,
we'd have to resort to [`unsafeCompiler`][unsafeCompiler]). So here's the code:

[unsafeCompiler]:
    https://hackage.haskell.org/package/hakyll-4.8.3.2/docs/Hakyll-Core-Compiler.html#v:unsafeCompiler
    "Hakyll.Core.Compiler.unsafeCompiler :: IO a -> Compiler a"

```Haskell
gzip item = do
  (TmpFile tmpFile) <- newTmpFile "gzip.XXXXXXXX"
  withItemBody
      (unixFilter "tee" [tmpFile])
      item
  body <- unixFilterLBS
              "zopfli"
              [ "-c"      -- write result to stdout
              , tmpFile]
              (LBS.empty) -- no need to feed anything on stdin

  makeItem body
```

Simple, right? If you're using anything less than `--i100`, though, consider
[7-zip][7zip]—at its best (`-mx9`) it's very close to default Zopfli, but `7z`
is wa-a-ay faster, and can behave as a filter (use `-si -so`).

[7zip]: http://www.7-zip.org/ "7-Zip homepage"

**P.S.** Right before publishing this post, I was searching for some other
Hakyll-related stuff and stumbled upon [a three-years-old conversation on the
mailing list](https://groups.google.com/forum/#!topic/hakyll/OZpggt3SaBw) that
covers everything but the Zopfli bit. *Search engines will kill blogging.*
