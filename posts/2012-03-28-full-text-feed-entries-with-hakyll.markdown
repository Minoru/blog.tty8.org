---
title: Full-text feed entries with Hakyll
---

It's nice when RSS or Atom feed contain full-text posts because people can read
them in their favourite reader. It was easy to do in my old blog at
Blogspot, but with Hakyll it turned out into quite a challenging task.

Basic code to generate the feed looks like this:

```Haskell
match "feed.rss" $ route idRoute
create "feed.rss" $ requireAll_ "posts/*"
  >>> renderRss feedConfiguration

feedConfiguration = ...
```

The problem here is that both `renderRss` and `renderAtom` require
`description` field to contain the text that would be shown in the feed. Well,
that's easy to accomplish: let's just copy the text (post's body) into that
field!

```Haskell
import Control.Arrow (arr)

create "feed.rss" $ requireAll_ "posts/*"
  >>> mapCompiler (arr $ copyBodyToField "description")
  >>> renderRss feedConfiguration
```

Oops… Now our feed contains a lot of garbage because we included full HTML page
into it! And that's the moment of truth. I'll first show what I originally did,
just so you understand what you **should not do**:

```Haskell
create "feed.rss" $ requireAll_ "posts/*"
  >>> mapCompiler (arr $ \p -> Page {
        pageBody = unixFilter
          "sed" ["-n", "/<article[^>]*>/,/<\\/article>/p"]
          $ pageBody p } )
  >>> mapCompiler (arr $ copyBodyToField "description")
  >>> renderRss feedConfiguration
```

(I expoloited the fact that my blog uses HTML5 markup and article is wrapped
into `<article>` tag)

So yeah, just a bit of Unix magic and we're done. Well, almost: those
`<article>` tags are still in the feed.

And that's when I googled it properly (at last!)

It turned out that Roman Cheplyaka (the guy who introduced me to Haskell)
[already asked][groups] the same question. From there, it was easy: all we need
to do is copy body into description field when we're creating the page. One
should be careful to do that before applying any templates, or your feed entry
will end up re-formatted according to template (that may be a win for someone,
though). So if you had code like that:

```Haskell
match "posts/*" $ do
  route $ setExtension ".html"
  compile $ pageCompiler
    >>> applyTemplateCompiler "templates/post.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler
```

then all you need to do is to stick one tiny line after `pageCompiler`:

```Haskell
match "posts/*" $ do
  route $ setExtension ".html"
  compile $ pageCompiler
    >>> arr $ copyBodyToField "description"
    >>> applyTemplateCompiler "templates/post.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler
```

Now what if you want to be able to provide short descriptions for some posts?
Obviously we should check if the `description` field is set, and only populate
it with page's body when it's empty. Let's get down to the code:

```Haskell
hasDescription :: Page a -> Bool
hasDescription = (/= "") . getField "description"

pageHasDescription :: Compiler (Page a)
        (Either (Page a) (Page a))
pageHasDescription = arr (\p -> if hasDescription p
        then Right p else Left p)
```

Those two pieces was easy: first we define predicate that checks if
`description` is empty, then we turn that predicate into arrow. We need second
function in order to split control flow in two depending on whether or not
`description` contain anything. And now the most interesting part:

```Haskell
match "posts/*" $ do
  route $ setExtension ".html"
  compile $ pageCompiler
    >>> pageHasDescription
    >>> arr (copyBodyToField "description")
        |||
        id
    >>> applyTemplateCompiler "templates/post.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler

```

Here we use `(|||)` operator of type `ArrowChoice a => a b d -> a c d -> a
(Either b c) d`. What it does is take two arrows, `a b d` and `a c d` (note the
same resulting type, `d`) and turns them into new arrow that takes value of
type `Either b c` (i.e. `Left b` or `Right c`), applies one of the arrows on it
(first one for `Left`, second one for `Right`) and returns the result. Easy!

So what our code does is pretty straightforward: `pageHasDescription` returns
input page wrapped into either `Left` or `Right` depending on whether
`description` field is empty or not, and then we either populate the field with
page's body or just left things intact.

See you!

[groups]: https://groups.google.com/forum/?fromgroups#!topic/hakyll/KmGmD2CtVSw "Including full text into feed"
