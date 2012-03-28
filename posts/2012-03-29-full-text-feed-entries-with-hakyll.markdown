---
title: Full-text feed entries with Hakyll
---

It's nice when RSS or Atom feed contain full-text posts because from there
people can do whatever they want: aggregate posts to planets, filter them by
keywords, after all, just read it in their favourite RSS reader and not on your
(possibly ugly) website. Creating such a feed was easy at Blogspot, but with
Hakyll it turned out to be quite a challenging task. In this post I'll provide
a howto (with a little bonus for those who took a burden of learning basics of
[arrows][arrows]) on the topic.

Let's start with the basic code to generate the feed:

```Haskell
match "feed.rss" $ route idRoute
create "feed.rss" $ requireAll_ "posts/*"
  >>> renderRss feedConfiguration

feedConfiguration = ...
```

There's a problem, though: Hakyll gets text for feed entry from the
`description` field of the page, and that one usually is empty. Just as a quick
remark, here's how you can populate it with some text in Markdown:

```Markdown
---
description: Here goes a description of a post
---
```

Okay, so `description` is empty. Where's your post, then? In the `body` field!
So let's just copy the contents from field to field!

```Haskell
import Control.Arrow (arr)

create "feed.rss" $ requireAll_ "posts/*"
  >>> mapCompiler (arr $ copyBodyToField "description")
  >>> renderRss feedConfiguration
```

Oops… Now our feed contains a lot of garbage because we included full HTML page
into it! And that's the moment of truth. I'll first show you what I originally
did, just so you understand what **should not be done**:

```Haskell
create "feed.rss" $ requireAll_ "posts/*"
  >>> mapCompiler (arr $ \p -> Page {
        pageBody = unixFilter
          "sed" ["-n", "/<article[^>]*>/,/<\\/article>/p"]
          $ pageBody p } )
  >>> mapCompiler (arr $ copyBodyToField "description")
  >>> renderRss feedConfiguration
```

(I exploited the fact that my blog uses HTML5 markup and article is wrapped
into `<article>` tag)

So yeah, just a bit of Unix magic and we're done. Well, almost: those
`<article>` tags are still in the feed.

And that's when I googled it properly (at last!)

It turned out that Roman Cheplyaka (the guy who introduced me to Haskell, by
the way) [already asked][groups] the same question. From there, it was easy:
all we need to do is copy `body` into `description` **when we're creating the
page**. One should be careful to do that **before** applying any templates, or
your feed entry will end up being re-formatted according to the template (that
may be a win for someone, though). So if you had code like that:

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

And here goes the bonus I promised you at the beginning: what if you want to be
able to provide short descriptions for some posts?  Obviously we should check
if the `description` field is set, and only populate it with page's body when
it's empty. Let's get down to the code:

```Haskell
hasDescription :: Page a -> Bool
hasDescription = (/= "") . getField "description"

pageHasDescription :: Compiler (Page a)
                               (Either (Page a) (Page a))
pageHasDescription = arr (\p -> if hasDescription p
                                   then Right p
                                   else Left  p)
```

Those two pieces was easy: first we define predicate that checks if
`description` is empty, then we turn that function into arrow. We need second
function in order to split control flow in two depending on whether or not
`description` contain anything. And now is the most interesting part:

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
same resulting type) and turns them into new arrow that takes value of type
`Either b c`, applies first arrow (`a b d`) if it's `Left b` or second one (`a
c d`) if it's `Right c`, and returns the result (of type `d`). Easy!

So what our code does is pretty straightforward: `pageHasDescription` returns
input page wrapped in either `Left` or `Right` depending on whether
`description` field is empty or not, and then we either populate the field with
page's body or just left things intact.

That's all for today, folks. See you!

[groups]: https://groups.google.com/forum/?fromgroups#!topic/hakyll/KmGmD2CtVSw "Including full text into feed"
[arrows]: https://en.wikibooks.org/wiki/Haskell/Understanding_arrows "Understanding arrows"
