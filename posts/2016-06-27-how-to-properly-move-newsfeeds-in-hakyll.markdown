----
title: How to properly move newsfeeds in Hakyll
language: english
description: How to change feed's root URL without causing all the old articles
    re-appear as new
tags: programming
----

Theoretically, static sites are location-independent, i.e. they don't care where
they are deployed—there are no references, neither in generator nor in the
generated HTML and CSS themselves, to the domain and/or path where the site is
going to be situated. If you're using `relativizeUrls` in all your Hakyll
compilers, your site is location-independent already and you can move it easily
by just copying the files to a new address.

Unless, of course, you have an Atom or RSS feed, in which case the idyll is
broken.

The first reason to this, the universal one, is that feeds contain a link to
themselves, or at least to the site from which they can be fetched. Atom feeds
carry their own original location in `<id>` tag inside the `<feed>`; RSS 2.0
feeds have a `<link>` to the site inside the `<channel>`, but can also include
`<atom:link>` (which is just like Atom's `<id>`).

The second reason is specific to Hakyll, but to explain it, I have to delve
a bit into how newsfeeds work in general.

# How computers tell news items apart

A newsfeed is a collection of items (or entries, in Atom's parlance). An item
has a title, an author, a publication date and some contents, plus a number of
optional additions like enclosures with pictures or mp3 files.

To us humans, it might appear that these properties are sufficient to figure out
if two items are the same, but in reality it isn't so. Think for a second: if
I publish an item and then fix a typo in its title, is that the same item as
before, or a new one? Of course it's the same, you say! But unfortunately, it
isn't that obvious to a machine.

Instead of devising complex comparison strategies, the problem can be solved by
addition of yet another property—an ID. The publisher (e.g. Hakyll) generates
a unique ID for each item, and readers (the programs, not the humans) can
remember IDs they see in order to distinguish updates from new items. Genius,
right?

But what does it have to do with Hakyll? How does it make our sites
location-dependent?

# How Hakyll generates item IDs

Since it is a static generator without any sort of database, it doesn't have
much choice about the way to generate an ID: it has to use some of post's data,
but it can't rely on title, date or content as these are exactly the things that
might be changed during an update.

What's left is post's URL, which includes both your site's domain name and the
path to the post itself. This is why there is `feedRoot` field in
[`Hakyll.Web.Feed.FeedConfiguration`][feedconfiguration]—Hakyll already knows
post's path relative to the site's root, but it has no idea about the domain
name and the path where you'll put the site.

When you move your site, you'll change `feedRoot`, which will in turn change all
the IDs of your feed items, leading readers (the programs) to believe that these
are new ones, yet unseen. This will probably irritate your readers (the humans).

The solution should now be obvious, though: for items that were published before
the move, the old `feedRoot` should be used, while newer posts should use a new
value. How can we achieve that?

# Let's write some code!

You must've noticed that both `renderRss` and `renderAtom` accept `Context
String` parameter. Contexts are important because when a template is being
filled out, it's the context that supplies the values. Cool thing about contexts
is that they can associate a key (the thing that appears in your template as
a string between dollar signs, e.g. `$title$`) not only with a constant value
which is the same for all items, but also with a function that is run on each
item and produces the value to be substituted for the key.

This, and the fact that `feedRoot` is just another piece of context (bound to
key "root"), is enough to implement the solution.

Somewhere in your code, you have a line like this one (it doesn't really matter
if it's `renderRss` or `renderAtom`):

```haskell
renderRss feedConfig defaultContext
```

What we need to do is to replace it with the following code:

```haskell
renderRss
  feedConfig
  (defaultContext
  `mappend`
  field "root" (\item -> do
                 let id = itemIdentifier item
                 published <- getItemUTC defaultTimeLocale id

                 httpsSwitchDate <-
                   parseTimeM
                     False
                     defaultTimeLocale
                     "%FT%TZ"
                     "2016-06-26T00:00:00Z"

                 if published > httpsSwitchDate
                   then return newRootUrl
                   else return oldRootUrl))
```

As you can see, `defaultContext` is being augmented with a field called "root",
which is mapped to a lambda. The lambda figures out when the given item was
published and compares that date to a constant value we have embedded in the
code. This constant is the day you've moved your site to the new location.
Everything posted after it will have new `feedRoot`, while older posts will keep
using the old `feedRoot`.

To tell the truth, I find the solution a bit messy and hacky, but such is the
price of compatibility.

[feedconfiguration]: https://hackage.haskell.org/package/hakyll-4.7.5.1/docs/Hakyll-Web-Feed.html#t:FeedConfiguration
