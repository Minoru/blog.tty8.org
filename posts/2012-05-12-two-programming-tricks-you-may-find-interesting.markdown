---
title: Two programming tricks you may find interesting
---

Every one of us have a different path to expertise in programming, which
inevitably leads to unique bag of tricks that each programmer have. Free
software and open source enables you to find out about the ones you don't know
yet, and you must be a fool not to use that opportunity.

In this short post I'm going to describe two tricks I learned while working on
open source projects.

The first one may seem trivial and obvious to those who still remember
"Scoping" chapter of their favourite programming book, but to me it was quite
enlightening. It's really simple: in C++ (and other C-like languages, I
believe, including C itself) you can use curly braces inside `switch` `case`'s
to make up separate scopes for them, enabling you to e.g. declare variables
with the same name over and over again (and even with different types). [That
came in handy in one of my student
projects](https://github.com/SmirnoffYM/AI-simulator/blob/8f0e3e615f4704b7ea11f98681f39fbcf12ccde3/commodule.cpp#L121).

The second trick is a lot more interesting. I stumbled upon it while reading
[shooter](https://github.com/grouzen/shooter), a TUI game. Just as the first
one, it deals with dispatching, only this time we want to call different
functions depending on the message type. Approach is stunning: let's get rid of
`switch`! To do so, we must:

1. define static array of pointers to functions we want to call; and
2. use message type as an index in the array of pointers.

That, of course, requires message types to be defined in some `enum`eration,
and pointers to functions should be in the same order as the corresponding
message types. While the first requirement is true for `switch` as well, the
second one is the main disadvantage of the trick: if order gets out of sync,
you'll get quite tricky bug to find.

Effectively, that's a [vtable](https://en.wikipedia.org/wiki/Virtual_method_table).

To see how this trick looks like in the wild, see [that array
definition](https://github.com/grouzen/shooter/blob/ea20156449a8e7681868241d7511ef3af8bb483b/src/cdata.c#L268)
and [this usage
example](https://github.com/grouzen/shooter/blob/ea20156449a8e7681868241d7511ef3af8bb483b/src/cdata.c#L314).

Thanks for reading!
