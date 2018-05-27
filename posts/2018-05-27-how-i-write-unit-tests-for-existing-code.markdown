---
title: How I write unit tests for existing code
language: english
description: First write an outline, then fill it in; finally, increase the
    coverage as much as you can.
tags: howto, programming
---

I have a confession to make: even though I recognize the value of automated
testing, I never got a hang of TDD, and all of my projects got their tests
written after the fact. As a result, I had to come up with my own techniques for
designing test suites. Here's one: start with a test plan.

First, look at the signatures of your methods or functions[^1], and also at
their docs. Think about actions they perform and relations between their inputs
and outputs. Do *not* look at their implementations, because that'll spoil your
high-level outlook. At this stage, all you care about is descriptions. You
should end up with something like this:

```c++
TEST_CASE("strprintf() fills a string with provided parameters", "[strprintf]")
{
    SECTION("Empty format results in empty string")
    {
    }

    SECTION("%i format is replaced by an integer")
    {
    }

    SECTION("%s format is replaced by a string")
    {
    }
}
```

Now jump into low-level mode, and implement the actual checks. Still, do not
look at method implementations. Try to get as many tests as possible to pass;
comment out the rest.

After that, you're finally allowed to look at methods' code. Figure out why
failing tests fail. A coding error? Wrong assumptions? Make all the tests pass.

Finally, look at the test coverage, and try to increase it as much as you can.
Make an extra effort to ensure that your descriptions could be understood by
someone who doesn't know the implementation details.

There are a couple benefits to writing tests this way:

1. Descriptions are (mostly) tied to methods' purpose, not the way they're
   coded.
2. Separate "high-level" stage helps you notice interactions between methods
   (e.g. connection between serialization and deserialization).
3. By the time you get to "low-level" stage, you already have a good
   understanding of what properties you're testing and where, so it's easier to
   write minimal tests that only check the essential. This promotes DRY code.
4. I find it easier to do "high-level" and "low-level" thinking in separate
   passes, rather than constantly switching between the two.

[^1]: Signatures contain name, input arguments (with types if your language
  supports that), and output type (if your language support that).

[catch]:
    https://github.com/catchorg/Catch2
    "catchorg/Catch2 — A modern, C++-native, header-only, test framework for
    unit-tests, TDD and BDD - using C++11, C++14, C++17 and later (or C++03 on
    the Catch1.x branch)"
