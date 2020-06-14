---
title: Teach yourself commit hygiene with git-bisect
language: english
description: # No double quotes; end with a period
tags: howto, git
---

"Make atomic commits". "Write descriptive messages". "Don't put too much into
a single submission". How many times did you read that in Git style guides and
muttered to yourself: "that's easier said than done"? These guides only tell you
how the result should look; they never explain how to get there.

Truth is, it's always a judgment call. You have to train for this. But how? Get
a playground with lots of commits that you didn't make and adopt a systematic
approach to learn from them. Luckily, both of these things already exist.

Open source is your playground; Git bisect is your analysis tool. It performs
a simple operation: given a pair of a "good" and a "bad" commits, it finds the
first "bad" commit in between the two. Bisect is mostly used to track down the
commit that introduced a particular bug. Examining that commit can help with
fixing the bug — if the commit author followed the advice from the style guides.

In this post, we will: 1) use bisect to take us to commits that introduced bugs;
2) analyse how commit messages and size help or hurt our ability to understand
the bug.

# Pick a project

Choose a non-trivial program that you're actually using and which is written in
a language that you understand. I'm going to use Pip, Python's package
installer. Let's clone its repository:

```
$ git clone https://github.com/pypa/pip.git
$ cd pip
```

And then [set up a development environment][pip-devenv]:

```
$ python -m venv venv
$ source venv/bin/activate
$ python -m pip install -e .
$ python -m pip --version
```

The last command should print out something like "pip 20.2.dev1 from
/tmp/pip/src/pip (python 3.8)". Onwards!

# Pick a bug

It should be something that you can reproduce locally. Start your search with
the ones that are already closed — they're more likely to have a well-written
report with reproduction steps.

I chose [pypa/pip#8120][the-bug]. It's simple: with Pip 20.1b1, running `pip
cache list 'twine*' --no-cache-dir` will just crash. Let's verify that:

```
$ git checkout 20.1b1
$ python -m pip cache list 'twine*' --no-cache-dir
```

It crashes indeed! Now all that's left for us to do is to...

# Pinpoint the cause

As I mentioned earlier, bisect needs a "good" and a "bad" commit. The bad one is
sorted: it's the 20.1b1 tag. What about the good one? You'll have to make
a guess and poke at the program a bit. I tried the 20.0 tag, and immediately
scored:

```
$ git checkout 20.0
$ python -m pip cache list 'twine*' --no-cache-dir
ERROR: unknown command "cache" - maybe you meant "check"
```

There is no bug, so this commit is "good". Now let's tell `git-bisect` about it:

```
$ git bisect start
$ git bisect good 20.0
$ git bisect bad 20.1b1
```

The last command checks out a commit for us to examine. Let's do just that:

```
$ python -m pip cache list 'twine*' --no-cache-dir
ERROR: unknown command "cache" - maybe you meant "check"
```

Okay, so this one is good, then:

```
$ git bisect good
```

That checks out another commit. Examine it and tell Git the results. After one
more iteration, you'll arrive at 3c1cf3e1…, which crashes. Pass that info on to
`git-bisect`:

```
$ git bisect bad
```

Repeat this until Git announces that it found the culprit:

```
04c0b0e6eb5798240cbaff49479be7892eb34453 is the first bad commit
commit 04c0b0e6eb5798240cbaff49479be7892eb34453
Author: Ellen Marie Dash <me@duckie.co>
Date:   Mon Apr 8 13:00:09 2019 -0400

    Add 'pip cache' command.

 src/pip/_internal/commands/__init__.py |   4 ++
 src/pip/_internal/commands/cache.py    | 106 +++++++++++++++++++++++++++++++++
 src/pip/_internal/utils/filesystem.py  |  14 ++++-
 tests/functional/test_cache.py         |  68 +++++++++++++++++++++
 4 files changed, 191 insertions(+), 1 deletion(-)
 create mode 100644 src/pip/_internal/commands/cache.py
 create mode 100644 tests/functional/test_cache.py
```

What does this tell us? Apparently, the bug was there from day one, ever since
the `pip cache` got introduced. Could the commit message here be any better?
Probably not; it tells us enough. It's a good commit message.

How about the changes that this commit introduces? Run `git show` and scroll
through. The diff can be separated into three chunks:

- a module that implements the command, complete with a test suite;
- a small change that hooks the new module into the command-line parser;
- a new function in the `utils.filesystem` module.

Ask yourself: which of those changes don't belong here? Could any of these be
done in separate commits? In my opinion, no. Each part supports the others, and
if any of them were pulled out, those new commits would look unsubstantiated.
Why would we add a utility function that is not used anywhere? Why would we add
a module that can't be invoked? Why would we add a CLI command that doesn't do
anything? It makes sense to bundle those into a single, atomic commit.

Now look at it from a different perspective: what's missing? Documentation is an
obvious answer, but does it really belong to this commit? I'd argue that it
does, but it's no secret that docs usually get written after the fact, so I'm
not surprised by their absence here.

At this point, it's useful to copy the first few characters of the commit ID,
run `git log --graph master`, and find the commit there. Then scroll around to
get a feel of the surrounding commits. You'll see that the commit we're looking
at was an initial implementation, followed by a couple fixups, and then the docs
were added. A lot of polish followed, some of it co-authored by another person,
probably a reviewer. Finally, the branch got merged.

Is this a good commit? I personally prefer a cleaner history, where fixups are
squashed into the commits that they fix. But as far as "dirtier" histories go,
I think this one is fine.

# Now repeat

Run `git bisect reset` and start anew. Pick a different bug, track down another
commit, and ask yourself the same questions.

Fold this into your daily practice. If you're reading this, you're probably
using Git for some projects; try this technique there. The exercise will be
useful even if you end up re-reading your own commits for which you can still
recall some details.

Pay attention to your internal voice saying "I wish this was smaller" or "I wish
this mentioned *why* the change was done". These little frustrations should
become your guiding lights. They will help you make your next commit better.

Practice will make perfect.

[pip-devenv]:
    https://pip.pypa.io/en/latest/development/getting-started/#running-pip-from-source-tree
    "Running pip From Source Tree — pip documentation"

[the-bug]:
    https://github.com/pypa/pip/issues/8120
    "'pip cache info' fails when no-cache-dir set — Issue #8120 — pypa/pip"
