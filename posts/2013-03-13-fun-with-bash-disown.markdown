---
title: On BASH that has no &!, and old tricks suddenly rediscovered
tags: linux
language: english
description: In BASH, &! sends the process to background but doesn't detach it.
    Yet I managed to run it in such a way that I thought it *did* detach.
    Here’s a postmortem.
---

**TL;DR:** *don't want to spoil the fun, so please check [Summary](#summary) at
the end.*

A friend of mine just posted [a
recipe](http://bytopia.org/2013/03/13/using-emacs-as-default-system-editor/)
describing how to avoid long Emacs startup delays using Emacs daemon. Being kind
of a portability advocate, I pointed out to him that `&!` works only in
[ZSH](http://zsh.org) and thus should not be used in a seemingly shell-agnostic
post.

"It's specific to ZSH and won't work in BASH", I said.

"Yet it does. I checked", was his answer.

So I checked too: I opened a terminal window, fired up BASH (had to do it
manually because I'm ZSH user myself) and ran the first GUI application that
came to my mind:

    % bash                 # percent prompt indicates ZSH
    $ easytag &!           # dollar sign prompt means BASH

That gave me some output, which I ignored. A moment later, EasyTag appeared on
the screen. I then quitted BASH by pressing `Ctrl-D`, and was really surprised
when EasyTag didn't die. It didn't even die when I closed the window, which
surprised me even further.

"That's not right", I thought. "Wait, was was that output I ignored?.."

So yeah, in BASH, `&!` prints this:

    [1] 15878

Looks familiar, isn't it? That's because it *is* familiar:

    $ easytag &
    [1] 15926

Can it be so that BASH ignores trailing exclamation mark? Let's write a command
list and see what happens:

    $ echo a & echo b
    [1] 16010
    b
    a
    $ echo a &! echo b
    [2] 16012
    b
    a
    [1]   Done                    echo a

Seems logical to take one more step:

    $ echo a & ! echo b
    [3] 16033
    b
    [2]   Done                    echo a
    a

So indeed, BASH parses ampersand and exclamation mark as two separate operators.
It's obvious what ampersand does, but what about `!`? Intuition suggests that it
should have something to do with negation, but there's no variable to negate
here. Or is there?

    $ true ; echo $?
    0
    $ false ; echo $?
    1

That's right: each command returns an integer called *an exit code*, and we can
negate it with exclamation mark.


    $ ! true ; echo $?
    1
    $ ! false ; echo $?
    0

As you probably figured out already, `$?` variable holds exit code of the last
executed command.

So picture just got clearer: ampersand sends a process into background, while
exclamation mark negates exit code of the following command. Except that in the
original line, there was *no* following command. So what does that `!` operator
negate, then?

    $ echo $?
    0
    $ !
    $ echo $?
    1
    $ !
    $ echo $?
    1

Without argument, negation returns positive integer, indicating false. Logic
suggests that empty command should return 0:

    % bash -c '' ; echo $?
    0

Good.

So we effectively established that in out case exclamation mark does nothing and
can be safely ignored. But why, then, EasyTag won't die when we exit the shell
that started it?

This one is simple: it's because I'm ZSH user :)

When I fire up a terminal, an instance of ZSH starts up automatically. I then
run `bash` inside of it. That means that ZSH forks and runs `bash` in the child
process, waiting for it to finish. I then run `easytag` inside of BASH, which
again means forking and waiting. As I use `&` at the end of the command, EasyTag
is being put into foreground, returning control to me and thus enabling me to
exit shell — which I do.

At this point, a lot of interesting things happen. First, as I close `bash`, its
children (only one in this case, i.e. EasyTag) are being left orphans. `init`,
the first process, adopts them. That means that EasyTag is no longer tied to
`bash` instance from which it originated. There's no connection to ZSH either,
so I can exit it too — EasyTag would keep running right until `init` kills it
(probably upon shutdown) or user quits it.

As for `bash` instance that I quit, its exit code is collected by ZSH.

This pattern is called double forking and was described in Stevens' "Advanced
Programming in the UNIX Environment" long time ago. It is used to spawn
processes that should run independently of their parents — exactly what `disown`
does.

And that is the solution to the whole mystery of the test where BASH that has
no &! somehow disowns commands when `&!` is used.

# Summary

Both BASH and ZSH have a nice builtin called `disown`. It enables user to detach
process from the shell in a way much similar to that of `nohup`. ZSH has a nice
sugar for it: you can put `&!` at the end of the command, and it would be
detached. BASH doesn't have such sugar, but in my tests the same `&!` worked
nonetheless. It confused me at first, but after a little investigation it turned
out to be a two-faced problem.

First of all, BASH parses `&!` not as a single operator but as a two separate
ones. `&` puts a job into background, while `!` negates exit code of the next
command. When there's no argument to negation operator, it simply returns 1
(false). If one doesn't care about exit codes, `&!` is equivalent to simple `&`.

Second, I tested this while running bash inside ZSH, as the latter is my default
shell. By chance, that led me to performing an old systems programming trick
known as a ["double
fork"](http://thinkiii.blogspot.com/2009/12/double-fork-to-avoid-zombie-process.html)
(first described in W. Richard Stevens' "Advanced Programming in the UNIX
Environment"). That means that I effectively disowned by hand.

So with a little effort, I yet again ensure that I'm sane, machines are fine and
world hasn't gone crazy. Till next time, then!

**Update (04.05.2013):** rewrite a few sentences.
