---
title: Package Installation Hygiene (for Babies & Older Ones)
language: english
description: Using aptitude and deborphan to get rid of stale, old and unused
    packages.
tags: debian, linux, howto
---

Two times a year, when all exams are successfully passed, every student does
that: cleaning. Different courses require different software, and it tends to
pile up, slowing updates (because there's more stuff to update) and eating disk
space (which isn't that scarce nowadays, but you still can find better use for
this GB or two). For students, cleaning also give that satisfactory feeling of
really finishing with the course.

In this post I'm going to describe how one can remove unused packages using
power of `aptitude` and `deborphan`.

# `aptitude`: removing packages we installed ourselves

Usually it's the user who pollutes the system the most. The easiest way to
figure out what to remove is to just look for packages you installed manually,
that is, what have been checked by you in Synaptics or what you specified on a
command line invoking `aptitude` or `apt-get`. Hopefully, `aptitude` enables us
to do that with a simple search query:

    $ aptitude search '~i !~M'

That command asks for installed packages (`~i`) that wasn't (`!`) installed
automatically (`~M`). This way you get a list of everything you installed
yourself. Now just scroll through it and remove everything you don't need
anymore. If you're a developer, pay close attention to `-dev` packages; on the
other hand, ordinary users may safely skip everything starting with the `lib`.

Oh, by the way, when removing packages you won't need ever more it's better to
use `purge`, not `remove`. The difference is that `purge` not only removes the
package but also deletes its configuration and data files, freeing more space
than mere `remove`.

Note that `purge` would delete files in `/etc` and other places but not in your
home directory. Skim through hidden directories (those whose names begin with a
dot) in your `$HOME` and remove everything that belongs to removed software.

# `deborphan`: removing libraries you don't need any more

It so happens that some libraries doesn't get removed along the software that
required them. To fix that, just run `deborphan` — it would show you the libs
that aren't used by any package. After examining its output you may run the
following command:

    $ sudo aptitude purge `deborphan`

(note the backtics). It would remove everything `deborphan` found. You may need
to re-run the command several times because `deborphan` may find still new
"orphans" when you `purge`d the previous ones.

# Conclusion

So that's all to it. Just don't do any of that before the trip or when you
expect to move over somewhere where Internet is a luxury: it's better to wait a
week or so to reinstall everything (if anything) you removed erroneously.
