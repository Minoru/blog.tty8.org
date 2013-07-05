---
title: On home directory's file hierarchy
language: english
---

I'm thinking again of following into Joey Hess' steps and putting all my stuff
into VCS (namely, git). Right now I've got my public dotfiles under version
control (and [published at GitHub][github-dotfiles] for the world to see), and I
also keep a few directories under [git-annex][git-annex], but that's it. I
wonder what my life would look like if I keep everything but `/tmp` under VCS…

So I've been reading Joey's notes on living like that, starting with [the one he
wrote back when he had his home dir checked into CVS][cvshome]. One of the
things that astounded me is how similarly strict we are regarding the top-level
structure of our home directories. Here's Joey's listing:

    joey@silk:~>ls
    CVS/  bin/  debian/  doc/  html/  lib/  mail/  src/  tmp/

and here's mine:

    % ls                                                                ~
    audio/    docs/     misc/   pictures/  urls    words
    Desktop/  library/  music/  torrents/  video/

Motivation behind this strictness is different for us — I try to keep things
tidy so `ls -l` listings span no more than one screen, and for Joey it was CVS
that made him think about proper location and the name of the file upfront.
That's an interesting thought, by the way: by making it hard to move files
around, CVS facilitates better names and file structure. Less is more.

And that `~/tmp` is plain ingenious. Experimenting with things in `/tmp` is kind
of second nature to me, so I sometimes suffered from the pain of loosing some
interesting code I wasn't done with because of reboot. But dedicated `tmp` dir
inside my home directory should solve the problem in an easy and simple way.
Thanks Joey!

Windows desktop so polluted with icons that you can't even see the wallpaper is
a byword nowadays. I wonder how many Linux users keep their home directories as
clean as Joey's. Don't hesitate to drop me a line regarding this!

[github-dotfiles]: https://github.com/Minoru/dotfiles "Minoru/dotfiles GitHub"
[git-annex]: http://git-annex.branchable.com/ "git-annex"
[cvshome]: http://joeyh.name/cvshome/ "CVS homedir, or keeping your life in CVS"

