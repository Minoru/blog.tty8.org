----
title: Looking back at 2015
language: english
description: 
tags: linux, programming, debian
----

In this post, I'd like to give shootouts to all the nice pieces of software and
technology that entered my life in year 2015. I can't stress enough how much
[VCSH][vcsh] helped in writing this post, so if you aren't a user yet,
I strongly encourage you to check it out. Мои русскоязычные читатели могут найти
полезной [мою заметку][vcsh-ru] об этой программе.

Let's start with new software.

<img src="/images/funtoo_logo.png" width="147px" height="95px"
     style="float: right; padding: 1em 0 0 0;" />
The first thing I want to mention is [Funtoo][funtoo], a Linux distro based off
Gentoo. That's right: I'm not a Debian user anymore. Well, at heart I am, but my
laptops are running Funtoo now, and I keep FreeBSD installation on my VPS.

The reason, as you probably guessed, is systemd. And no, shims aren't a valid
solution: I'm against systemd to the point I don't even want to run software
with compiled-in support for it. Source-based distros are the only ones that
give me freedom to do so, hence the switch.

I chose Funtoo because it appears to be "modernized Gentoo". I should say that
the ride is bumpy—I'd rather give up some control to Debian maintainers in
return for less administration stress—but, at least for now, that's the best
option I have.

<img src="/images/plantuml-logo.png" width="116px" height="112px"
     style="float: left; padding: 0 1em 0 0;" />
I received my Master's degree in Computer Science this past year. While
preparing my thesis, I had to create a dozen of UML diagrams. Luckily, we had no
requirements regarding the software to use, so I employed a nice front-end to
graphviz called [PlantUML][plantuml]. Back in class, we used to use an inferior
tooling that made one waste a ton of time on inconsequential things like
alignment of boxes and the way arrows overlap. PlantUML takes all these pains
out of the process: you write a text file, you run a tool, you get an image.
A nice set of [syntax files for Vim][plantuml-vim] is just the icing on the
cake. Enjoy!

<img src="/images/vim-plug-logo.png" width="200px" height="50px"
     style="float: right; padding: 1em 0 0 0;" />
I'm not a big fan of Vim plugins, but I do have a few. I've used to use
Pathogen, but it's more like a *loop running `:source`*, not a real *manager*.
[Vim-Plug][vim-plug], on the other hand, is able to pull from GitHub repos,
where all the cool kids seem to put their stuff these days anyway. I now just
run `:PlugUpdate` once in a while, and stay confident that all the stuff I want
is up to date. I keep Vim-Plug itself in [my dotfiles repo][minoru-dotfiles],
so bootstrapping is that same `:PlugUpdate` command.

# Tabular

Talking about Vim: [gsomix][gsomix] directed me to [Tabular][tabular] plugin the
other day. It robs you of yet another procrastinator's excuse: aligning your
assignment statements or what have you. Check it out.

# Tmux Plugin Manager, Tmux-Resurrect and Tmux-Continuum

Tmux is an awesome, sublime piece of software, and the only thing that could
upset me while I'm using it is an occasional reboot—Tmux can't save and restore
its own state! (Tmuxinator is not a solution for me because I don't really have
pre-set layouts—they just evolve as I work.)

Well, turns out [people write *plugin managers*][tpm] for Tmux. After that, they
proceed to actually [writing plugins][tmux-plugins]. The ones that I found most
useful are [Tmux-Resurrect][tmux-resurrect] and
[Tmux-Continuum][tmux-continuum]. The former remembers and restores your
sessions' layouts and sometimes even their contents (it can start Vim and
`:source` your Vim session file, for example). Tmux-Continuum builds on top of
that, relieving you from manually doing saves and restores—it just saves
continuously while you work, and restores once you start tmux server. Highly
commended.

# Mcabber 1.0.0

It probably was the most anticipated release for me, ever. And it [finally came
out][mcabber]! The best feature shipped is, undoubtedly, buffer smart
scrolling—if a message comes in while you're reading the backlog, you won't be
teleported back to the bottom of the buffer anymore, just as if you did `/buffer
scroll_lock` beforehand. I'll understand if you're more happy for [Message
Carbons][message-carbons] support, though.

Now on to the little discoveries in technology…

# ZSH and word splitting

One fine Saturday I bumped into a problem:
```shell
$ echo 'echo \"$1\" \"$2\" \"$3\"' > test.sh
$ chmod +x test.sh
$ D="one two three"
$ ./test.sh $D
"one two three" "" ""
```

BASH and Dash behave sanely:
```shell
$ ./test.sh $D
"one" "two" "three"
```

Turned out that one need to `setopt SH_WORD_SPLIT`.

# RFC 3676

This RFC is actually the only thing I originally wanted to write about, but it
snowballed from there. Sorry…

Okay, so you see, I'm a young guy with an old-schoolish views. I like my email
to be `plain/text`. I debated this with friends and family numerous times, and
the only argument that could sway my belief in superiority of plain text was the
problem of different screen sizes. 72 columns is fine for any computer and
pretty much any font size people around the world use, but not for mobile
devices. I couldn't look at my own emails on my smartphone without disgust. So
maybe HTML proponents are right after all?

No. There is [an elegant solution][rfc3676] to the problem. It was proposed more
than ten years ago! The gist of the idea: let's put spaces at the end of the
lines to indicate the ones that are part of a paragraph. That's it! That's the
only thing the plaintext was missing! Now viewers can understand which lines
form a paragraph and act accordingly; they can twist and wrap it however they
please, and it's totally fine! (And if the author wants some lines not to be
treated in this manner, why, they just remove the trailing space!). The common
name of the thing is `text=flowed`.

I'm so excited I couldn't write the previous paragraph without so many
exclamation marks. Sorry for that.

Mutt and Vim support that, of course; put `set text_flowed=yes` into your
`.muttrc`, `setlocal formatoptions+=aw` in your `.vimrc`, and you're ready for
the future!

&nbsp;

That's it, folks; pretty much all the fun stuff that I stumbled upon in 2015.
Hope it makes your 2016 a tad better, too. See ya!



[vcsh]: https://github.com/RichiH/vcsh/ "vsch, config manager based on Git"

[vcsh-ru]: /posts/2013-12-16-managing-home-dotfiles-with-vcsh.html "Управление
дотфайлами в $HOME с помощью vcsh"

[funtoo]: http://www.funtoo.org/ "Funtoo Linux"

[plantuml]: http://plantuml.com/ "PlantUML: Open-source tool that uses simple
textual descriptions to draw UML diagrams"

[plantuml-vim]: https://github.com/aklt/plantuml-syntax "Vim PlantUML
Syntax/Plugin/FTDetect"

[vim-plug]: https://github.com/junegunn/vim-plug "Vim-Plug, a minimalist Vim
Plugin Manager"

[minoru-dotfiles]: https://github.com/Minoru/dotfiles "@Minoru's dotfiles"

[gsomix]: https://github.com/gsomix "gsomix at GitHub"

[tabular]: https://github.com/godlygeek/tabular "Tabular, Vim script for text
filtering and alignment"

[tpm]: https://github.com/tmux-plugins/tpm "TPM, Tmux Plugin Manager"

[tmux-plugins]: https://github.com/tmux-plugins "Tmux Plugins on GitHub"

[tmux-resurrect]: https://github.com/tmux-plugins/tmux-resurrect
    "tmux-resurrect: Persists tmux environment across system restarts"

[tmux-continuum]: https://github.com/tmux-plugins/tmux-continuum
    "tmux-continuum: Continuous saving of tmux environment. Automatic restore
    when tmux is started. Automatic tmux start when computer is turned on."

[mcabber]: http://mcabber.com/ "MCabber Homepage"

[message-carbons]: http://xmpp.org/extensions/xep-0280.html "XEP-0280: Message Carbons"

[rfc3676]: https://tools.ietf.org/html/rfc3676 "RFC3676:  The Text/Plain Format
and DelSp Parameters"
