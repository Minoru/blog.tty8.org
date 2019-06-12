---
title: From Obnam to Borgbackup
language: english 
description: # No double quotes; end with a period
tags: linux, thoughts
---

It's hard to start making backups: you need external storage, some sort of
reminders or automation, and you have to configure the software. For years, this
kept me from putting any sort of backup solution in place.

Then the stars aligned just right: I got into habit of using Taskwarrior every
day, I just bought a huge USB HDD, and most important of all—I stumbled upon
[Obnam][obnam]. Through some personal story, its user manual got me excited
about backups, and within minutes I found myself making one. A week later,
I made another, and kept doing it for two more years.

Then Obnam was [retired][the-announcement], and I had to turn to other programs
for replacement. I started running Zbackup, Restic and Borgbackup in parallel,
for comparison's sake.

Eight months later, here's the things I learned:

- Zbackup:
    - provides outstanding compression and deduplication, but you pay with
        backup time (roughtly 3× smaller but 5× slower than Borg). I backup to
        an USB HDD, so the balance was wrong for me;
    - it's just a "storage layer", so you have to come up with means to gather
        your files and snapshot naming convention (most probably `tar` and `date
        +%y.%m.%d-%H:%M:%S`);
- Restic:
    - asks for password on each invocation. I have LUKS on my backup medium, and
        I don't share that disk with anyone, so Restic encryption was
        meaningless to me;
    - repository is roughly 1.5 times larger than Borg's, while backup is only
        slightly faster;
- Borgbackup:
    - smarter than Zbackup but dumber than Restic, it requires you to name your
        snapshots yourself (I went with `date` again);
    - perfect balance between speed and repository size (for me, at least).

As you already know from the post's title, and as logically follows from the
list above, I chose Borg. But the real motivation behind this post is different:
using these programs highlighted one aspect of Obnam that I never noticed while
using it. That aspect is user experience, and how streamlined in was in Obnam.

For example, Obnam relied more on the config file than command line arguments,
which is logical—hopefully you configure it once and run thousands of times.
Obnam took care of snapshot naming, tagging them by date-time and not asking for
anything else. Obnam didn't split repo initialization and backup itself,
thus removing yet another little obstacle.

In short, Obnam provided a *solution* rather than a *building block*.

The truth is, I wouldn't have started making backups with Borg, or even Restic
(which has a bit nicer CLI). They bombard the novice with a lot of
barely-relevant details. Obnam solved my problem *before* teaching me to solve
it, and for that, I'm eternally grateful.

[obnam]: https://obnam.org/
    "Obnam"

[the-announcement]: https://blog.liw.fi/posts/2017/08/13/retiring_obnam/
    "Retiring Obnam"
