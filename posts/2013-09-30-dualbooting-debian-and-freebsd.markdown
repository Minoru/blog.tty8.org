---
title: Dualbooting Debian and FreeBSD
language: english
---

There's no magic in dualbooting Debian and FreeBSD. It's versions that make all
the difference. Dualbooting Squeeze and FreeBSD 9.1 turned out to be a hell of a
challenge, and only upgrading to Wheezy made it possible to finally get what I
want. But let's describe things in order.

Just to make sure we're on the same wave, here's a short recap of how GRUB2
works.

When you turn your machine on, BIOS reads Master Boot Record (MBR) of your boot
device (most probably a hard drive) and executes the code stored there. The code
is smart enough to load the next stage of the bootloader, the one that resides
in the `/boot` directory of your root partition (or wherever it is you keep it).
This latter stage presents you with a (pseudo)graphical selection of things to
load. From there on, things might go different routes, but the ultimate
destination of all is an operating system that is loaded and ready to use.

Now that we all have some mental picture of the booting process, let's proceed
to the three ways in which one can load FreeBSD with GRUB2.

The first and easiest one is chainloading. It's simple and can be used with
*any* other bootloader (operating systems always install one in the boot sector
if their "root" partition). When this mechanism is in use, GRUB2 merely loads
the code that is stored in the boot record of the specified partition, trusting
it to do the right thing. GRUB2 config (`/etc/grub.d/40_custom`, to be specific)
goes like this:

```
menuentry "FreeBSD 9.1 (chainloading)" {
    set root='(hd0,msdos1)'
    chainloader +1
}
```

Here, `(hd0,msdos1)` means *the first* partition on *the first* hard drive. Note
that hard drives are counted from zero while partitions are counted from one.

In case of FreeBSD, this approach has a side effect of presenting you with two
menus in turn: first you see GRUB2, then, after selecting FreeBSD entry there,
you see FreeBSD loader. That might or might not be the way you want it, so let's
look at the other two options.

The second way is not much different from the first: we rely on FreeBSD's own
loader again, only this time we call it directly, not through executing the
code in the boot sector. A nice way for Linux to pay its due to his brother of
the UNIX family, but that's pretty much it: nothing is gained compared to the
first approach.

If you want to use it, change your config to look like this:

```
menuentry "FreeBSD 9.1 (/boot/loader)" {
    insmod ufs2
    insmod part_bsd
    set root='(hd0,msdos1,bsd1)'
    search  --no-floppy --fs-uuid --set=root 5248225c853c926d
    kfreebsd /boot/loader
}
```

Here we load `part_bsd` module to make GRUB2 see BSD partitions inside slices
(counted from one), and `ufs2` module to enable it to read the file system on
the partition[^other-filesystems]. By setting `root` to the partition you know
to be the root of your FreeBSD installation, and then issuing `search` command
to look for it again using its UUID, you make sure that you won't get problems
later on if you decide to re-partition your disk[^why-use-uuid]. We then
proceed by specifying which partition contains `/boot` and then finally calling
the loader.

The third, and the last, approach is the most complex: it's GRUB2 loading
FreeBSD kernel directly. Here's how it looks in the config file:

```
menuentry "FreeBSD 9.1 (direct boot)" {
    insmod ufs2
    insmod part_bsd
    set root='(hd0,msdos1,bsd1)'
    search  --no-floppy --fs-uuid --set=root 5248225c853c926d
    kfreebsd /boot/kernel/kernel
    kfreebsd_loadenv /boot/device.hints
    set kFreeBSD.vfs.root.mountfrom=ufs:/dev/ufsid/5248225c853c926d
    set kFreeBSD.vfs.root.mountfrom.options=rw
}
```

Don't run away yet, it's not as bad as it looks :) The first four lines should
be familiar to you by now, and the last four are all about loading the kernel
and setting things up before finally trying to boot. Last two lines provide a
way to specify the root partition and its mount options.

Now that we reviewed all three of them, let's consider pros and cons of each
approach. Starting from the end: the third one is unreliable because it depends
on GRUB2 knowing how FreeBSD kernel wants to be loaded and initialized. Should
you ever want to stick to old Debian release while keeping up with FreeBSD
development, you might end up in a situation when your GRUB2 no longer knows how
to boot your FreeBSD.

That's not just a story I came up with to illustrate some possible
consequences: GRUB2 that is shipped with Debian Squeeze
(1.98+20100804-14+squeeze1) can't boot FreeBSD 9.1. It just fails with errors
like this (actual number might be different in your case):

```
address 0x94058 is out of range
```

To be honest, it failed with the same-looking error when I tried to apply the
second technique (calling the loader directly) — some ABIs must have changed
really much since the 1.97 release.

The first two approaches are much more reliable — the first one can even be
called "unfailable" — because they don't need that much knowledge about the
operating system they're loading. On the other hand, they both force you to go
through FreeBSD loader, which might be undesirable — me, for example, I wanted
to select an entry in GRUB2 and go right to the loading process, without need to
confirm my choice again in another menu.

And now the last fact for this post: [to boot FreeBSD 9.1 on amd64, you need
GRUB2 version 1.99-27+deb7u2][699002]. I assume 1.99-27+deb7u1, the version
shipped with Debian 7.1 (the latest point release so far), would be sufficient
for any other architecture, but for amd64, they broke booting FreeBSD ≥ 9.1 and
fixed it only in the u2. As of this writing, 1.99-27+deb7u2 is not in
Wheezy 7.1, but it is [already accepted][grub2-proposed-updates-accepted] into
[`wheezy-proposed-updates`][proposed-updates]. If you have
Debian 7.2[^debian-7.2-scheduled] or above, you've probably got the proper
version of GRUB2. For those running 7.0 and 7.1, put this line into your
`/etc/apt/sources.list` (or into a file somewhere under
`/etc/apt/sources.list.d/`):

```
deb http://ftp.debian.org/debian wheezy-proposed-updates main
```

You might want to change the address to the mirror closest to you (or make use
of [http.debian.net][hdn]) and specify `contrib` and `non-free` in addition to
`main`, but the line should be sufficient as it is.

After updating your GRUB2, you would be able to use any of the aforementioned
techniquest to boot your FreeBSD.

That's it, you've got a dualboot with Debian Wheezy and FreeBSD 9.1. I wish you
to have as much fun with it as I did figuring it all out. Happy hacking!

**Update 01.09.2013:**

* link to the announcement of Debian 7.2 schedule;
* use FreBSD's "slice" term properly;
* add a note regarding ZFS and UFS1 modules for GRUB2.

[^why-use-uuid]: And if you think *that* won't ever happen, consider the
possibility of your HDD dying of age or something.

[^debian-7.2-scheduled]: Currently [scheduled][debian-7.2-schedule-announce]
for Saturday October 12th, 2013.

[debian-7.2-schedule-announce]:
http://lists.debian.org/debian-project/2013/09/msg00089.html "Upcoming stable
point release (7.2)"

[^other-filesystems]: You should load `zfs` module if you're using ZFS instead
of UFS2, of course, or `ufs1` if you're still on UFS1.

[699002]: http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=699002 "#699002:
grub: grub 2.00 in experimental may be missing kfreebsd >= 9.1 amd64 fix"

[proposed-updates]: http://www.debian.org/releases/proposed-updates.html
"Debian Wiki: The “proposed-updates” mechanism"

[grub2-proposed-updates-accepted]:
http://release.debian.org/proposed-updates/stable.html#grub2_1.99-27+deb7u2
"Debian Queue Overview for “proposed-updates”"

[hdn]: http://lists.debian.org/debian-mirrors/2012/01/msg00025.html
"introducing http.debian.net"

