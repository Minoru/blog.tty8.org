---
title: Automounting with udev and pmount
tags: linux, howto, debian
language: english
description: Writing a script that automatically mounts /dev/sd* as soon as
    they appear.
---

For the past few years I was relying on a lengthy `udev` rules file to mount
all the USB sticks and SD cards I put into my computer. Sometimes it
mysteriously didn't work, and each time I opened it to tweak something I gave
myself a promise to rewrite it in a more maintainable way. Well, the day has
come.

First of all, I checked out `udisks`, but it doesn't suit my needs as
it doesn't handle stuff like proper mount options (each FS requires
some unique mix of those) and disk labels (I don't like figuring out
whether `/media/sdb` is my USB pendrive or my player's SD card). As
for wrappers, none of those listed in [Arch Wiki entry on
udev][udev-arch-wiki] are available in Debian Wheezy. It was quite
obvious I have to write something of my own, again.

But I was lucky to come across [the post by Martin Monperrus][monperrus]
describing how to mount disks using `pmount`. His recipe didn't work in all
cases for me, though:

* `pmount` doesn't handle disk labels by itself — I had to write something on
  top of it to pass the label as a parameter;
* my player couldn't be mounted because right after plugging into the
  computer `pmount` failed, saying that no medium is present. The
  issue didn't appear if I tried mounting not right away but a few
  seconds later, so I figured I need to insert some delay here;
* some files I work with have names with cyrillics in them, so disks should be
  mounted with UTF-8 encoding;
* just a detail: my Nook Simple Touch seems to arrange items in the
  library depending on the access time, so I had to specify
  `--noatime` option to preserve ordering between mounts.

After all the tweaking, I got this:

```
# We wrap commands into calls to /bin/sh because PROGRAM and RUN
# accept only single commands, not command lists.
# PROGRAM figures out the label of the disk. Caching only gets in a
# way, returning old labels instead of querying for new ones, so we
# disable it. We use sed to replace non-alphanumeric symbols with
# underscore to facilitate readability and "typeability".
# RUN mounts the disk with specified label.
ACTION=="add",    KERNEL=="sd[b-z]*", PROGRAM="/bin/sh -c '/bin/sleep 2; /sbin/blkid -c /dev/null -s LABEL -o value /dev/%k | /bin/sed -r s#[^a-zA-Z0-9-]#_#g'", RUN+="/bin/sh -c '/bin/sleep 3 ; /usr/bin/pmount --umask 000 --noatime --charset utf8 %k %c'"
ACTION=="remove", KERNEL=="sd[b-z]*", RUN+="/usr/bin/pumount %k"

# the same as above, but for memory cards
ACTION=="add",    KERNEL=="mmcblk*", PROGRAM="/bin/sh -c '/bin/sleep 3; /sbin/blkid -c /dev/null -s LABEL -o value /dev/%k | /bin/sed -r s#[^a-zA-Z0-9-]#_#g'", RUN+="/bin/sh -c '/bin/sleep 4 ; /usr/bin/pmount --umask 000 --noatime --charset utf8 %k %c'"
ACTION=="remove", KERNEL=="mmcblk*", RUN+="/usr/bin/pumount %k"
```

I won't explain any of this as there's a comment at the top of the
code already. It all looks like a mess, but unfortunately there's not
much room for pretty formatting in udev rules. Anyway, it's much
better than what I had before, so (for now) I'm satisfied.

See you!

**Update (01.04.2013):** added `--charset utf8` and a note about the option.

[udev-arch-wiki]: https://wiki.archlinux.org/index.php/Udev "udev -
Archwiki"
[monperrus]:
http://www.monperrus.net/martin/automounting+usb+flash+drives+on+linux+with+udev+and+pmount
"automounting usb flash drives on linux with udev and pmount (by Martin
Monperrus)"
[man-1-udisks]: http://linux.die.net/man/1/udisks "udisks(1)"
