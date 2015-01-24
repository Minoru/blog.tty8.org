---
title: Migrating to LVM on LUKS
tags: debian, linux
language: english
---

In this post, I'll describe how one can migrate his/her system from whatever
partitioning scheme there already is to the following one:

* `/dev/sda1` as `/boot`;
* `/dev/sda2` as encrypted LUKS device containing LVM with `lvm-root`,
  `lvm-home` and `lvm-swap` logical volumes.

As a result, one would be prompted for a password each time Debian boots (that
is, after choosing some entry in GRUB, but before any services are being
started). LUKS also has other authenticating schemes, like a key file on
separate device, but we won't get into that.

Motivation
==========

**LUKS**: it enables one to encrypt disks, so if computer is stolen or
something, one can be pretty sure of confidential information not being
accessed.  I don't have any strong argument for LUKS in particular (that is,
except the fact that it works for me), so you can go with whatever you like.
Note, though, that this post was written with LUKS in mind, exclusively, so your
mileage with other tools may wary.

**LVM**: it enables one to resize volumes on the fly (that is, without rebooting
machine and using live CD to tweak partition table). Now this is not something
one would do often, if ever, but it was two times already that I wished for LVM
to be there. Better have it but not need it than the other way round.

Warnings and notes
==================

This post is *not* for newbies. Not because this stuff is really advanced or
something, but because you might run into troubles that you won't be able to fix
yourself. Having another Internet-enabled machine to ask questions on the forums
would definitely help here.

Backing up your data is one of the *mandatory* steps of the guide, so don't
worry if you somehow screw up — you can always go back to square one and start
afresh.

I've written this instructions after migrating two machines, one with i686 system
and another with amd64. Both are running Debian Wheezy (at the moment of writing
it was frozen for a month already, so this instructions would remain true for
the Wheezy release). All the troubles I run into are documented in the
respecting section of that post. In no way it means that I covered all
corner cases, so be prepared!

Last but not least, beware of typos! I re-read that post a few times, but it's
inherent for humans to make errors. I would very much appreciate it if you drop
me a line upon spotting any error in this post (or any other, to that matter). Be
it a typo, clumsy sentence or total incompetence on my part — check [About section
of this blog](/about.html) and write me an email.

What we need
============

1. Place to store backups.
2. Live CD with `cfdisk` (or any other disk partitioning software you're
   familiar with), `mkfs` (some filesystems need different tools to create
   them, be sure to have everything *you* need), `chroot`, `cryptsetup` and
   `lvm`.  [RIP Linux](http://rip.7bf.de/current/) is my choice here.
3. Half an hour at least (not counting time to create and then unroll backups),
   might be up to hundred times more if you run into troubles.  Definitely not
   an endeavour to start before going to sleep!
4. A day or more if you choose to wipe your disk before re-partitioning it
   (more on that later).

Let's go!
=========

While we still have working system, let's install a few packages:

```
$ sudo aptitude install lvm2 cryptsetup
```

Make sure they really install without any errors (look out for initramfs updates
and Grub errors). If there are any, better fix them before continuing, or you
would need to face them again when your system won't boot.

Then backup your data. If you have something large stored in your home
directory, move it elsewhere (I used `rsync` and another machine connected by
the network cable). Don't mind dotfiles yet, we'll back them up later. Ideally,
`ls ~` should list nothing before we start.

Now really fun stuff begins. Reboot your machine to live CD. Mount all your
partitions, then `cd` into each mountpoint and backup files with that command:

```
# tar cvpf /backups/BACKUPNAME.tar .
```

(assuming you have your backup device mounted into `/backups`). That would
`c`reate `tar` `f`ile with a given name, packing everything that you have in
the current directory, `p`reserving permissions and being `v`erbose about
what's going on.

You might also add `j` for bzip2, `J` for xz, `z` for gzip, or `a` to
auto-detect compression program from the filename. Useful if you got slow device
for backups (e.g., USB2.0 stick) but fast HDD and some CPU time to spare.

Now that backups are ready, we can really start breaking things. It's
recommended to populate your whole disk with some random data. That way, if
someone gains physical access to disk (by stealing your machine, for example),
they won't be able to say where separate files or even LVM volumes are — it all
will look like one huge randomized mess. If you don't worry about that much, or
you're sure that you willfill whole disk up at least once *before* your
machine would be stolen (I really hope it will never happen!), just skip that
step and save yourself a day or so.

For those who want to perform wiping, there are a lot of ways to populate disk
with random bits. The most secure, of course, is using some real good random
generator, say, `/dev/random`. That particular solution is not very practical,
though, because it's painfully slow. So in practice, you better write zeroes
onto encrypted volume — my laptop with Intel Core i3, for example, can generate
4 *gigabytes* of zeroes per second, which is way more faster than `/dev/random`
(and even `/dev/urandom`) and is just as secure (thanks to Łukasz Stelmach for
pointing this out; I used to suggest filling the disk with `/dev/urandom`'s
output, optionally encrypted).

Okay, so to wipe your disk, you should run the following:

```
# cryptsetup -d /dev/random -c aes-xts-plain -s 512 \
    create crypt /dev/sda
# dd if=/dev/zero of=/dev/mapper/crypt bs=1M
# cryptsetup remove crypt
```

Now on to the re-partitioning of your disk. Here I'll describe how to do that
using `cfdisk`, but you may use whatever you're comfortable with. Type the
following command:

```
# cfdisk -z /dev/sda
```

(Note that I assume you have your primary, and only, disk as `/dev/sda`. If
that's not the case, change — and possibly re-run — commands accordingly.)
Something like that would show up:

<pre><font size="-2">                           cfdisk (util-linux 2.20.1)

                              Disk Drive: /dev/sda
                       Size: 500107862016 bytes, 500.1 GB
             Heads: 255   Sectors per Track: 63   Cylinders: 60801

    Name        Flags      Part Type  FS Type          [Label]        Size (MB)
 ------------------------------------------------------------------------------
                            Pri/Log   Free Space                      500107.87*














     [   Help   ]  [   New    ]  [  Print   ]  [   Quit   ]  [  Units   ]
     [  Write   ]

                      Create new partition from free space</font></pre>

Don't panic yet — your partitions are still intact, it's just `-z` flag that
makes `cfdisk` pretend there's none. You could omit it, but then you would need
to remove all your partitions by hand.

So what we do now is create two partitions, one occupying about 100M and another
taking everything else. The first partition, which is a future `/boot`, doesn't
really need that much space; on my machines, it never occupies more than 40M,
and I have a few kernels and memtest86+ installed.

When you're done creating new partitions, write new table to disk and quit the
tool. Now, let's create file system on first partition and mount it somewhere:

```
# mkfs.ext2 /dev/sda1
# mkdir /mnt/sda1
# mount -t ext2 /dev/sda1 /mnt/sda1
```

If you had separate `/boot` partition before, you should have separate backup
for it. To unroll it, do the following:

```
# cd /mnt/sda1 && tar xvf /backups/boot.tar
```

If you didn't have separate partition for `/boot`, things are just a tiny bit
more complicated — you have to take `/boot` out of your backup of root
partition:

```
# cd /mnt/sda1 && tar xvf /backup/root.tar './boot'
# mv boot/* . && rmdir boot
```

Now it's time to deal with other partitions. First of all, let's create LUKS
device on `/dev/sda2`:

```
# cryptsetup luksFormat -c aes-xts-plain -s 512 /dev/sda2
```

That's when you would be prompted for a password. Don't worry too much, you can
change it later. Now we need to open encrypted device, i.e. create something
that would point *inside* it:

```
# cryptsetup luksOpen /dev/sda2 sda2_luks
```

Check `/dev/mapper` - you now have `sha2_luks` in there! Ain't that great? We're
halfway through now, so go have some tea, I'll wait.

Okay, we've got LUKS device open. What we do now is creating LVM inside it. To
do that, run the following commands:

```
# lvm pvcreate /dev/mapper/sda2_luks
# lvm vgcreate lvm /dev/mapper/sda2_luks
# lvm lvcreate -L15G -n root lvm
# lvm lvcreate -L8G -n swap lvm
# lvm lvcreate -l100%FREE -n home lvm
```

All new devices would be available under `/dev/mapper` and have names like
`lvm-root`. There's a few notes on that commands, though.

First of all, `lvm` at the beginning can be omitted — all of `lvm`'s subcommands
are available as separate ones. Next, sizes of volumes might be different for
you. I found that 10G is enough for root on my i686 netbook, but not for amd64
notebook. Third, it is generally suggested to have twice as much swap as RAM in
the machine, so adjust accordingly. As for home, note that there's lowercase L
there, not an uppercase one — that's what enables us to use percentage instead
of absolute value. Last, you could have created smaller volumes and then grow
them as you need — it's perfectly valid scheme, and you might like it if you
don't know how much space you would need.

Now that we have volumes, let's create filesystems and unroll backups:

```
# for vol in root home ; do \
    mkfs.ext4 /dev/mapper/lvm-$vol \
  done
# mkswap /dev/mapper/lvm-swap
# mkdir /mnt/lvm-root /mnt/lvm-home
# mount -t ext4 /dev/mapper/lvm-root /mnt/lvm-root
# mount -t ext4 /dev/mapper/lvm-home /mnt/lvm-home
# cd /mnt/lvm-root && tar xvf /backups/root.tar && \
    rm -rf boot/*
# cd /mnt/lvm-home && tar xvf /backups/home.tar
# umount /mnt/lvm-home
```

Note that we purged `/boot`'s contents — directory would be used as a mount
point now.

Our next step is `chroot`ing into the system and tweaking it so it would boot
from encrypted device:

```
# mount -o bind /dev /mnt/lvm-root/dev
# LANG=C chroot /mnt/lvm-root
# mount -t proc proc /proc
# mount -t sysfs sys /sys
# mount -t devpts devpts /dev/pts
# mount /dev/sda1 /boot
# editor /etc/initramfs-tools/modules
```

The file we just opened contains list of modules that should be present in
initramfs for it to be able to boot Linux from encrypted partition. Put the
following lines at the end of the file:

```
aes-i586
dm-crypt
dm-mod
```

If you're running 64-bit system, first line is not needed — it would only
produce "module not found" errors each time you boot. (I'm not really sure
about the last one — some guides I've read omitted them, and I didn't
experiment myself.)

Now let's edit `/etc/fstab` so system knows where to find root and other
partitions:

```
# editor /etc/fstab
```

It should look like that:

```
proc                 /proc         proc        defaults    0 0
/dev/sda1            /boot         ext2        defaults    0 0
/dev/mapper/lvm-root /             ext4        defaults    0 0
/dev/mapper/lvm-home /home         ext4        defaults    0 0
/dev/mapper/lvm-swap none          swap        sw          0 0
/dev/sr0             /media/cdrom0 udf,iso9660 user,noauto 0 0
```

"But how would machine know where to find LVM?", I hear you asking. Good
question! For that, we have another file, `/etc/crypttab`. You should edit it to
look like that:

```
sda2_luks   /dev/sda2   none   luks,tries=3
```

Last step is to tell your Debian system to actually read that file and prepare
to boot from encrypted device. We should also update GRUB to let it know of
separate boot partition:

```
# update-initramfs -k all -u -v
# update-grub
```

I think plenty of you are already curious of what initramfs is. To save you some
googling, here's the simplest explanation possible: it's a little executable
that is being read by GRUB from the `/boot` partition when you want to boot with
particular kernel (there's separate initramfs for each). We update them all to
include modules that would ask you for password, open LUKS device, map LVM
volumes into `/dev/mapper`, and proceed with booting your system.

So that's all. Let's exit chroot and reboot to our migrated system:

```
# umount /boot
# exit
# umount /mnt/lvm-root/dev
# umount /mnt/lvm-root
# shutdown -r now
```

If everything went well, you would be asked for a passphrase and then boot into
your Debian just as usual. If not, don't give up — and read on.

Mounting LUKS and LVM by hand
=============================

When you ran `lvm  lvcreate`, volumes were mapped automatically. But if you
reboot into live CD, `/dev/mapper` would be empty. So here's a quick note on how
to get it all going again.

First, we open LUKS:

```
# cryptsetup luksOpen /dev/sda2 sda2_luks
```

…and then we instruct lvm to look for a "lvm" volume group (read [LVM
HOWTO](http://tldp.org/HOWTO/LVM-HOWTO/) for details on LVM inner structure):

```
# lvm lvchange -ay lvm
```

*He's dead, Jim!* (troubleshooting section)
===========================================

**I can't boot, after GRUB menu there's an error message and nothing happens**.

If you're sure you did install `lvm2` and `cryptsetup` packages, google the
error you get and figure things out yourself — I don't know anything about your
problem and can't help.

If you did forget to install the packages, just chroot into the system and do
it now.  If you can't get network up, use another machine to get packages from
[packages.debian.org](http://packages.debian.org) or directly from your
favourite mirror, and install them using `sudo dpkg -i FILENAME.deb`. There
might be troubles with dependencies, those you would need to solve yourself.
Just download more packages and then re-install those that didn't configure
properly due to dependency problems (`sudo apt-get -f install` might be useful
here). It's tiresome, but manageable (unless you had your system in terribly
desynchronised state — then you might end up upgrading it all by hand).

**I can't chroot, it says something about ELF being wrong**.

You're trying to chroot into 32-bit system from the 64-bit one, or vice versa.
That won't work (not without a lot of trouble). Just boot with appropriate
kernel and try again.

**`update-initramfs` complains that it "can't find canonical device for X"**.

That means you have X specified as a resume device (or something), but since we
re-partitioned the disk, X doesn't exist anymore. Use the following command to
find out which file you need to change:

```
$ sudo grep X -R /etc
```

**I can't resume from hibernate (suspend to disk) anymore**.

If you use `pm-hibernate` (`pm-utils` package), add resume parameter to
`GRUB_CMDLINE_LINUX` in `/etc/default/grub`, like this:

```
GRUB_CMDLINE_LINUX="resume=/dev/mapper/lvm-swap"
```

If you use `uswsusp` package, just update its config, `/etc/uswsusp.conf`:

```
resume device = /dev/mapper/lvm-swap
```

Useful links
============

I recommend you reading [LVM HOWTO](http://tldp.org/HOWTO/LVM-HOWTO/). It's a
little bit outdated, but still useful to newbies.

You would probably find [Debian wiki entry on LVM on AES-XTS encrypted
volume](http://wiki.debian.org/AesXtsEncryptedLvm) useful. It's mostly commands
right now, though — someone willing to add more explanations?

[Gentoo wiki](https://wiki.archlinux.org/index.php/Dm-crypt_with_LUKS) has a
good entry on disk encryption as well.

 

That's all, guys and gals. Stay safe!

**Update 21.01.2015:** suggest writing zeroes onto encrypted volume as a way to
securely wipe the disk (thanks to Łukasz Stelmach for pointing this out).
