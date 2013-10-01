---
title: Novice's mind blown by an application of one of Unix principles
language: english
---

In Unix, everything is a file ([Unless It
Isn't][10-things-I-hate-about-(u)nix] — but that's another story).

That statement is known widely enough to be recognized even by computer folks
who never used Unix-like OS before. But knowing is one thing, applying is
another.

So I watched a friend of mine last night who installed Ubuntu and then tried to
run Windows under VMWare so he could use his printer and play some games. He
knew the principle, he just never had an opportunity to apply it. Before.

"Why, you could just run the OS you already have installed on your HDD!", I
said. After a minute of staring in amazement[^thus-the-title], and then some
more to set things up, we had everything nice and running.

I found that to be pretty educating, so I'm sharing the experience bundled with
some recipes to spread the knowledge.

So what's going on here? Well, we all are used to the idea of virtual machine
(VM) having a file on a disk representing its HDD. It usually has some special
format, like qcow2 (used by Qemu), VMDK (VMWare Virtual Machine Disk) or VDI
(Virtual Disk Image, used by VirtualBox), often providing additional features
like compression and ability to grow as more files are written to it. But some
VMs also support raw disk format, which is just a stream of bytes. Keep that
fact in mind.

Now we return to the Unix principle I mentioned in the beginning. Since
everything is a file, partitions of your HDD — why, even your entire HDD — is
just a file somewhere under `/dev`. If you have a SCSI drive, it's `/dev/sda`
(the second one would be `/dev/sdb`, third `/dev/sdc` and so on), in case of ATA
the name would be `/dev/hda` (with the same rule to get names for second, third
and so forth). These are block devices, meaning they aren't your ordinary files,
but they're still *files* of some kind! All you need now is a virtual machine
that supports them as a disk format.

<blockquote class="warning">
Before reading any further, please note that booting the same operating system
twice (once as a host, second time inside a virtual machine) will most likely
damage your files and corrupt the system.

Loading things in read-only mode is okay, though, so you are safe to boot
LiveCDs — just don't mount your `/home` or any other volume in read-write mode
if it's already mounted somewhere else.

You can also safely boot things that aren't loaded yet, so if you have dual-boot
in place you can load second OS while running the first <sup>[<i style="color:
rgb(6, 69, 173);">yo-dawg poster needed</i>]</sup>.
</blockquote>

As a matter of fact, pretty much any VM solution you might care about does
support raw disks. In case of my beloved Qemu, all you need to do is start your
virtual machine like that:

```
$ sudo qemu /dev/sda
```

That's what I like about CLI applications: the command feels natural and quite
obvious. As we will see next, GUIs tend to cloud things a lot in this case.

In case of VMWare, [you need to jump through a bit of menus][vmware-tutorial]
and "wizards"[^wizards] in order to get there, but that's doable — it worked
just fine last night. VirtualBox [apparently encourages using CLI to accomplish
the task][virtualbox-tutorial], although I'm pretty sure they should have got
some GUI for that as well. So you don't need to use some special VM — go with
whichever you like best (and if it happens not to be mentioned in this post,
don't give up and check the manual).

There's one little detail still worth mentioning: you probably won't have
permissions to read and write on `/dev/sda` (or whatever device you need).
That's exactly why I ran Qemu with `sudo` a couple of paragraphs ago. The
cleanest way to fix this is to look up which group owns the file you need, check
if you're in it and if not, add yourself in:

```console
$ stat --format "%G" /dev/sda
disk
$ # empty output of the next command means you're not in the
$ # specified group
$ groups | grep disk
$ # $USER should be expanded to your username by your shell
$ sudo usermod --append --groups disk $USER 
```

You'll have to log out and in again for groups membership to take effect, and
after that you'll finally be able to boot your VM with HDD partition as an,
well, HDD.

That's it, you learned the trick. Keep studying and don't forget to apply your
knowledge — that's the best path to mastery!

[10-things-I-hate-about-(u)nix]:
http://www.informit.com/articles/article.aspx?p=424451 "10 Things I Hate About
(U)NIX"

[^thus-the-title]: Thus the title.

[vmware-tutorial]: http://www.vmware.com/support/ws5/doc/ws_disk_add_raw.html
"Adding Physical Disks to a Virtual Machine"

[virtualbox-tutorial]: http://www.virtualbox.org/manual/ch09.html#rawdisk
"Oracle VM VirtualBox User Manual. Chapter 9. Advanced topics. 9.8.1 Using a raw
host hard disk from a guest"

[^wizards]: I'm really sorry for Windows folks, who are giving up their
opportunity to become wizards themselves in favour of some unsophisticated
programs.

