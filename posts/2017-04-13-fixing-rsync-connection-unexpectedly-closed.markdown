----
title: "Fixing “rsync: connection unexpectedly closed”"
language: english
description: "If you run into “rsync: connection unexpectedly closed” error and
    the official FAQ doesn't help, check if you have a command specified in
    ~/.ssh/authorized_keys on the remote end."
tags: tips'n'tricks, linux
----

I roll this blog out with `rsync`. Yesterday I realized that my server is
accumulating stale files, like no-longer-used stylesheets and images, so I added
`--delete` to the list of options. Upon my next deploy, I got this error:

```shell
rsync: connection unexpectedly closed (0 bytes received so far) [sender]
rsync error: error in rsync protocol data stream (code 12) at io.c(226)
[sender=3.1.2]
```

Even though the Internet has a number of solutions for this, none worked for me.
I ended up figuring it out myself, and since it's not even *hinted at* on [rsync
debugging page][rsync-issues-and-debugging], I decided to document it here.

Turns out[^1], I limited my remote shell to running just one command—the one
that `rsync` runs when it's syncing the files. I *think* I did it in order to
prevent crackers from easily exploiting the shell even if they somehow guessed
the key. In other words, my remote user's `~/.ssh/authorized_keys` looked like
this:

```
command="rsync --server -vre.iLsfxC --partial . ." ssh-rsa KEY me@host
```

When I changed my local `rsync`'s arguments, its expectations about the remote's
output changed, and the transfer failed. `rsync` sent an updated command to the
server, of course, but it was ignored because of the `command` entry above. (See
[the corresponding section in sshd(8)][man-sshd-section] for details.)

The immediate fix was simple enough: run `rsync` with `-e 'ssh -v'`, grep the
output for "debug1: Sending command", update `authorized_keys`. The long-term
solution (which I haven't deployed yet) seem to be [the `rrsync`
script][rrsync], which is bundled with `rsync` and is created to prevent
*exactly* the problem I ran into. Why didn't I just use it in the first place?


[^1]: It's always fun to re-discover the decisions you've made and completely
  forgot about, isn't it?

[rsync-issues-and-debugging]: https://rsync.samba.org/issues.html "current
    issues and debugging"

[fully-restricting-rsync-options-server-side]:
    https://learninginlinux.wordpress.com/2009/05/07/rsync-fixed-server-side-options/
    "Fully restricting rsync options server-side"

[securing-automated-rsync-over-ssh]:
    http://www.sakana.fr/blog/2008/05/07/securing-automated-rsync-over-ssh/
    "Securing automated rsync over SSH"

[man-sshd-section]:
    https://manpages.debian.org/jessie/openssh-server/sshd.8.en.html#AUTHORIZED_KEYS_FILE_FORMAT
    "sshd(8): AUTHORIZED_KEYS FILE FORMAT"

[rrsync]: https://git.samba.org/?p=rsync.git;a=blob;f=support/rrsync;hb=HEAD
    "git.samba.org - rsync.git/blob - support/rrsync"
