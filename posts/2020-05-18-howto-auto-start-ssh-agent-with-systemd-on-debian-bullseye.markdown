---
title: "Howto: auto-start ssh-agent with systemd on Debian Bullseye"
language: english 
description: Create a systemd user service and configure PAM to inject a variable into your environment.
tags: debian, howto, linux
---

*Please note: at the time of writing, Debian Bullseye is not stable yet. This
recipe might not work in a released version. Please [email
me](mailto:eual.jp@gmail.com) if that happens!*

I just moved from Xfce4 (and X11) to Sway (and Wayland). Unlike Xfce, which is
a full-fledged desktop environment, Sway is just a window manager; all it does
is lay out windows on my screen. Of all the bells and whistles that a DE like
Xfce provides, the one I most sorely miss is starting ssh-agent upon login.

# The problem

Quick recap: SSH is a way to log into remote machines, securely. Aside from
passwords, it can authenticate using cryptographic keys. Unfortunately, keys
themselves have passwords, but thanks to the magic of math, decrypted keys can
be stored in memory and re-used without much security risk. That's what
ssh-agent does.

Upon startup, ssh-agent creates a "local socket", a particular kind of file that
other programs can use to talk to the agent. But how do these programs find that
file? They read the path from an environment variable called `SSH_AUTH_SOCK`.
Since processes inherit their parents' environments, the easiest way to
initialize this variable is to set it upon login, even before the DE or WM
starts up.

# Potential solutions

To start ssh-agent before a WM, I could add a few lines to *~/.xinitrc*, but
that's for X11. How do I do this for Wayland? Unlike X11, where WMs talk to an
X client, Wayland compositors like Sway talk directly to the kernel. I could
tweak Sway's config, but that's lame. What if I switch to a different
compositor? I want a universal solution.

What other parts of the system know that I'm logging in? Systemd does! It even
has a [`graphical-session-pre.target`][graphical-session-pre], and
openssh-client comes with a user `ssh-agent.service`, hooking into the target
mentioned above. Perfect!

Except it doesn't work. Apparently, [no-one uses
`graphical-session-pre.target`][target-unused]. That's definitely a case on my
system:

```
$ systemctl --user status graphical-session-pre.target
● graphical-session-pre.target - Session services which should run early before the graphical session is brought up
     Loaded: loaded (/usr/lib/systemd/user/graphical-session-pre.target; static; vendor preset: disabled)
     Active: inactive (dead)
       Docs: man:systemd.special(7)
```

The [generally][superuser] [accepted][arch-wiki] workaround is to create
a systemd user service. That's like an autostart in a DE, but with systemd. The
ArchWiki recipe needed a tweak to work on Bullseye, so let me recount it in
full.

# The final solution

First, create a file *~/.config/systemd/user/ssh-agent.service*:

```ini
[Unit]
Description=SSH key agent

[Service]
Type=simple
# %t resolves to XDG_RUNTIME_DIR; see SPECIFIERS section in systemd.unit(5)
ExecStart=/usr/bin/ssh-agent -D -a "%t/ssh-agent.socket"

[Install]
WantedBy=default.target
```

This is a systemd unit that forks ssh-agent upon the user's first login. Now,
instruct systemd to use the service:

```
$ systemctl --user enable ssh-agent.service
```

Next, add the following to *~/.pam_environment*:

```
SSH_AUTH_SOCK DEFAULT="${XDG_RUNTIME_DIR}/ssh-agent.socket"
```

Note that the path here must be the same as the one we used in the unit file,
above.

Finally, the tweak that makes Debian Bullseye actually read the user's
environment file: append the following at the end of
*/etc/pam.d/common-session*:

```pamconf
# Read ~/.pam_environment
session required pam_env.so readenv=0 user_readenv=1
```

This file is sourced by other PAM configs, including *passwd*, *sshd*, and
*gdm-password*. By adding a single line here, we make it so that
*~/.pam_environment* is read no matter how you log into your system. And that's
it!

Bonus: with this, you don't need Xfce's ssh-agent anymore, so [disable
it][xfce4]:

```
$ xfconf-query \
    -c xfce4-session \
    -p /startup/ssh-agent/enabled \
    -n -t bool -s false
```

[graphical-session-pre]:
    https://www.freedesktop.org/software/systemd/man/systemd.special.html#graphical-session-pre.target
    "graphical-session-pre.target — systemd.special(7)"

[target-unused]:
    https://forum.manjaro.org/t/graphical-session-targets/51557/10 
    "Graphical session targets — forum.manjaro.org"

[arch-wiki]:
    https://wiki.archlinux.org/index.php/SSH_keys#ssh-agent
    "SSH keys / ssh-agent — ArchWiki"

[superuser]:
    https://superuser.com/questions/759759/writing-a-service-that-depends-on-xorg/1128905#1128905
    "Writing a service that depends on Xorg — Super User"

[xfce4]:
    https://docs.xfce.org/xfce/xfce4-session/4.14/advanced#ssh_and_gpg_agents
    "SSH and GPG agents — docs.xfce.org"
