---
title: "Howto: delay a systemd service until the clock is synchronized"
language: english
description: # No double quotes; end with a period
tags: howto, linux
---

_**Summary:** use `Wants=time-sync.target` and `After=time-sync.target`. Then,
if you're using `systemd-timesyncd.service`, enable
[systemd-time-wait-sync.service(8)][]. Otherwise, tweak your NTP service to
create /run/systemd/timesync/synchronized, or [write a service][helper-service]
that blocks until the time is synchronized._

<br/>

I am using [Orange Pi PC 2][orange-pi-pc-2] and [Zabbix][] to collect statistics
about my local network. PC 2 is an excellent device for this: small, silent, and
power-efficient. It also boots up immediately when the power is applied, so it
automatically restores itself after power outages. I've been using this board
for over a year now, and it never required any maintenance.

Imagine my surprise, then, as I opened the stats the other day and found
that Zabbix is not running\! (Who's gonna watch the watchers, right?)
Luckily, the logs had a complete picture of the situation, but it
requires a bit of context to explain.

Orange Pi PC 2 lacks a hardware clock, meaning it can't count the time while
it's turned off. To compensate for that, Armbian (the distro I'm using) employs
two techniques. First of all, it occasionally saves the current time to disk.
Upon boot, this time is restored. That's obviously not very accurate, so Armbian
then waits for the network to come up, and uses NTP to synchronize the time to
within milliseconds.

While reading a file from a disk is near-instant, waiting for network and NTP is
not. Furthermore, the wait doesn't block the boot process, so Zabbix was
launched right away. Once Chrony (the NTP client) got online, it jerked the time
full 17 minutes ahead — a move that impressed Zabbix so much that it promptly
died. (`journald` noted that "the service has successfully entered the 'dead'
state". How cheerful.)

<br/>

Now on to the solution. Armbian 20.08.17 uses systemd 241, and that provides
a special unit called `time-sync.target`. According to the
[systemd.special(8)][time-sync.target], "all services where correct time is
essential should be ordered after this unit". Perfect\! Except it doesn't work:
on Armbian, the target is reached right after Chrony start-up, without waiting
for it to synchronize.

Clearly, I needed to delay the target until the synchronization is complete.
A bit of search led me to [this NixOS pull request][helper-service], which
inserts a new service in between Chrony and `time-sync.target`. That service
runs `chronyc waitsync` to, um, wait for the sync. This worked for me, but the
solution felt a bit too obscure, so I decided to blog about it.

It's while conducting the due diligence that I stumbled upon
[systemd-time-wait-sync.service(8)][]. Added [in systemd 239][added-in-239], it
does what the NixOS PR did, but more generically: it waits for either
_/run/systemd/timesync/synchronized_ to appear, or for [adjtimex(2)][] call
to happen. [The latter approach is unreliable][adjtimex-unreliable], so the file
always takes precedence.

After enabling `systemd-time-wait-sync.service` and doing a few test reboots,
I got to experience the unreliability first-hand. Chrony made such a small
adjustment that it didn't register with the waiting service, and [a dozen units
just hung, waiting for the sync to happen][hungs]. To get around that, I ditched
Chrony and switched to `systemd-timesyncd.service`, which touches the file
mentioned above and thus *reliably* unblocks `time-sync.target`.

<br/>

Long story short, here are the steps to make a systemd service wait
until the system clock is synchronized:

1.  make sure you're using `systemd-timesyncd.service`:

    ```
    sudo apt remove chrony
    sudo systemctl enable --now systemd-timesyncd.service
    ```

    Alternatively, you could amend your existing time-synching tool so that it
    creates _/run/systemd/timesync/synchronized_ once it finishes synchronizing.

    As yet another alternative, you can write a helper service that blocks until
    the time is synchronized, and order it in between `time-sync.target` and
    your time-synching tool. You then make the service pull both the
    `time-sync.target` and the helper. See [this pull request][helper-service]
    for an example of this approach.

2.  make `time-sync.target` wait until the clock is synchronized:
 
    ```
    sudo systemctl enable --now systemd-time-wait-sync.service
    ```

3.  make the service wait until `time-sync.target` is reached:

    ```
    sudo systemctl edit <name>.service
    ```

    An editor will open; type this in:
   
    ```ini
    [Unit]
    After=time-sync.target
    Wants=time-sync.target
    ```
 
    If you're the author of the service that you're delaying, you can just edit
    its _.service_ file directly; `systemctl edit` is only necessary for the
    services provided by your OS.

4.  to test the result, reboot and run the following:

    ```
    sudo journalctl -b \
        -u systemd-timesyncd.service \
        -u systemd-time-wait-sync.service \
        -u time-sync.target \
        -u <name>.service
    ```

    This will output a part of the boot log, from which it should be clear that
    your service doesn't start until the clock is synchronized.

<br/>

**Bonus:** if your service only wants *some* time to be set&hairsp;—even if
imprecise—&hairsp;you can order it after [`time-set.target`][], which [was added
in systemd 242][added-in-242].


[time-sync.target]:
    https://www.freedesktop.org/software/systemd/man/systemd.special.html#time-sync.target
    "systemd.special(8) · time-sync.target"

[systemd-time-wait-sync.service(8)]:
    https://www.freedesktop.org/software/systemd/man/systemd-time-wait-sync.service.html
    "systemd-time-wait-sync.service(8)"

[helper-service]:
    https://github.com/NixOS/nixpkgs/pull/51338/files
    "nixos: make time-sync.target block until initial adjustment with all NTP
    daemons by thoughtpolice · Pull Request #51338 · GitHub"

[orange-pi-pc-2]:
    http://www.orangepi.org/orangepipc2/
    "orange Pi Pc 2 - Orangepi"

[zabbix]:
    https://www.zabbix.com/
    "Zabbix :: The Enterprise-Class Open Source Network Monitoring Solution"

[added-in-239]:
    https://github.com/systemd/systemd/blob/7a1fe27f81dace11a25a0573dc170d86d1f92023/NEWS#L3369-L3374
    "NEWS · systemd"

[adjtimex(2)]:
    https://www.man7.org/linux/man-pages/man2/adjtimex.2.html
    "adjtimex(2) · Linux manual page"

[adjtimex-unreliable]:
    https://github.com/systemd/systemd/blob/9654645b62226263b923e79a0511fa8a4368d5dc/src/time-wait-sync/time-wait-sync.c#L117-L139
    "systemd/time-wait-sync.c at 9654645b62226263b923e79a0511fa8a4368d5dc
    · systemd/systemd"

[hungs]:
    https://github.com/systemd/systemd/issues/14061
    "systemd-time-wait-sync blocks indefinitely with alternative NTP · Issue #14061 · systemd/systemd"

[`time-set.target`]:
    https://www.freedesktop.org/software/systemd/man/systemd.special.html#time-set.target
    "systemd.special(8) · time-set.target"

[added-in-242]:
    https://github.com/systemd/systemd/blob/7a1fe27f81dace11a25a0573dc170d86d1f92023/NEWS#L2415-L2419
    "NEWS · systemd"
