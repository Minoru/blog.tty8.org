---
title: Notes from migrating to a new computer
language: english
tags: debian, hardware, linux
---

A few weeks back, my trusty Dell 7577 started making a loud whirring noise.
Replacing the faulty fan made the machine quieter, but didn't fix it completely.
After discovering [it's a known problem with this model][dell-fan-noise],
I added some space between the fan and the plate it's mounted on, but that only
changed the pitch of the sound.

[dell-fan-noise]: https://www.reddit.com/r/Dell/comments/zoe63y/inspiron_7577_left_fan_noise/
    "Inspiron 7577 Left Fan Noise — r/Dell"

This laptop has *two* fans actually, and I can browse Internet with one
disabled, but heavy workloads like compiling programs overwhelms the cooling
system and CPU reaches 80°C in 20 seconds. I could put the laptop on one of
those cooling stands that blow air into the chassis, but I already spent two
weeks without a working computer, and I'd rather do something more software-y.
Thus I bit the bullet and fulfilled a decade-long dream of moving back to
a desktop PC.

That new computer comes with working SecureBoot and different CPU vendor, so
migration was a bit more involved than simply putting the old disk into the new
machine. Here's the list of all the problems I ran into, and how I fixed them.

# No graphical login screen

The laptop had Intel CPU and iGPU, the new PC has AMD silicon, and I didn't
think of installing the AMD graphics drivers beforehand. Upon first boot,
I still saw some logs, but GNOME Display Manager never came up.

This was trivial to fix by switching to a different TTY (e.g. Ctrl-F2), logging
in, and running `sudo apt install firmware-amd-graphics`. (Luckily I had another
computer—well, a smartphone—so I could [find out which driver to install via
Debian Wiki][debian-wiki-graphics-card].)

[debian-wiki-graphics-card]: https://wiki.debian.org/GraphicsCard
    "GraphicsCard — Debian Wiki"

# Wireguard not working

After installing the AMD graphics drivers, I found that my Wireguard tunnel
stopped working: it sent packets, but never received any responses.

This one took me a while to figure out. Initially I blamed graphics firmware (as
it was the most proximate cause), but uninstalling it fixed nothing. Then
I latched onto a kernel message described in the next section, but that also led
nowhere. Since Wireguard doesn't log anything by default, I learned about
[kernel's dynamic debugging support][dyndebug]. The logs said something about
"invalid handshake initiation" which apparently means the keys are wrong, but
the keys didn't change, nor did any configs on the client or the server.

[dyndebug]: https://serverfault.com/questions/1020279/how-to-see-debug-logs-for-wireguard-e-g-to-see-authentication-attempts
    "debian buster — How to see debug logs for WireGuard (e.g. to see authentication attempts) — ServerFault"

In the end, I fixed it by restarting Wireguard interface on the server. I still
have no idea what borked it so badly.

# RDSEED32 is broken

That's the error message from the kernel:

    RDSEED32 is broken. Disabling the corresponding CPUID bit.

Turns out [Zen5 has a vulnerability][zen5-rng] in its hardware RNG that makes it
return 0.

[zen5-rng]: https://www.amd.com/en/resources/product-security/bulletin/amd-sb-7055.html
    "RDSEED Failure on AMD “Zen 5” Processors — amd.com"

I'm smart though. I installed `amd64-microcode` before moving the disk from my
old Intel system to the new AMD one. That package applies any outstanding
microcode fixes on boot. Why am I still seeing the message then?

Apparently [an earlier vulnerability][microcode-signature] forced AMD to change
how microcode updates are loaded. I'm not entirely sure that's what prevented me
from getting `RDSEED32` patched via a Debian package, but it seems to be it.

[microcode-signature]: https://www.amd.com/en/resources/product-security/bulletin/amd-sb-7033.html
    "AMD CPU Microcode Signature Verification Vulnerability — amd.com"

I fixed that by upgrading the BIOS.

# NVIDIA proprietary driver not loading

Another message from the kernel:

    ERROR: could not insert 'nvidia_current': Key was rejected by service debian

That last part about the rejected key rang a bell. I had issues with SecureBoot
on my old system, so I ended up disabling it. Despite that, I still used signed
GRUB, kernel, and DKMS modules, because that's just standard nowadays. As my new
system has working SecureBoot, it checked signatures on all these components,
found that NVIDIA driver is signed with an unknown key, and rejected to load it.

Debian Wiki to the rescue once again! The SecureBoot page clearly describes [how
to enroll the DKMS key][debian-wiki-dkms]:

[debian-wiki-dkms]: https://wiki.debian.org/SecureBoot#DKMS_and_Secure_Boot
    "DKMS and Secure Boot — SecureBoot — Debian Wiki"

    # Come up with a one-time password and enter it when asked
    $ sudo mokutil --import /var/lib/dkms/mok.pub

    $ sudo systemctl reboot

    # When the MOK dialog comes up,
    # enter the one-time password to enroll the key

    # After booting, verify that the key is loaded
    $ sudo dmesg | grep cert

With that done, the proprietary driver loaded just fine. I then proceeded to
[power-limit the GPU on boot][nv-power-limit]; I didn't do that previously
because the GPU was in an eGPU box which I only connected when I needed it.

[nv-power-limit]: /posts/2026-09-03-how-to-power-limit-nvidia-cards-on-boot-in-debian.html
    "How to power-limit NVIDIA cards on boot in Debian — Debiania"

<hr/>

Well, this wasn't hard at all, even if Wireguard had me puzzled for a bit. I'm
very thankful for the Debian Wiki, and I think [the ongoing effort to improve
its quality][make-deb-wiki-better] bears results already. Forums and Reddit seem
to fill the gaps quite nicely.

[make-deb-wiki-better]: https://lwn.net/Articles/1032604/
    "Arch shares its wiki strategy with Debian — LWN.net"

I'm also very happy with the new hardware. It's quieter than my laptop, way
beefier, and doesn't have weird parts that I can only buy at one place that
ships in a week. Yay progress!

I still intend to fix the laptop and re-use it in the home lab, so [stay tuned
for more posts][subscribe].

[subscribe]: /subscribe.html
    "Subscribe — Debiania"
