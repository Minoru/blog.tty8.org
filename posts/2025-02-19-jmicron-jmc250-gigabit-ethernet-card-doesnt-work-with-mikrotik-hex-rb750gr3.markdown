---
title: JMicron JMC250 Gigabit Ethernet card doesn't work with MikroTik hEx (RB750Gr3)
language: english
description: # No double quotes; end with a period
tags: hardware
---

I would've bet it's impossible, but: two otherwise healthy Ethernet devices can
refuse to talk to each other!

Recently I was looking for some light distraction from grown-up
responsibilities, and so decided to start a homelab: a little environment where
I could play with networking, virtualisation, and maybe even home automation.
The hypervisor would run on the first laptop I ever bought, the Asus P52F from
2011. Its Intel Core i3-380M is pitifully slow by modern standards, but it will
do for a start.

I wanted to put the hypervisor behind the TV, where my "NAS"[^1] is already
located, but I didn't want to run yet another Ethernet cable  to there. So
I picked up a MikroTik hEx that was left over from earlier networking
experiments, and configured it to properly pass all the relevant VLANs.

After plugging everything in, the NAS and the TV worked fine, but the hypervisor
simply wouldn't get an IP. In fact, it wouldn't even bring up the Ethernet link;
the port was dead as if nothing was plugged into it. Some messing with  cables
and ports showed that:

* the hEx and its ports are fine, because they work with other devices
* the laptop is fine too, because its Ethernet works with MikroTik
    CRS112-8G-4S-IN and some unmanaged Mercusys 5-port Gigabit switch I have
* there must be some incompatibility between the hEx and the laptop.

This was so against my experience with Ethernet so far that I literally spent
like 10 minutes pacing the room and thinking what I could have missed.

After finally accepting the reality, I turned to web search and quickly found
[Ubuntu bug #1497005][ubuntu-bug]. There is a technology called Green Ethernet
which lets the devices negotiate the transmission power; this saves energy
because most links don't span the 100 metres that Ethernet supports. Apparently
my laptop is so old that it doesn't support Green Ethernet, and that probably
breaks the negotiation with this particular MikroTik model.

The bug suggests to run the port at 100 Mbit/s, but that's not acceptable for
me, so I didn't even try. Instead, I put the laptop on a separate Ethernet
cable, and connected everything else through the hEx.

There's the first time for everything.

[^1]: I put "NAS" in quotes because it's actually a Raspberry Pi with USB direct
    attached storage. It works for me so far, but it's a janky solution that is
    likely to fail terribly at some point.

[ubuntu-bug]: https://bugs.launchpad.net/ubuntu/+source/linux/+bug/1497005
    "Ubuntu bug #1497005: 197b:0250 JMicron JMC250 Gigabit ethernet doesn't work"
