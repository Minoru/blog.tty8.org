---
title: Just use wired
language: english
description: After years of being on Wi-Fi, I re-discover the blessing that
    wired connections are.
tags: hardware
---

[Michael Snoyman's blog post about his new home network setup][snoyberg-post]
reminded me of my own networking woes. After years of using Wi-Fi in hostel
rooms and rented flats, I was getting fed up with unpredictable performance,
glitches, and the constant troubleshooting. Finally, a recent move gave me an
opportunity to turn this frustration into some action, and fix the situation for
good—until another move, at least. Here's how that went.

The day I moved in, this place only had an Ethernet cable coming from ISP. With
the flat in a bit of a mess, and no real plan of what's going where, I simply
went out and bought a TP-LINK WR741ND, to spread that Ethernet to all my wired
and wireless devices. WR741ND is a cheap-ish home router: a single WAN port,
100 Mbit 4-port switch, 2.4 GHz Wi-Fi. It got me the Internet I craved, but soon
it also got me angry.

This flat is shaped like an L, so despite putting router at the very center
—that is, at the bend of the L—I still had dead spots. It was particularly bad
in the kitchen: I often listen to random YouTube chatter while I cook, but the
kitchen is at the tip of the L, insulated from the router by two walls. Signal
was never good there.

To add insult to injury, this flat is surrounded by other flats in every
dimension, and the 2.4 GHz spectrum here is like a bazaar on a Sunday morning:
crowded beyond any hope of a quiet spot somewhere.

The final nail in WR741ND's coffin was its performance. When family members used
Wi-Fi in the evening, with someone streaming something on a TV, and someone else
watching YouTube on their tablet, it was pretty much guaranteed that I'll get
lags on IRC, or the router will just clam up and require a hard reset. That only
happened once in a while, so totally a first-world problem, but it was none the
less frustrating.

All of that made me purchase TP-LINK Archer C60, ~~also known as AC1350~~[^1].
This device is a bit more expensive than the previous one, but it has a number
of useful features to show for it:

* more powerful transmitter, punching through the walls like it's nothing;
* more antennas, letting it focus the transmission, and better "hear" the
    responses;
* support for 5 GHz band, which can't go through obstacles as easily, but is
    also much less crowded. In fact, I can only see two other access points on
    5 GHz, compared to twelve on 2.4 GHz.

Now, this *almost* solved my problems. The Wi-Fi on tablets and phones became
stellar: I could now watch YouTube from any corner of the flat, for example.
However, the situation with laptops and the TV hasn't improved as much as
I hoped for. These devices are on the desks, close to the walls; the TV is
basically surrounded by a wooden closet full of stuff. With the router flat
against one of the walls, the signal had to travel in weird paths, or even
bounce around to get to the TV. Not a great situation, and it reflected in Wi-Fi
performance.

So I finally gave in, took a tape-measure, and figured out where I'd lay the
cables. A few weeks later, I spent a weekend moving furniture around and putting
all that cabling into the baseboard (which luckily had channels just for that!)
Using the old trusty WR741ND as a switch for the room at the other tip of the L,
I now had a wired "backhaul" which spread the Ethernet throughout the house,
plus Wi-Fi for phones and tablets.

This solved my problems so thoroughly that I honestly didn't think about it
again up until Michael's post dropped in my lap. With stable, reliable Ethernet
for immobile devices, and 5 GHz Wi-Fi for everything else, it's a bliss. The
only networking problems I have these days are with Windows machines, which
sometimes get confused and require a reboot to come back to their senses.

Use wired, folks, if at all possible. It's a bit more work than Wi-Fi, but if
you aren't staying in that place for a single night, Ethernet is well worth the
effort.

[^1]: Update 11.02.2020: AC\<number> is not a model name, it's just [a
  marketing
  term](https://en.wikipedia.org/wiki/IEEE_802.11ac#Advertised_Speeds) for
  throughput.

[snoyberg-post]: https://www.snoyman.com/blog/2019/06/my-new-home-network-setup
    "My new home network setup — snoyman.com"
