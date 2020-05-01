---
title: "How to fix: Bluetooth headphones immediately disconnecting"
language: english
description: "If Blueman tells you \"Connection failed: Protocol not
    available\", you need to manually start the PulseAudio user daemon."
tags: debian, howto, linux
---

The other day, I ran into a strange problem with my Bluetooth headphones: they
connect to my PC, but immediately disconnect. When I open Blueman and try to
re-connect manually, the same thing happens, and Blueman displays a message
saying "Connection failed: Protocol not available". The headphones stay
connected to my smartphone the whole time, though, so it's obvious that the
problem is in the PC itself.

Almost by chance, I found out the cause: PulseAudio's user daemon not running.
That's easy enough to fix:

```
$ pulseaudio --exit-idle-time=-1
```

For some reason, the daemon exits automatically after 20 seconds of inactivity,
so I have to pass it a -1 to disable that "feature".
