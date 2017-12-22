---
title: Installing TWRP on Sony Xperia SP
language: english
description: TODO # No double quotes; end with a period
tags: howto, tech, hardware
---

In order to install [LineageOS][lineageos-site] (ex-CyanogenMod) on a friend's
Sony Xperia SP, I had to flash a so-called "recovery image" ([TWRP][twrp-site],
specifically). Instructions from the manual didn't work for me, but eventually
I found a solution that I describe below.

**IMPORTANT!** 1. Please read the whole post before executing any commands, lest
you miss something. 2. I'm not an Android expert and I can't be sure that this
will work for everyone. It worked for me though.

# The problem

The official [LineageOS installation guide][lineageos-installation-guide]
suggests to flash TWRP into `recovery` partition, but that fails:

```
$ sudo fastboot flash recovery twrp-3.2.1-0-huashan.img
target didn't report max-download-size
sending 'recovery' (11922 KB)...
(bootloader) USB download speed was 13905kB/s
OKAY [  0.893s]
writing 'recovery'...
(bootloader) Flash of partition 'recovery' requested
FAILED (remote: Partition not found)
finished. total time: 0.905s
```

[As TWRP site explains][twrp-problem-explanation], Sony phones indeed don't have
one. The ROM has to be flashed into "FOTA kernel" partition instead. This
requires root access to the phone.

I ended up using [TowelRoot][towelroot-site] for that. I clicked the big red
lambda (λ) to download `tr.apk`, then ran `adb install tr.apk` to install the
app. Turned on the Wi-Fi ([the app needs it to check if the phone is
rootable][why-internet-for-towelroot]), pressed the button, and that's it.

# Installing TWRP

Now the second catch: installing the image into `FOTAKernel` partition didn't
help at all. It flashed all right (i.e. `dd` gave no errors), but I couldn't get
into recovery no matter what buttons I pressed.

Thanks to [a thread on XDA-Developers][xda-thread], I found that I needed to
flash a *different* image. There are links to `twrp-3.x.x-x-boot-huashan.img`
and `twrp-3.x.x-x-fota-huashan.zip` in the top post there; download both. The
`-boot-` one needs to be flashed into the `boot` partition:

```
$ sudo fastboot flash boot twrp-3.2.1-20171218-boot-huashan.img
target didn't report max-download-size
sending 'boot' (12955 KB)...
(bootloader) USB download speed was 13891kB/s
OKAY [  0.971s]
writing 'boot'...
(bootloader) Flash of partition 'boot' requested
(bootloader) S1 partID 0x00000003, block 0x00003000-0x0000cfff
(bootloader) Erase operation complete, 0 bad blocks encountered
(bootloader) Flashing...
(bootloader) Flash operation complete
OKAY [  3.321s]
finished. total time: 4.291s
```

Upon reboot, the phone went straight into TWRP. That scared me at first, because
I couldn't boot into Android anymore; but it turned out okay in the end.

At this point, I also flashed the same image into the `FOTAKernel` partition.
XDA post mentioned this, but I don't know if it's really necessary given
I somehow booted into TWRP already. Here are the commands:

```
$ sudo adb push twrp-3.2.1-0-huashan.img /sdcard/twrp.img
twrp-3.2.1-0-huashan.img: 1 file pushed. 15.5 MB/s (12208597 bytes in 0.752s)
$ sudo adb shell
~ # dd if=/sdcard/twrp.img of=/dev/block/platform/msm_sdcc.1/by-name/FOTAKernel
23844+1 records in
23844+1 records out
12208597 bytes (11.6MB) copied, 2.841966 seconds, 4.1MB/s
```

Note that ADB here is provided by TWRP, not Android, and I suspect TWRP contains
its own `su`. So maybe I could've done without rooting the stock Android at all.
Too late for me to experiment with that, though.

With all of that done, I finally had working TWRP and could flash LineageOS.
Upon reboot, I found that the phone no longer boots straight into TWRP, but into
Android (as it should). The recovery combos work too, so looks like I didn't
break anything. Yay!

[lineageos-site]: https://lineageos.org/ "LineageOS"
[lineageos-installation-guide]: https://wiki.lineageos.org/devices/huashan/install
    "Install LineageOS on huashan"
[twrp-site]: https://twrp.me/
    "TeamWin Recovery Project"
[twrp-problem-explanation]: https://twrp.me/sony/sonyxperiasp.html 
    "TWRP for Sony Xperia SP"
[towelroot-site]: https://towelroot.com/
    "TowelRoot"
[why-internet-for-towelroot]: https://android.stackexchange.com/questions/165255/why-does-towelroot-require-internet-network-access
    "Why Does TowelRoot Require Internet / Network Access?"
[xda-thread]: https://forum.xda-developers.com/xperia-sp/orig-development/recovery-twrp-3-0-0-touch-recovery-t3309938
    "[Recovery][OFFICIAL][UBL] TWRP 3.2.1 Touch Recovery for Xperia SP"
