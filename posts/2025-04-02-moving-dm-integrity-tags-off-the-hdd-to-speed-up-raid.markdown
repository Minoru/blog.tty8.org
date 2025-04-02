---
title: Moving dm-integrity tags off the HDD to speed up RAID
language: english
description: I ran into a bunch of problems trying to move dm-integrity tags
    onto a separate disk; here's how I solved them.
tags: debian, hardware, howto, linux
enable-mathjax: true
---

[Putting dm-integrity under RAID][dm-integrity-under-raid] gives reasonable
protection against silent data corruption, but on HDDs it slashes the
read-write speeds in half. The slowdown is caused by the nature of
dm-integrity. It reserves a portion of the disk for "tags", which are checksums
of data sectors. Whenever a sector is written, the corresponding tag has to be
updated; whenever a sector is read, the tag has to be read as well. On HDDs,
all that seeking tanks performance.

To combat the slowdown, dm-integrity allows storing tags on a separate disk.
Setting it up is trivial, but fitting it with the rest of the storage puzzle is
another matter. Here's the problems I ran into, along with the solutions
I settled upon.

# Note: use "flexible I/O tester", `fio`

You'll probably want to benchmark your disks before and after applying my
advice. For this, [`fio`][fio] is indispensable. It's a versatile tool written by
kernel block layer maintainers, and seem to be the de-facto standard for disk
benchmarking on Linux. Here's how to check 4K write performance of
a dm-integrity device:

    $ sudo fio --name=test --filename=/dev/mapper/integrity_29dd2d87 \
        --direct=1 --ioengine=sync --rw=write --bs=4k --numjobs=1 \
        --runtime=300s --time_based

The same command can benchmark the raw disk when given `--filename=/dev/sda`.

# Pitfall: `provided_data_sectors` always uses 512-byte sectors

You can find out more about your dm-integrity device with the following command:

    $ sudo integritysetup dump /dev/mapper/nvme0n1p3_lvm-integrity_tags_701637e5
    Info for integrity device /dev/mapper/nvme0n1p3_lvm-integrity_tags_701637e5.
    superblock_version 5
    log2_interleave_sectors 0
    integrity_tag_size 20
    journal_sections 190
    provided_data_sectors 1953521664
    sector_size 4096
    log2_blocks_per_bitmap 12
    flags fix_hmac

Here, $provided\_data\_sectors \times sector\_size$ hints at 7.27TiB, but in
fact this is a 1TiB disk. Don't get alarmed that `provided_data_sectors`
doesn't change when you specify different `--sector-size`; it always uses
512-byte sectors (I believe that's what the Linux storage layer always does).

# Problem: it's hard to precisely size the tags partition

Storing dm-integrity tags on a separate device is simple:

    $ sudo integritysetup format TAG_DEVICE --data-device=DATA_DEVICE

`DATA_DEVICE` contains the data, and is probably an entire disk. Tags are
shorter than the data sectors they're generated from, so `TAG_DEVICE` can be
smaller, but how small exactly?

It definitely can't be smaller than $tag\_size \times
\frac{data\_disk\_size}{data\_sector\_size}$. For example, SHA-1 produces 20
bytes, so for 3TiB disk with 4K sectors dm-integrity tags will require $20
\times \frac{3 * 1024 * 1024 * 1024}{4}$ bytes, or 15GiB.

On top of the tags, dm-integrity needs space for its journal. (It also supports
bitmap mode, but journal is more crash-resistant, so that's the only thing
I tried.) In my experiments, it seems like 40MiB is enough for it; additional
storage doesn't affect `journal_sections` in `integritysetup dump` output.

I might be leaving some performance on the table by not tuning the journal
further, but the loss must be negligible compared to the 2x boost I get from
moving the tags onto a separate disk. If you do dig into this, please write
a blog post and I'll link to it from here ;)

# Problem: not all integrity algorithms are supported by /etc/integritytab

This might not be a problem for you if you're running a newer distribution,
don't use /etc/integritytab, or don't rely on systemd at all.

One way to open dm-integrity devices upon boot is to describe them in
/etc/integritytab. This is a systemd config, and your version of systemd might
not support all the options that `integritysetup` supports. In my case, systemd
252 shipped with RaspberryOS doesn't provide xxhash64 hashing algorithm, so
I had to go with a larger, heavier SHA-1.

# Problem: mdraid assembles RAID straight from data devices

`integritysetup TAG_DEVICE --data-device=DATA_DEVICE` only puts its magic
numbers onto the `TAG_DEVICE`. `DATA_DEVICE` remains unchanged and looks like
a pristine disk. When dm-integrity device is added into a RAID array, mdraid
magic numbers end up on `DATA_DEVICE` at exactly the place where mdraid would
look for them, so upon next boot mdraid will add `DATA_DEVICE` into RAID without
waiting for dm-integrity to come up.

To work around that, let's make a couple tweaks to /etc/mdadm/mdadm.conf:

1. specify where to look for mdraid magic numbers:

        DEVICE /dev/mapper/integrity_*

    Assuming all your dm-integrity devices are named integrity_SOMETHING, this
    line means that mdraid only scans devices with these names. In other words,
    /dev/sda and others will be ignored.

    Theoretically this should be enough. mdraid would wait for dm-integrity
    devices to appear, read its magic numbers from them, and add them to the
    RAID. In practice though, this didn't work for me on Linux 6.12 with mdadm
    4.2.

    This line is still needed to prevent mdraid from assembling a *new* array
    out of dm-integrity data disks. Keep it.

2. to help mdraid with building the array from the correct devices, let's
   specify them directly in the config:

        ARRAY /dev/md0 metadata=1.2
            name=raspberrypi5:0
            UUID=0c778aa8:8edea278:52c7517a:33131878
            devices=/dev/mapper/integrity_29dd2d87,/dev/mapper/integrity_701637e5

    Everything up to `devices=...` part can be obtained from `mdadm --detail
    --scan`. Then you have to manually add `devices=`, enumerating dm-integrity
    disks that actually constitute the array.

After updating the config, run `update-initramfs -u` to add a copy into your
initramfs. (The command might be different if you're running something other
than Debian.)

# Problem: XFS won't mount after changing the sector size

In the process of moving the tags off the HDD, I changed dm-integrity sector
size from 512 to 4096 bytes to better match the underlying disks. To my
surprise, this prevented XFS from mounting. Here's the error from dmesg:

    XFS (dm-3): device supports 4096 byte sectors (not 512)

Thanks to the [helpful thread on the Unraid forum][xfs-offset-0], I found that
I could mount the partition with `-o offset=0`. This creates a loop device and
makes a mess of `lsblk`, but works well.

[fio]: https://fio.readthedocs.io/en/latest/
    "Welcome to FIO's documentation!"

[dm-integrity-under-raid]: https://securitypitfalls.wordpress.com/2020/09/27/making-raid-work-dm-integrity-with-md-raid/
    "Making RAID work (dm-integrity with MD-RAID) — SecurityPitfalls"

[xfs-offset-0]: https://forums.unraid.net/topic/110764-unable-to-mount-a-data-disk-in-another-linux-computer/
    "Unable to mount a data disk in another (linux) computer — Unraid forum"
