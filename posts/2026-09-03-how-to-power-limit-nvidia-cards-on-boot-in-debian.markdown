---
title: How to power-limit NVIDIA cards on boot in Debian
language: english
tags: debian, hardware, howto, linux
---

There is an ocean of blog posts about running `nvidia-smi --power-limit` on
boot, but they lack Debian's specifics and may employ SysV-style hacks like
running the command on a timer. This post is going to show The Proper Way™.

Power limit is part of the driver state, but that state is dropped when no-one
is using the driver, e.g. if the GPU is used for occasional compute rather than
video output. NVIDIA provides two ways to make the driver state persistent.
Debian uses one, and every post out there suggests to use the other. There's no
conflict really, but it seems inelegant to me.

Specifically, the non-free `nvidia-driver` package recommends
`nvidia-persistenced`, and APT follows recommendations by default. As a result,
on Debian there is no need to run `nvidia-smi --persistence-mode 1`; it's
already handled by the daemon.

With that in mind, the systemd unit should look like this:

```
[Unit]
Description=Set power limit for the NVIDIA 3090 Ti
After=nvidia-persistenced.service
Wants=nvidia-persistenced.service
ConditionPathExists=/usr/bin/nvidia-smi

[Service]
Type=oneshot
ExecStart=/usr/bin/nvidia-smi \
    --id GPU-3cd8e640-ed92-5bda-0a88-84ef5f0f6bd6 \
    --power-limit=250W

[Install]
WantedBy=multi-user.target
```

You can find out the ID of your GPU with `nvidia-smi --query-gpu=gpu_name,uuid
--format=csv`.

If you have multiple GPUs, you can add an `ExecStart` line for each. The
commands will be executed in order.

Save the code to _/etc/systemd/system/nvidia-power-limit.service_, make it
world-readable, and finally enable the unit:

```
$ sudo chmod a+r /etc/systemd/system/nvidia-power-limit.service

# Make systemd notice the new file
$ sudo systemctl daemon-reload

# Add --now to run the command immediately
$ sudo systemctl enable nvidia-power-limit.service
```

Reboot the machine and verify the result with `nvidia-smi`: it should display
the new limit right under the name of your GPU.
