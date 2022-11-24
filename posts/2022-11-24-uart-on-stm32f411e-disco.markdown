---
title: "UART on STM32F411E-DISCO"
language: english
description: # No double quotes; end with a period
tags: hardware
---

I'm toying with this nice evaluation board from STMicroelectronics and ran into
a couple gotchas. Finding the solutions was harder than I anticipated, so I'd
like to document them here.

# You can't connect to UART over USB

Some other boards provide a virtual COM port: you connect them to the computer
and use `screen`, `minicom` or even `echo` to communicate with `USART1`. Not so
with STM32F411E, because its ST-LINK debug probe is only V2[^1], which does
*not* support a virtual COM port[^2]. This is a hardware limitation; you can't
fix it by upgrading ST-LINK firmware or re-installing the drivers.

The solution is to get an external USB-to-UART dongle and connect it to the
board's pins. That's when you run into the second problem…

# `PA9` and `PA10` don't work

The first UART peripheral, `USART1`, can be connected to two sets of MCU pins:
(`PA9`, `PA10`) and (`PB6`, `PB7`). Unfortunately, the Discovery board re-uses
the first set for the USB peripheral. If you try to use UART with `PA9` and
`PA10`, you'll see the green LED at the bottom of the board light up — but no
signals on UART.

That's why you should use the second set of pins, or one of the other two USART
peripherals (`USART2` or `USART6`).

[^1]: see UM1842, User Manual, Discovery kit with STM32F411VE MCU, in [ST's documentation section][docs]

[^2]: see TN1235, Technical Note, Overview of ST-LINK derivatives, in [ST's documentation section][docs]

[docs]: https://www.st.com/en/evaluation-tools/32f411ediscovery.html#documentation
    "32F411EDISCOVERY — Discovery kit with STM32F411VE MCU — STMicroelectronics"
