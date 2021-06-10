---
title: Remapping keys under Wayland
language: english
description: # No double quotes; end with a period
tags: howto, linux
---

The Internet is full of advice on how to edit XKB files and tweak your keyboard
layout — under Xorg. With Wayland though, that advice didn't quite work for me.
Here are the problems I ran into, and the solutions I found.

# `setxkbmap -print` only shows the US layout

This command is supposed to dump your entire keymap, including layouts and
options, but for me, it only displayed the basic US layout:

```
$ setxkbmap -print

xkb_keymap {
  xkb_keycodes  { include "evdev+aliases(qwerty)" };
  xkb_types     { include "complete"      };
  xkb_compat    { include "complete"      };
  xkb_symbols   { include "pc+us+inet(evdev)"     };
  xkb_geometry  { include "pc(pc105)"     };
};
```

I expected to see my "ru" layout mentioned in `xkb_symbols`, as well as the
option to toggle layouts with Alt+Space. To fix this, I had to explicitly pass
in all my layouts and options, like so:

```
$ setxkbmap -layout us,ru -option grp:alt_space_toggle -print

xkb_keymap {
  xkb_keycodes  { include "evdev+aliases(qwerty)" };
  xkb_types     { include "complete"      };
  xkb_compat    { include "complete"      };
  xkb_symbols   { include "pc+us+ru:2+inet(evdev)+group(alt_space_toggle)" };
  xkb_geometry  { include "pc(pc105)"     };
};
```

Notice how `xkb_symbols` now correctly lists all my settings.

# `xkbcomp <file> $DISPLAY` doesn't affect anything

If you put the result of `setxkbmap -print` into a file, edit it, and run
`xkbcomp <file> $DISPLAY`, the edits should be applied to your keyboard.
However, under Wayland, `xkbcomp` seemingly doesn't do anything (and prints some
errors at that!)

Actually, it *does* do something: it changes the settings of XWayland, an X11
server that Wayland runs for backwards compatibility with Xorg. You can verify
this by running `xterm`: as an X11 app, it'll use XWayland, and thus the
settings you introduced with `xkbcomp`.

How to apply a keymap to both Wayland and XWayland though? I found two ways.

## Create a custom keyboard layout

This only works for the `xkb_symbols` section; if you want to edit anything
else, see further down.

Take your edited `xkb_symbols` and put it in a file under
_~/.config/xkb/symbols/_ (you might have to create those directories first).
Give the file a plain name with no extension. If in doubt, check
[_xkeyboard-config(7)_ manpage][man-xkeyboard-config] to ensure there's no
clashes with existing layouts. Remember, the file should only contain the
`xkb_symbols` section:

```
xkb_symbols {
    include "pc+us+ru(ruu):2+inet(evdev)+group(alt_space_toggle)"
};
```

Now specify that file as your layout. In Sway WM, that means tweaking your
_~/.sway/config_ as follows ("runomi" is my file's name):

```
input type:keyboard {
    xkb_layout runomi
}
```

As you can see, there is no need to add `xkb_options` to the config — the
options are taken care of by the layout file. I guess you could also use this
layout in Xorg now, but I didn't test that.

## Specify a complete keymap with Sway WM

If you'd like to edit not just `xkb_symbols`, but other sections too, you got to
replace the whole keymap. I don't know how to do it in an arbitrary environment,
but here's a recipe for Sway WM.

First, dump your current keymap into a file using `setxkbmap` as described
above. Let's call it _~/.Xkeymap_.

Then, edit your _~/.sway/config_ like so:

```
input type:keyboard {
    xkb_file .Xkeymap
}
```

That's it: your entire keymap is now defined by that single file, and you can
tweak it to your heart's content. Enjoy!

[man-xkeyboard-config]:
    https://man.archlinux.org/man/xkeyboard-config.7
    "xkeyboard-config - XKB data description files"
