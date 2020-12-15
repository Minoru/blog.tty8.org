---
title: Vimperator and Pentadactyl replacements for Firefox 57+
language: english
description: Firefox 57 deprecated the old API that Vimperator and Pentadactyl
    used. In this post, I'm going to review some alternative add-ons.
tags: review
---

With the release of Firefox 57 back in mid-November, thousands of Vimperator
users—or, rather, *ex-users*—learned that their add-on won't be updated anymore.
Luckily, a number of viable alternatives were already in development, so one
could just wait out and see which one will get the most traction, right?

Well, 1.5 months later there is still no clear winner. Vimperator's README lists
[all candidates][vimperator-readme-alternatives], but doesn't provide any
guidance in choosing between them. So I used each one for almost a week, and
wrote this post to share my impressions.

First off, let me list some limitations that are common to all add-ons reviewed
here. They:

- don't change browser's appearance. Instead, users should write their own
    `userChrome.css` styles (see [an example][userChrome-example] in Tridactyl
    repo);
- don't steal focus from address bar. It's auto-focused by Firefox when you open
    an empty tab. You'll have to press F6 to get it back yourself;
- don't work while page is loading. You'll have to rely on standard shortcuts
    like Ctrl-W or Ctrl-Tab if you want to close or escape such a page;
- don't work on `about:` pages and on addons.mozilla.org. This is due to
    security reasons, and probably won't ever be changed. You'll have to get
    used to it, I guess;
- don't work with X11 primary selection. You will have to manually select and
    paste URLs in some cases, as `yy` and `p` will only "see" the text in
    clipboard selection;
- don't store their configurations in readily accessible text files. Some addons
    let you backup and restore the preferences, but that's it.

With that out of the way, let's dive into distinctive features. If you're in
a hurry, you might want to skip right to [conclusions](#conclusions).

# Vimium-FF

With 8K GitHub stars and almost 11K installations, it's easily the most popular
add-on reviewed here. This is probably due to the fact that Vimium began as
a Chrome extension and has been in development since September 2009, whereas
other add-ons were started in 2017.

Default Vimium keybinds are mostly equivalent to Vimperator's, save for `o`
/ `O` / `t` / `T` which were replaced with `o` / `ge` / `O` / `gE`. I didn't
spot any more differences, but you better press `?` and check for yourself.

Similarity to Vimperator ends with keybinds, though. Instead of a Vim-style
command line, Vimium adopts that monstrosity called an omnibar. I could never
get used to it in Chromium, and now it plagues my favourite browser as well:

<div class="center">
<img src="/images/vomnibar-600px.png"
    sizes="(min-width: 769px) 35rem, 100vw"
    srcset="/images/vomnibar-600px.png 1x,
            /images/vomnibar-960px.png 1.5x,
            /images/vomnibar-1920px.png 2x"
    loading="lazy"
    alt="Vimium's “vomnibar”"
    class="bleed" />
</div>

Vimium is configured via a config with syntax similar to that of vimperatorrc.
You can back it up and restore it later.

Overall, I think anyone but Vimperator power users will find themselves right at
home with Vimium. It seems to provide pretty much every convenience possible
with new APIs.

The add-on can be found [on addons.mozilla.org][vimium-amo], and its source code
is hosted [on GitHub][vimium-github].

# Saka Key

What makes Saka Key stand out is its settings dialog:

<div class="center">
<img src="/images/saka-key-settings-600px.png"
    sizes="(min-width: 769px) 35rem, 100vw"
    srcset="/images/saka-key-settings-600px.png 1x,
            /images/saka-key-settings-754px.png 1.5x"
    loading="lazy"
    alt="Saka Key settings"
    class="bleed" />
</div>

Changing a keybind is as easy as finding it in a list and typing in a new one.
There are a few pre-defined profiles: "default", "power", "one-handed", and
"vimium"; the latter being, of course, the most convenient for us Vimperator
refugees.

Saka Key also allows you to tweak the appearance of its URL hints, using either
a user-friendly dialog, or no-less-user-friendly CSS. This is much harder to do
in other add-ons.

Saka Key omits a big chunk of Vim commands that could be brought over to
Firefox, e.g. counts, the command line, macros, autocommands, even dot command
that repeats your last action. But things that it *does* implement, it strives
to implement well. For example, when you press `gi`, other add-ons put the
cursor into the first input field they find; Saka Key hints all fields and lets
the user choose:

<div class="center">
<img src="/images/saka-key-input-fields-hints-460px.png"
    loading="lazy"
    alt="Saka Key input field hints"
    class="bleed" />
</div>

Another example: instead of reaching for omnibar, Saka Key completely defers to
well-known Firefox controls. It has a key to create a new (empty) tab, but if
you want to go somewhere, why, just put that URL where URLs always went—in the
address bar. While this slightly increases the keystroke count, I still find it
a very graceful solution.

It feels like Saka Key wants to be only as intrusive as user wants it to, which
is a good thing for people who don't necessarily want complete Vim power at
their fingertips. You can even recommend this add-on to your non-vimmer
friends who want to drive their browser with a keyboard but don't want to deal
with this whole Vim paradigm.

Unlike other add-ons, this one has a [homepage][saka-homepage]. You can also
find it [on addons.mozilla.org][saka-amo] and [on GitHub][saka-github].

# Vim Vixen

Vim Vixen is a relative newcomer because it was started in August 2017, but it's
already more popular than Saka Key (which started in February 2017). Despite
that, I still found it rough around the edges, and I struggle to provide
a coherent, structured review.

Unlike Vimium and Saka Key, Vim Vixen sports a real command line, but only four
commands are available: `:open`, `:tabopen`, `:winopen`, and `:buffer`. Keybinds
don't support counts. Configuration can be tweaked either through a crude UI in
preferences, or through a blob of JSON (and you might lose some settings if you
convert from JSON back to GUI).

It seems to "overlay" its keybinds on top of what a page already offers, whereas
other add-ons provide an "ignore mode" instead. Vim Vixen's approach might be
okay if you know for sure what keys the page handles, but I found this behaviour
to be an annoyance.

You can find Vim Vixen [on addons.mozilla.org][vim-vixen-amo], and the source
code is [on GitHub][vim-vixen-github].

# Tridactyl

First thing you'll notice about Tridactyl is the page it displays in new tabs.
It both teaches you a bit about add-on's functionality, and saves you from
a context switch (you won't have to remember native Firefox shortcuts).

Tridactyl implements a command line, but doesn't have command autocompletion
there yet. The add-on is configured just like Vim and Vimperator—with `:set`
command. Of course, there's no autocompletion for settings either. You'll be
reading the help file *a lot*, I fear. And there is no way to find out the
current value of a setting. On the upside, settings are persisted on every
change, so you can just tweak it as you go with very tight feedback loop.

Default keybinds are mostly equivalent to Vimperator, but `gt` sticks out like
a sore thumb: it mimics Vim, not Vimperator, i.e. its count argument is treated
as tab index, not as a "step size". Luckily that's easy to fix with `:bind gt
tabnext`. I should also note that Tridactyl is the only add-on that implements
dot command.

A couple of times I managed to somehow confuse Trydactyl so much that it stopped
accepting my keystrokes. I had to click somewhere to return it to its senses.

Out of all the add-ons reviewed here, Tridactyl resembles Vimperator the most.
It still has a lot of features to implement, and a lot of battles to fight to
get APIs for those features; but the groundwork is clearly there. I sincerely
hope it will become the replacement for Vimperator and Pentadactyl that it
strives to be.

You can find Tridactyl [on addons.mozilla.org][tridactyl-amo] (please give [beta
releases][tridactyl-amo-betas] a shot!) and [on GitHub][tridactyl-github].

# Conclusions

Saka Key is the most user-friendly and user-centric of all add-ons reviewed
here. It can be recommended not just to vimmers, but also to anyone who's
looking to drive their browser with a keyboard.

Vimium-FF is a solid add-on that's been around for almost a decade. It seem to
be at peace with limitations of WebExtensions API, embracing the omnibar and
letting the command line rest. I think it's the most full-featured and polished
add-on of all reviewed here.

Vim Vixen is a mixed bag. Configuration isn't all that friendly, but it's
usable. Command line is implemented, but with only four commands. Keybinds cover
all the usual stuff, but counts aren't supported. Despite all that, it's more
popular than Saka Key and Tridactyl. I probably missed some important point
while using it.

Tridactyl is the hope. In terms of features, it's almost on par with Vimium, but
what really matters is where it's *going*: Tridactyl aims to be a replacement of
Vimperator and Pentadactyl. Where it's limited by the APIs, its developers *file
bugs* with Mozilla instead of embracing or working around the problem. So if you
used to be a Vimperator power user, and especially if you're a programmer—pick
this one, it's where all the interesting stuff is going to happen.

[vimperator-readme-alternatives]:
    https://github.com/vimperator/vimperator-labs#end-of-life-and-alternatives
    "Vimperator — End of Life and Alternatives"

[userChrome-example]:
    https://raw.githubusercontent.com/cmcaine/tridactyl/23ccd185986267ccc3dcb9474488abe287b15f32/src/static/userChrome-minimal.css
    "cmcaine/tridactyl — src/static/userChrome-minimal.css"

[tridactyl-github]:
    https://github.com/cmcaine/tridactyl
    "cmcaine/tridactyl — GitHub"
[tridactyl-amo]:
    https://addons.mozilla.org/en-US/firefox/addon/tridactyl-vim/
    "Tridactyl — addons.mozilla.org"
[tridactyl-amo-betas]:
    https://addons.mozilla.org/en-US/firefox/addon/tridactyl-vim/versions/beta
    "Tridactyl beta releases — addons.mozilla.org"

[vimium-github]:
    https://github.com/philc/vimium
    "philc/vimium — GitHub"
[vimium-amo]:
    https://addons.mozilla.org/en-US/firefox/addon/vimium-ff/
    "Vimium-FF — addons.mozilla.org"

[saka-github]:
    https://github.com/lusakasa/saka-key
    "lusakasa/saka-key — GitHub"
[saka-amo]:
    https://addons.mozilla.org/en-US/firefox/addon/saka-key/
    "Saka Key — addons.mozilla.org"
[saka-homepage]:
    https://key.saka.io/
    "Saka Key"

[vim-vixen-github]:
    https://github.com/ueokande/vim-vixen
    "ueokande/vim-vixen — GitHub"
[vim-vixen-amo]:
    https://addons.mozilla.org/en-US/firefox/addon/vim-vixen/
    "Vim Vixen — addons.mozilla.org"
