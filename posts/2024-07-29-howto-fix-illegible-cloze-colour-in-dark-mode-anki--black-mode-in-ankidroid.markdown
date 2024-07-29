---
title: "Howto: fix illegible cloze colour in dark-mode Anki (black mode in AnkiDroid)"
language: english
description: # No double quotes; end with a period
tags: howto
---

I use Anki to memorize quotes and learn poetry. Built-in support for ["cloze
tests"][cloze-tests] is perfect for this. It hides part of the text to check
how easily I can recall it.

Most of the time, I study on my phone which features an OLED screen. These are
capable of displaying perfect black, so I use "black mode" in AnkiDroid
(version of Anki for Android). Unfortunately, clozes are downright unreadable
in "black mode":

<div class="center">
<img src="/images/unreadable-cloze-black.jpg"
    width="360px" height="203px"
    alt="White text on black background saying: &quot;Plans are nothing; planning is everything.&nbsp;&mdash; Dwight D. Eisenhower, 34th president of USA&quot;. The word &quot;nothing&quot; is highlighted in dark blue, which is extremely hard to read on a black background" />
</div>

"Dark mode" is only marginally better:

<div class="center">
<img src="/images/unreadable-cloze-dark.jpg"
    width="360px" height="203px"
    alt="The same quote, this time as white text on dark-grey background. The word &quot;nothing&quot; is still highlighted in dark blue, which is somewhat readable on this background but not comfortably so" />
</div>

Judging by [responses from the developer][dae-response], this is not a bug.
Indeed, each card type has its own styles for its cards. As far as
I understand, the style is filled in when the card type is created, and from
there on, it can only be updated manually. Any type created before Anki
introduced "night mode" will have the contrast problem shown above.

Luckily, this is easy enough to fix, if only a bit tedious. All one has to do
is go through all the card types and edit their `.cloze` styles. Here are the
specific steps, both for desktop Anki and mobile AnkiDroid.

# What to do in desktop Anki

1. Open Anki.

2. Go to *Tools → Manage Note Types*, or press
   <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>N</kbd>.

<div class="center">
<img src="/images/anki-note-types.png"
    width="402px" height="303px"
    alt="Anki dialog with a list of note types and various buttons to edit them"
    class="bleed" />
</div>

3. For each type, select it in the list on the left and press *Cards*.

4. In the dialog that opens, choose *Styling* radio button on the left, and
   scroll the styles to find `.cloze`.

   If there is no such style, simply exit the dialog and continue to other note
   types.

<div class="center">
<img src="/images/anki-card-types-editor.png"
    width="1049px" height="688px"
    alt="Anki dialog with settings for a particular card type"
    class="bleed" />
</div>

5. When you find `.cloze` style, add a more specific version that's only active
   in dark/black modes, like so:

    ```css
    .cloze {
     font-weight: bold;
     color: blue;
    }

    .nightMode .cloze {
     color: lightblue;
    }
    ```

    `lightblue` can be [any color supported by CSS][css-colors]. Don't be
    afraid to experiment! The preview on the right shows the results you're
    getting. You can reveal the cloze by clicking *Back Preview* radio button,
    and hide it again by clicking *Front Preview*.

6. Once you've got the desired colour, click *Save* and move on to the next
   type.

# What to do in AnkiDroid (Anki for Android)

1. Fire up AnkiDroid.

2. Tap the three vertical dots in the top right and choose *Manage note types*.

<div class="center">
<img src="/images/ankidroid-note-types.jpg"
    width="270px" height="540px"
    alt="AnkiDroid dialog with a list of note types" />
</div>

3. For each type, tap *Edit cards*.

4. In the dialog that opens, choose *Styling* at the bottom right and scroll
   the styles to find `.cloze`.

   If there is no such style, simply exit the dialog and continue to other note
   types.

<div class="center">
<img src="/images/ankidroid-card-types-editor.jpg"
    width="270px" height="540px"
    alt="Anki dialog with settings for a particular card type" />
</div>

5. When you find `.cloze` style, add a more specific version that's only active
   in dark/black modes, like so:

    ```css
    .cloze {
     font-weight: bold;
     color: blue;
    }

    .nightMode .cloze {
     color: lightblue;
    }
    ```

    `lightblue` can be [any color supported by CSS][css-colors]. Don't be
    afraid to experiment! Tap the "eye" icon at the top right to preview how
    it'll look.

6. Once you've got the desired colour, tap the checkmark button to save and
   move on to the next type.

[cloze-tests]:
    https://en.wikipedia.org/wiki/Cloze_test
    "Cloze test — Wikipedia"

[dae-response]:
    https://forums.ankiweb.net/t/different-cloze-color-for-night-mode-desktop/7194/7
    "It was only added to the default notetypes when Anki introduced night mode, so if you’ve been using Anki for a number of years, your templates won’t have it. — @dae on forums.ankiweb.net"

[css-colors]:
    https://www.w3schools.com/cssref/css_colors.php
    "CSS colors — W3Schools"
