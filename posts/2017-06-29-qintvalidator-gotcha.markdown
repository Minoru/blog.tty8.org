---
title: QIntValidator gotcha
language: english
description: If validator's range is “sloped” towards the negative, e.g. it’s
    from -128 to 127, your validator will seemingly accept “128”. In fact,
    it’ll see 128 as intermediate state, so you can detect and handle that in
    your UI.
tags: tips'n'tricks, programming
---

<div class="epigraph">
<p style="font-style: italic;">
Programmers are not to be measured by their ingenuity and their logic but by the
completeness of their case analysis.
</p>
<p>
—Alan J. Perlis, [“Epigrams on Programming”][epigrams]
</p>
</div>

The problem I'm going to present is actually dirt-simple, and its causes are
even described in [the official docs][validate-docs]. Yet the Google itself
couldn't find the solution when I needed it, so let's help the tech giant out
by blogging about it.

A few days ago I was working on a seemingly simple task: given a description of
an integer type (e.g. signed 32-bit, unsigned 64-bit), ensure that a `QLineEdit`
will only accept numbers that fit into that type. Luckily, Qt already has some
input-checking facilities, letting us attach a so-called *validator* to an input
field. Furthermore, there's a standard `QIntValidator` that determines if an
integer is in the given range—just what I needed!

I tested it with unsigned 8-bit first, and everything went well. But then
I tried *signed* type and immediately discovered that I can input "128" even
though the upper bound is 127. But why? Maybe `QIntValidator`'s range is
actually a half-open interval? Nope, [it's a closed one all
right][constructor-docs]. Have I made an error calculating the bounds? A few
`qDebug()`s confirmed I didn't, and hard-coding the upper limit didn't change
a thing either. What the hell?

Going through the customary ritual of losing faith first in myself, then in my
compiler, and finally in my machine, I printed out the result the validator
returns to `QLineEdit`. Sure enough, it was `Acceptable` for -128, and
`Intermediate` for 128. Because, y'know, "128" is just "-128" without a minus;
the validator deems it okay for the user to input their numbers in whichever
order they please.

So if you've come here from Google, don't worry: `QIntValidator`'s lower bound
is inclusive; and `QIntValidator`'s upper bound is inclusive, too. You just
have a range that's "sloped" towards the negative numbers, and thus your
validator seemingly accepts values bigger than it should. Connect to
[`QLineEdit::​textEdited(const QString&)`][textedited] and change input's
border color to red or something if the result is not `Acceptable`, and you'll
be fine.

[epigrams]:
    https://web.archive.org/web/19990117034445/http://www-pu.informatik.uni-tuebingen.de/users/klaeren/epigrams.html
    "Epigrams on Programming — Internet Archive Wayback Machine"
[validate-docs]:
    https://doc.qt.io/qt-5/qintvalidator.html#validate
    "QIntValidator class | Qt GUI 5.9"
[constructor-docs]:
    https://doc.qt.io/qt-5/qintvalidator.html#QIntValidator-1
    "QIntValidator class | Qt GUI 5.9"
[textEdited]:
    https://doc.qt.io/qt-5/qlineedit.html#textEdited
    "QLineEdit class | Qt Widgets 5.9"
