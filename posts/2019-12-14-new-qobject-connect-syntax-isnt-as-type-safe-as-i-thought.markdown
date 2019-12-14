---
title: New QObject::connect syntax isn’t as type-safe as I thought
language: english
description: All the type-checking is done by the C++ compiler, so implicit type
    conversion is still a thing with the new signal-slot syntax.
tags: programming, qt
---

<!--
- What problem were I solving?
- How did I arrive at this particular solution?
(http://frantic.im/blogpost-contexts)

Plain Language checklist:
- written for the average reader
- has useful headings
- uses “you” and other pronouns to speak to the reader
- uses active voice
- uses short sections and short sentences
- uses the simplest tense possible—simple present is best
- omits excess words
- uses concrete, familiar words
- places words carefully:
   - avoids large gaps between the subject, the verb and the object
   - puts exceptions last
   - places modifiers correctly
- uses lists and tables to simplify complex material
- uses no more than two or three subordinate levels
(https://www.plainlanguage.gov/resources/checklists/checklist/)
-->

Qt 5 [introduced][new-syntax-wiki] a new syntax for `connect`; instead of this:

```c++
connect(
    senderObject, SIGNAL(signalName(type1, type2)),
    receiverObject, SLOT(slotName(type1, type2)));
```

one can now write this:

```c++
connect(
    senderObject, &Sender::signalName,
    receiverObject, &Receiver::slotName);
```

It has both pros and cons, but for me, the main advantage is that everything is
checked at the compile time. With the old syntax, I could make a typo in
a signal's name, and it would compile just fine. Not so with the new syntax.

By extension, I also thought that the new syntax is more type-safe: since the
types of signal/slot arguments are checked at the compile time now, I would find
out *immediately* if I were to use a slot with an incorrect arguments, right?

Not quite. Recently, I was showing the new syntax to a colleague, and we ended
up with the following code:

```c++
// this.h
private slots:
    void test(int);

// this.cpp
connect(
    ui->pushButton, &QPushButton::clicked,
    this, &This::test);
```

[`clicked` has a `bool` argument][clicked-signature], while our `test` expects
an `int` — clearly this won't compile! Yet it did.

Confused, I suggested that `connect` might be casting function pointers to
`void*`, making them pass type-checking even though they don't match. The
reality turned out to be much more mundane: this is just your ordinary C++ with
its implicit type conversion. The docs [try to paint that as an advantage of the
new syntax][type-checking-and-implicit-type-conversion], but I'm not entirely
convinced. It *does* make signals and slots blend better with C++; I'm just
skeptical that blending with that particular side of C++ is a good idea :)

Luckily I'm not the only skeptic out there. Qt 5.8 [introduced
a `QT_NO_NARROWING_CONVERSIONS_IN_CONNECT` macro][macro] that disables some of
the dumbest conversions, like `int` to `bool` and `int` to `double`. Better than
nothing, I guess.

[new-syntax-wiki]:
    https://wiki.qt.io/New_Signal_Slot_Syntax
    "New Signal Slot Syntax — Qt Wiki"

[clicked-signature]:
    https://doc.qt.io/qt-5/qabstractbutton.html#clicked
    "QAbstractButton class — Qt Widgets"

[type-checking-and-implicit-type-conversion]:
    https://doc.qt.io/qt-5/signalsandslots-syntaxes.html#type-checking-and-implicit-type-conversions
    "Differences between String-based and Functor-based Connections — Qt"

[macro]:
    https://www.kdab.com/disabling-narrowing-conversions-in-signal-slot-connections/
    "Disabling narrowing conversions in signal/slot connections — KDAB"
