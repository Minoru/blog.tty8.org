---
title: "Howto: optional type-in field in Anki"
language: english
description: "This post explains how to make Anki generate either: 1) a card with
    a question on one side and an answer on the other; or 2) a card with
    a question and an input field for the answer, all on the front side."
tags: howto
---

[Anki][anki] is a program that lets you quiz yourself periodically. It models
the so-called [forgetting curve][forgetting-curve]: by recalling some fact,
you strengthen the memory of it. I use Anki to memorize English words and
idioms, and I'm pretty sure I wouldn't have been able to express myself *this*
freely if I didn't practice with the program.

To make Anki quiz you on something, you create a *note* with one or more
*fields*. Anki turns notes into *cards*; during the quiz, it shows you the front
side of the card and waits for you to recall the back. You then flip the card
and check your answer. The appearance of each side can be controlled through
*templates* and CSS styling.

Most of the time, it's sufficient to have a single card with a "question" field
on the front and an "answer" field on the back. But other times, it's not
enough. For example, some of my cards require me to type-in the answer,
manually, *e.g.* to ensure that I memorized the spelling. I also got a number
of notes that turn into different sets of cards depending on what flags
I specify in the note.

Now here's the problem: I have a deck (a collection of notes) with some random
facts. For some of those, I'd like a simple question-answer card, but for
others, I want to input the answer by hand. How do I do this?

Anki doesn't generate cards with empty front side, so my first thought was:

1. create a new field named "Add a card with input field for Answer";
2. create a new card template where the front side is generated only if the
   aforementioned field is non-empty;
3. in my existing question-answer card, change the template to only generate the
   front when the field is empty.

Steps two and three create mutually exclusive cards; Anki would generate one or
the other, but not both. Unfortunately, the third step doesn't work: as of Anki
2.1, [the docs say][cond-replacement-docs] that "negated expressions can not be
used to control card generation". I can make Anki generate a card when the
field is non-empty, but I can't prevent it from generating a card when the
field *is* empty. Bummer.

After a bit more thinking, I realized that the same goal can be accomplished by
changing the *back* side: it will show either the answer or an input field. The
final tweak was to put the input field on the front of the card, because that's
how Anki wants it (and it does make sense). And that works!

Here's the final template I used for the front:

```plain
{{Question}}

{{#Add a card with input for Answer}}
{{type:Answer}}
{{/Add a card with input for Answer}}
```

It always shows the question, but the input field is only shown when the note
field "Add a card with input for Answer" is not empty.

The template for the back of the card looks like this:

```plain
{{FrontSide}}

{{^Add a card with input for Anwer}}
<hr id=answer>

{{Answer}}
{{/Add a card with input for Answer}}
```

It repeats the front side (which is handy because that's where the question
was), but only shows the answer if the "Add a card…" field was empty. Thus,
I get one of two cards, depending on the value of that field: either
a "question-answer" card, or a "question with a type-in field" card. Mission
accomplished!

[anki]:
    https://apps.ankiweb.net/
    "Anki - powerful, intelligent flashcards"

[forgetting-curve]:
    https://en.wikipedia.org/wiki/Forgetting_curve
    "Forgetting curve — Wikipedia"

[srs]:
    https://en.wikipedia.org/wiki/Spaced_repetition
    "Spaced repetition — Wikipedia"

[cond-replacement-docs]:
    https://apps.ankiweb.net/docs/manual.html#conditional-replacement
    "Anki Manual - Conditional Replacement"
