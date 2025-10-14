---
title: Getting Things Done with Logseq
language: english
description: # No double quotes; end with a period
tags: review, thoughts
---

I've been using [Logseq][logseq] as the core of my GTD system for slightly over
a year now. It works, but there are a number of problems, large and small, that
make me want to switch to a better app.

Warning: this post is mostly a rant.

# Where I'm coming from

I first stumbled upon GTD back in 2008. At the time, I was studying in
university, so I didn't have a need for such a comprehensive system. The only
part that stuck was [Inbox Zero][inbox-zero] (which isn't even in the book!).
I also adopted [Taskwarrior][taskwarrior] to manage the small lists that I did
have.

Taskwarrior is very nice for jotting things down and marking them off, but is
fairly unwieldy when it comes to organizing. It's just too much typing even for
the smallest of actions. When I started working full-time, I gradually
transitioned to a paper-based system of notes and some lists.

As the workload increased, I found myself in need of a better system, and ended
up implementing full GTD on paper. I have used that for a bit over a year. Nice
pen and quality paper made it super enjoyable; I doubt any computer interface
can rival the feeling. The paper system is also very customizable. [My favourite
"hack"][decide-before-act] is to put "@Work/decide" page before the "@Work"
page, which subtly prompts me to unblock other people before I start working on
my own stuff.

The biggest pain point with paper was putting projects on hold and picking them
back up. I had to leaf through all contexts and copy all relevant next actions
to a new page; then do the opposite when the project became active again. All
this rewriting really wore me down.

# Why Logseq

I wanted "scriptable paper": something as easy to use as paper, but with some
"smarts" built-in to relieve the tedium. I also wanted it to sync between my
computer and my phone.

"Second brain" apps is all the rage these days. I considered
[Obsidian][obsidian]; it fell short of the sync requirement, and is also
proprietary, so I kept looking. [Anytype][anytype] seemed to tick all the boxes,
but is also proprietary. [Logseq][logseq] is kind of "open-source Obsidian", so
I went with that, plus [Syncthing][syncthing] for synchronization.

None of these apps have task management as their primary focus, but that's okay
because I wanted to build GTD on top anyway.

# My system as it stands

I started off with [Gregory J. Gaynor's setup][facedragons-gtd-in-logseq] and
gradually morphed it to my own liking.

Logseq journal is my primary capture tool. Clarifying also happens in the
journal. "Standalone" next actions—those that don't belong to any project—are
kept in the journal as well. Actions take the form of `NOW [[@work]] Create
ticket to refactor libfoo::BarBaz for clarity`.

Contexts are separate pages, containing queries that show the relevant actions
from all other pages (journals and projects). Some people keep separate systems
for work and personal stuff. I approach this differently: there is only one
system, but there are two separate pages to show next actions related to the two
sides of my life. I often work off them rather than a specific context.

Each project is a separate page. I have a Logseq template for them, which links
to the area of focus and contains a Logseq query to show any `WAITING` entries.
There's also a header with `project-status` for queries on the "Projects" page,
which categorizes projects by their status: active, delegated, dormant,
someday/maybe, completed, abandoned.

"Waiting for" is a separate page, containing a query to show all `WAITING` items
of all projects, plus a list of scheduled items to remind me of stuff I need to
do in the future. I also rely heavily on the calendar and reminders on my
Android phone.

Areas of focus are also separate pages, with queries similar to those on the
"Projects" page.

In addition, my Logseq graph contains checklists for daily and weekly review,
plus some assorted support material that I don't have a better place to store.

Hopefully this description paints a clear enough picture. Email me if you'd like
me to publish my setup.

# How Logseq fares at each GTD step

## Capturing

As already mentioned, Logseq journal is my primary capture tool. It works okay,
but there are many "but"s.

*Logseq is slow to start*. Both on the desktop and on Android, it takes about
10 seconds. This is less of a problem on the desktop where I just keep the app
open, but on Android this is a real PITA when I need to make a quick note while
out and about.

*On Android, it's hard to predict the page that "quick share" will end up at*.
I filed [an issue about this][quick-share-issue], but developers never
responded. There is a method to Logseq's madness, but it depends on whether or
not Android unloads Logseq when you're not using it.

*On Android, Logseq doesn't record audio when the screen turns off*. There is no
warning about this, neither from Android nor from Logseq—just an audio file with
silence. Given my earlier bug report was ignored for a year and then closed by
stale-bot, I'm not filing anything about this one.

*Logseq doesn't play nice with Syncthing*. Or maybe it's the other way around,
but I didn't notice any problems with other files I share via Syncthing. The
crux of the matter is that at the start of the day, Logseq creates a new
journal. If that happens on two disconnected machines *and* I update the journal
on one of them, eventual synchronization might erase that update. This seem to
especially affect the first entry of the journal, which led me to always put
a few empty entries at the start of each journal.

## Clarifying

Logseq works really good for this; much better than Taskwarrior and especially
paper. I can jot stuff down, fill in some more details when I have a minute, and
turn it into concrete next actions when I sit at the computer in the evening.
But there are still a few gripes.

*Logseq can lose input because the UI is sluggish*. Especially if I have a few
side pages open, or if my input affects a query on the page, Logseq can lag so
much that it loses parts of words. It's also quite slow when creating a new
entry with <kbd>Enter</kbd>; I routinely end up with the first few characters of
the new entry stuck at the end of the previous one, because I started typing
before <kbd>Enter</kbd> was handled.

*Logseq can lose input if `/commands` are invoked*. This is yet another bug
I won't bother to report. If one edits an entry, types some text and then
immediately invokes a `/command`, some of the text might get deleted. The
workaround is to exit the editing with <kbd>Escape</kbd>, then start it again
with <kbd>Enter</kbd>, and then invoke the `/command`.

## Organizing

Remember, this is the step where I had the most problems with paper. Logseq
works fairly well for this, and fulfills my dream of "built-in "smarts". Those
new capabilities bring new problems though.

*New projects might not show up on queries until I re-index my graph*.
Re-indexing takes time proportional to the number of pages, and since I add one
new journal per day, this wastes more and more of my time every day. Worse
still, this behaviour makes me distrust my "trusted system", because it might
not show me some of the stuff I have to do.

*Search UI is sluggish*. I'm now in the habit of waiting a couple seconds before
tapping a result, because I know that results might change under my finger and
I'll end up on a page I didn't intend to visit.

*Queried entry might disappear from under you as you edit it*. This is something
that I understand as a programmer but hate as a user. If I have a query that
shows all the actions that mention `@smartphone`, and I remove `@smartphone`
while editing it, it *might* disappear because it no longer matches the query.
Or it might not; I don't have a mental model of when exactly that happens.

## Reflecting

This works good because my demands are low. OTOH I think I'm at the limit of
what Logseq can do for me here.

*No easy way to see what I did recently*. I like dashboards and other ways to
appreciate my recent past. I haven't found a way to list my recently completed
actions in Logseq, which makes me sad because my memory certainly don't hold any
of this stuff.

*UI is sluggish when many side pages are open*. During weekly review, I usually
open all my active projects and go through them one by one. With 20 to 40 side
pages open, Logseq becomes noticeably slow to scroll and accept input.

## Engaging

This works well, even though the UI could be improved.

*Query output is sparse*. There are two ways to display query output: table and
list. Table doesn't suit my needs because the entire next action might not fit
on a single line. List works better, but it adds a little header with a page
name to each action, plus a lot of white space in between actions from different
pages. Given how most of the time a list is a mix of actions from a dozen
projects plus a few journals, a lot of space is wasted, and I end up scrolling
around a lot.

*Query output changes from invocation to invocation*. Tables can be sorted by
a particular column; lists can't. If I click on an action, complete it, and go
back to the list, the order of the entries might be different. This is sometimes
a problem when I had a plan to work on items in particular order, and now I have
to look through the list again to find my next item.

# Why I'll be looking elsewhere

Clearly I have a love-hate relationship with Logseq. It enabled me to create
a system where juggling projects is easy; but it also added so many paper cuts
that I sometimes dread using the tool.

This experience led me to realize that I want an actual "GTD app", i.e.
something that operates directly with the concepts of "next action", "project",
"context" and so forth. Building all of that on top of Logseq was fun initially,
but it's annoying to see all the duct tape showing through the cracks.

Perfect is the enemy of the good, though, and I don't have time to look for
a new app right now anyway, so I'll hobble along with my current setup for the
foreseeable future.

[inbox-zero]: https://www.43folders.com/2006/03/13/inbox-zero
    "Inbox Zero | 43Folders"

[taskwarrior]: https://taskwarrior.org/
    "Taskwarrior"

[decide-before-act]: https://functional.cafe/@minoru/111517542528431212
    "@minoru@functional.cafe: One benefit of implementing GTD with a paper notebook is…"

[obsidian]: https://obsidian.md/
    "Obsidian — Sharpen your thinking"

[anytype]: https://anytype.io/
    "The Everything App"

[logseq]: https://logseq.com/
    "Logseq — a privacy-first, open-soruce knowledge base"

[syncthing]: https://syncthing.net/
    "Syncthing"

[facedragons-gtd-in-logseq]: https://facedragons.com/productivity/gtd-in-logseq/
    "Set Up GTD in Logseq in 3 Steps - Face Dragons"

[quick-share-issue]: https://github.com/logseq/logseq/issues/11464
    "On Android, quick capture sometimes is inserted into the currently open page rather than the journal #11464 — logseq/logseq — GitHub"
