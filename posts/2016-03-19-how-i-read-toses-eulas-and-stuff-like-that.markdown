----
title: How I read TOSes, EULAs and stuff like that
language: english
description: Three pieces of advice for people who find Terms of Service too
    burdensome a read to even try.
tags: tech
----

TL;DR:

* Legal documents are repetitive ⇒ try to memorize common things to speed up
  comprehension.
* Legal documents are explicit ⇒ read the examples, make sure you got the spirit
  of the clause right, then forget the examples.
* After reading through the document, recall all the clauses that you found
  unusual; decline if you don't like them; otherwise you can accept, memorizing
  **only** them, as everything else is part of your nature already.

 

*If you are the sort of person who might sue me for not writing a disclaimer,
all in caps and saying that I'm not a lawyer and this post shouldn't be taken as
a legal advice, please don't read any further. Thank you.*

A lot of people just ignore all those pesky Terms of Service, End User License
Agreements, software licenses etc. While some really believe those documents
don't concern them, others give more realistic subterfuges, one of which I'll
deal with today: those texts are mind-bogglingly detailed, and thus overly long
and boring.

Well, the first thing to realize about legal documents is that they're
repetitive as hell. In literature we have intertextuality, a song of allusions,
archetypes and quotations; in legal practice, we have outright copy-paste of
clauses. Still, that doesn't mean you can skip paragraphs that look like you've
seen them before. But it means you can *understand* the clause once, and then
just re-use that understanding forever.

The goal is to be able to read a paragraph of text and immediately realize that
it's, say, your run-off-the-mill disclaimer. You do that by remembering how
disclaimers usually look, and checking that the one in front of your eyes is
indeed the ordinary one. That you do by reading through and checking that
nothing fishy is going on. See, it's easier than you thought!

Another plight of legal documents, one that is highly related to repetitiveness,
is their explicitness—tendency to give you example after example after example
of things authors have in mind. Let's take a look at the following disclaimer,
taken from the [three-clause BSD license][3-clause-bsd]:

> **This software is provided by the copyright holders and contributors "as is"
> and any express or implied warranties**, including, but not limited to, the
> implied warranties of merchantability and fitness for a particular purpose
> **are disclaimed. In no event shall the copyright holder or contributors be
> liable for any** direct, indirect, incidental, special, exemplary, or
> consequential **damages** (including, but not limited to, procurement of
> substitute goods or services; loss of use, data, or profits; or business
> interruption) however caused and on any theory of liability, whether in
> contract, strict liability, or tort (including negligence or otherwise)
> arising in any way out of the use of this software, **even if advised of the
> possibility of such damage.**

It would probably take you some three to five passes through the raw,
capitalized version of this to realize that only the parts I've highlighted are
really necessary. You could even say that the second sentence is totally
superfluous, and you'll probably be right. The reason all these examples are
listed there is to make the clause "clear", to make—actually, to *force*—you to
understand the *spirit* of the clause, not just its letter. But once you've got
what the author is saying, you don't need those examples anymore.

Now combine this with my first piece of advice, and you'll realize that there
isn't really all that much to remember after all. And the check for fishiness
now boils down to first making sure that "skeleton sentences" of the clause are
okay, then ensuring that examples make sense in the context of the clause.

And that's it! Now go try to prove yourself against disclaimers in [popular open
source licenses][oss-licenses]. See how easy it is when you know how to read,
evaluate and remember them?

Now let me take a moment to point out how beneficial, in that particular regard,
the "well-known" licenses are. By "well-known" here I mean the ones that are
used by more than one project without any significant change to the text, e.g.
all sorts of GPL, Creative Commons and BSD licenses, the MIT license and so on.
If you encounter a piece of software (a manual, picture, whatever) licensed
under X, and you've already read and understood X, you are done. You can
immediately tell if license allows you to do what you wanted to do, and if so,
under what terms.

Debian kind of adds a meta-layer on top of this; I mean their [Free Software
Guidelines][dfsg], of course. If you've read DFSG and agreed with all its
points, you've effectively accepted all the licenses in Debian's `main` archive.
Read 418 words to gain access to 20,625 source packages[^packages-count]? I'd be
a fool to refuse such a deal.

There are Linux and BSD distributions that let users specify directly which
licenses they accept and reject. See [FreeBSD wiki page][licenses_accepted] for
details.

Even if you happen to forget if particular kind of use is allowed by particular
license or not, you at least don't need to study the whole text anew. It's more
like revisiting physics equations you didn't see ever since high school: you
might not remember the specifics, but you have a general understanding of how
things are pieced together, and thus can navigate the field rather quickly.

Okay, back to the topic. So you've read the document from beginning to end,
noticing a few unusual points here and there. Now what? Well, if the document is
unacceptable, you decline and forget about it entirely. But if you are ready to
agree to it, you now know which parts about this deal should be memorized: the
ones you found unusual!

It's only logical: all the other stuff is, well, usual, i.e. you've accepted
those particular clauses before, so you most probably already abide by them
without even thinking. So all that's left is to make the new, unusual bits part
of your behaviour. Easy, is it not?

And that's it: three techniques to make reading legal documents bearable, if not
outright easy. I'm now going to close with an idea that came to me [three years
ago][bnw-idea].

I don't think online services will ever be able to come up with a set of
"well-known" TOSes, but it should be possible to factor out some common bits.
Disclaimers, for example, are pretty universal.

So the idea is for services to list their terms as an array of clauses, and for
the user to store their own arrays of accepted and rejected clauses. When user
tries to sign up for a new service, the arrays are compared, presenting user
with questions regarding new clauses and then notifying him or her if the terms
are acceptable or not.

After couple (dozens) of accepted and rejected TOSes it should be possible to
tell outright if new terms are acceptable. In case of new clauses, it should
require just a few questions to figure things out. That will greatly simplify
accepting updates to TOSes, too.

I haven't found anyone interested enough to discuss and improve the concept, but
if you find the idea interesting, go for it.

And with that, I thank you for reading and encourage you to go make the world
a better place. See you around

[3-clause-bsd]:
    https://opensource.org/licenses/BSD-3-Clause
    "The BSD 3-Clause License | Open Source Initiative"

[oss-licenses]:
    https://opensource.org/licenses/
    "Licenses &amp; Standards | Open Source Initiative"

[dfsg]:
    https://www.debian.org/social_contract#guidelines
    "Debian Social Contract: The Debian Free Software Guidelines (DFSG)"

[^packages-count]:
    [metrics.debian.net](http://metrics.debian.net) is down, so I downloaded
    /debian/dists/stable/main/source/Sources.xz, grepped for "Package:" and
    counted the results.

[licenses_accepted]:
    https://wiki.freebsd.org/action/info/PortsLicenseInfrastructure
    "PortsLicenseInfrastructure - FreeBSD Wiki"

[bnw-idea]:
    https://bnw.im/p/GQPYVB
    "BNW"
