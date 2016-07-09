----
title: What changed in Let's Encrypt Subscriber Agreement
language: english
description: Human-readable list of changes with quotations of the old and new versions of each point
tags: EULA
----

<i>**Disclaimer**: I'm not a lawyer, and this document does not constitute
legal advice. My paraphrases might be imprecise or entirely wrong.</i>

Let's Encrypt just released [a new version][new-sa] of their Subscriber
Agreement. It will take effect on August 1. Despite the fact that Let's Encrypt
is a young, progressive initiative, they're preparing the document in Microsoft
Word and publishing it as PDF. And that's it: no list of changes, nothing at
all. The community is [already grunting][crying] about that on the forums,
while more enthusiastic of us post [text versions][txt] and [git
diffs][git-diff] of the documents.

Unfortunately, the output of `git diff` (even with `--word-diff` option) is
hard to comprehend at times, so I decided to save you some effort and prepare
a more human-readable list of changes.

[new-sa]: https://letsencrypt.org/documents/LE-SA-v1.1.1-August-1-2016.pdf
    "Let's Encrypt Subscriber Agreement Version 1.1.1"

[crying]: https://community.letsencrypt.org/t/subscriber-agreement-1-0-1-1-1-1-change-please-publish-a-diff/17740
    "Subscriber agreement 1.0.1 -> 1.1.1 change - please publish a diff"

[txt]: https://www.osso.nl/blog/letsencrypt-license-update-show-differences/
    "letsencrypt / license update / show differences"

[git-diff]: https://gist.github.com/kennwhite/9541c8f37ec2994352c4554ca2afeece/revisions

So without further ado—what was changed?

* A new definition added:

    > “Key Compromise”— A Private Key is said to be compromised if its value has
    > been disclosed to an unauthorized person, an unauthorized person has had
    > access to it, or there exists a practical technique by which an
    > unauthorized person may discover its value. A Private Key is also
    > considered compromised if methods have been developed that can easily
    > calculate it based on the Public Key or if there is clear evidence that the
    > specific method used to generate the Private Key was flawed.

* in 3.1 ("Warranties"), list items starting with the third one now state that
  you give warranties "to ISRG and the public-at-large";

* in 3.1, a clarification was added to the fourth list item.

    Before:

    > You warrant that all information You have provided to ISRG is accurate,
    > current, complete, reliable, complete, and not misleading.

    After:

    > You warrant to ISRG and the public-at-large that all information You have
    > provided to ISRG is, and You agree that all information you will provide to
    > ISRG at any time will be, accurate, current, complete, reliable, and not
    > misleading.

* the last two list items in 3.1 were merged together, and the same
  clarification as with fourth item was added. You also give much broader
  warranties regarding the measures to secure the keys *and* the certificate.

    Before:

    > * You warrant that You have taken all appropriate, reasonable, and
    >   necessary steps to secure and keep your Private Key secret.
    >
    > * You warrant that You will not use Your Certificates to attack, defraud or
    >   intercept the traffic of others.

    After:

    > * You warrant to ISRG and the public-at-large that You have taken, and You
    >   agree that at all times You will take, all appropriate, reasonable, and
    >   necessary steps to maintain sole control of, secure, properly protect and
    >   keep secret and confidential the Private Key corresponding to the Public
    >   Key in Your Certificate (and any associated activation data or device,
    >   e.g. password or token).

* in 3.4 ("Key Pair Generation"), it was clarified that the generated public
  and private keys are your property;

* in 3.5 ("Inspection and Acceptance of Certificates"), you now not just
  *agree*, but also *warrant* ("to ISRG and the public-at-large", of course)
  that you'll immediately check the issued certificates and will revoke them if
  there are any errors;

* 3.6 was renamed from "Use of Your Certificate" to "Installation and Use
  of Your Certificate"; new paragraph was added:

    > You may reproduce and distribute Your Certificate on a nonexclusive and
    > royalty-free basis, provided that it is reproduced and distributed in full
    > and in compliance with this Agreement. You warrant to ISRG and the
    > public-at-large, and You agree, that You will install Your Certificate only
    > on servers that are accessible at the subjectAltName(s) listed in Your
    > Certificate, and that you will use Your Certificate solely in compliance
    > with all applicable laws and solely in accordance with this Agreement. Your
    > Certificate will remain the property of ISRG, subject to Your right to use
    > it as set forth in this Agreement.

    Preexisting paragraph was amended to say that certificates are used not
    just for encryption but also for authentication.

* in 3.7 ("When to Revoke Your Certificate"), list items were rephrased a bit.
  Compare them yourselves, that's quicker than reading any of my summaries.

    Before (formatting is mine):

    > You must immediately request that Your Certificate be revoked if:
    >
    > (i) You suspect or discover that Your Private Key has been, or is in danger
    > of being, lost, stolen, otherwise compromised, or subjected to unauthorized
    > use, or
    >
    > (ii) any information in Your Certificate is no longer accurate, current or
    > complete, or any such information becomes misleading.

    After (formatting is mine):

    > You warrant to ISRG and the public-at-large, and You agree, that You
    > will immediately request that Your Certificate be revoked if:
    >
    > (i) there is any actual or suspected misuse or Key Compromise of the
    > Private Key associated with the Public Key included in Your Certificate, or
    >
    > (ii) any information in Your Certificate is, or becomes, misleading,
    > incorrect or inaccurate.

    There are a few more sentences there, but they weren't changed;

* 3.8 ("When to Cease Using Your Certificate") got simplified: instead of
  repeating everything about key compromise and so on, it now simply says
  "don't use a certificate that's revoked":

    Before (formatting is mine):

    > You must immediately cease using Your Certificate if: 
    >
    > (i) You suspect or discover that the Private Key corresponding to Your
    > Certificate has been or may be stolen, lost, or otherwise compromised or
    > subjected to unauthorized use,
    >
    > (ii) any information in Your Certificate is no longer accurate, current or
    > complete, or any such information becomes misleading, or
    >
    > (iii) upon the revocation or expiration of Your Certificate.

    After (formatting is mine):

    > You warrant to ISRG and the public-at-large, and You agree, that You will
    > promptly cease using Your Certificate 
    >
    > (i) if any information in Your Certificate is, or becomes, misleading,
    > incorrect or inaccurate, or
    >
    > (ii) upon the revocation or expiration of Your Certificate.

* 3.9 ("Indemnification") was moved to 3.10;

* the new 3.9 is titled "When to Cease Using Your Private Key" and contains the
  following:

    > You warrant to ISRG and the public-at-large, and You agree, that You
    > will promptly cease all use of the Private Key corresponding to the
    > Public Key included in Your Certificate upon revocation of Your
    > Certificate for reasons of known or suspected Key Compromise.

* 4.1 ("Privacy") was shortened and is now essentially a reference to [Let’s
  Encrypt Privacy Policy][le-repository]:

    > Because others may rely on your use of Your Certificates to encrypt
    > Internet communications, much of the information You send to ISRG will
    > be published by ISRG and will become a matter of public record. ISRG’s
    > collection, storage, use and disclosure of such information are
    > governed by the Let’s Encrypt Privacy Policy at:
    > https://letsencrypt.org/privacy/.

* 4.3 ("Suspension and Revocation") now begins with "You acknowledge and accept
  that", stressing that this is a legal document, not just a collection of
  things-that-are-nice-to-know.

    The mention of certificate restoration was removed; from now on, even if
    ISRG revoked a certificate by error, it won't be returned to you.

    In the second paragraph, it's stressed that the certificate can be revoked
    *immediately*. Before:

    > ISRG may also, without advance notice, revoke Your Certificate if …

    After:

    > You also acknowledge and accept that ISRG may, without advance notice,
    > immediately revoke Your Certificate if …

    List item (v) now contains a note that the condition applies to the
    Agreement, too.

    Before (formatting is mine):

    > (v) You have violated any applicable law, agreement or other obligation

    After (formatting is mine):

    > (v) You have violated any applicable law, agreement (including this
    > Agreement), or other obligation

    Two new items were added in the middle of the list. They look like this
    (formatting is mine):

    > (vi) Your Certificate is being used, or has been used, to enable any
    > criminal activity (such as phishing attacks, fraud or the distribution of
    > malware);
    >
    > (vii) Your Certificate is being used, or has been used, to intercept the
    > traffic of others;

* in 4.4 ("IMPORTANT DISCLAIMER OF WARRANTIES AND LIMITATION OF LIABILITY"),
  the first two sentences were merged into one. The paragraph now begins with
  references to [ISRG's Certificate Policy][le-repository] and [ISRG's
  Certificate Practice Statement][le-repository].

    Before:

    > LET’S ENCRYPT CERTIFICATES AND SERVICES ARE PROVIDED “AS-IS.”
    > ISRG DISCLAIMS ANY AND ALL WARRANTIES …

    After:

    > EXCEPT AS EXPRESSLY SET FORTH IN ISRG’S CERTIFICATE POLICY AND
    > CERTIFICATE PRACTICE STATEMENT, LET’S ENCRYPT CERTIFICATES AND
    > SERVICES ARE PROVIDED “AS-IS” AND ISRG DISCLAIMS ANY AND ALL
    > WARRANTIES …

And that's it: all the differences between versions 1.0.1 and 1.1.1 of Let's
Encrypt Subscriber Agreement, listed nicely in a human-readable form. Hope this
helped!

[le-repository]: https://letsencrypt.org/repository/
    "Let's Encrypt: Policy and Legal Repository"
