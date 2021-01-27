---
title: Cirrus CI continues to amaze
language: english
description: # No double quotes; end with a period
tags: review
---

_This writeup is not sponsored, nor am I affiliated with Cirrus Labs. I'm just
a pleasantly delighted user [of 1.5 years][my-first-cirrus-commit]._

If you're a FLOSS maintainer hosting your projects on GitHub, Cirrus CI can
offer you three killer features.

# FreeBSD VMs

Don't snigger! Even if you don't care about FreeBSD as a target platform, you
can still take advantage of its affinity with macOS. Both operating systems are
descendants of BSD, and some build failures affect the whole family. If you
don't have a Mac lying around, [you can't legally run OS X in a virtual
machine][macos-vm], and so FreeBSD becomes a very compelling option. Not to
mention that its source is open, and it has up-to-date documentation.

# Massive concurrency

All your jobs [can simultaneously use up to][cirrus-limits] 16 CPUs for Linux,
12 for macOS, 8 for FreeBSD, and 8 for Windows. You are free to slice them
however you want ([except on macOS][macos-no-user-limits]). For example, you can
give a linter job a single core, while the compilation jobs get four each.
Furthermore, the limits are *per-user*, so you won't get starved of CI time no
matter how many pull requests you get!

The only meaningful competition here is GitHub Actions: [they provide an
unlimited number of dual-core VMs][github-actions-hw]. But if you have just
a handful of heavy-weight jobs, you're still better off with Cirrus.

You can probably get a competitive proposition from CircleCI and Travis, but
they bill by a minute, so the dynamics will change every time you change your
pipeline. Either way, you probably don't want to wake up to an email
notification saying that you've run out of your compute credits for the month.

**You pay for concurrency by accepting some flakiness.** Cirrus uses AWS Spot
instances to make such high limits viable, and Amazon can take those machines
back at any time. Cirrus papers over this by automatically restarting affected
jobs, but GitHub would still display them as "failed" until the restarted ones
succeed. From my experience, about once a week some job won't be restarted, or
won't start at all. 

# Dockerfile as an environment

You specify a path to a Dockerfile; Cirrus builds and caches the image, then
creates a container and runs all your tasks inside of it.

Contrast this with what other CI providers give you. At best, you are allowed to
specify a Docker image to use. If no image fits your needs perfectly, you'll
have to build, publish, and maintain your own. At worst, your CI provider has
a VM image with some popular stuff pre-installed, and you have to waste CI time
installing the rest—every time your job runs.

**This feature can break reproducibility.** You can't download a Docker image
from Cirrus cache; you have to build one locally. As a result, your container
might be slightly different from the one in CI, and you won't reproduce the
failure. This hasn't bitten me yet, but I think it's just a matter of time.

# Too good to be true?

With [Travis changing their billing just a few months
ago][travis-billing-change], it's an obvious question to ask: how long will the
Cirrus-FLOSS paradise last? [\@cirrus_labs@twitter.com][cirrus-twitter] says
they're an "NYC-based startup", which to me indicates that their future is less
clear than that of GitHub (owned by Microsoft) or Travis (owned by Idera). Maybe
this post will help Cirrus get a few more paying customers, and thus delay the
exit a bit :)

If/when the time comes, I don't think that migrating off Cirrus will be any more
complicated than migrating off any other CI provider. The dockerfile feature
means you'll have your build environments in Docker already, which is widely
supported (if not as conveniently as in Cirrus). The pipeline config you won't
keep anyway, as no two CI providers use the same format. If Cirrus limits the
resources, you could keep just the FreeBSD jobs on there, as other platforms are
well-represented with other providers.

Other than that, my biggest gripe with Cirrus CI is lack of support for any
forges other than GitHub. When I migrate to a different one, it'd be sad to
leave all this wealth of features behind. But until then, I think Cirrus is
going to serve well.

[my-first-cirrus-commit]: https://github.com/newsboat/newsboat/commit/f1e3d184401015846a572e775ae7dfca7c79e2d4#diff-62587956f943bb2503db7bc6dd27d0d888074a1c0ecaab3f570ad611aff0f7bb
    "Build Newsboat on FreeBSD with Cirrus CI — newsboat/newsboat@f1e3d18"

[macos-vm]: https://discussions.apple.com/thread/5785112
    "Is it legal to use Macintosh virtual machine hosted by Windows operating system? … — Apple Community"

[cirrus-limits]: https://cirrus-ci.org/faq/#are-there-any-limits
    "Are there any limits? — FAQ — Cirrus CI"

[macos-no-user-limits]: https://github.com/cirruslabs/cirrus-ci-docs/issues/770
    "Limit CPU and RAM for a macOS task — Issue #770 — cirruslabs/cirrus-ci-docs/issues — GitHub"

[github-actions-hw]: https://docs.github.com/en/actions/reference/specifications-for-github-hosted-runners#supported-runners-and-hardware-resources
    "Supported runners and hardware resources — GitHub Actions"

[travis-billing-change]: https://blog.travis-ci.com/oss-announcement
    "The Travis CI Blog: Open Source at Travis CI - An Update"

[cirrus-twitter]: https://twitter.com/cirrus_labs
    "Cirrus Labs (@cirrus_labs) / Twitter"
