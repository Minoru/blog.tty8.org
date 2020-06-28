---
title: What does git checkout origin do?
language: english
description: It check out the commit at which the default branch points.
tags: git
---

Suppose you cloned a repository that has a branch named "origin", and you try to
check it out:

```
$ git checkout origin
Note: switching to 'origin'.
You are in 'detached HEAD' state.
```

I'm sorry, what? It should've created a local "origin" branch, but it detached
the `HEAD` instead. Believe me or not, this *is* the correct behaviour. To
explain it, though, we'll have to learn a bit about Git.

**N.B.** I'm assuming that you didn't change Git's default names for original
remote (`origin`) and default branch (`master`). If you did, everything I say
here remains valid, but you'll have to substitute your preferred names for the
default ones.

# HEAD, and what it means to detach it

`HEAD` is Git's notion of your current position in the repo's history. Most of
the time, it points at a branch, e.g. "master". When you create a new commit,
it's added to that branch, but `HEAD` itself stays still. When you check out
a new branch, `HEAD` is updated to match.

But you can also check out a tag, or even a random commit. In that case, `HEAD`
would point at a specific commit. That's what's called a "detached HEAD" state:
the `HEAD` is not attached to any branch. Committing something now would move
the `HEAD`, but won't affect any branches. That can be useful for one-off
experiments, but can also lead to data loss; please see [the "Detached HEAD"
section of
git-checkout(1)](https://git-scm.com/docs/git-checkout#_detached_head).

# Remote HEAD

When you're cloning a repo, the remote's `HEAD` takes up a new meaning: it's not
just the "current state" of that repo, it's the default branch. If you ever
changed the default on GitHub or GitLab, and then wondered how the clients know
about your decision—that's how.

[Documentation for `git remote
set-head`](https://git-scm.com/docs/git-remote#Documentation/git-remote.txt-emset-headem)
mentions the following:

> Having a default branch for a remote is not required, but allows the name of
> the remote to be specified in lieu of a specific branch. For example, if the
> default branch for `origin` is set to `master`, then `origin` may be specified
> wherever you would normally specify `origin/master`.

That's literally what happens in our case: `git checkout origin` acts like `git
checkout origin/master`. But `origin/master` is a branch, so why do we end up in
the "detached HEAD" state?

# Remote and local branches

Why does `git checkout origin/master` result in a detached `HEAD`? Because
`origin/master` is a *remote* branch. It's your local clone's idea of where
`master` points to in the remote repository.

A crucial difference between the `origin/master` and your local `master` is how
they are updated:

* local: `git commit`, `git merge`, `git rebase`, some `git checkout` options;
* remote: `git fetch`, `git pull`, `git push`, some `git remote` commands.

As a result, it doesn't make sense for Git to point `HEAD` at a remote branch:
your local commands like `git commit` won't be able to move the branch anyway.
That only leaves one option: detach the `HEAD` and point it at the *commit*
referenced by the remote branch.

"But wait", I hear you cry, "Why does `git checkout feature/123-improve-perf` in
a fresh clone doesn't fail, then?" That's a valid question. The key here is that
`feature/123-improve-perf` is neither a remote branch nor a local one. There is
only a remote branch named `origin/feature/123-improve-perf`—note the prefix.
Git recognizes this situation and creates a local branch
`feature/123-improve-perf` that tracks the remote one.

That's all we need to know right now. For more details, please refer to [the
relevant chapter](https://git-scm.com/book/en/v2/Git-Branching-Remote-Branches)
of *Pro Git*.

# The answer

Putting all of the above together, we finally understand what `git checkout
origin` does:

1. `origin` gets resolved into the default branch, i.e. `origin/master`;
2. `origin/master` is a remote branch, so checking it out results in a detached
   `HEAD` that points at the commit that's at the tip of `origin/master`.

To achieve what you *actually* wanted, tell Git to create a branch named
"origin" that points at the same commit as `origin/origin`:

```
$ git checkout -b origin origin/origin
```
