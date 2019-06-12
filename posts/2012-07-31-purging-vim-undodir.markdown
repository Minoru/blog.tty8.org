---
title: Purging Vim undodir
language: english
description: Leveraging cron to make sure your persistent undo files don’t get
    out hand.
tags: vim, howto
---

In two weeks, it would be the second anniversary of vim 7.3 release. Among other
things, that release brought a nice feature of persistent undo. Here's a short
excerpt from `:help persistent-undo` for those who didn't hear of it before:

> When unloading a buffer Vim normally destroys the tree of undos created for
> that buffer.  By setting the 'undofile' option, Vim will automatically save
> your undo history when you write a file and restore undo history when you edit
> the file again.
>
> Undo files are normally saved in the same directory as the file.  This can be
> changed with the 'undodir' option.
>
> NOTE: undo files are never deleted by Vim.  You need to delete them yourself.

The last note is the reason I wrote this post. Using vim as my `$EDITOR` of
choice, I tend to pollute undodir a lot: Git commits, experiments in `/tmp` and
`/var/tmp`, refactoring of code bases — all that generates a lot of undofiles
that I would never use again, simply because I deleted the original files.

I googled for existing solutions and it seems that people just run something
like `rm -rf ~/.vim/undofiles/` once in a while, but that's not the best
solution ever: losing history that I may need later makes me a little nervous.

Some other people use quite more wise yet bad solution: they run `find
~/.vim/undofiles -mtime +7 -delete`, deleting everything that is more than
seven days old. While effective against undofiles associated with temporary
files, it's still a risk: there's scripts that I rarely edit, still I want to
preserve all the history, just in case I need it.

So I present you yet another solution: shell script that removes only those
undofiles that are no longer needed, i.e. does not have associated files in the
filesystem anymore. It is intended to be run as a cron job. Without further
ado, here's the code:

```Shell
#!/bin/sh

# Purge vim's undodir (:help 'undodir', vim >= 7.3) from undofiles that does
# not have corresponding files in the filesystem anymore
#
# Intended to be called from crontab(1) job like that:
#
# # Purge undodir every week at 8:05AM
# 5 8 * * 1 /home/minoru/.bin/purge_vim_undodir /home/minoru/.vim/undofiles
# 
# Do not forget about the newline at the end of crontab file!

undodir="$1"

if [ -z "$undodir" ]
then
    echo "Path to undodir not specified." >&2
    exit 1
fi

if [ ! -d "$undodir" ]
then
    echo "Undodir ($undodir) does not exist (or isn't a directory)." >&2
    exit 2
fi

cd "$undodir"

for undofile in *
do
    filepath=`echo -n "$undofile" | sed 's#%#/#g'`
    if [ ! -e "$filepath" ]
    then 
        rm -f "$undofile"
    fi
done

cd - >/dev/null

```

Just copy the code to a file named `purge_vim_undodir`, put it somewhere and
set executable flag (using `chmod +x purge_vim_undodir`). Then edit your
crontab using `crontab -e`. You can come up with your own timing, or just use
the one I propose in comments to the script. That's it — from now on, your
undodir would be cleared automatically, only containing files that it is worth
storing.

Hope you'll find that useful.

**Update (27.08.2012)**: add a comment about the newline at the end of crontab
file.
