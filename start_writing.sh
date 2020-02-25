#!/bin/sh

# Author: Alexander Batischev <eual.jp@gmail.com>
#
# This script fires up tmux with my writing setup

tmux has-session -t debiania || (
    # session doesn't exist, let's create one

    # first window is for editing

    vimoptions="set spell spelllang=ru_yo,en filetype=markdown nofoldenable tw=80"
    # Start vim:
    #   - go to the second line, i.e. the title
    #   - go to the Insert mode right away
    #   - set the options defined above
    #   - read initial file from the stdin
    vim="vim +2 -c 'startinsert!' -c \"$vimoptions\" -"
    # initial text. This is post metadata as used by Hakyll.
    text=`(
        echo    '---'
        echo    'title: '
        echo    'language: english russian'
        echo    'description: # No double quotes; end with a period'
        echo -n 'tags: ' && ./gather_tags
        echo    'enable-mathjax: true # delete if you do not need MathJax'
        echo    '---'
        echo    ''
        echo    '- What problem were I solving?'
        echo    '- How did I arrive at this particular solution?'
        echo    '(http://frantic.im/blogpost-contexts)'
        echo    ''
        echo    'Plain Language checklist:'
        echo    ''
        echo    '- written for the average reader'
        echo    '- has useful headings'
        echo    '- uses “you” and other pronouns to speak to the reader'
        echo    '- uses active voice'
        echo    '- uses short sections and short sentences'
        echo    '- uses the simplest tense possible—simple present is best'
        echo    '- omits excess words'
        echo    '- uses concrete, familiar words'
        echo    '- places words carefully:'
        echo    '   - avoids large gaps between the subject, the verb and the object'
        echo    '   - puts exceptions last'
        echo    '   - places modifiers correctly'
        echo    '- uses lists and tables to simplify complex material'
        echo    '- uses no more than two or three subordinate levels'
        echo    '(https://www.plainlanguage.gov/resources/checklists/checklist/)'
    )`
    tmux new-session -d -s debiania "echo \"$text\" | $vim"
    tmux rename-window -t debiania:0 vim

    # none of the windows need activity monitoring, so I disable it
    tmux set-option -t debiania:preview -g monitor-activity off

    # second window is for version control
    tmux new-window -d -n git -t debiania

    # third window runs "debiania preview"
    tmux new-window -d -n preview -t debiania 'cabal run preview'
)

tmux attach -t debiania
