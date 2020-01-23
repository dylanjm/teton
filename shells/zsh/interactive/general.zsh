#!/usr/bin/local/zsh

autoload -Uz allopt zed zmv zcalc
autoload -Uz colors && colors

autoload -Uz edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

# Treat these characters as part of a word.
WORDCHARS='*?_-.[]~&;!#$%^(){}<>'
autoload -Uz select-word-style
select-word-style shell

# Automatically escape URLs.
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# Make bracketed paste slightly smarter. This causes url-quote-magic
# below to work correctly.
autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic

# show nonzero exit codes
setopt print_exit_value

# allow comments with #
setopt interactive_comments

## General
setopt COMBINING_CHARS    # Combine zero-length punc chars (accents) with base char
setopt RC_QUOTES          # Allow 'Henry''s Garage' instead of 'Henry'\''s Garage'
setopt HASH_LIST_ALL      # Make sure the entire command path is hashed first before correcting.

unsetopt BRACE_CCL        # Allow brace character class list expansion.

## Jobs
setopt LONG_LIST_JOBS     # List jobs in the long format by default.
setopt AUTO_RESUME        # Attempt to resume existing job before creating a new process.
setopt NOTIFY             # Report status of background jobs immediately.

unsetopt BG_NICE          # Don't run all background jobs at a lower priority.
unsetopt HUP              # Don't kill jobs on shell exit.
unsetopt CHECK_JOBS       # Don't report on jobs when shell exit.

zstyle ":history-search-multi-word" page-size "15"
