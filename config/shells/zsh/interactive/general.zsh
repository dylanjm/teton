#!/usr/bin/env zsh
autoload -Uz allopt zed zmv zcalc
autoload -Uz colors && colors

autoload -Uz edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

# Treat these characters as part of a word.
export WORDCHARS='*?_-.[]~&;!#$%^(){}<>'
autoload -Uz select-word-style
select-word-style shell

# Automatically escape URLs.
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# Make bracketed paste slightly smarter. This causes url-quote-magic
# below to work correctly.
autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic

# allow comments with #
setopt interactive_comments

## General
setopt   combining_chars    # combine zero-length punc chars (accents) with base char
setopt   rc_quotes          # allow 'henry''s garage' instead of 'henry'\''s garage'
setopt   hash_list_all      # make sure the entire command path is hashed first before correcting.
unsetopt brace_ccl        # Allow brace character class list expansion.

## Jobs
setopt long_list_jobs     # list jobs in the long format by default.
setopt auto_resume        # attempt to resume existing job before creating a new process.
setopt notify             # report status of background jobs immediately.
unsetopt bg_nice          # don't run all background jobs at a lower priority.
unsetopt hup              # don't kill jobs on shell exit.
unsetopt check_jobs       # Don't report on jobs when shell exit.

###
### Directory
###
setopt auto_cd              # auto changes to a directory without typing cd.
setopt auto_pushd           # push the old directory onto the stack on cd.
setopt pushd_ignore_dups    # do not store duplicates in the stack.
setopt pushd_silent         # do not print the directory stack after pushd or popd.
setopt pushd_tohome         # push to home directory when no argument is given.
setopt pushd_minus          # allows you to 'cd -n' to n previous directories
setopt cdable_vars          # change directory to a path stored in a variable.
setopt mult_ios             # write to multiple descriptors.
setopt extended_glob        # use extended globbing syntax.
setopt chase_links          # fully resolve file-path.
unsetopt clobber            # do not overwrite existing files with > and >>.
                            # use >! and >>! to bypass.

###
### Completion
###
zstyle ':completion::complete:*' cache-path "${XDG_CACHE_HOME}/zcompcache"
zstyle ':autocomplete:list-choices:*' min-input 3
zstyle ':autocomplete:list-choices:*' max-lines 40%
zstyle ':autocomplete:space:*' magic correct-word expand-history
zstyle ':autocomplete:tab:*' completion select
zstyle ':autocomplete:*:too-many-matches' message ''
zstyle ':autocomplete:*:no-matches-yet' message ''
zstyle ':autocomplete:*:no-matches-at-all' message ''

###
### History
###
setopt bang_hist                 # Treat the '!' character specially during expansion.
setopt extended_history          # Write the history file in the ':start:elapsed;command' format.
setopt share_history             # Share history between all sessions.
setopt hist_expire_dups_first    # Expire a duplicate event first when trimming history.
setopt hist_ignore_dups          # Do not record an event that was just recorded again.
setopt hist_ignore_all_dups      # Delete an old recorded event if a new event is a duplicate.
setopt hist_find_no_dups         # Do not display a previously found event.
setopt hist_ignore_space         # Do not record an event starting with a space.
setopt hist_save_no_dups         # Do not write a duplicate event to the history file.
setopt hist_verify               # Do not execute immediately upon history expansion.
setopt append_history            # Default
setopt inc_append_history        # this is default, but set for share_history
setopt hist_fcntl_lock

bindkey '^U' backward-kill-line
bindkey '^[_' redo

## History wrapper
## https://github.com/ohmyzsh/ohmyzsh/blob/master/lib/history.zsh
function omz_history {
  local clear list
  zparseopts -E c=clear l=list

  if [[ -n "$clear" ]]; then
    # if -c provided, clobber the history file
    echo -n >| "$HISTFILE"
    echo >&2 History file deleted. Reload the session to see its effects.
  elif [[ -n "$list" ]]; then
    # if -l provided, run as if calling `fc' directly
    builtin fc "$@"
  else
    # unless a number is provided, show all history events (starting from 1)
    [[ ${@[-1]-} = *[0-9]* ]] && builtin fc -l "$@" || builtin fc -l "$@" 1
  fi
}

# Timestamp format
case ${HIST_STAMPS-} in
  "mm/dd/yyyy") alias history='omz_history -f' ;;
  "dd.mm.yyyy") alias history='omz_history -E' ;;
  "yyyy-mm-dd") alias history='omz_history -i' ;;
  "") alias history='omz_history' ;;
  *) alias history="omz_history -t '$HIST_STAMPS'" ;;
esac
