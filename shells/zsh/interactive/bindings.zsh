#! zsh
_zsh_highlight() {}
zle-line-init() {}
zle -N zle-line-init

zstyle ':prezto:module:editor' key-bindings 'emacs'

autoload -Uz select-word-style
select-word-style shell

# Allow S-Tab to backtrack through the completion menu.
bindkey $terminfo[kcbt] reverse-menu-complete

# Backward-kill long paths one directory at a time.
bindkey '^u' backward-kill-word
zstyle ':zle:backward-kill-word' word-style unspecified
zstyle ':zle:backward-kill-word' word-chars ' /'


# Allow for magic such as url-quote-magic to apply when your terminal supports
# bracketed paste.
autoload -Uz bracketed-paste-magic 
zle -N bracketed-paste bracketed-paste-magic

# url-quote-magic will automatically quote special chars in URLs pasted into
# the terminal.
autoload -Uz url-quote-magic
url-quote-magic-with-highlight() { url-quote-magic; _zsh_highlight }
zle -N self-insert url-quote-magic-with-highlight
