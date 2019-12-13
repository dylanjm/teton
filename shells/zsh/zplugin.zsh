#!/usr/local/bin/zsh
typeset -F4 SECONDS=0

###
### Check for zplugin install
###
[[ ! -f $ZPLGM[HOME_DIR]/bin/zplugin.zsh ]] && {
    command mkdir -p $ZPLGM[HOME_DIR]
    command git clone https://github.com/psprint/zplugin $ZPLGM[HOME_DIR]/bin
}

source $ZPLGM[HOME_DIR]/bin/zplugin.zsh
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin

### Check if zmodule is installed and built.
zmodload zdharma/zplugin 2>/dev/null || zpl module build

### Zplugin is hard-coded to create this directory.
### See: https://github.com/zdharma/zplugin/issues/197
test -d $CACHE/zplugin && { rm -rf $CACHE/zplugin }

###
### Load Zsh Plugins
###
# Set this variable to light or load for easy debugging.
# Change to load=load for debugging.
load=light

zplugin ice wait lucid
zplugin $load mafredri/zsh-async

zplugin ice wait'!' lucid nocd atinit"source $ZSH/themes/slimline.zsh"
zplugin $load mengelbrecht/slimline

zplugin ice wait atpull"zplugin creinstall -q ." blockf lucid
zplugin $load zsh-users/zsh-completions

zplugin ice wait'1' lucid
zplugin $load hlissner/zsh-autopair

zplugin ice wait'1' lucid
zplugin $load laggardkernel/zsh-thefuck
zstyle ":prezto:runcom" zpreztorc "$HOME/.zshrc"

zplugin ice wait'1' lucid
zplugin snippet https://raw.githubusercontent.com/junegunn/fzf/master/shell/completion.zsh

zplugin ice wait'1' lucid
zplugin snippet https://raw.githubusercontent.com/junegunn/fzf/master/shell/key-bindings.zsh

export LS_COLORS="$(vivid generate solarized-dark)"
export EXA_COLORS="da=38;5;4:uu=38;5;2:sn=38;5;124;lp=38;5;5;b0=37;41;1"

# zplugin ice wait'1' lucid
# zplugin $load davidparsson/zsh-pyenv-lazy
zplugin ice wait lucid blockf
zplugin $load $ZSH/interactive

zplugin ice wait'1' lucid blockf
zplugin $load unixorn/git-extra-commands

zplugin ice wait'1' lucid blockf
zplugin $load zdharma/zsh-lint

zplugin ice wait'1a' lucid blockf
zplugin $load rupa/z

zplugin ice wait'1b' lucid blockf
zplugin $load changyuheng/fz

zplugin ice wait'1d' lucid
zplugin $load wookayin/fzf-fasd

zplugin ice wait'2b' atload"ZPLGM[COMPINIT_OPTS]=-C; _zpcompinit_fast; zpcdreplay" lucid
zplugin $load zdharma/fast-syntax-highlighting

zplugin wait'2c' atload"zle -N zle-line-init autosuggest_start" lucid
zplugin $load zsh-users/zsh-autosuggestions
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_USE_ASYNC=true
ZSH_AUTOSUGGEST_MANUAL_REBIND=true

zplugin ice wait'3' lucid
zplugin $load zsh-users/zsh-history-substring-search

zflai-msg "[zshrc] Zplugin block took ${(M)$(( SECONDS * 1000 ))#*.?} ms"
