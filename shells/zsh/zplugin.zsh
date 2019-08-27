#! zsh
typeset -A ZPLGM
ZPLG_HOME=${XDG_CACHE_HOME:=~/.cache}/zsh/zplugin
ZPLGM[HOME_DIR]=$ZPLG_HOME
ZPLGM[ZCOMPDUMP_PATH]=$XDG_CACHE_HOME/zsh/zcompdump

if [[ ! -f $ZPLG_HOME/bin/zplugin.zsh ]]; then
	git clone https://github.com/psprint/zplugin $ZPLG_HOME/bin
	zcompile $ZPLG_HOME/bin/zplugin.zsh
fi
source $ZPLG_HOME/bin/zplugin.zsh
load=light

zplugin ice wait '0' silent
zplugin $load willghatch/zsh-saneopt

zstyle ':completion:*' insert-tab false

zplugin light mafredri/zsh-async

zplugin ice wait svn lucid
zplugin snippet PZT::modules/directory

zplugin ice wait lucid
zplugin snippet OMZ::lib/history.zsh

zplugin ice wait lucid
zplugin snippet OMZ::lib/completion.zsh

# Handle keybindings
zstyle ':prezto:module:editor' key-bindings 'emacs'
zplugin ice svn silent
zplugin snippet PZT::modules/editor

zplugin ice wait silent
zplugin $load davidparsson/zsh-pyenv-lazy

zplugin ice wait'1' silent
zplugin load romkatv/gitstatus

zplugin ice wait'!' lucid atload'source .p10k; _p9k_precmd' nocd
zplugin light romkatv/powerlevel10k

zplugin ice atclone"dircolors -b LS_COLORS > c.zsh" atpull'%atclone' pick"c.zsh"
zplugin load "trapd00r/LS_COLORS"

zplugin ice as"program" make'!' atclone'./direnv hook zsh > zhook.zsh' \
    atpull'%atclone' pick"direnv" src"zhook.zsh"
zplugin load direnv/direnv

zplugin ice wait:1 atload:_zsh_autosuggest_start silent
zplugin $load zsh-users/zsh-autosuggestions

zplugin ice silent blockf;
zplugin $load zsh-users/zsh-completions

zplugin ice silent wait #atinit"zpcompinit; zpcdreplay"
zplugin $load zdharma/fast-syntax-highlighting
