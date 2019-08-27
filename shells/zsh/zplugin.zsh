#! zsh
typeset -A ZPLGM
ZPLG_HOME=${CACHE_HOME:=~/.cache}/zsh/zplugin
ZPLGM[HOME_DIR]=$ZPLG_HOME
ZPLGM[ZCOMPDUMP_PATH]=$CACHE_HOME/zsh/zcompdump

if [[ ! -f $ZPLG_HOME/bin/zplugin.zsh ]]; then
	git clone https://github.com/psprint/zplugin $ZPLG_HOME/bin
	zcompile $ZPLG_HOME/bin/zplugin.zsh
fi
source $ZPLG_HOME/bin/zplugin.zsh
load=light

zstyle ':completion:*' insert-tab false
zstyle ':prezto:module:editor' key-bindings 'emacs'

zplugin ice wait'0' silent
zplugin $load mafredri/zsh-async

zplugin ice wait svn lucid wait'0'
zplugin snippet PZT::modules/directory

zplugin ice wait'0' lucid
zplugin snippet OMZ::lib/history.zsh

zplugin ice wait'0' lucid
zplugin snippet OMZ::lib/completion.zsh

# Handle keybindings
#zplugin ice svn silent wait'0'
#zplugin snippet PZT::modules/editor

zplugin ice wait'0' silent
zplugin $load davidparsson/zsh-pyenv-lazy

zplugin ice wait'0' silent
zplugin $load romkatv/gitstatus

zplugin ice wait'!0' atinit"config-powerline"
zplugin $load romkatv/powerlevel10k

zplugin ice wait'0' atclone"gdircolors -b LS_COLORS > c.zsh" atpull'%atclone' pick"c.zsh"
zplugin $load "trapd00r/LS_COLORS"

zplugin ice as"program" make'!' atclone'./direnv hook zsh > zhook.zsh' \
    atpull'%atclone' pick"direnv" src"zhook.zsh" wait"0"
zplugin $load direnv/direnv

zplugin ice wait'!1' atload:'_zsh_autosuggest_start'
zplugin $load zsh-users/zsh-autosuggestions

zplugin ice wait:'0' silent blockf;
zplugin $load zsh-users/zsh-completions

zplugin ice silent wait'0' atinit"zpcompinit; zpcdreplay"
zplugin $load zdharma/fast-syntax-highlighting
