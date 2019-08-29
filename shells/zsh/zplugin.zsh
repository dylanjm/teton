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

# Handle keybindings
#zplugin ice svn wait'1' lucid
#zplugin snippet PZT::modules/editor

zstyle ':completion:*' insert-tab false
zstyle ':prezto:module:editor' key-bindings 'emacs'

zplugin ice svn wait'1' lucid
zplugin snippet PZT::modules/directory

zplugin ice wait'1' lucid
zplugin snippet OMZ::lib/history.zsh

zplugin ice wait'1' lucid
zplugin snippet OMZ::lib/completion.zsh

zplugin ice wait'1' lucid
zplugin $load davidparsson/zsh-pyenv-lazy

zplugin ice wait'!0' atload"source ~/.p10k.zsh" lucid
zplugin $load romkatv/powerlevel10k

zplugin ice wait'1' atclone"gdircolors -b LS_COLORS > clrs.zsh" \
    atpull'%atclone' pick"clrs.zsh" nocompile'!' \
    atload'zstyle ":completion:*" list-colors “${(s.:.)LS_COLORS}”' lucid
zplugin $load trapd00r/LS_COLORS

zplugin ice as"program" make'!' atclone'./direnv hook zsh > zhook.zsh' \
    atpull'%atclone' pick"direnv" src"zhook.zsh" wait"1" lucid
zplugin $load direnv/direnv

#zplugin ice id-as"moose" pick"moose_profile.sh" load'[[ $PWD = */bison* ]]' lucid
#zplugin $load /usr/local/opt/moose

zplugin ice wait'1' atload:_zsh_autosuggest_start lucid
zplugin $load zsh-users/zsh-autosuggestions

zplugin ice wait'1' blockf lucid
zplugin $load zsh-users/zsh-completions

zplugin ice wait'!1' atinit"zpcompinit; zpcdreplay" lucid
zplugin $load zdharma/fast-syntax-highlighting
