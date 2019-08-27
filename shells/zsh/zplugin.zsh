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

zplugin $load willghatch/zsh-saneopt

zplugin $load mafredri/zsh-async
zplugin $load rupa/z

zplugin ice wait atload:'source ~/.dotfiles/shells/zsh/p10k.zsh'
zplugin $load romkatv/powerlevel10k

zplugin ice silent wait:1 atload:_zsh_autosuggest_start
zplugin $load zsh-users/zsh-autosuggestions

zplugin ice blockf;
zplugin $load zsh-users/zsh-completions

zplugin ice silenitt wait!1
zplugin $load zdharma/fast-syntax-highlighting
