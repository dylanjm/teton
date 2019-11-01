#!/usr/bin/local/zsh
ITERM2_DIR=$CACHE/zsh/iterm2

if [[ ! -r $ITERM2_DIR/startup.zsh ]]; then
	mkdir -p $ITERM2_DIR/bin

	print Installing iTerm2 shell integration...
	curl -sfLo $ITERM2_DIR/startup.zsh https://iterm2.com/shell_integration/zsh

	print Installing iTerm2 ZSH utilities...
	for cmd in imgcat imgls it2attention it2check it2copy it2dl it2getvar it2setcolor it2setkeylabel it2ul it2universion; do
		curl -sfLo $ITERM2_DIR/bin/$cmd https://iterm2.com/utilities/$cmd &&
		chmod u+x $ITERM2_DIR/bin/$cmd &&
		print Installed iTerm2 utility: $cmd
	done
fi
path+=($ITERM2_DIR/bin)
