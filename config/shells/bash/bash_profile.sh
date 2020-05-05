#!/usr/local/bin/bash
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

mesg n

echo
echo "Have a nice day!"
echo

export PATH="/Users/mcdodj/.config/cargo/bin:$PATH"

export PATH="$HOME/.cargo/bin:$PATH"
export PARALLEL_HOME="$HOME/.config/parallel"
