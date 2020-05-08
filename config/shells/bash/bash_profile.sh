#!/usr/bin/env bash

export CACHE="$HOME/.cache"
export DOTFILES="$HOME/teton"
export PYENV_ROOT="$HOME/.pyenv"

export PATH="$PYENV_ROOT/bin:$PYENV_ROOT/shims:$DOTFILES/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/texlive/2019/bin/x86_64-darwin:/usr/local/opt/texinfo/bin:$PATH"

# shellcheck source=settings/lscl.bash
source "$DOTFILES/shells/bash/settings/lscl.bash"

# shellcheck source=settings/history.bash
source "$DOTFILES/shells/bash/settings/history.bash"

# shellcheck source=settings/defaults.bash
source "$DOTFILES/shells/bash/settings/defaults.bash"

# shellcheck source=settings/navigation.bash
source "$DOTFILES/shells/bash/settings/navigation.bash"

# shellcheck source=plugins/git_completion.bash
source "$DOTFILES/shells/bash/plugins/git_completion.bash"


export LESS="-R -F -i -J -M -R -W -x4 -X -z-4"
export LESSOPEN="|lessfilter %s"
export LESS_ADVANCED_PREPROCESSOR=1

export PATH="/Users/mcdodj/.config/cargo/bin:$PATH"

export PATH="$HOME/.cargo/bin:$PATH"
export PARALLEL_HOME="$HOME/.config/parallel"
