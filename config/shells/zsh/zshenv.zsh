#!/usr/bin/env zsh
unsetopt GLOBAL_RCS  # Don’t source /etc/zshrc or other global zsh startup files.
typeset -gaU fpath module_path path
declare -gA ZINIT
typeset -g ZPLG_MOD_DEBUG=1

###
### XDG-Configuration
###
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_BIN_HOME="${HOME}/.local/bin"
export CACHE="${XDG_CACHE_HOME}"

###
### LS_COLORS
###
#export LS_COLORS="ln=0;38;2;177;98;134:or=1;38;2;204;36;29;48;2;29;32;33:mi=1;38;2;204;36;29;48;2;29;32;33:so=1;38;2;177;98;134;48;2;40;40;40:*~=0;38;2;102;92;84:fi=0;38;2;235;219;178:cd=1;38;2;215;153;33;48;2;40;40;40:di=0;38;2;69;133;136:pi=0;38;2;215;153;33;48;2;40;40;40:no=0;38;2;235;219;178:bd=1;38;2;215;153;33;48;2;40;40;40:ex=1;38;2;214;93;14:*.p=0;38;2;184;187;38:*.h=0;38;2;184;187;38:*.z=1;38;2;204;36;29:*.c=0;38;2;184;187;38:*.d=0;38;2;184;187;38:*.t=0;38;2;184;187;38:*.r=0;38;2;184;187;38:*.m=0;38;2;184;187;38:*.o=0;38;2;102;92;84:*.a=0;38;2;214;93;14:*.pl=0;38;2;184;187;38:*.gz=1;38;2;204;36;29:*.hs=0;38;2;184;187;38:*.mn=0;38;2;184;187;38:*.ko=0;38;2;214;93;14:*.ll=0;38;2;184;187;38:*.rs=0;38;2;184;187;38:*.td=0;38;2;184;187;38:*.la=0;38;2;102;92;84:*.xz=1;38;2;204;36;29:*.vb=0;38;2;184;187;38:*.rb=0;38;2;184;187;38:*.7z=1;38;2;204;36;29:*.kt=0;38;2;184;187;38:*.bc=0;38;2;102;92;84:*.el=0;38;2;184;187;38:*.cs=0;38;2;184;187;38:*.as=0;38;2;184;187;38:*.ps=0;38;2;254;128;25:*.hh=0;38;2;184;187;38:*.bz=1;38;2;204;36;29:*.ml=0;38;2;184;187;38:*.lo=0;38;2;102;92;84:*.go=0;38;2;184;187;38:*.fs=0;38;2;184;187;38:*.jl=0;38;2;184;187;38:*.py=0;38;2;184;187;38:*.gv=0;38;2;184;187;38:*.so=0;38;2;214;93;14:*.sh=0;38;2;184;187;38:*.md=0;38;2;250;189;47:*.ui=0;38;2;235;219;178:*css=0;38;2;184;187;38:*.rm=1;38;2;177;98;134:*.ex=0;38;2;184;187;38:*.js=0;38;2;184;187;38:*.ts=0;38;2;184;187;38:*.nb=0;38;2;184;187;38:*.pp=0;38;2;184;187;38:*.cr=0;38;2;184;187;38:*.di=0;38;2;184;187;38:*.hi=0;38;2;102;92;84:*.pm=0;38;2;184;187;38:*.cc=0;38;2;184;187;38:*.cp=0;38;2;184;187;38:*.ini=0;38;2;235;219;178:*.vim=0;38;2;184;187;38:*.mid=0;38;2;131;165;154:*.erl=0;38;2;184;187;38:*.zip=1;38;2;204;36;29:*.rar=1;38;2;204;36;29:*.sty=0;38;2;102;92;84:*.swp=0;38;2;102;92;84:*.otf=0;38;2;211;134;155:*.ilg=0;38;2;102;92;84:*.vob=1;38;2;177;98;134:*.arj=1;38;2;204;36;29:*.tex=0;38;2;184;187;38:*.bag=1;38;2;204;36;29:*.xmp=0;38;2;235;219;178:*.idx=0;38;2;102;92;84:*.swf=1;38;2;177;98;134:*.ods=0;38;2;254;128;25:*.elm=0;38;2;184;187;38:*.odt=0;38;2;254;128;25:*.exs=0;38;2;184;187;38:*.sxw=0;38;2;254;128;25:*.pod=0;38;2;184;187;38:*.txt=0;38;2;235;219;178:*.jpg=0;38;2;211;134;155:*.ics=0;38;2;254;128;25:*.m4v=1;38;2;177;98;134:*.xcf=0;38;2;211;134;155:*.ltx=0;38;2;184;187;38:*.zsh=0;38;2;184;187;38:*.xlr=0;38;2;254;128;25:*.aif=0;38;2;131;165;154:*.svg=0;38;2;211;134;155:*.tmp=0;38;2;102;92;84:*.ppt=0;38;2;254;128;25:*.bib=0;38;2;235;219;178:*.pps=0;38;2;254;128;25:*.bak=0;38;2;102;92;84:*.dot=0;38;2;184;187;38:*.git=0;38;2;102;92;84:*.bst=0;38;2;235;219;178:*.png=0;38;2;211;134;155:*.fnt=0;38;2;211;134;155:*.aux=0;38;2;102;92;84:*.nix=0;38;2;235;219;178:*.h++=0;38;2;184;187;38:*.mp4=1;38;2;177;98;134:*.gvy=0;38;2;184;187;38:*.img=1;38;2;204;36;29:*.mli=0;38;2;184;187;38:*.bcf=0;38;2;102;92;84:*.pyc=0;38;2;102;92;84:*.wmv=1;38;2;177;98;134:*.ico=0;38;2;211;134;155:*.wav=0;38;2;131;165;154:*.dpr=0;38;2;184;187;38:*.hxx=0;38;2;184;187;38:*.php=0;38;2;184;187;38:*.bz2=1;38;2;204;36;29:*.mov=1;38;2;177;98;134:*.doc=0;38;2;254;128;25:*.def=0;38;2;184;187;38:*.iso=1;38;2;204;36;29:*.tsx=0;38;2;184;187;38:*.asa=0;38;2;184;187;38:*.epp=0;38;2;184;187;38:*.inl=0;38;2;184;187;38:*.jar=1;38;2;204;36;29:*.htm=0;38;2;250;189;47:*.mkv=1;38;2;177;98;134:*.rpm=1;38;2;204;36;29:*.dll=0;38;2;214;93;14:*.toc=0;38;2;102;92;84:*.com=0;38;2;214;93;14:*.fsi=0;38;2;184;187;38:*.tml=0;38;2;235;219;178:*.htc=0;38;2;184;187;38:*.mp3=0;38;2;131;165;154:*.mpg=1;38;2;177;98;134:*.tgz=1;38;2;204;36;29:*.vcd=1;38;2;204;36;29:*.sbt=0;38;2;184;187;38:*.cgi=0;38;2;184;187;38:*.clj=0;38;2;184;187;38:*.rtf=0;38;2;254;128;25:*.kex=0;38;2;254;128;25:*.yml=0;38;2;235;219;178:*.csv=0;38;2;250;189;47:*.inc=0;38;2;184;187;38:*.pkg=1;38;2;204;36;29:*.odp=0;38;2;254;128;25:*.kts=0;38;2;184;187;38:*.xls=0;38;2;254;128;25:*.tcl=0;38;2;184;187;38:*.lua=0;38;2;184;187;38:*.wma=0;38;2;131;165;154:*.mir=0;38;2;184;187;38:*.bsh=0;38;2;184;187;38:*.fon=0;38;2;211;134;155:*.out=0;38;2;102;92;84:*.ttf=0;38;2;211;134;155:*.awk=0;38;2;184;187;38:*.sql=0;38;2;184;187;38:*.pdf=0;38;2;254;128;25:*.pro=0;38;2;184;187;38:*.rst=0;38;2;250;189;47:*.tbz=1;38;2;204;36;29:*.sxi=0;38;2;254;128;25:*.cfg=0;38;2;235;219;178:*.csx=0;38;2;184;187;38:*.xml=0;38;2;250;189;47:*.ps1=0;38;2;184;187;38:*.hpp=0;38;2;184;187;38:*TODO=1;38;2;104;157;106:*.tar=1;38;2;204;36;29:*.deb=1;38;2;204;36;29:*.ppm=0;38;2;211;134;155:*.c++=0;38;2;184;187;38:*.fls=0;38;2;102;92;84:*.pas=0;38;2;184;187;38:*.fsx=0;38;2;184;187;38:*.dox=0;38;2;184;187;38:*.pbm=0;38;2;211;134;155:*.gif=0;38;2;211;134;155:*.flv=1;38;2;177;98;134:*.bmp=0;38;2;211;134;155:*.tif=0;38;2;211;134;155:*.ogg=0;38;2;131;165;154:*.blg=0;38;2;102;92;84:*.cpp=0;38;2;184;187;38:*.ipp=0;38;2;184;187;38:*.ind=0;38;2;102;92;84:*.bat=0;38;2;214;93;14:*.bbl=0;38;2;102;92;84:*.pgm=0;38;2;211;134;155:*.cxx=0;38;2;184;187;38:*hgrc=0;38;2;184;187;38:*.log=0;38;2;102;92;84:*.apk=1;38;2;204;36;29:*.dmg=1;38;2;204;36;29:*.pid=0;38;2;102;92;84:*.bin=1;38;2;204;36;29:*.exe=0;38;2;214;93;14:*.avi=1;38;2;177;98;134:*.less=0;38;2;184;187;38:*.tbz2=1;38;2;204;36;29:*.bash=0;38;2;184;187;38:*.lisp=0;38;2;184;187;38:*.orig=0;38;2;102;92;84:*.pptx=0;38;2;254;128;25:*.html=0;38;2;250;189;47:*.yaml=0;38;2;235;219;178:*.purs=0;38;2;184;187;38:*.toml=0;38;2;235;219;178:*.epub=0;38;2;254;128;25:*.h264=1;38;2;177;98;134:*.dart=0;38;2;184;187;38:*.fish=0;38;2;184;187;38:*.make=0;38;2;184;187;38:*.rlib=0;38;2;102;92;84:*.mpeg=1;38;2;177;98;134:*.docx=0;38;2;254;128;25:*.jpeg=0;38;2;211;134;155:*.xlsx=0;38;2;254;128;25:*.java=0;38;2;184;187;38:*.diff=0;38;2;184;187;38:*.psd1=0;38;2;184;187;38:*.json=0;38;2;235;219;178:*.hgrc=0;38;2;184;187;38:*.flac=0;38;2;131;165;154:*.psm1=0;38;2;184;187;38:*.lock=0;38;2;102;92;84:*.conf=0;38;2;235;219;178:*.xhtml=0;38;2;250;189;47:*.swift=0;38;2;184;187;38:*.class=0;38;2;102;92;84:*.shtml=0;38;2;250;189;47:*passwd=0;38;2;235;219;178:*.cmake=0;38;2;184;187;38:*.patch=0;38;2;184;187;38:*.cache=0;38;2;102;92;84:*.ipynb=0;38;2;184;187;38:*.toast=1;38;2;204;36;29:*.mdown=0;38;2;250;189;47:*.dyn_o=0;38;2;102;92;84:*shadow=0;38;2;235;219;178:*.scala=0;38;2;184;187;38:*.cabal=0;38;2;184;187;38:*README=0;38;2;177;98;134:*LICENSE=0;38;2;131;165;154:*.gradle=0;38;2;184;187;38:*INSTALL=0;38;2;177;98;134:*COPYING=0;38;2;131;165;154:*.dyn_hi=0;38;2;102;92;84:*TODO.md=1;38;2;104;157;106:*.matlab=0;38;2;184;187;38:*.groovy=0;38;2;184;187;38:*.flake8=0;38;2;184;187;38:*.ignore=0;38;2;184;187;38:*.config=0;38;2;235;219;178:*Doxyfile=0;38;2;184;187;38:*setup.py=0;38;2;184;187;38:*Makefile=0;38;2;184;187;38:*TODO.txt=1;38;2;104;157;106:*.gemspec=0;38;2;184;187;38:*.desktop=0;38;2;235;219;178:*COPYRIGHT=0;38;2;131;165;154:*.fdignore=0;38;2;184;187;38:*README.md=0;38;2;177;98;134:*.DS_Store=0;38;2;102;92;84:*configure=0;38;2;184;187;38:*.markdown=0;38;2;250;189;47:*.rgignore=0;38;2;184;187;38:*.cmake.in=0;38;2;184;187;38:*.kdevelop=0;38;2;184;187;38:*CODEOWNERS=0;38;2;184;187;38:*.gitconfig=0;38;2;184;187;38:*.scons_opt=0;38;2;102;92;84:*INSTALL.md=0;38;2;177;98;134:*.gitignore=0;38;2;184;187;38:*Dockerfile=0;38;2;235;219;178:*SConstruct=0;38;2;184;187;38:*README.txt=0;38;2;177;98;134:*.localized=0;38;2;102;92;84:*SConscript=0;38;2;184;187;38:*.synctex.gz=0;38;2;102;92;84:*.travis.yml=0;38;2;184;187;38:*Makefile.in=0;38;2;102;92;84:*MANIFEST.in=0;38;2;184;187;38:*Makefile.am=0;38;2;184;187;38:*.gitmodules=0;38;2;184;187;38:*LICENSE-MIT=0;38;2;131;165;154:*appveyor.yml=0;38;2;184;187;38:*configure.ac=0;38;2;184;187;38:*CONTRIBUTORS=0;38;2;177;98;134:*.fdb_latexmk=0;38;2;102;92;84:*.applescript=0;38;2;184;187;38:*.clang-format=0;38;2;184;187;38:*.gitattributes=0;38;2;184;187;38:*CMakeCache.txt=0;38;2;102;92;84:*LICENSE-APACHE=0;38;2;131;165;154:*INSTALL.md.txt=0;38;2;177;98;134:*CMakeLists.txt=0;38;2;184;187;38:*CONTRIBUTORS.md=0;38;2;177;98;134:*requirements.txt=0;38;2;184;187;38:*CONTRIBUTORS.txt=0;38;2;177;98;134:*.sconsign.dblite=0;38;2;102;92;84:*package-lock.json=0;38;2;102;92;84:*.CFUserTextEncoding=0;38;2;102;92;84"

###
### Config
###
export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"
export DOTFILES="${HOME}/teton"
export ORG_FILES="${HOME}/Documents/org-files"
export EDITOR="/usr/local/bin/emacsclient -t -c"
export EMACS="/Appplications/Emacs.app"

###
### History
###
export HISTFILE="${XDG_DATA_HOME}/zsh_history"
export HIST_STAMPS="mm/dd/yyyy"
export HISTORY_IGNORE='(cd *|lx *|rm *)'
export HISTSIZE=500000
export SAVEHIST=500000

###
### Atom
###
export ATOM_HOME="${XDG_DATA_HOME}/atom"

###
### CCACHE
###
export CCACHE_DIR="${XDG_CACHE_HOME}/ccache"

###
### Conda
###
export CONDARC="${XDG_CONFIG_HOME}/condarc"
export CONDA_ENVS_PATH="/usr/local/Caskroom/miniconda/base/envs:${XDG_DATA_HOME}/conda/envs"

###
### Emacs-Anywhere
###
export EA_PATH="${XDG_DATA_HOME}/emacs_anywhere"

###
### Exa
###
#export EXA_COLORS="tr=38;5;3:tw=38;5;1:tx=38;5;2:su=38;5;5:sf=38;5;5:xa=38;5;15:sn=38;5;6:sb=38;5;14:df=38;5;6:ds=38;5;14:uu=38;5;10:un=38;5;8:gu=38;5;11:gn=38;5;8:lc=38;5;202:lm=38;5;211:ga=38;5;112:gm=38;5;11:gd=38;5;9:gv=38;5;91:gt=38;5;202:xx=38;5;102:da=38;5;12:in=38;5;5:bl=38;5;6:hd=38;5;250:lp=38;5;7:cc=38;5;208:b0=37;41;1:"

###
### FZF
###
export FZF_DEFAULT_COMMAND='fd -HI -L --exclude .git --color=always'
export FZF_DEFAULT_OPTS='
  --ansi
  --info inline
  --height 40%
  --reverse
  --border
  --multi
  --color fg:#ebdbb2,bg:#282828,hl:#fabd2f,fg+:#ebdbb2,bg+:#3c3836,hl+:#fabd2f
  --color info:#83a598,prompt:#bdae93,spinner:#fabd2f,pointer:#83a598,marker:#fe8019,header:#665c54
'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--preview '(cat {} 2> /dev/null || exa --tree --color=always {}) 2> /dev/null | head -200'"
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview'"
export FZF_ALT_C_OPTS="--preview 'exa --tree --color=always {} | head -200'"

###
### Gnupg
###
export GPG_TTY=$(tty)
export GNUPGHOME="${XDG_DATA_HOME}/gnupg"
export SSH_AUTH_SOCK=$(/usr/local/bin/gpgconf --list-dirs agent-ssh-socket)

###
### Goku
###
export GOKU_EDN_CONFIG_FILE="${XDG_CONFIG_HOME}/karabiner/karabiner.edn"

###
### Guile
###
export GUILE_AUTO_COMPILE=1
export GUILE_HISTORY="${XDG_DATA_HOME}/guile_history"

###
### Java
###
export JAVA_HOME="/Library/Java/JavaVirtualMachines/openjdk-13.0.2.jdk/Contents/Home"

###
### Julia
###
export JULIA_NUM_THREADS=16
export JULIA_DEPOT_PATH="${XDG_DATA_HOME}/julia"

###
### ZSH
###
export ZSH="${DOTFILES}/config/shells/zsh"

###
### Zplugin
###
export ZINIT[HOME_DIR]="${XDG_DATA_HOME}/zinit"
export ZINIT[BIN_DIR]="$ZINIT[HOME_DIR]/bin"
export ZINIT[PLUGINS_DIR]="$ZINIT[HOME_DIR]/plugins"
export ZINIT[ZCOMPDUMP_PATH]="${XDG_CACHE_HOME}/zcompdump"
export ZINIT[COMPINIT_OPTS]="-C"
export ZPFX="$ZINIT[HOME_DIR]/polaris"

###
### Less
###
export LESS="-g -i -M -R -S -w -z-4"
export LESSOPEN='| /usr/bin/env lessfilter %s 2>&-'
export LESSHISTFILE="${XDG_DATA_HOME}/lesshst"
export LESS_TERMCAP_mb=$'\E[01;31m'    # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m'    # begin bold
export LESS_TERMCAP_me=$'\E[0m'        # end mode
export LESS_TERMCAP_se=$'\E[0m'        # end standout-mode
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'        # end underline
export LESS_TERMCAP_us=$'\E[01;32m'    # begin underline
export PYGMENTIZE_STYLE='paraiso-dark'

###
### R
###
export R_USER="${XDG_CONFIG_HOME}/R"
export R_ENVIRON_USER="${XDG_CONFIG_HOME}/R/Renviron"
export R_PROFILE_USER="${XDG_CONFIG_HOME}/R/Rprofile"
export R_MAKEVARS_USER="${XDG_CONFIG_HOME}/R/Makevars"
export R_HISTFILE="${XDG_DATA_HOME}/Rhistory"
export R_LIBS_USER="${HOME}/Library/R/4.0/library"
export R_HISTSIZE=100000
export R_STARTUP_DEBUG=TRUE
export MKL_NUM_THREADS=16
export OMP_NUM_THREADS=16

###
### Parallel
###
export PARALLEL_HOME="${XDG_DATA_HOME}/parallel"

###
### Python
###
export PYTHONPATH="$HOME/Documents/projects/moose/python:$PYTHONPATH"
export PYENV_ROOT="${XDG_DATA_HOME}/pyenv"
export IPYTHONDIR="${XDG_CONFIG_HOME}/ipython"
export PYLINTHOME="${XDG_CACHE_HOME}/pylint.d"
export PYTHONSTARTUP="${XDG_CONFIG_HOME}/pythonrc"
export MPLCONFIGDIR="${XDG_DATA_HOME}/matplotlib"
export MYPY_CACHE_DIR="${XDG_CACHE_HOME}/mypy_cache"

###
### Rust/Cargo
###
export CARGO_HOME="${XDG_DATA_HOME}/cargo"
export RUSTUP_HOME="${XDG_DATA_HOME}/rustup"

###
### Subversion
###
export SUBVERSION_HOME="${XDG_CONFIG_HOME}/subversion"

###
### Terminfo
###
export TERMINFO="${XDG_DATA_HOME}/terminfo"

###
### Weechat
###
export WEECHAT_HOME="${XDG_CONFIG_HOME}/weechat"

###
### Z
###
export _Z_DATA="${XDG_DATA_HOME}/z"

###
### ZSH-Auto-Suggestions
###
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)

###
### MOOSE
###
export MOOSE_DIR="$HOME/Documents/projects/moose"

###
### Paths
###
fpath+=(/usr/local/Cellar/zsh/5.8/share/zsh/functions
        /usr/local/share/zsh/site-functions
        $ZINIT[HOME_DIR]/completions
        $DOTFILES/local/share/zsh_completions
        $ZSH/functions)

module_path+=($ZINIT[BIN_DIR]/zmodules/Src)

path=($CARGO_HOME/bin               # Rust CLI Utils
      $PYENV_ROOT/bin               # Pyenv CLI Utils
      $PYENV_ROOT/shims             # Python Libraries
      $ZPFX/bin                     # Zplugin Installed Programs
      $XDG_BIN_HOME                 # Personal CLI Tools
      /usr/local/bin                # Homebrew Installations
      /usr/bin
      /bin
      /usr/local/sbin
      /usr/sbin
      /sbin
      /usr/local/texlive/2019/bin/x86_64-darwin
      /usr/local/opt/texinfo/bin
      /opt/X11/bin)

export PATH
export FPATH

export MANPATH="/usr/local/share/man:${MANPATH}"
export INFOPATH="/usr/local/share/info:${INFOPATH}"
export HELPDIR="${MANPATH}"

alias -L run-help > /dev/null && unalias run-help
autoload -Uz  run-help    run-help-git  run-help-ip   run-help-openssl \
              run-help-p4 run-help-sudo run-help-svk  run-help-svn

###
### Personal Functions
###
autoload -Uz time-shell \
         zicompinit_fast \
         brew \
         nuke \
         pyenv \
         knitit \
         vterm_printf \
         vterm_cmd \
         vterm_prompt_end \
         colorlist
