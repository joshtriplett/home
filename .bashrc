export PATH="$HOME/local/bin:/usr/lib/ccache:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games"
if [ -d $HOME/local/lib ] ; then
    export LD_LIBRARY_PATH="$HOME/local/lib${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH"
fi
export LC_COLLATE=C

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Append history rather than replacing it
shopt -s histappend

# Do not complete an empty command (listing every program in $PATH)
shopt -s no_empty_cmd_completion

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

export HISTSIZE=100000
export HISTFILESIZE=100000

export EMAIL='josh@freedesktop.org'
export DEBEMAIL=$EMAIL
export GIT_AUTHOR_NAME='Josh Triplett'
export GIT_AUTHOR_EMAIL=$EMAIL
export GIT_COMMITTER_NAME='Josh Triplett'
export GIT_COMMITTER_EMAIL=$EMAIL
export EDITOR=emacs
export ENSCRIPT='--header=|$n --highlight --media=letter --color'
export LESS='-R'
export LESSHISTFILE=-
export GZIP='-9'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto -v'
else
    alias ls='ls -v'
fi

# enable color in grep
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;32'
# For use when piping into something like less that can handle color
alias cgrep='grep --color=always'

# Make Control-v paste, if in X and if xsel available
if [ -n "$DISPLAY" ] && [ -x /usr/bin/xsel ] ; then
    # Work around a bash bug: \C-@ does not work in a key binding
    bind '"\C-x\C-m": set-mark'
    # The '#' characters ensure that kill commands have text to work on; if
    # not, this binding would malfunction at the start or end of a line.
    bind 'Control-v: "#\C-b\C-k#\C-x\C-?\"$(xsel -b -o)\"\e\C-e\C-x\C-m\C-a\C-y\C-?\C-e\C-y\ey\C-x\C-x\C-d"'
fi

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

PS1='${debian_chroot:+($debian_chroot)}\[\e[01;32m\]\u@\h\[\e[00m\]:\[\e[01;34m\]\w\[\e[00m\]\$ '

# If this is an xterm or rxvt set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    # Using PS1 seems to cause the cursor to flash to the beginning of the line.
    # PS1="\[\e]0;\u@\h: \w\a\]$PS1"
    export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/#$HOME/~}\007"'
    ;;
esac

if [ -z "$BASH_COMPLETION" ] && [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
