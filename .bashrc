. ~/.environment

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Turn off posix mode if it somehow got turned on.
set +o posix

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Enable the recursive wildcard **
shopt -s globstar 2>/dev/null

# Append history rather than replacing it
shopt -s histappend

# Do not complete an empty command (listing every program in $PATH)
shopt -s no_empty_cmd_completion

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

HISTSIZE=100000
unset HISTFILESIZE

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto -v'
else
    alias ls='ls -v'
fi

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

# Only show username and hostname if remote or unusual
if [ -n "$SSH_CONNECTION" ] || [ "$(id -un)" != "josh" ] ; then
    prompt_remote=true
fi

PS1='$(e="$?";[ "$e" -ne 0 ] && echo -n "\[\e[01;31m\]($e) ")${debian_chroot:+\[\e[01;37m\]($debian_chroot) }${prompt_remote:+\[\e[01;32m\]\u@\h\[\e[00m\]:}\[\e[01;34m\]\w\[\e[00m\]\$ '

# Set title as appropriate for terminal
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${prompt_remote:+\u@\h: }\w\a\]$PS1"
    ;;
screen)
    PS1="\[\ek${prompt_remote:+\u@\h: }\w\e"'\\'"\]$PS1"
    ;;
esac

if [ -z "$BASH_COMPLETION" ] && [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
