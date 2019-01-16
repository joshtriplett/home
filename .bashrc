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

# Disable flow control, so Ctrl-s and Ctrl-q work
stty -ixon

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

HISTSIZE=-1
unset HISTFILESIZE

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto -v'
else
    alias ls='ls -v'
fi
alias tree='tree -v'

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'

# set variable identifying the chroot you work in (used in the prompt below)
if [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# Only show username and hostname if remote or unusual
if [ -n "$SSH_CONNECTION" ] || [ "$USER" != "josh" ] ; then
    prompt_remote=true
fi

PS1='$(e="$?";[ "$e" -ne 0 ] && echo -n "\[\e[01;31m\]($e) ")${debian_chroot:+\[\e[01;37m\]($debian_chroot) }${prompt_remote:+\[\e[01;32m\]\u@\h\[\e[00m\]:}\[\e[01;34m\]\w\[\e[00m\]\$ '

# Set title as appropriate for terminal
case "$TERM" in
screen*)
    PS1="\[\ek${prompt_remote:+\u@\h:}\w\e\\\\\]$PS1"
    ;&
xterm*|rxvt*)
    PS1="\[\e]0;${prompt_remote:+\u@\h: }\w\a\]$PS1"
    ;;
esac

unset PROMPT_COMMAND

if [ "${VTE_VERSION:-0}" -ge 3405 ]; then
    # Based on vte-2.91.sh from vte
    vte_urlencode() (
      # This is important to make sure string manipulation is handled
      # byte-by-byte.
      LC_ALL=C
      str="$1"
      while [ -n "$str" ]; do
        safe="${str%%[!a-zA-Z0-9/:_\.\-\!\'\(\)~]*}"
        printf "%s" "$safe"
        str="${str#"$safe"}"
        if [ -n "$str" ]; then
          printf "%%%02X" "'$str"
          str="${str#?}"
        fi
      done
    )

    PS1='\[\e]7;file://\h$(vte_urlencode "{PWD}")\e\\\]'"$PS1"
fi

if [ -z "$BASH_COMPLETION_COMPAT_DIR" ] && [ -z "$BASH_COMPLETION" ]; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi
