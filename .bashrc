# If not running interactively, don't do anything
[[ $- != *i* ]] && return

if [ -z "$BASH_EXECUTION_STRING" ]; then exec fish; fi

alias ls='ls --color=auto'
PS1="\h@\u \W [\$?]> \[$(tput sgr0)\]"
