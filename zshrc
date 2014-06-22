# zshrc

# Exports
export PATH="~/bin:$PATH:/sbin:/usr/sbin"
export EDITOR='vim'
export HISTSIZE='5000'
export HISTFILE="$HOME/.zsh_history"
export SAVEHIST="$HISTSIZE"
export GOPATH="$HOME/go"

# Options
setopt AUTO_CD
setopt EXTENDED_GLOB
setopt HIST_IGNORE_ALL_DUPS
setopt PROMPT_SUBST
setopt SHARE_HISTORY

# Modules
autoload -U colors compinit promptinit vcs_info
colors
compinit
promptinit

# Prompt
PROMPT="%{$fg_bold[magenta]%}%n@%m:%{$reset_color%}%{$fg_bold[blue]%}%~\
%{$reset_color%}%{$fg_bold[yellow]%}\${vcs_info_msg_0_}%{$reset_color%}\
%{$fg_bold[cyan]%}[%?]%{$reset_color%}%#> "

# Hook functions
precmd()
{
    vcs_info
    # No hs, tsl or fsl in the termcap entry
    printf "\e]0;${USER}@${HOST}:${PWD}\a"
}

# vcs_info
zstyle ':vcs_info:*' enable git hg
zstyle ':vcs_info:git*' formats "(%s/%b)"

# User specific aliases
alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -A'
alias grep='grep --color=auto'
alias rm='rm -i'
alias cp='cp -i'
alias g='xdg-open'
alias tree='tree -C'
alias dlnf='aptitude search ~i~snon-free'
alias dlnd='aptitude search ~i\!~ODebian'
alias mirror='rsync -aci --delete'

# zle
bindkey -e

# misc
umask 022
