# zshrc

# Exports
export EDITOR='nano'
export HISTSIZE='5000'
export HISTFILE="$HOME/.zsh_history"
export SAVEHIST="$HISTSIZE"
export GOPATH="$HOME/go"
export PATH="~/bin:/usr/lib/ccache:$PATH:/sbin:/usr/sbin:/usr/local/go/bin:$GOPATH/bin"
export KEYTIMEOUT=1

# Options
setopt AUTO_CD
setopt EXTENDED_GLOB
setopt HIST_IGNORE_ALL_DUPS
setopt PROMPT_SUBST

# Modules
autoload -U colors compinit promptinit terminfo vcs_info
colors
compinit
promptinit

# zle
bindkey -e

bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}"  end-of-line
bindkey '^r'                 history-incremental-search-backward

# Custom widgets
zle-line-init zle-keymap-select()
{
	zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

# Prompt
PROMPT='%{$fg_bold[white]%}%n@%m:%{$reset_color%}%{$fg_bold[blue]%}%~\
%{$reset_color%}%{$fg_bold[yellow]%}${vcs_info_msg_0_}%{$reset_color%}\
%{$fg_bold[magenta]%}{$(basename "$VIRTUAL_ENV")}%{$fg_bold[cyan]%}[%?]\
%{$reset_color%}%{$reset_color%}> '

# Hook functions
precmd()
{
    vcs_info
    # No hs, tsl or fsl in the termcap entry
    printf "\e]0;${USER}@${HOST}:${PWD}\a"
}

# Completion configuration
zstyle ':completion:*' menu select # Keyboard navigable completion list
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
zstyle ':completion:*' rehash true

# vcs_info
zstyle ':vcs_info:*' enable git hg
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:git*' formats "(%s/%b/%c/%u/%m)"
zstyle ':vcs_info:git*+set-message:*' hooks git-aheadbehind


# User aliases
alias cp='cp -i'
alias debuild='debuild -us -uc'
alias disassemble='objdump -d'
alias g='xdg-open'
alias grep='grep --color=auto'
alias http-mirror='wget -mkEp'
alias http-server='python3 -mhttp.server 8080'
alias la='ls -A'
alias ll='ls -l'
alias ls='ls --color=auto'
alias mirror='rsync -aci --delete'
alias mkvirtualenv3='mkvirtualenv -p /usr/bin/python3'
alias mv='mv -i'
alias openssl-cert-info='openssl x509 -text -noout -in'
alias ppager='ps axuf | pager'
alias reset-terminal='dconf reset -f /org/gnome/terminal/legacy/profiles:/'
alias safe-vim='vim -i NONE'
alias rm='rm -i'
alias tree='tree -C'

# Misc
umask 022

# Functions

# git: Show +N/-N when your local branch is ahead-of or behind remote HEAD
# Make sure you have added misc to your 'formats': %m
function +vi-git-aheadbehind()
{
    local ahead behind
    local -a gitstatus

    ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null \
            | wc -l)
    (($ahead)) && gitstatus+=("%B%F{blue}+${ahead}%f%b")

    behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null \
             | wc -l)
    (($behind)) && gitstatus+=("%B%F{red}-${behind}%f%b")

    hook_com[misc]+=${(j::)gitstatus}
}

function ssh-tunnel()
{
    local mode="$1"
    local port="$2"
    local host="$3"
    local dest="$4"
    shift 4

    print ssh "$mode" "$port:$dest" "$host" "$@"
    ssh "$mode" "$port:$dest" "$host" "$@"
}

function ssh-remote-tunnel()
{
    ssh-tunnel -R "$@"
}

function ssh-local-tunnel()
{
    ssh-tunnel -L "$@"
}

function tcp-file()
{
    local file="$1"
    local port="$2"

    socat -u FILE:"$file" TCP-L:"$port",reuseaddr,fork
}

# Startup actions
[[ $TERM =~ '^screen' ]] || tmux attach

VENV_WRAPPER=/usr/share/virtualenvwrapper/virtualenvwrapper.sh
[[ -f $VENV_WRAPPER ]] && source $VENV_WRAPPER

[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh

# Local hook
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
