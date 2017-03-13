# zshrc

# Exports
export EDITOR='vim'
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
bindkey -v

bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}"  end-of-line
bindkey '^r'                 history-incremental-search-backward

# Custom widgets
accept-search-vi-end-of-line()
{
	zle accept-search
	zle vi-end-of-line
}

zle-line-init zle-keymap-select()
{
	zle reset-prompt
}

zle -N accept-search-vi-end-of-line
zle -N zle-line-init
zle -N zle-keymap-select

bindkey '^g'   accept-search-vi-end-of-line
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

# Prompt
VI_MODE_CMD="%{$fg_bold[red]%}>%{$reset_color%}"
VI_MODE_INS="%{$fg_bold[green]%}>%{$reset_color%}"

vi_mode_prompt_info()
{
	[ "$KEYMAP" = 'vicmd' ] && echo $VI_MODE_CMD || \
	    echo $VI_MODE_INS
}

PROMPT='%{$fg_bold[white]%}%n@%m:%{$reset_color%}%{$fg_bold[blue]%}%~\
%{$reset_color%}%{$fg_bold[yellow]%}${vcs_info_msg_0_}%{$reset_color%}\
%{$fg_bold[magenta]%}{$(basename "$VIRTUAL_ENV")}%{$fg_bold[cyan]%}[%?]\
%{$reset_color%}%{$reset_color%}$(vi_mode_prompt_info) '

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

# User specific aliases
alias debuild='debuild -us -uc'
alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -A'
alias grep='grep --color=auto'
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias g='xdg-open'
alias tree='tree -C'
alias dlnf='aptitude search ~i~snon-free'
alias dlnd='aptitude search ~i\!~ODebian'
alias mirror='rsync -aci --delete'
alias safe-gvim='gvim -i NONE'
alias http-mirror='wget -mkEp'
alias http-server='python3 -mhttp.server 8080'
alias gwl='grep -c'
alias ppager='ps axuf | pager'
alias disassemble='objdump -d'
alias openssl-cert-info='openssl x509 -text -noout -in'
alias reset-terminal='dconf reset -f /org/gnome/terminal/legacy/profiles:/'
alias mkvirtualenv="mkvirtualenv --always-copy"
alias mkvirtualenv3="mkvirtualenv -p /usr/bin/python3 --always-copy"

# misc
umask 022

# git: Show +N/-N when your local branch is ahead-of or behind remote HEAD
# Make sure you have added misc to your 'formats': %m
function +vi-git-aheadbehind()
{
    local ahead behind
    local -a gitstatus

    # for git prior to 1.7
    # ahead=$(git rev-list origin/${hook_com[branch]}..HEAD | wc -l)
    ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)
    (( $ahead )) && gitstatus+=( "%B%F{blue}+${ahead}%f%b" )

    # for git prior to 1.7
    # behind=$(git rev-list HEAD..origin/${hook_com[branch]} | wc -l)
    behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)
    (( $behind )) && gitstatus+=( "%B%F{red}-${behind}%f%b" )

    hook_com[misc]+=${(j::)gitstatus}
}

function reverse-tunel()
{
    local port="$1"
    local server="$2"

    ssh -R $port:localhost:22 -N $server
}

function tcp_file()
{
    local file="$1"
    local port="$2"

    socat -u FILE:"$file"  TCP-L:"$port",reuseaddr,fork
}

# Startup actions
[[ -n $WINDOWID ]] && {
    wmctrl -i -r $WINDOWID -b add,maximized_vert,maximized_horz
}

[[ $TERM =~ '^screen' ]] || tmux attach

source /usr/share/virtualenvwrapper/virtualenvwrapper.sh

# Local hook
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
