# zshrc

# Enable persistent history
HISTFILE=~/config/zsh_history
HISTSIZE=100000
SAVEHIST=100000

# If a command can't be executed and it is a directory name, cd to it.
setopt auto_cd

# Treat #, ~ and ^ as part of patterns for # filename generation, etc.
setopt extended_glob

# Adding a command that duplicates an older one removes the previous
# entry from the history.
setopt hist_ignore_all_dups

# Like APPEND_HISTORY except that new history lines are added
# incrementally as soon as they are entered.
setopt inc_append_history

# Disable all beeps
setopt no_beep

# Parameter expansion, command substitution and arithmetic expansion
# are performed in prompts.
setopt prompt_subst

# Autoload functions supplied with zsh without alias expansion
autoload -U add-zsh-hook compinit vcs_info

# Initialize new style completion
compinit

# Add vcs_info to precmd hook to get updated info in the prompt
add-zsh-hook precmd vcs_info

# Setup a minimalist prompt
prompt='%n@%m:%F{cyan}%~
%F{magenta}[%l]%F{yellow}${vcs_info_msg_0_} %(?.%F{green}✔.%F{red}✘) %F{white}> '

# Menu completion will be started unconditionally
zstyle ':completion:*' menu select

# Use $LS_COLORS for completion as well
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# Automatically find new executables in $PATH
zstyle ':completion:*' rehash true

# Only try to detect git
zstyle ':vcs_info:*' enable git

# Show when the working directory has uncommitted changes
zstyle ':vcs_info:*' check-for-changes true

# Prompt before overwrite/removal
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

# Use colors when stdout is connected to a terminal
alias ls='ls --color=auto'

# List all entries except . and ..
alias la='ls -A'

# Use long listing format with human readable sizes
alias ll='ls -lh'

# Combine ll and la
alias lla='ll -A'

# See the ls alias
alias grep='grep --color=auto'

# Always use colors
alias tree='tree -C'

# Call command based on file type
alias x=xdg-open

# The default permissionis are 644 for files and 755 for directories
umask 022

# Setup sdkman, virtualenvwrapper and fzf
. ~/.sdkman/bin/sdkman-init.sh
. /usr/share/virtualenvwrapper/virtualenvwrapper.sh
. <(fzf --zsh)

# Local hook
. ~/.zshrc.local
