export ZSH="/home/concaveusr/.oh-my-zsh"
ZSH_THEME="sorin"
plugins=(
    adb
    autojump
    aws
    colored-man-pages
    django
    docker
    emacs
    git
    pip
    python
    ubuntu
    virtualenvwrapper
)
source $ZSH/oh-my-zsh.sh

setopt autocd extendedglob globdots histignorespace rm_star_silent
bindkey -e

alias -g ...='../..'
alias -g ....='../../..'
alias -g G='| rg'
alias -g L='| less'
alias _='sudo'
alias l='ls -Ahlt'
alias md='mkdir -p'
alias pipupdate="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U --user"
alias totpacct=~/dotfiles/nostow/totpacct
