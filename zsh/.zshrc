export FZF_BASE="/usr/local/opt/fzf"
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="robbyrussell"
plugins=(
    adb
    autojump
    aws
    colored-man-pages
    docker
    git
    kubectl
    minikube
    pip
    python
    virtualenvwrapper
    rbenv
    nvm
    fzf
)

source $ZSH/oh-my-zsh.sh

setopt autocd extendedglob globdots histignorespace rm_star_silent

alias -g G='| rg'
alias -g L='| less'
alias _='sudo'
alias pipupdate="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U --user"
alias totpacct=~/dotfiles/nostow/totpacct
