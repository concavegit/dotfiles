export ZSH="/Users/concaveusr/.oh-my-zsh"
ZSH_THEME="robbyrussell"
plugins=(
    adb
    autojump
    aws
    colored-man-pages
    django
    docker
    git
    kubectl
    minikube
    pip
    python
    virtualenvwrapper
    rbenv
    nvm
)

source $ZSH/oh-my-zsh.sh

setopt autocd extendedglob globdots histignorespace rm_star_silent

export HOMEBREW_GITHUB_API_TOKEN=ab45ce649004b8f8640a6825a3d90cbf27727a35

alias -g G='| rg'
alias -g L='| less'
alias _='sudo'
alias pipupdate="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U --user"
alias totpacct=~/dotfiles/nostow/totpacct
