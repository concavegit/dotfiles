export FZF_BASE="/usr/local/opt/fzf"
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="robbyrussell"
plugins=(
    autojump
    aws
    colored-man-pages
    docker
    git
    pip
    python
    fzf
)

source $ZSH/oh-my-zsh.sh

setopt autocd extendedglob globdots histignorespace rm_star_silent