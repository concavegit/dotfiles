if [ ! -d ~/antigen/ ]; then
    git clone https://github.com/zsh-users/antigen.git ~/antigen
fi

source ~/antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundle autojump
antigen bundle git

antigen bundle olivierverdier/zsh-git-prompt
antigen bundle zsh-users/zsh-syntax-highlighting

antigen apply

setopt autocd extendedglob globdots histignorespace rm_star_silent
bindkey -e

ZSH_THEME_GIT_PROMPT_PREFIX=''
ZSH_THEME_GIT_PROMPT_SUFFIX=''
ZSH_THEME_GIT_PROMPT_SEPARATOR=''
ZSH_THEME_GIT_PROMPT_CHANGED="%{$fg[blue]%}%{+%G%}"
ret_status="%(?:%{$fg[blue]%}:%{$fg[magenta]%})"
PS1="${ret_status}%n@%m %{$fg[cyan]%}%c%{$reset_color%} "
RPS1='$(git_super_status)'

alias -g ...='../..'
alias -g ....='../../..'
alias -g G='| rg'
alias -g L='| less'
alias _='sudo'
alias l='ls -Ahlt'
alias md='mkdir -p'
alias pipupdate="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U --user"
alias totpacct=~/dotfiles/nostow/totpacct

. /usr/share/virtualenvwrapper/virtualenvwrapper_lazy.sh
