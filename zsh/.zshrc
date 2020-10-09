# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.

# ZSH_THEME="pygmalion"
if [ -n "$INSIDE_EMACS" ]; then
    export ZSH_THEME="rawsyntax"
else
    export ZSH_THEME="bullet-train"
fi

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias emacs='emacsclient -nc -a ""'

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

URLTOOLS_METHOD="xnode"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
if  [[ "$(uname)" != "Darwin" ]]
then
    # Linux
    plugins=(battery colorize extract git git-extras git-flow gitfast node npm urltools)
    alias pbcopy='xsel --clipboard --input'
    alias pbpaste='xsel --clipboard --output'
else
    # Mac only
    plugins=(battery brew colorize extract git git-extras git-flow gitfast node npm urltools zsh-syntax-highlighting)
    export ANDROID_HOME=/Applications/Android\ Studio.app/sdk
    test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
fi

source $ZSH/oh-my-zsh.sh

export ANDROID_HOME=/Applications/Android\ Studio.app/sdk
export GOPATH=$HOME/Development/go

# NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Customize to your needs...
export PATH=/usr/local/bin:$PATH:$HOME/.rvm/bin:/usr/local/share/npm/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:${GOPATH//://bin:}/bin:$GOPATH/bin:$HOME/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/.local/bin:$PATH

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

### Add the yarn path
export PATH="$(yarn global bin | grep -o '/.*'):$PATH"

# eval $(thefuck --alias)

export GPG_TTY=$(tty)
function gi() { curl -L -s https://www.gitignore.io/api/$@ ;}

autoload -U +X bashcompinit && bashcompinit

export PATH="$PATH:`yarn global bin`"
function gi() { curl -L -s https://www.gitignore.io/api/$@ ;}


# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /home/victor/.nvm/versions/node/v10.15.2/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh ]] && . /home/victor/.nvm/versions/node/v10.15.2/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /home/victor/.nvm/versions/node/v10.15.2/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh ]] && . /home/victor/.nvm/versions/node/v10.15.2/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh
# tabtab source for slss package
# uninstall by removing these lines or running `tabtab uninstall slss`
[[ -f /home/victor/.nvm/versions/node/v10.15.2/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.zsh ]] && . /home/victor/.nvm/versions/node/v10.15.2/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.zsh

export NPM_CONFIG_PREFIX=~/.npm-global
export PATH="$PATH:$HOME/.npm-global/bin"
export PATH="$PATH:$HOME/.linuxbrew/bin"
export PATH="$PATH:$HOME/.cargo/bin"

# Colors
RED=`tput setaf 1`
GREEN=`tput setaf 2`
BLUE=`tput setaf 4`

# Better specific
if hash pyenv 2>/dev/null; then
    eval "$(pyenv init -)"
    source /Users/vquinn/.artifactoryrc
fi

if hash exa 2>/dev/null; then
    alias ls="exa"
else
    echo "${GREEN}exa not installed, recommend downloading ${BLUE}https://the.exa.website/${RESET}"
fi

if hash bat 2>/dev/null; then
    alias ccat=/usr/bin/cat
    alias cat=bat
else
    echo "${GREEN}bat not installed, recommend downloading ${BLUE}https://github.com/sharkdp/bat${RESET}"
fi

# These should be conditional and check that the alternatives exist before using them

# HSTR configuration - add this to ~/.zshrc
alias hh=hstr                    # hh to be alias for hstr
setopt histignorespace           # skip cmds w/ leading space from history
export HSTR_CONFIG=hicolor       # get more colors
bindkey -s "\C-r" "\C-a hstr -- \C-j"     # bind hstr to Ctrl-r (for Vi mode check doc)

alias ping=prettyping
alias du="ncdu --color dark -rr -x --exclude .git --exclude node_modules"

export KUBECONFIG=$KUBECONFIG:~/.kube/config

alias restart-audio="pulseaudio --kill && pulseaudio --start"

source /usr/share/nvm/init-nvm.sh
export PATH="/usr/local/opt/ruby@2.5/bin:$PATH"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/victor/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/victor/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/victor/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/victor/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# Start up zsh trap which will store all commands in a sqlite database
source ~/.zsh_trap.sh

# Kill processes matching the supplied text
function killit () {
    ps -eaf | grep $1 | grep -v grep | awk '{ print $2 }' | xargs kill -9
}
