# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Theme selection based on environment
if [ -n "$INSIDE_EMACS" ]; then
    export ZSH_THEME="rawsyntax"
else
    export ZSH_THEME="agnoster"
fi

# Platform detection
if [[ "$(uname)" != "Darwin" ]]; then
    PLATFORM="linux"
    plugins=(battery colorize extract git git-extras git-flow gitfast node npm urltools)
    alias pbcopy='xsel --clipboard --input'
    alias pbpaste='xsel --clipboard --output'
else
    PLATFORM="mac"
    plugins=(battery brew colorize extract git git-extras git-flow gitfast node npm urltools zsh-syntax-highlighting)
    export ANDROID_HOME=/Applications/Android\ Studio.app/sdk
    test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
fi

# Oh My Zsh settings
COMPLETION_WAITING_DOTS="true"
URLTOOLS_METHOD="xnode"

source $ZSH/oh-my-zsh.sh

# Function to detect package manager
detect_package_manager() {
    if [ "$PLATFORM" = "mac" ]; then
        echo "brew"
    elif [ "$PLATFORM" = "linux" ]; then
        if [ -f /etc/os-release ]; then
            . /etc/os-release
            case "$ID" in
                ubuntu|debian|pop|linuxmint) echo "apt" ;;
                arch|manjaro|endeavouros) echo "pacman" ;;
                fedora) echo "dnf" ;;
                opensuse*) echo "zypper" ;;
                *) echo "unknown" ;;
            esac
        else
            echo "unknown"
        fi
    fi
}

# Function to get install command for a package
get_install_command() {
    local pkg_name=$1
    local pkg_manager=$(detect_package_manager)

    case "$pkg_manager" in
        brew) echo "brew install $pkg_name" ;;
        apt) echo "sudo apt-get install -y $pkg_name" ;;
        pacman) echo "sudo pacman -S --noconfirm $pkg_name" ;;
        dnf) echo "sudo dnf install -y $pkg_name" ;;
        zypper) echo "sudo zypper install -y $pkg_name" ;;
        *) echo "" ;;
    esac
}

# Function to check and suggest installation
check_command() {
    local cmd=$1
    local pkg_name=${2:-$1}  # Use second arg as package name, or default to command name
    local alias_cmd=$3

    if hash $cmd 2>/dev/null; then
        [ -n "$alias_cmd" ] && alias $alias_cmd=$cmd
        return 0
    else
        local install_cmd=$(get_install_command $pkg_name)
        if [ -n "$install_cmd" ]; then
            echo "${GREEN}$cmd not found. Install with: ${BLUE}$install_cmd${RESET}"
        else
            echo "${GREEN}$cmd not found. Please install $pkg_name manually.${RESET}"
        fi
        return 1
    fi
}

# Colors
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
BLUE=$(tput setaf 4)
RESET=$(tput sgr0)

# Tool checks and aliases
check_command eza eza ls
check_command bat bat cat && alias ccat=/usr/bin/cat
check_command prettyping prettyping ping
check_command thefuck thefuck && eval $(thefuck --alias)
check_command ncpamixer ncpamixer mixer
check_command ncdu ncdu && alias du="ncdu --color dark -rr -x --exclude .git --exclude node_modules"

# Path configuration - build it cleanly
PATH_ADDITIONS=(
    "/usr/local/bin"
    "$HOME/.rvm/bin"
    "/usr/local/share/npm/bin"
    "/usr/local/sbin"
    "${GOPATH//://bin:}/bin"
    "$GOPATH/bin"
    "$HOME/bin"
    "$HOME/.yarn/bin"
    "$HOME/.config/yarn/global/node_modules/.bin"
    "$HOME/.local/bin"
    "$HOME/Library/Python/3.9/bin"
    "/usr/local/heroku/bin"
    "$HOME/.npm-global/bin"
    "$HOME/.linuxbrew/bin"
    "$HOME/.cargo/bin"
)

# Add yarn global bin if yarn exists
if hash yarn 2>/dev/null; then
    PATH_ADDITIONS+=("$(yarn global bin)")
fi

# Build PATH without duplicates
for p in "${PATH_ADDITIONS[@]}"; do
    case ":$PATH:" in
        *":$p:"*) ;;
        *) [ -d "$p" ] && PATH="$p:$PATH" ;;
    esac
done

export PATH

# Environment variables
export ANDROID_HOME=/Applications/Android\ Studio.app/sdk
export GOPATH=$HOME/Development/go
export GPG_TTY=$(tty)
export KUBECONFIG=$KUBECONFIG:~/.kube/config

# NVM configuration (only once)
export NVM_DIR="$HOME/.nvm"
if [ -s "/opt/homebrew/opt/nvm/nvm.sh" ]; then
    source "/opt/homebrew/opt/nvm/nvm.sh"
    [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && source "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"
elif [ -s "$NVM_DIR/nvm.sh" ]; then
    source "$NVM_DIR/nvm.sh"
    [ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"
elif [ -s "/usr/share/nvm/init-nvm.sh" ]; then
    source "/usr/share/nvm/init-nvm.sh"
fi

# HSTR configuration
if hash hstr 2>/dev/null; then
    alias hh=hstr
    setopt histignorespace
    export HSTR_CONFIG=hicolor
    bindkey -s "\C-r" "\C-a hstr -- \C-j"
fi

# Custom functions and aliases
# alias emacs='emacsclient -nc -a ""'
alias restart-audio="pulseaudio --kill && pulseaudio --start"
alias python="python3"
alias pip="pip3"
alias yw="yarn workspace"
alias yws="yarn workspaces"

function gi() { curl -L -s https://www.gitignore.io/api/$@ ;}

function killit() {
    if [ $# -eq 0 ]; then
        echo "Error: No argument supplied\n"
        echo "Usage: killit {process_name}"
    else
        ps -eaf | grep "$1" | grep -v grep | awk '{ print $2 }' | xargs kill -9
    fi
}

# Load additional configs if they exist
[ -f "$HOME/.secrets" ] && source "$HOME/.secrets"
[ -f "/Users/victor/.config/op/plugins.sh" ] && source "/Users/victor/.config/op/plugins.sh"

unset NPM_CONFIG_PREFIX
