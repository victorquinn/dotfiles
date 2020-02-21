#!/bin/zsh
files=(emacs git tmux zsh)

# Bail if stow is not installed
command -v stow >/dev/null 2>&1 || echo "Dependency stow not installed. Install it first then run this again" || exit 1

if [ ! -d ~/.oh-my-zsh ]
then
    echo "Installing Oh My ZSH"
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
else
    echo "Skipping Oh My ZSH install since it's already installed"
fi

if [ ! -d ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting ]
then
    echo "Installing the zsh-syntax-highlighting plugin"
    pushd ~/.oh-my-zsh/custom/plugins
    git clone git://github.com/zsh-users/zsh-syntax-highlighting.git
    popd
    mkdir -p "$HOME/.zfunctions"
    cp "./zsh/pure.zsh" "$HOME/.zfunctions/prompt_pure_setup"
    cp "./zsh/async.zsh" "$HOME/.zfunctions/async"
    cp "./zsh/bullet-train.zsh-theme" "$HOME/.oh-my-zsh/themes"
else
    echo "Skipping zsh-syntax-highlighting plugin because it's already installed"
fi

if  [[ "$(uname)" != "Darwin" ]]
then
    # Linux
    echo "Installing on a Linux machine"
else
    # Mac
    echo "Installing on a Mac"
    command -V brew >/dev/null 2>&1 || echo "Dependency brew is not installed. Install it first, then run this again" || exit 1
fi

for file in "${files[@]}"
do
    echo "Symlinking $file"
    stow "$file"
done

echo "Sourcing .zshrc"
source ~/.zshrc
