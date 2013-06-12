# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

# Include local.zsh file for local configurations
[[ -r ~/.zsh/local.zsh ]] && . ~/.zsh/local.zsh

setopt appendhistory autocd

# Ensure /usr/local/bin is first so Homebrew works.
export PATH="/usr/local/bin:/usr/local/sbin:$HOME/sclang:$PATH"

# Emacs!
export EDITOR="emacs"


# PHP path for MAMP
export DRUSH_PHP="/Applications/MAMP/bin/php/php5.2.17/bin/php"

(( ${+PAGER} ))   || export PAGER="less"
bindkey -e

bindkey "\e[3~" delete-char        # Delete

# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/rand/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# default apps
  (( ${+BROWSER} )) || export BROWSER="w3m"
  (( ${+PAGER} ))   || export PAGER="less -R"

cl() {
    if [ -d "$1" ]; then
	cd "$1"
	ls
    fi
}

# zgitinit and prompt_wunjo_setup must be somewhere in your $fpath, see README for more.

cp_p()
{
   strace -q -ewrite cp -- "${1}" "${2}" 2>&1 \
      | awk '{
        count += $NF
            if (count % 10 == 0) {
               percent = count / total_size * 100
               printf "%3d%% [", percent
               for (i=0;i<=percent;i++)
                  printf "="
               printf ">"
               for (i=percent;i<100;i++)
                  printf " "
               printf "]\r"
            }
         }
         END { print "" }' total_size=$(stat -c '%s' "${1}") count=0
}

# prompt (if running screen, show window #)
if [ x$WINDOW != x ]; then
    export PS1="$WINDOW:%~%# "
else
   export PS1='%~ %# '
fi

setopt promptsubst

# Load the prompt theme system
autoload -U promptinit
promptinit

# Use the bart prompt theme
prompt bart

#alias cp='cp_p'

# format titles for screen and rxvt
function title() {
  # escape '%' chars in $1, make nonprintables visible
  a=${(V)1//\%/\%\%}

  # Truncate command, and join lines.
  a=$(print -Pn "%40>...>$a" | tr -d "\n")

  case $TERM in
  screen)
    print -Pn "\ek$a:$3\e\\"      # screen title (in ^A")
    ;;
  xterm*|rxvt)
    print -Pn "\e]2;$2 | $a:$3\a" # plain xterm title
    ;;
  esac
}

# precmd is called just before the prompt is printed
function precmd() {
  title "zsh" "$USER@%m" "%55<...<%~"
}

# preexec is called just before any command line is executed
function preexec() {
  title "$1" "$USER@%m" "%35<...<%~"
}

# colorful listings
zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

autoload -U compinit
compinit

# aliases
#alias ls='pwd; ls --color=auto'
alias ls='ls -FG'
#alias ack='ack-grep'
alias git='hub'
alias md='mkdir -p'
alias rd='rmdir'
alias cd..='cd ..'
alias ..='cd ..'
alias po='popd'
alias pu='pushd'
alias tsl="tail -f /var/log/syslog"
alias df="df -hT"
# alias drush='~/.drush/drush/drush'
alias drush='/usr/local/bin/drush'
export COLUMNS
alias du='du -h'
alias untar='tar xzfv'
alias diff='colordiff'
alias rake="noglob rake"
alias jenkins="nohup java -jar ~/jenkins.war --httpPort=8081 --ajp13Port=8010 > /tmp/jenkins.log 2>&1 &"
alias phpsh="/usr/local/share/python/phpsh"

# Necessary on Mac systems because otherwise opening emacs from the command line
# won't work.
# alias emacs='open -a /Applications/Emacs.app $1'
alias emacs='emacsclient $1'

# functions
mdc() { mkdir -p "$1" && cd "$1" }
setenv() { export $1=$2 }  # csh compatibility
sdate() { date +%Y.%m.%d }
pc() { awk "{print \$$1}" }
rot13 () { tr "[a-m][n-z][A-M][N-Z]" "[n-z][a-m][N-Z][A-M]" }
gitrmtree () { for i in `git status | grep deleted | awk '{print $3}'`; do git rm $i; done }

# shuffle input lines. Nice for mp3 playlists etc...
shuffle() {
  RANDOM=`date +%s`
  (
  while IFS= read -r i; do
    echo $RANDOM$RANDOM "$i";
  done
  ) | sort | sed 's/^[0-9]* //'
}

cdw () {
  cd $(dirname $(which $1))
}

#show git info in prompt
# autoload -Uz vcs_info
# zstyle ':vcs_info:*' stagedstr '%F{28}●'
# zstyle ':vcs_info:*' unstagedstr '%F{11}●'
# zstyle ':vcs_info:*' check-for-changes true
# zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
# zstyle ':vcs_info:*' enable git svn
# precmd() {
#   if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
#     zstyle ':vcs_info:*' formats ' [%F{green}%b%c%u%F{yellow}]'
#   } else {
#     zstyle ':vcs_info:*' formats ' [%F{green}%b%c%u%F{red}●%F{yellow}]'
#   }

#   vcs_info
# }
# setopt prompt_subst
PROMPT='%F{yellow}%n@%m %% %{$reset_color%}%F{white}'

# bashmarks
#source ~/.local/bin/bashmarks.sh

#Pygmentize
alias pcat=pygmentize

function pless() {
    pcat "$1" | less -R
}

#init rvm
[[ -s "/Users/victorquinn/.rvm/scripts/rvm" ]] && source "/Users/victorquinn/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# ZSH Syntax Highlighting
source ~/.zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Load the default version of PHP
export PHP_VERSIONS=/usr/local/Cellar/php53
[ -f $(brew --prefix php-version)/php-version.sh ] && source $(brew --prefix php-version)/php-version.sh && php-version 5.3.20 >/dev/null

#export PHP_VERSIONS=$(dirname $(brew --prefix php54))
#source $(brew --prefix php-version)/php-version.sh && php-version 5.x.x >/dev/null

