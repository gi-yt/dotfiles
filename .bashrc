### EXPORT
export TERM="xterm-256color"                      # getting proper colors
export HISTCONTROL=ignoredups:erasedups           # no duplicate entries
export EDITOR="emacsclient -t -a ''"              # $EDITOR use Emacs in terminal
export VISUAL="emacsclient -c -a emacs"           # $VISUAL use Emacs in GUI mode
### "bat" as manpager
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

export PATH="$HOME/.local/share/cargo/bin:$PATH"
### PATH
if [ -d "$HOME/.bin" ] ;
then PATH="$HOME/.bin:$PATH"
fi
if [ -d "$GOPATH/bin" ] ;
then PATH="$GOPATH/bin:$PATH"
fi
if [ -d "$HOME/.local/bin" ] ;
then PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.config/npm-packages/bin" ] ;
then PATH="$HOME/.config/npm-packages/bin:$PATH"
fi

[[ $- != *i* ]] && return

### SHOPT
shopt -s autocd # change to named directory
shopt -s cdspell # autocorrects cd misspellings
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s dotglob
shopt -s histappend # do not overwrite history
shopt -s expand_aliases # expand aliases
shopt -s checkwinsize # checks term size when bash regains control
shopt -s extglob
#ignore upper and lowercase when TAB completion
bind "set completion-ignore-case on"

x () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1   ;;
            *.tar.gz)    tar xzf $1   ;;
            *.bz2)       bunzip2 $1   ;;
            *.rar)       unrar x $1   ;;
            *.gz)        gunzip $1    ;;
            *.tar)       tar xf $1    ;;
            *.tbz2)      tar xjf $1   ;;
            *.tgz)       tar xzf $1   ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1;;
            *.7z)        7z x $1      ;;
            *.deb)       ar x $1      ;;
            *.tar.xz)    tar xf $1    ;;
            *.tar.zst)   unzstd $1    ;;
            *)           echo "'$1' cannot be extracted via ex()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

alias em="/usr/bin/emacs -nw"

alias ls='exa -hal --color=always --group-directories-first --icons' # my preferred listing
alias la='ls'
alias l.='ls | egrep "^\."'
alias lcd="ls && cd $1"

# Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB

alias merge='xrdb -merge ~/.config/X11/Xresources'
alias diff="diff -h"

# youtube-dl
alias yta-aac="youtube-dl --extract-audio --audio-format aac "
alias yta-best="youtube-dl --extract-audio --audio-format best "
alias yta-flac="youtube-dl --extract-audio --audio-format flac "
alias yta-m4a="youtube-dl --extract-audio --audio-format m4a "
alias yta-mp3="youtube-dl --extract-audio --audio-format mp3 "
alias yta-opus="youtube-dl --extract-audio --audio-format opus "
alias yta-vorbis="youtube-dl --extract-audio --audio-format vorbis "
alias yta-wav="youtube-dl --extract-audio --audio-format wav "
alias ytv-best="youtube-dl -f bestvideo+bestaudio "

# termbin
alias tb="nc termbin.com 9999"

# the terminal rickroll
alias rr='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'

colorscript random

# VTERM
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi
PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME}:${PWD}\007"'
vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'
vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}
find_file() {
    vterm_cmd find-file "$(realpath "${@:-.}")"
}

say() {
    vterm_cmd message "%s" "$*"
}
open_file_below() {
    vterm_cmd find-file-below "$(realpath "${@:-.}")"
}
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
       && [[ -n ${EMACS_VTERM_PATH} ]] \
       && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
    source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh

    alias vim="find_file"
fi
# starship
eval "$(starship init bash)"

source /usr/share/blesh/ble.sh

for command in mount umount systemctl pacman updatedb su ; do
    alias $command="sudo $command"
done; unset command

## a quick way to get out of current directory ##
alias ..='cd ..'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'
alias .4='cd ../../../../'
alias .5='cd ../../../../..'
alias mkdir='mkdir -pv'

alias pubip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="sudo ifconfig | grep -Eo 'inet (addr:)?([0-9]*\\.){3}[0-9]*' | grep -Eo '([0-9]*\\.){3}[0-9]*' | grep -v '127.0.0.1'"
alias pserver="python -m http.server --directory=$1"
alias mnt="mount | awk -F' ' '{ printf \"%s\t%s\n\",\$1,\$3; }' | column -t | egrep ^/dev/ | sort"
alias vim=nvim
alias htop=bpytop
alias gliwebcomp="ssh arya@gnulinuxindia.org sudo gnulinuxindia-website-compile.sh"
alias perwebcomp="ssh root@192.168.0.110 webcomp.sh"
alias schildichat="schildichat-desktop --use-gl=desktop"
fcp() { 
FILE=$1
wl-copy -t text/uri-list $FILE
}
random() { tr -dc A-Za-z0-9 </dev/urandom | head -c ${1:-10}; }
matrix() {
clear && while :;do echo $LINES $COLUMNS $(( $RANDOM % $COLUMNS)) $(printf "\U$(($RANDOM % 500))");sleep 0.05;done|gawk '{a[$3]=0;for (x in a){o=a[x];a[x]=a[x]+1;printf "\033[%s;%sH\033[2;32m%s",o,x,$4;printf "\033[%s;%sH\033[1;37m%s\033[0;0H",a[x],x,$4;if (a[x] >= $1){a[x]=0;} }}'
}
percentage() {
    read -p "Total Amount: " total
    read -p "x out of ${total}: " number
    new=$(echo "scale=2; $number/$total" | bc)
    ans=$(echo "scale=2; $new*100" | bc)
    echo $ans
}
export GPG_TTY=$(tty)
export QT_QPA_PLATFORM=xcb


# BEGIN_KITTY_SHELL_INTEGRATION
if test -n "$KITTY_INSTALLATION_DIR" -a -e "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; then source "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; fi
# END_KITTY_SHELL_INTEGRATION
