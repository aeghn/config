#### Pure Zsh Only Configuration

autoload -U add-zsh-hook

########################################
### Editor
########################################
# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    function zle-line-init () {
        echoti smkx
    }
    function zle-line-finish () {
        echoti rmkx
    }
    zle -N zle-line-init
    zle -N zle-line-finish
fi


bindkey -e # use Emacs keybindings

[[ -n "${terminfo[kend]}" ]] && bindkey -- "${terminfo[kend]}" end-of-line
[[ -n "${terminfo[khome]}" ]] && bindkey -- "${terminfo[khome]}" beginning-of-line

WORDCHARS='*?_-[]~&;!#$%^(){}<>|'

########################################
### Completion
########################################
autoload -Uz compinit bashcompinit

compinit
bashcompinit

export EDITOR=vim

## Autocompletion with an arrow-key driven interface
zstyle ':completion:*' menu select

## Case insensitive
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'

## For autocompletion of command line switches for aliases
setopt COMPLETE_ALIASES

## COMMAND HISTORY
HISTFILE="$CHIN_ZSH_CACHE_DIR/cmd-hist"
# Modify this to make startup faster
HISTSIZE=5000
SAVEHIST=500000
HISTDUP=erase               #Erase duplicates in the history file

setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_IGNORE_SPACE
setopt HIST_SAVE_NO_DUPS
setopt HIST_VERIFY
setopt APPENDHISTORY     #Append history to the history file (no overwriting)
setopt SHAREHISTORY      #Share history across terminals
setopt INCAPPENDHISTORY  #Immediately append to the history file, not just when a term is killed


autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "${terminfo[kcuu1]}" ]] && bindkey -- "${terminfo[kcuu1]}" up-line-or-beginning-search
[[ -n "${terminfo[kcud1]}" ]] && bindkey -- "${terminfo[kcud1]}" down-line-or-beginning-search


## DIRECTORY NAVI
export DIR_HISTORY_FILE="$CHIN_ZSH_CACHE_DIR/dir-hist"

function chin-chpwd-append-dir() {
    [ -n  "$HISTFILE" ] && echo "$PWD" >> "$DIR_HISTORY_FILE"
}
add-zsh-hook -Uz chpwd chin-chpwd-append-dir

function osc7 {
    local LC_ALL=C
    export LC_ALL

    setopt localoptions extendedglob
    input=( ${(s::)PWD} )
    uri=${(j::)input/(#b)([^A-Za-z0-9_.\!~*\'\(\)-\/])/%${(l:2::0:)$(([##16]#match))}}
    print -n "\e]7;file://${HOSTNAME}${uri}\e\\"
}
add-zsh-hook -Uz chpwd osc7

########################################
### Frame
########################################
fpath=("$ZSH_CONFIG_DIR/functions" "$fpath[@]")
autoload -Uz promptinit
promptinit

## To list all available prompt themes by: prompt -l
if [ -f "$ZSH_CONFIG_DIR/functions/prompt_chin_setup" ]; then
    prompt chin
else
    export PROMPT="%(?..%F{red}[%?]%f )%(1j.%F{cyan}(%j)%f .)%B%F{yellow}%n%f%b at %B%F{blue}%m%f%b in %B%F{green}%~%f%b${prompt_newline}> "
fi

# Clear Buffer
function chin-clear-scrollback-buffer {
    clear
    # clear buffer. The following sequence code is available for xterm.
    printf '\e[3J'
    # .reset-prompt: bypass the zsh-syntax-highlighting wrapper
    # https://github.com/sorin-ionescu/prezto/issues/1026
    # https://github.com/zsh-users/zsh-autosuggestions/issues/107#issuecomment-183824034
    # -R: redisplay the prompt to avoid old prompts being eaten up
    # https://github.com/Powerlevel9k/powerlevel9k/pull/1176#discussion_r299303453
    zle .reset-prompt && zle -R
}
zle -N chin-clear-scrollback-buffer
bindkey '^L' chin-clear-scrollback-buffer



########################################
### Title
########################################
function xterm_title_precmd () {
    print -Pn -- '\e]2;%n@%m %~\a'
    [[ "$TERM" == 'screen'* ]] && print -Pn -- '\e_\005{g}%n\005{-}@\005{m}%m\005{-} \005{B}%~\005{-}\e\\'
}

function xterm_title_preexec () {
    print -Pn -- '\e]2;%n@%m %~ %# ' && print -n -- "${(q)1}\a"
    [[ "$TERM" == 'screen'* ]] && { print -Pn -- '\e_\005{g}%n\005{-}@\005{m}%m\005{-} \005{B}%~\005{-} %# ' && print -n -- "${(q)1}\e\\"; }
}

if [[ "$TERM" == (foot*|alacritty|screen*|xterm*|tmux*|putty*|konsole*|gnome*) ]]; then
    add-zsh-hook -Uz precmd xterm_title_precmd
    add-zsh-hook -Uz preexec xterm_title_preexec
fi


########################################
### Title
########################################

# Read Private config
chin-prvt() {
    source "$CHIN_PRIVATE_ENV"
    echo "$1"
}
