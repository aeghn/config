########################################
### Languages Settings
########################################
## Go Settings
export GOPROXY=https://proxy.golang.com.cn,direct


## Python Settings
export PIP_CACHE_DIR=$CHIN_CACHE_DIR/.pip-cache
alias load-conda='[ -f /opt/miniconda3/etc/profile.d/conda.sh ] && source /opt/miniconda3/etc/profile.d/conda.sh'


## Node Settings
export NODE_PATH=$CHIN_CACHE_DIR/node_modules
[ -d "$HOME/vendor/node_modules/bin/" ] && export PATH=$HOME/vendor/node_modules/bin:$PATH

# pnpm
export PNPM_HOME="$HOME/.local/share/pnpm"
case ":$PATH:" in
    *":$PNPM_HOME:"*) ;;
    *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end


## Rust Settings
[ -d ~/.cargo/bin ] && export PATH=$HOME/.cargo/bin:$PATH

########################################
### Tool Settings
########################################
## Fzf Utils
j() {
    local folder option
    option="$(tac "${DIR_HISTORY_FILE}" | awk 'arr[$0]++ == 0' | fzf -q "$1" --reverse --height 40% | sed -r 's/\r?\n?$//g')"

    if [[ -z "$option" ]]; then
        return 0
    fi

    if [ -d "${option}" ]; then
        folder="$option"
    elif [ -e "${option}" ]; then
        printf "Open its parent dir: \n\t%s\033[31m/%s\033[0m\n" "${option%/*}" "${option##*/}"
        folder="${option%/*}"
    else
        printf "$option is not existed.\n"
        return
    fi

    cd "$folder"
    unset folder option
}

[ -f /usr/share/fzf/key-bindings.zsh ] && . /usr/share/fzf/key-bindings.zsh


########################################
### Alias Settings
########################################
## Aliases
alias ls='ls --color=auto'
alias rga="rg --no-ignore-files --no-ignore --hidden -i"
alias fda='NO_COLOR=1 fd -I --hidden'
alias rmi='/usr/bin/rm -I -r'
alias rm="printf 'Avoid using rm, use rmi instead.\n'"
alias mpa='mpv --no-video'
alias aria2n='aria2c --no-conf=true -j4 -x4 -s4'
alias pc='proxychains -q'
alias hpx='export HTTP_PROXY=http://127.0.0.1:7890; export HTTPS_PROXY=http://127.0.0.1:7890'
alias fgt="unset HISTFILE"
alias td='cd "$CHIN_PG_DIR"'

cpci() {
    local fn="$(basename "$1")"
    cp -r "$1" "$CHIN_PG_DIR/0-$fn"
}

cptd() {
    local fn="$(basename "$1")"

    cp -r "$1" "$CHIN_PG_DIR/$(date -u +%y%m-%d)-${fn}"
}

if $_CHIN_IS_MSYS2; then
    alias rg="rg --path-separator //"
    alias jdk8="export JAVA_HOME=/D/vendor/jdks/corretto-1.8/; export PATH=/D/vendor/jdks/corretto-1.8/bin:$PATH"
fi
