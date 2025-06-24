########################################
### Languages Settings
########################################
## Go Settings
export GOPROXY=https://proxy.golang.com.cn,direct

## Python Settings
export PIP_CACHE_DIR=$CHIN_CACHE_DIR/.pip-cache
alias load-conda='[ -f /opt/miniconda3/etc/profile.d/conda.sh ] && source /opt/miniconda3/etc/profile.d/conda.sh'


## Node Settings
# pnpm
export PNPM_HOME="$HOME/.local/share/pnpm"
# pnpm end


## Rust Settings
export RUSTUP_DIST_SERVER="https://rsproxy.cn"
export RUSTUP_UPDATE_ROOT="https://rsproxy.cn/rustup"
[ -d ~/.cargo/bin ] && inspath "$HOME/.cargo/bin"

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
alias fgt="unset HISTFILE"
alias pd='cd "$CHIN_PG_DIR"'

hpx() {
    if [ -z "$HTTP_PROXY" ]; then
        export HTTP_PROXY="http://127.0.0.1:7890"
        echo "set proxy to $HTTP_PROXY"
    else
        export HTTP_PROXY=
        echo "unset proxy"
    fi
    export HTTPS_PROXY="$HTTP_PROXY"
}

cppd() {
    local fn="$(basename "$1")"

    cp -r "$1" "$CHIN_PG_DIR/$(date -u +%y%m-%d)-${fn}"
}

# Read Private config
chin-prvt() {
    source "$CHIN_PRIVATE_ENV"
    echo "$1"
}