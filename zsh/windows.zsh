[ -d "/d/Tools/cmd" ] && export PATH=/d/Tools/cmd:$PATH

export CHIN_WORK_DAYS_DIR="/e/work"
export CHIN_FILES_DIR="/e/files"
export CHIN_PG_DIR="/e/playground-firm"

export PATH=$CHIN_FILES_DIR/config/scripts:$PATH
export PATH=/ucrt64/bin/:$PATH
export PATH=/D/Tools/cmd/poppler-24.02.0/Library/bin:$PATH

alias td='mkdir -p "$CHIN_WORK_DAYS_DIR/$(date +%y%m/%d)" && cd "$CHIN_WORK_DAYS_DIR/$(date +%y%m/%d)"'

cpci() {
    cp -r "$1" "$CHIN_WORK_DAYS_DIR/ci"
}

cptd() {
    mkdir -p "$CHIN_WORK_DAYS_DIR/$(date +%y%m/%d)"

    cp -r "$1" "$CHIN_WORK_DAYS_DIR/$(date +%y%m/%d)"
}

topg() {
    local fn="$(basename "$1")"

    cp -r "$1" "$CHIN_PG_DIR/$(date -u +%y%m-%d)-$1"
}

alias rg="rg --path-separator //"

alias jdk8="export JAVA_HOME=/C/Users/w/.jdks/corretto-1.8.0_362/; export PATH=/C/Users/w/.jdks/corretto-1.8.0_362/bin:$PATH"
