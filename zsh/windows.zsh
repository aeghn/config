[ -d "/d/Tools/cmd" ] && export PATH=/d/Tools/cmd:$PATH

export CHIN_WORK_DAYS_DIR="/e/work"
export CHIN_FILES_DIR="/e/files"

export PATH=$CHIN_FILES_DIR/config/scripts:$PATH
export PATH=/ucrt64/bin/:$PATH

alias td='mkdir -p "$CHIN_WORK_DAYS_DIR/$(date +%y%m/%d)" && cd "$CHIN_WORK_DAYS_DIR/$(date +%y%m/%d)"'

cpci() {
    cp -r "$1" "$CHIN_WORK_DAYS_DIR/ci"
}

cptd() {
    mkdir -p "$CHIN_WORK_DAYS_DIR/$(date +%y%m/%d)"

    cp -r "$1" "$CHIN_WORK_DAYS_DIR/$(date +%y%m/%d)"
}
