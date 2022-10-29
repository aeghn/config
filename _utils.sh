#!/usr/bin/env bash

# is GNU/Linux ?
is_gl() {
    [ $(uname) = "Linux" ]
}

# is Windows
is_nt() {
    test -f "C:/"
}

_info() {
    echo ". $@"
}

_warn() {
    echo "* $@"
}

_error() {
    echo -e "\033[31m! $@\033[0m"
}

_die() {
    _error "$@"
    exit 1
}



# make symbolic link for file {SOURCE} to {DESTINATION}.
# if any one of them or the {DESTINATION} is already linked
# to another file, die.
df_ln() {
    local src="$1"
    local des="$2"
    local rsrc="$(realpath "$src")"

    # Detect if source file is existed?
    if [ ! -e "$src" ]; then
        _die "src: $src doesn't exist."
    fi

    # Detect if destination file is existed?
    if [ -e "$des" ]; then
        if [ "x$rsrc" = "x$(realpath "$des")" ]; then
            return
        else
            _die "des: $des existed."
        fi
    fi

    local parent="${des%/*}"

    if [ ! -e "$parent" ]; then
        mkdir -p "$parent"
    fi

    if ! ln -s "$rsrc" "$des"; then
        _die "Unable to make symbolic links from \`$rsrc' to \`$des'"
    fi
}

# make symbolic links for all items in current directory to
# the {DESTINATIONa} dir.
df_ln_all() {
    local des_dir="$1"
    for i in *; do
        df_ln "$i" "$des_dir/$i"
    done
}
