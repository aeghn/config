#!/usr/bin/env bash

set -e

MAP_SCRIPT="$(realpath "$0")"
CONFIG_DIR="${MAP_SCRIPT%/*}"

set -a
source "$CONFIG_DIR/_utils.sh"
set +a

find -name ".mapper" | while read line; do
    _info "Begin to map $line"
    cd "$(dirname "$line")"
    bash ".mapper"
    cd "$CONFIG_DIR"
done
