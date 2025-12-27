#!/usr/bin/env bash

source "$(realpath "$0" | sed 's|/[^/]*$||g')/init.sh"

git_bak "DAT_DIR" "$DAT_DIR" 

rm -rf "$DAT_DIR" 
mkdir -p "$DAT_DIR"
cd "$DAT_DIR"
sudo acpidump -b

git_bak "DAT_DIR" "$DAT_DIR" 
