#!/usr/bin/env bash

## This script is a template script for creating textual file previews in Joshuto.
##
## Copy this script to your Joshuto configuration directory and refer to this
## script in `joshuto.toml` in the `[preview]` section like
## ```
## preview_script = "~/.config/joshuto/preview_file.sh"
## ```
## Make sure the file is marked as executable:
## ```sh
## chmod +x ~/.config/joshuto/preview_file.sh
## ```
## Joshuto will call this script for each file when first hovered by the cursor.
## If this script returns with an exit code 0, the stdout of this script will be
## the file's preview text in Joshuto's right panel.
## The preview text will be cached by Joshuto and only renewed on reload.
## ANSI color codes are supported if Joshuto is build with the `syntax_highlight`
## feature.
##
## This script is considered a configuration file and must be updated manually.
## It will be left untouched if you upgrade Joshuto.
##
## Meanings of exit codes:
##
## code | meaning    | action of ranger
## -----+------------+-------------------------------------------
## 0    | success    | Display stdout as preview
## 1    | no preview | Display no preview at all
##
## This script is used only as a provider for textual previews.
## Image previews are independent from this script.
##


# set -o noclobber -o noglob -o nounset -o pipefail
echo "$@"
exit 

FILE_PATH=""
export PREVIEW_COLUMNS=10
export PREVIEW_LINES=10


while [ "$#" -gt 0 ]; do
    case "$1" in
    "--path")
        shift
        FILE_PATH="$1"
        ;;
    "--preview-width")
        shift
        PREVIEW_COLUMNS="$1"
        ;;
    "--preview-height")
        shift
        PREVIEW_LINES="$1"
        ;;
    esac
    shift
done

echo "$PREVIEW_COLUMNS $PREVIEW_LINES"
tuipv "$FILE_PATH"
exit 0
