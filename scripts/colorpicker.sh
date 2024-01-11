#!/usr/bin/env bash

F="$HOME/Pictures/grim/$(date +%y-%m-%d_%H.%M.%S).png"


get_color() {
    local c=$(grim -g "$(slurp -p)" -t ppm - | convert - -format '%[pixel:p{0,0}]' txt:- | tail -1 | awk '{print $3}')
    local f="/tmp/$(date +%y-%m-%d_%H.%M.%S).png"
    convert -size 100x100 xc:"$c" "$f"

    notify-send -i "$f" Color "$c"
}


get_color
