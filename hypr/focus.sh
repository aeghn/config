#!/bin/bash

# https://github.com/hyprwm/Hyprland/issues/1087#issuecomment-1508497738

groupSize=$(hyprctl activewindow -j | jq -r '.grouped | length')

if [[ "$groupSize" > 0 ]]; then
    IN_GROUP=1
else
    IN_GROUP=0
fi

if [[ "$1" == "left" ]]; then
    if [[ "$IN_GROUP" -eq 1 ]]; then
        hyprctl dispatch changegroupactive b
    else
        hyprctl dispatch movefocus l
    fi
else
    if [[ "$IN_GROUP" -eq 1 ]]; then
        hyprctl dispatch changegroupactive f
    else
        hyprctl dispatch movefocus r
    fi
fi
