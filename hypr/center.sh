#!/usr/bin/env bash

set -x

addr_w_h=($(hyprctl activewindow -j | jq '.address, .size[0], .size[1]' -r))

sw_sh=($(hyprctl monitors -j | jq '.[]| select( .focused == true ) |.x, .y, .width/.scale, .height/.scale'))


echo ${sw_sh[@]}

xs=$(( ${sw_sh[0]%.*} + ${sw_sh[2]%.*}/2 - ${addr_w_h[1]}/2 ))
ys=$(( ${sw_sh[1]%.*} + ${sw_sh[3]%.*}/2 - ${addr_w_h[2]}/2 ))


hyprctl dispatch movewindowpixel "exact $xs $ys,address:${addr_w_h[0]}"
