#!/usr/bin/env bash

pkill swaybg
swaybg -m fill -i "$(find ~/files/media/pics/wallpapers -type f | shuf -n1)" &! 
