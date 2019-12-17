#!/bin/bash

time=$(date +"%H")
if [[ "$time" -gt 16 ]]; then
    result=$(swaymsg 'output "*" background ~/Pictures/wallpapers/gradient_dark_red.jpg stretch')
    text="night"
elif [[ "$time" -gt 13 ]]; then
    result=$(swaymsg 'output "*" background ~/Pictures/wallpapers/gradient_space.jpg stretch')
    text="evening"
elif [[ "$time" -gt 11 ]]; then
    result=$(swaymsg 'output "*" background ~/Pictures/wallpapers/gradient_purple_blue.jpg stretch')
    text="afternoon"
else
    result=$(swaymsg 'output "*" background ~/Pictures/wallpapers/gradient_purple.jpg stretch')
    text="morning"
fi

echo "$text"
