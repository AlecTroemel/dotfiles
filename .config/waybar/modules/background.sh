#!/bin/bash

time=$(date +"%H")
if [[ "$time" -gt 16 ]]; then
    result=$(swaymsg 'output "*" background ~/Pictures/wallpapers/day/night.jpg stretch')
    text="night"
elif [[ "$time" -gt 14 ]]; then
    result=$(swaymsg 'output "*" background ~/Pictures/wallpapers/day/early_night.jpeg stretch')
    text="afternoon"
elif [[ "$time" -gt 12 ]]; then
    result=$(swaymsg 'output "*" background ~/Pictures/wallpapers/day/late_day.jpg stretch')
    text="late day"
elif [[ "$time" -gt 10 ]]; then
    result=$(swaymsg 'output "*" background ~/Pictures/wallpapers/day/day.jpg stretch')
    text="day"
elif [[ "$time" -gt 8 ]]; then
    result=$(swaymsg 'output "*" background ~/Pictures/wallpapers/day/morning.jpg stretch')
    text="morning"
else
    result=$(swaymsg 'output "*" background ~/Pictures/wallpapers/day/early_morning.jpeg stretch')
    text="early morning"
fi

echo "$text"
