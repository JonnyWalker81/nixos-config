#!/usr/bin/env bash

# Get current layout
layout=$(hyprctl getoption general:layout | head -1 | cut -d' ' -f2)

if [ "$layout" = "master" ]; then
    # In master layout, swap with master window
    hyprctl dispatch layoutmsg swapwithmaster
else
    # In dwindle layout, move window to the leftmost position
    # This simulates making it the "master" window
    hyprctl dispatch movewindow l
    hyprctl dispatch movewindow l
    hyprctl dispatch movewindow l
fi