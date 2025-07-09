#!/usr/bin/env bash

# Get current layout
current_layout=$(hyprctl getoption general:layout | head -1 | cut -d' ' -f2)

# Cycle between master and dwindle layouts
if [ "$current_layout" = "master" ]; then
    hyprctl keyword general:layout "dwindle"
else
    hyprctl keyword general:layout "master"
fi