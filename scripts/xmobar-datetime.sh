#!/usr/bin/env bash
# Display local time and UTC time for xmobar
echo "$(date '+%b %d %Y - %H:%M') ($(date -u '+%H:%M') UTC)"
