#!/usr/bin/env bash
# Script to launch predefined window layouts in Hyprland workspaces

set -e

WORKSPACE=$1
LAYOUT_NAME=$2

# Default to workspace 1 and "dev" layout if not specified
[[ -z "$WORKSPACE" ]] && WORKSPACE=1
[[ -z "$LAYOUT_NAME" ]] && LAYOUT_NAME="dev"

# Switch to the workspace first
hyprctl dispatch workspace $WORKSPACE

case "$LAYOUT_NAME" in
  "dev")
    # Development layout: terminal, editor, browser
    kitty --title "Terminal" &
    sleep 0.5
    ghostty --title "Editor" &
    sleep 0.5
    firefox --new-window "https://github.com" &
    ;;
    
  "chat")
    # Chat layout: Discord, Slack, etc
    firefox --new-window "https://discord.com/app" &
    sleep 0.5
    firefox --new-window "https://web.telegram.org" &
    ;;
    
  "media")
    # Media layout: browser, media player
    firefox --new-window "https://youtube.com" &
    sleep 0.5
    mpv &
    ;;
    
  "write")
    # Writing layout: editor in center, browser for research
    ghostty --title "Writing" &
    sleep 0.5
    firefox --new-window &
    ;;
    
  *)
    echo "Unknown layout: $LAYOUT_NAME"
    echo "Available layouts: dev, chat, media, write"
    exit 1
    ;;
esac

# Arrange windows according to Hyprland layout rules
sleep 2
hyprctl dispatch layoutmsg "orientationcycle"

echo "Launched $LAYOUT_NAME layout on workspace $WORKSPACE"