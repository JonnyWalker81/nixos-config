# Parallels VM Click-Through Fix for Hyprland

## The Problem
When you click on the Parallels VM window to focus it from macOS, that click is passed through to Hyprland, causing random clicks in the Linux guest.

## Root Cause
This is a known issue with Parallels Desktop's Wayland support. Parallels Tools doesn't properly handle Wayland's input protocol, causing the focus click to be passed to the guest OS.

## Solutions (in order of effectiveness)

### 1. **Change Parallels Mouse Settings (MOST EFFECTIVE)**
In macOS, configure Parallels Desktop:
- Open Parallels Desktop
- Go to your VM settings: **Actions → Configure → Hardware → Mouse & Keyboard**
- Change "Mouse & Keyboard" from "Auto-detect" to **"Optimize for games"**
- This will require using **Cmd+Ctrl** to capture/release the mouse
- This completely prevents click-through but requires manual mouse capture

### 2. **Use Keyboard to Focus (Recommended Workaround)**
Instead of clicking to focus the VM:
- Use **Cmd+Tab** to switch to Parallels Desktop
- The VM window will gain focus without a click
- Now you can click normally inside the VM

### 3. **Use Hyprland VM Mode**
I've configured a special mode in Hyprland:
- When switching to the VM, press **Alt+V** to enter "VM Focus Mode"
- This mode captures the first click without passing it through
- The mode automatically exits after the first click

### 4. **Use Focus Commands**
- **Alt+F**: Focus window under cursor (no click needed)
- **Alt+Ctrl+F**: Lock focus settings to prevent issues

### 5. **Switch to X11 (Last Resort)**
If nothing else works:
- Log out
- Select an X11 session instead of Wayland
- X11 doesn't have this issue with Parallels

## Why This Happens
- Parallels Desktop was designed primarily for X11
- Wayland handles input events differently
- Parallels Tools hasn't been updated for proper Wayland support
- The macOS host's click event is being translated incorrectly

## Permanent Fix
The only permanent fix is for Parallels to update their Linux Tools with proper Wayland protocol support. Until then, using "Optimize for games" mouse mode or keyboard focusing are the most reliable workarounds.