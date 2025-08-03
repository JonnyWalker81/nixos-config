{
  pkgs,
  config,
  lib,
  inputs,
  ...
}:

with lib;
{
  # Enable waybar with systemd service
  programs.waybar = {
    enable = true;
    systemd.enable = true;
  };

  # Ensure waybar starts after Hyprland is fully initialized
  systemd.user.services.waybar = {
    Unit = {
      After = [ "hyprland-session.target" ];
      PartOf = [ "hyprland-session.target" ];
    };
  };

  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    systemd.enable = true;
    plugins = [ ];
    extraConfig =
      let
        modifier = "ALT";
      in
      concatStrings [
        ''
          # Monitor configuration - using 1.0 scale for normal sizing
          monitor=,3840x2025@60,0x0,1.0

          # Window rules - restored to original values
          windowrulev2 = opacity 0.85, class:.*
          windowrulev2 = opacity 1.0, class:firefox
          windowrulev2 = opacity 1.0, class:Firefox
          
          # Parallels VM specific rules
          windowrulev2 = float, title:Parallels Shared Clipboard
          windowrulev2 = size 1 1, title:Parallels Shared Clipboard
          windowrulev2 = move -1 -1, title:Parallels Shared Clipboard
          
          # Simple window rules
          # windowrule = float, ^(steam)$
          # windowrule = size 1080 900, ^(steam)$
          # windowrule = center, ^(steam)$
          # windowrule = float, ^(MPlayer)$
          # windowrule = float, ^(Gimp)$

          # General settings - Tokyo Night Storm theme with bright active window borders
          general {
            gaps_in = 5
            gaps_out = 5
            border_size = 4
            # Bright blue/purple gradient for active windows (Tokyo Night accent colors)
            col.active_border = rgba(7aa2f7ff) rgba(bb9af7ff) rgba(7dcfffff) 45deg
            # Muted dark border for inactive windows
            col.inactive_border = rgba(414868ff)
            layout = master
            resize_on_border = true
          }

          # Input settings - optimized for VM usage
          input {
            kb_layout = us
            kb_options = caps:super
            follow_mouse = 1  # Enable focus follows mouse (1 = focus on hover)
            mouse_refocus = true  # Refocus window when mouse moves between windows
            
            touchpad {
              natural_scroll = false
            }
            
            sensitivity = 0
            accel_profile = flat
            float_switch_override_focus = 2  # Prevent focus switching between floating windows
          }
          
          
          # Binds configuration
          binds {
            scroll_event_delay = 0
            allow_workspace_cycles = true
          }

          # Wayland environment variables
          env = NIXOS_OZONE_WL, 1
          env = NIXPKGS_ALLOW_UNFREE, 1
          env = XDG_CURRENT_DESKTOP, Hyprland
          env = XDG_SESSION_TYPE, wayland
          env = XDG_SESSION_DESKTOP, Hyprland
          env = GDK_BACKEND, wayland,x11
          env = QT_QPA_PLATFORM, wayland;xcb
          env = SDL_VIDEODRIVER, wayland
          env = CLUTTER_BACKEND, wayland
          env = MOZ_ENABLE_WAYLAND, 1
          env = MOZ_WEBRENDER, 1
          env = XCURSOR_SIZE, 24
          env = QT_WAYLAND_DISABLE_WINDOWDECORATION, 1
          env = WLR_NO_HARDWARE_CURSORS, 1
          env = WLR_RENDERER_ALLOW_SOFTWARE, 1
          
          # VM-specific performance optimizations
          env = XCURSOR_THEME, Adwaita

          gestures {
            workspace_swipe = true
            workspace_swipe_fingers = 3
          }

          misc {
            mouse_move_enables_dpms = true
            key_press_enables_dpms = false
            disable_hyprland_logo = true
            disable_splash_rendering = true
            # Critical settings for Parallels VM
            focus_on_activate = false  # Prevent apps from stealing focus
            layers_hog_keyboard_focus = false  # Don't let layers steal keyboard
            animate_manual_resizes = false
            animate_mouse_windowdragging = false
          }

          # Animations - restored to original values
          animations {
            enabled = yes
            bezier = myBezier, 0.05, 0.9, 0.1, 1.05
            animation = windows, 1, 3, myBezier
            animation = windowsOut, 1, 3, default, popin 80%
            animation = border, 1, 5, default
            animation = borderangle, 1, 3, default
            animation = fade, 1, 3, default
            animation = workspaces, 1, 3, default
          }

          # Decoration settings - Tokyo Night Storm theme
          decoration {
            rounding = 10
            
            shadow {
              enabled = true
              range = 8
              render_power = 2
              color = rgba(1a1b26ee)
            }
            
            blur {
              enabled = true
              size = 4
              passes = 2
              new_optimizations = true
              xray = true
              noise = 0.01
              contrast = 0.9
              brightness = 0.8
            }
          }

          # Layouts
          dwindle {
            pseudotile = true
            preserve_split = true
            # no_gaps_when_only = false
          }

          master {
            # new_is_master = false
            mfact = 0.5
            orientation = center
            # always_center_master = true
          }

          # Workspace definitions (matching XMonad)
          workspace = 1, persistent:true
          workspace = 2, persistent:true
          workspace = 3, persistent:true
          workspace = 4, persistent:true
          workspace = 5, persistent:true
          workspace = 6, persistent:true
          workspace = 7, persistent:true
          workspace = 8, persistent:true
          workspace = 9, persistent:true

          # Startup applications
          exec-once = $POLKIT_BIN
          exec-once = dbus-update-activation-environment --systemd --all
          exec-once = systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
          exec-once = swww init
          exec-once = sleep 1 && /home/cipher/nixos-config/scripts/setup-wallpaper.sh
          exec-once = swaync
          exec-once = wl-paste --type text --watch cliphist store
          exec-once = wl-paste --type image --watch cliphist store
          exec-once = emacs --daemon

          # XMonad-compatible keybindings

          # Terminal
          bind = ${modifier}SHIFT,Return,exec,ghostty

          # Launchers
          bind = ${modifier},p,exec,wofi --show run
          bind = ${modifier},w,exec,wofi --show window
          bind = ${modifier},r,exec,cliphist list | wofi --dmenu | cliphist decode | wl-copy
          bind = ${modifier}SHIFT,p,exec,gmrun

          # Window management
          bind = ${modifier}SHIFT,c,killactive,
          bind = ${modifier},space,layoutmsg,togglesplit
          bind = ${modifier}SHIFT,space,exec,hyprctl reload

          # Focus movement (XMonad-style stack navigation)
          bind = ${modifier},j,layoutmsg,cyclenext
          bind = ${modifier},k,layoutmsg,cycleprev
          bind = ${modifier},h,movefocus,l
          bind = ${modifier},l,movefocus,r
          bind = ${modifier},Tab,cyclenext

          # Window movement - make current window "master" (main/first position)
          bind = ${modifier},Return,exec,/home/cipher/nixos-config/scripts/hyprland-make-master.sh
          bind = ${modifier}SHIFT,j,movewindow,d
          bind = ${modifier}SHIFT,k,movewindow,u
          bind = ${modifier}SHIFT,h,movewindow,l
          bind = ${modifier}SHIFT,l,movewindow,r

          # Resize (XMonad-style)
          bind = ${modifier}SHIFT,h,resizeactive,-40 0
          bind = ${modifier}SHIFT,l,resizeactive,40 0

          # Floating
          bind = ${modifier},t,togglefloating
          bind = ${modifier},m,fullscreen,1

          # Master area control
          bind = ${modifier},comma,layoutmsg,addmaster
          bind = ${modifier},period,layoutmsg,removemaster

          # Layout switching
          bind = ${modifier},space,exec,/home/cipher/nixos-config/scripts/hyprland-cycle-layout.sh
          bind = ${modifier}SHIFT,z,exec,hyprctl keyword general:layout "master"

          # Spacing control (matching XMonad's Shift+d/i)
          bind = ${modifier}SHIFT,d,exec,hyprctl keyword general:gaps_in $(($(hyprctl getoption general:gaps_in | grep int | awk '{print $2}') - 1))
          bind = ${modifier}SHIFT,i,exec,hyprctl keyword general:gaps_in $(($(hyprctl getoption general:gaps_in | grep int | awk '{print $2}') + 1))

          # Status bar
          bind = ${modifier},b,exec,killall -SIGUSR1 waybar

          # Restart/Exit
          bind = ${modifier},q,exec,hyprctl reload
          bind = ${modifier}SHIFT,q,exit,
          bind = ${modifier}SHIFT,m,exec,waybar

          # Workspaces
          bind = ${modifier},1,workspace,1
          bind = ${modifier},2,workspace,2
          bind = ${modifier},3,workspace,3
          bind = ${modifier},4,workspace,4
          bind = ${modifier},5,workspace,5
          bind = ${modifier},6,workspace,6
          bind = ${modifier},7,workspace,7
          bind = ${modifier},8,workspace,8
          bind = ${modifier},9,workspace,9

          # Move to workspace
          bind = ${modifier}SHIFT,1,movetoworkspace,1
          bind = ${modifier}SHIFT,2,movetoworkspace,2
          bind = ${modifier}SHIFT,3,movetoworkspace,3
          bind = ${modifier}SHIFT,4,movetoworkspace,4
          bind = ${modifier}SHIFT,5,movetoworkspace,5
          bind = ${modifier}SHIFT,6,movetoworkspace,6
          bind = ${modifier}SHIFT,7,movetoworkspace,7
          bind = ${modifier}SHIFT,8,movetoworkspace,8
          bind = ${modifier}SHIFT,9,movetoworkspace,9

          # Mouse bindings (matching XMonad)
          bindm = ${modifier},mouse:272,movewindow
          bindm = ${modifier},mouse:273,resizewindow
          bind = ${modifier},mouse_down,workspace,e+1
          bind = ${modifier},mouse_up,workspace,e-1
          
          # Focus control
          bind = ${modifier},f,exec,hyprctl dispatch focuswindow mouse

          # Screenshot bindings
          bind = ${modifier},s,exec,grim ~/Pictures/screenshot-$(date +%Y-%m-%d_%H-%M-%S).png
          bind = ${modifier}SHIFT,s,exec,grim -g "$(slurp)" ~/Pictures/screenshot-$(date +%Y-%m-%d_%H-%M-%S).png

          # Additional functionality
          bind = ${modifier},n,exec,hyprctl dispatch togglefloating active; hyprctl dispatch centerwindow
          
          # Parallels clipboard fix - manually trigger when experiencing beach balls
          bind = ${modifier}SHIFT,x,exec,fix-parallels-clipboard
        ''
      ];
  };
}
