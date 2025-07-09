{
  pkgs,
  config,
  lib,
  inputs,
  ...
}:

with lib;
{
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
          monitor=,preferred,auto,1

          # Window rules
          # windowrule = float, ^(steam)$
          # windowrule = size 1080 900, ^(steam)$
          # windowrule = center, ^(steam)$
          # windowrule = float, ^(MPlayer)$
          # windowrule = float, ^(Gimp)$

          general {
            gaps_in = 5
            gaps_out = 5
            border_size = 2
            col.active_border = rgba(5DFFFFff)
            col.inactive_border = rgba(2c698dff)
            layout = master
            resize_on_border = true
          }

          input {
            kb_layout = us
            kb_options = caps:super
            follow_mouse = 1
            touchpad {
              natural_scroll = false
            }
            sensitivity = 0
            accel_profile = flat
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

          gestures {
            workspace_swipe = true
            workspace_swipe_fingers = 3
          }

          misc {
            mouse_move_enables_dpms = true
            key_press_enables_dpms = false
            disable_hyprland_logo = true
            disable_splash_rendering = true
          }

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

          decoration {
            rounding = 5
            # drop_shadow = false
            blur {
              enabled = false
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
          workspace = name:coding, persistent:true
          workspace = name:web, persistent:true
          workspace = name:services, persistent:true
          workspace = name:work, persistent:true
          workspace = name:misc, persistent:true
          workspace = name:6, persistent:true
          workspace = name:7, persistent:true
          workspace = name:8, persistent:true
          workspace = name:9, persistent:true

          # Startup applications
          exec-once = $POLKIT_BIN
          exec-once = dbus-update-activation-environment --systemd --all
          exec-once = systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
          exec-once = swww init
          exec-once = sleep 1 && swww img ~/Downloads/wallpaper/building_city_japan_tokyo_during_nighttime_hd_travel-1920x1080.jpg
          exec-once = waybar
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

          # Focus movement (vim-style like XMonad)
          bind = ${modifier},j,movefocus,d
          bind = ${modifier},k,movefocus,u
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
          bind = ${modifier},m,fullscreen,0

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
          bind = ${modifier},1,workspace,name:coding
          bind = ${modifier},2,workspace,name:web
          bind = ${modifier},3,workspace,name:services
          bind = ${modifier},4,workspace,name:work
          bind = ${modifier},5,workspace,name:misc
          bind = ${modifier},6,workspace,name:6
          bind = ${modifier},7,workspace,name:7
          bind = ${modifier},8,workspace,name:8
          bind = ${modifier},9,workspace,name:9

          # Move to workspace
          bind = ${modifier}SHIFT,1,movetoworkspace,name:coding
          bind = ${modifier}SHIFT,2,movetoworkspace,name:web
          bind = ${modifier}SHIFT,3,movetoworkspace,name:services
          bind = ${modifier}SHIFT,4,movetoworkspace,name:work
          bind = ${modifier}SHIFT,5,movetoworkspace,name:misc
          bind = ${modifier}SHIFT,6,movetoworkspace,name:6
          bind = ${modifier}SHIFT,7,movetoworkspace,name:7
          bind = ${modifier}SHIFT,8,movetoworkspace,name:8
          bind = ${modifier}SHIFT,9,movetoworkspace,name:9

          # Mouse bindings (matching XMonad)
          bindm = ${modifier},mouse:272,movewindow
          bindm = ${modifier},mouse:273,resizewindow
          bind = ${modifier},mouse_down,workspace,e+1
          bind = ${modifier},mouse_up,workspace,e-1

          # Additional functionality
          bind = ${modifier},n,exec,hyprctl dispatch togglefloating active; hyprctl dispatch centerwindow
        ''
      ];
  };
}
