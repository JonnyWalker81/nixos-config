# Omarchy-style command menu for DWM/X11.
#
# A nested `rofi -dmenu` menu mirroring Omarchy's `omarchy-menu`: apps, capture,
# style, toggles, system actions, and a power/session submenu (which also fills
# the gap of having no power/logout menu on DWM).
#
# Launched from sxhkd (Super+Ctrl+Space and Super+Escape) and, later, the polybar
# launcher glyph. Uses writeShellScriptBin (not writeShellApplication) because
# interactive rofi commands legitimately exit non-zero on Escape, which would trip
# `set -e`.
{
  writeShellScriptBin,
  lib,
  rofi,
  xdotool,
  systemd,
  libnotify,
  coreutils,
}:

writeShellScriptBin "omarchy-menu" ''
  # Nix-provided tools first; keep the session PATH so ghostty/btm/nvim resolve.
  export PATH="${lib.makeBinPath [ rofi xdotool systemd libnotify coreutils ]}:$PATH"

  THEME="$HOME/.config/rofi/omarchy.rasi"
  SHOT="/home/cipher/nixos-config/scripts/screenshot.sh"
  WALL="/home/cipher/nixos-config/scripts/wallpaper-rotate.sh"
  DWM_CONFIG="/home/cipher/Repositories/dwm/config.h"
  REBUILD="sudo nixos-rebuild switch --flake /home/cipher/nixos-config#vm-aarch64-prl"
  EDITOR="''${EDITOR:-nvim}"

  menu()    { rofi -dmenu -i -p "$1" -theme "$THEME"; }
  confirm() { printf 'No\nYes\n' | rofi -dmenu -i -p "$1" -theme "$THEME"; }

  # Fire a DWM (Alt/Mod1) keybinding. DWM's keygrabs work regardless of focus.
  # --clearmodifiers + a short delay avoids a still-held Super tainting the chord.
  dwmkey() { sleep 0.12; xdotool key --clearmodifiers "$1"; }

  term() { ghostty -e "$@"; }

  power_menu() {
    case "$(printf '%s\n' \
      "  Lock" "  Suspend" "  Reboot" "  Shut down" "  Log out" | menu "Power")" in
      *Lock*)      loginctl lock-session ;;
      *Suspend*)   systemctl suspend ;;
      *Reboot*)    if [ "$(confirm "Reboot?")" = "Yes" ]; then systemctl reboot; fi ;;
      *"Shut down"*) if [ "$(confirm "Shut down?")" = "Yes" ]; then systemctl poweroff; fi ;;
      *"Log out"*) if [ "$(confirm "Log out?")" = "Yes" ]; then
                     loginctl terminate-session "''${XDG_SESSION_ID:-self}"
                   fi ;;
    esac
  }

  capture_menu() {
    case "$(printf '%s\n' "  Full screen" "  Selection" | menu "Capture")" in
      *Full*)      "$SHOT" ;;
      *Selection*) "$SHOT" -s ;;
    esac
  }

  style_menu() {
    case "$(printf '%s\n' "  Next wallpaper" "  Edit DWM colors" | menu "Style")" in
      *wallpaper*) "$WALL" once ;;
      *colors*)    term "$EDITOR" "$DWM_CONFIG" ;;
    esac
  }

  toggle_menu() {
    case "$(printf '%s\n' "  Gaps   (Alt+Shift+g)" "  Bar   (Alt+b)" | menu "Toggle")" in
      *Gaps*) dwmkey "alt+shift+g" ;;
      *Bar*)  dwmkey "alt+b" ;;
    esac
  }

  system_menu() {
    case "$(printf '%s\n' "  Rebuild NixOS" "  Update flake" "  System monitor" | menu "System")" in
      *Rebuild*) term sh -c "$REBUILD; echo; read -r -p 'Press enter to close… ' _" ;;
      *Update*)  term sh -c "cd /home/cipher/nixos-config && nix flake update; echo; read -r -p 'Press enter to close… ' _" ;;
      *monitor*) term btm ;;
    esac
  }

  # Deep link: `omarchy-menu power` opens the power submenu directly (Super+Escape).
  if [ "''${1:-}" = "power" ]; then power_menu; exit 0; fi

  case "$(printf '%s\n' \
    "  Apps" "  Capture" "  Style" "  Toggle" "  System" "  Power" | menu "Menu")" in
    *Apps*)    rofi -show drun -theme "$THEME" ;;
    *Capture*) capture_menu ;;
    *Style*)   style_menu ;;
    *Toggle*)  toggle_menu ;;
    *System*)  system_menu ;;
    *Power*)   power_menu ;;
  esac
''
