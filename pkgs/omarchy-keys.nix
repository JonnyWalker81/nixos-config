# Omarchy-style keybinding cheatsheet for DWM/X11 (Super+K).
#
# Mirrors Omarchy's `omarchy-menu-keybindings`: a fuzzy-filterable list of every
# keybinding. Display-only — selecting an entry just closes the menu.
#
# Source of truth is a single curated list (~/.config/omarchy/keybindings.list),
# because DWM's Alt binds are compiled into config.h in a *separate* repo and can't
# be parsed from here, while the Super binds live in sxhkdrc. One flat list mirrors
# both and is trivial to keep in sync.
{
  writeShellScriptBin,
  lib,
  rofi,
  gnugrep,
  coreutils,
}:

writeShellScriptBin "omarchy-keys" ''
  export PATH="${lib.makeBinPath [ rofi gnugrep coreutils ]}:$PATH"

  LIST="$HOME/.config/omarchy/keybindings.list"
  THEME="$HOME/.config/rofi/omarchy.rasi"

  if [ ! -f "$LIST" ]; then
    rofi -e "keybindings.list not found at $LIST"
    exit 1
  fi

  # Drop blank/comment lines; selection is discarded (informational only).
  grep -vE '^[[:space:]]*(#|$)' "$LIST" \
    | rofi -dmenu -i -p "Keys" -theme "$THEME" -no-custom \
        -mesg "Fuzzy-filter keybindings — Esc to close" >/dev/null || true
''
