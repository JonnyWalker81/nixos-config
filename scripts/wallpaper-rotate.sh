#!/usr/bin/env bash
#
# Wallpaper rotation engine.
#
# Rotates the desktop wallpaper among a curated set of dark backgrounds that
# harmonize with the DWM "Tokyo Night" palette (see ~/Repositories/dwm/config.h).
# Idempotently downloads the set on first run, then picks a random image and
# applies it via feh (X11 / DWM) and/or swww (Wayland / Hyprland).
#
# Usage:
#   wallpaper-rotate.sh          # rotate once
#   wallpaper-rotate.sh once     # rotate once (explicit)
#   wallpaper-rotate.sh loop     # rotate now, then every $WALLPAPER_ROTATE_INTERVAL seconds
#
# Wallpapers are pulled from Omarchy's theme "backgrounds/" folders on the `dev`
# branch (Omarchy's default/canonical branch).

set -u

# Ensure core commands are available when launched from systemd user units or the
# DWM autostart hook (both run with a minimal PATH).
export PATH="/run/current-system/sw/bin:/etc/profiles/per-user/$USER/bin:/nix/var/nix/profiles/default/bin:$PATH"

WALLPAPER_DIR="$HOME/.local/share/wallpapers"
ROTATION_DIR="$WALLPAPER_DIR/rotation"
CURRENT_LINK="$WALLPAPER_DIR/current"
STATE_FILE="$WALLPAPER_DIR/.rotation-last"
ROTATE_INTERVAL="${WALLPAPER_ROTATE_INTERVAL:-900}" # seconds between rotations (default 15 min)

BASE="https://raw.githubusercontent.com/basecamp/omarchy/dev/themes"

# Curated dark set (all harmonize with the Tokyo Night palette). Format: "<url> <local-name>".
MANIFEST=(
	"$BASE/tokyo-night/backgrounds/4-oma-cityscape.jpg tn-cityscape.jpg"
	"$BASE/tokyo-night/backgrounds/2-pawel-czerwinski.jpg tn-pawel-ribbons.jpg"
	"$BASE/tokyo-night/backgrounds/3-milad-fakurian.jpg tn-milad-gradient.jpg"
	"$BASE/tokyo-night/backgrounds/5-oma.jpg tn-minimal.jpg"
	"$BASE/tokyo-night/backgrounds/1-sunset-lake.png tn-sunset-lake.png"
	"$BASE/matte-black/backgrounds/1-dark-waters.jpg mb-dark-waters.jpg"
	"$BASE/matte-black/backgrounds/2-dot-hands.jpg mb-dot-hands.jpg"
	"$BASE/nord/backgrounds/0-black-moon.jpg nord-black-moon.jpg"
)

ensure_downloaded() {
	mkdir -p "$ROTATION_DIR"
	local entry url name dest
	for entry in "${MANIFEST[@]}"; do
		read -r url name <<<"$entry"
		dest="$ROTATION_DIR/$name"
		# (Re)download if missing or suspiciously small (<50KB => probably an error page).
		if [ ! -f "$dest" ] || [ "$(stat -c%s "$dest" 2>/dev/null || echo 0)" -lt 51200 ]; then
			if curl -fsSL --retry 4 --retry-delay 2 -o "$dest" "$url"; then
				echo "wallpaper-rotate: downloaded $name"
			else
				echo "wallpaper-rotate: WARN failed to fetch $name" >&2
				rm -f "$dest"
			fi
			sleep 0.4 # be gentle with raw.githubusercontent rate limits
		fi
	done
}

apply_wallpaper() {
	local img="$1"
	ln -sf "$img" "$CURRENT_LINK"
	echo "$img" >"$STATE_FILE"

	# Wayland (Hyprland) via swww.
	if [ -n "${WAYLAND_DISPLAY:-}" ] && command -v swww >/dev/null 2>&1; then
		pgrep -x swww-daemon >/dev/null 2>&1 || {
			swww-daemon &
			sleep 1
		}
		swww img "$img" --transition-type fade --transition-duration 2 &&
			echo "wallpaper-rotate: swww -> $(basename "$img")"
	fi

	# X11 (DWM/XMonad/awesome) via feh.
	if [ -n "${DISPLAY:-}" ] && command -v feh >/dev/null 2>&1; then
		feh --no-fehbg --bg-fill "$img" && echo "wallpaper-rotate: feh -> $(basename "$img")"
		printf '#!/bin/sh\nfeh --no-fehbg --bg-fill %q\n' "$img" >"$HOME/.fehbg"
		chmod +x "$HOME/.fehbg"
	fi
}

pick_image() {
	shopt -s nullglob
	local images=()
	mapfile -t images < <(find "$ROTATION_DIR" -maxdepth 1 -type f \
		\( -iname '*.jpg' -o -iname '*.jpeg' -o -iname '*.png' \) | sort)

	if [ "${#images[@]}" -eq 0 ]; then
		echo "wallpaper-rotate: ERROR no wallpapers in $ROTATION_DIR" >&2
		return 1
	fi

	local last="" pick=""
	[ -f "$STATE_FILE" ] && last="$(cat "$STATE_FILE" 2>/dev/null)"
	# Try a few times to avoid repeating the previous wallpaper.
	local i
	for i in 1 2 3 4 5; do
		pick="${images[$((RANDOM % ${#images[@]}))]}"
		[ "${#images[@]}" -eq 1 ] && break
		[ "$pick" != "$last" ] && break
	done
	printf '%s\n' "$pick"
}

rotate_once() {
	ensure_downloaded
	local img
	img="$(pick_image)" || return 1
	apply_wallpaper "$img"
}

main() {
	case "${1:-once}" in
	loop)
		while true; do
			rotate_once
			sleep "$ROTATE_INTERVAL"
		done
		;;
	once | *)
		rotate_once
		;;
	esac
}

main "$@"
