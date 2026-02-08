#!/usr/bin/env bash
# DWM status bar script - chadwm-style colored pill segments with Nerd Font icons
# Runs in a loop, setting xsetroot -name for DWM's built-in bar
# Uses status2d escape codes: ^c#RRGGBB^ (fg), ^b#RRGGBB^ (bg), ^d^ (reset)
#
# Layout: weather | cpu | mem | disk | uptime | date

# Cache weather (refresh every 10 minutes)
WEATHER_CACHE="/tmp/dwm-weather-cache"
WEATHER_INTERVAL=600
WEATHER_LAST=0

get_weather() {
	local now
	now=$(date +%s)
	if [ $((now - WEATHER_LAST)) -ge $WEATHER_INTERVAL ] || [ ! -f "$WEATHER_CACHE" ]; then
		local result
		result=$(curl -s "wttr.in/91105?format=j1" 2>/dev/null | jq -r '.current_condition[0] | "\(.FeelsLikeF)\u00B0F, \(.weatherDesc[0].value)"' 2>/dev/null)
		if [ -n "$result" ] && [ "$result" != "null" ]; then
			echo "$result" >"$WEATHER_CACHE"
		fi
		WEATHER_LAST=$now
	fi
	if [ -f "$WEATHER_CACHE" ]; then
		cat "$WEATHER_CACHE"
	else
		echo "N/A"
	fi
}

get_cpu() {
	local cpu_line
	cpu_line=$(head -1 /proc/stat)
	local user nice system idle iowait irq softirq steal guest guest_nice
	read -r _ user nice system idle iowait irq softirq steal guest guest_nice <<<"$cpu_line"
	guest=${guest:-0}
	guest_nice=${guest_nice:-0}
	local total=$((user + nice + system + idle + iowait + irq + softirq + steal + guest + guest_nice))
	local idle_total=$((idle + iowait))

	if [ -f /tmp/dwm-cpu-prev ]; then
		read -r prev_total prev_idle </tmp/dwm-cpu-prev
		local diff_total=$((total - prev_total))
		local diff_idle=$((idle_total - prev_idle))
		if [ $diff_total -gt 0 ]; then
			local usage=$(((diff_total - diff_idle) * 100 / diff_total))
			echo "${usage}"
		else
			echo "0"
		fi
	else
		echo "-1"
	fi
	echo "$total $idle_total" >/tmp/dwm-cpu-prev
}

# Dynamic CPU color based on usage thresholds
get_cpu_color() {
	local usage=$1
	if [ "$usage" -lt 0 ] 2>/dev/null; then
		echo "#9ece6a" # green (loading)
	elif [ "$usage" -ge 80 ]; then
		echo "#f7768e" # Tokyo Night red (critical)
	elif [ "$usage" -ge 50 ]; then
		echo "#e0af68" # Tokyo Night yellow (warning)
	else
		echo "#9ece6a" # Tokyo Night green (normal)
	fi
}

get_memory() {
	local total used percent
	total=$(free -m | awk '/^Mem:/ {print $2}')
	used=$(free -m | awk '/^Mem:/ {print $3}')
	if [ "$total" -gt 0 ]; then
		percent=$((used * 100 / total))
	else
		percent=0
	fi
	echo "${percent}"
}

# Dynamic memory color based on usage thresholds
get_mem_color() {
	local usage=$1
	if [ "$usage" -ge 80 ]; then
		echo "#f7768e" # Tokyo Night red (critical)
	elif [ "$usage" -ge 50 ]; then
		echo "#e0af68" # Tokyo Night yellow (warning)
	else
		echo "#7aa2f7" # Tokyo Night blue (normal)
	fi
}

get_disk() {
	df -h / | awk 'NR==2 {print $4 " free"}'
}

get_uptime() {
	local up_seconds
	up_seconds=$(cat /proc/uptime | cut -d. -f1)
	local days=$((up_seconds / 86400))
	local hours=$(((up_seconds % 86400) / 3600))
	echo "${days}d ${hours}h"
}

get_datetime() {
	echo "$(date '+%b %d %Y - %H:%M') ($(date -u '+%H:%M') UTC)"
}

# Tokyo Night color palette (matching config.h)
COL_BG="#24283B"      # pill background
COL_SEP="#3e4554"     # separator color (dimmed)
COL_WEATHER="#7aa2f7" # blue - weather icon + text
COL_CPU="#9ece6a"     # green - cpu icon (value color is dynamic)
COL_MEM="#7aa2f7"     # blue - mem icon (value color is dynamic)
COL_DISK="#bb9af7"    # purple - disk
COL_UPTIME="#B4F9F8"  # cyan - uptime
COL_DATE="#9ece6a"    # green - date/time

# Main loop
while true; do
	weather=$(get_weather)
	cpu=$(get_cpu)
	cpu_color=$(get_cpu_color "$cpu")
	if [ "$cpu" -lt 0 ] 2>/dev/null; then
		cpu_display="..."
	else
		cpu_display="${cpu}%"
	fi
	mem=$(get_memory)
	mem_color=$(get_mem_color "$mem")
	mem_display="${mem}%"
	disk=$(get_disk)
	uptime_str=$(get_uptime)
	datetime=$(get_datetime)

	# chadwm-style status with Nerd Font icons and colored segments
	# Each segment: ^c<icon_color>^ <icon> ^c<value_color>^ <value>  ^c<sep>^|
	status=""
	status+="^c${COL_WEATHER}^  ${weather} "
	status+="^c${COL_SEP}^| "
	status+="^c${COL_CPU}^  ^c${cpu_color}^${cpu_display} "
	status+="^c${COL_SEP}^| "
	status+="^c${COL_MEM}^  ^c${mem_color}^${mem_display} "
	status+="^c${COL_SEP}^| "
	status+="^c${COL_DISK}^  ${disk} "
	status+="^c${COL_SEP}^| "
	status+="^c${COL_UPTIME}^  ${uptime_str} "
	status+="^c${COL_SEP}^| "
	status+="^c${COL_DATE}^  ${datetime} "
	status+="^d^"

	xsetroot -name "$status"
	sleep 2
done
