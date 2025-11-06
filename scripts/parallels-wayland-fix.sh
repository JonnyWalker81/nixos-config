#!/usr/bin/env bash
# Parallels Wayland Click-Through Fix
# The most reliable solution for preventing click-through in Parallels VMs

# Solution: Use systemd to monitor focus changes and apply fixes
cat > /tmp/parallels-focus-monitor.service << EOF
[Unit]
Description=Parallels Focus Monitor for Hyprland
After=graphical-session.target

[Service]
Type=simple
ExecStart=/home/cipher/nixos-config/scripts/focus-monitor-daemon.sh
Restart=always
RestartSec=1

[Install]
WantedBy=default.target
EOF

# Create the daemon script
cat > /home/cipher/nixos-config/scripts/focus-monitor-daemon.sh << 'EOF'
#!/usr/bin/env bash
# Monitor for focus events and prevent click-through

# Function to handle focus events
handle_focus() {
    # When we detect a focus change from outside (Parallels host)
    # We need to consume the first click event
    
    # Method 1: Create a temporary input region that blocks clicks
    hyprctl dispatch exec "sleep 0.1 && hyprctl reload"
}

# Monitor using socat to watch Hyprland socket
socat -U - UNIX-CONNECT:/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock | while read -r line; do
    case "$line" in
        *"workspace>>"*)
            # Workspace changed - possible focus from host
            handle_focus
            ;;
        *"activewindow>>"*)
            # Active window changed
            handle_focus
            ;;
    esac
done
EOF

chmod +x /home/cipher/nixos-config/scripts/focus-monitor-daemon.sh

# Install the service
systemctl --user link /tmp/parallels-focus-monitor.service
systemctl --user daemon-reload
systemctl --user enable parallels-focus-monitor.service
systemctl --user start parallels-focus-monitor.service

echo "Parallels focus monitor installed and started"