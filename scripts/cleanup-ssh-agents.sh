#!/usr/bin/env bash

# Clean up redundant SSH agent processes
# Keep only the most recent SSH agent and kill the rest

echo "Cleaning up SSH agent processes..."

# Get all SSH agent processes (excluding the grep process)
ssh_agents=$(ps -u $USER -o pid,cmd | grep 'ssh-agent' | grep -v grep | awk '{print $1}')

if [ -z "$ssh_agents" ]; then
    echo "No SSH agent processes found."
    exit 0
fi

# Count total agents
total_agents=$(echo "$ssh_agents" | wc -l)
echo "Found $total_agents SSH agent processes"

if [ $total_agents -le 1 ]; then
    echo "Only one SSH agent process found. No cleanup needed."
    exit 0
fi

# Kill all SSH agent processes except the current one (if SSH_AUTH_SOCK is set)
current_agent_pid=""
if [ -n "$SSH_AUTH_SOCK" ]; then
    # Extract PID from SSH_AUTH_SOCK path if possible
    current_agent_pid=$(echo "$SSH_AUTH_SOCK" | grep -o '[0-9]\+' | tail -1)
fi

killed_count=0
for pid in $ssh_agents; do
    if [ "$pid" != "$current_agent_pid" ]; then
        if kill -0 "$pid" 2>/dev/null; then
            echo "Killing SSH agent process: $pid"
            kill "$pid" 2>/dev/null || true
            killed_count=$((killed_count + 1))
        fi
    fi
done

echo "Cleaned up $killed_count SSH agent processes"

# Show remaining SSH agents
remaining=$(ps -u $USER -o pid,cmd | grep 'ssh-agent' | grep -v grep | wc -l)
echo "Remaining SSH agent processes: $remaining"