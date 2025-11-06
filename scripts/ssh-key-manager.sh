#!/usr/bin/env bash

# SSH Key Manager - Helps manage SSH keys to reduce password prompts

set -euo pipefail

# Function to add keys with longer timeout
add_keys_with_timeout() {
    local timeout=${1:-28800}  # 8 hours default

    echo "Adding SSH keys with ${timeout}s timeout..."

    # Add only essential keys first
    if [ -f ~/.ssh/id_ed25519 ]; then
        ssh-add -t ${timeout} ~/.ssh/id_ed25519 2>/dev/null || echo "Failed to add id_ed25519"
    fi

    if [ -f ~/.ssh/id_rsa ]; then
        ssh-add -t ${timeout} ~/.ssh/id_rsa 2>/dev/null || echo "Failed to add id_rsa"
    fi

    # Add work-specific keys only when needed
    if [ -f ~/.ssh/id_github ]; then
        ssh-add -t ${timeout} ~/.ssh/id_github 2>/dev/null || echo "Failed to add id_github"
    fi

    echo "SSH keys added to agent"
    ssh-add -l
}

# Function to remove problematic keys
remove_problematic_keys() {
    echo "Removing potentially problematic SSH keys..."

    # List current keys and look for Jenkins/buildfarm
    ssh-add -l | grep -i "jenkins\|buildfarm" | while read -r line; do
        key_file=$(echo "$line" | awk '{print $3}')
        echo "Removing key: $key_file"
        ssh-add -d "$key_file" 2>/dev/null || echo "Failed to remove $key_file"
    done
}

# Function to restart SSH agent
restart_ssh_agent() {
    echo "Restarting SSH agent..."

    # Kill existing agent
    if [ -n "${SSH_AGENT_PID:-}" ]; then
        kill "$SSH_AGENT_PID" 2>/dev/null || true
    fi

    # Start new agent
    eval "$(ssh-agent -s)"
    echo "SSH agent restarted"
}

# Main function
main() {
    case "${1:-help}" in
        "add")
            add_keys_with_timeout "${2:-28800}"
            ;;
        "remove")
            remove_problematic_keys
            ;;
        "restart")
            restart_ssh_agent
            ;;
        "status")
            echo "SSH Agent status:"
            ssh-add -l || echo "No keys loaded"
            ;;
        "help"|*)
            echo "Usage: $0 {add|remove|restart|status}"
            echo "  add [timeout]  - Add SSH keys with optional timeout (default: 8 hours)"
            echo "  remove         - Remove problematic Jenkins/buildfarm keys"
            echo "  restart        - Restart SSH agent"
            echo "  status         - Show current SSH agent status"
            ;;
    esac
}

main "$@"