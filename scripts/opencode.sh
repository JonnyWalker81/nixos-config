#!/usr/bin/env bash

# OpenCode wrapper script for NixOS
# Since OpenCode binaries don't work directly on NixOS, we use alternative methods

set -e

# Option 1: Try using npx with a custom NODE_PATH
run_with_npx() {
    echo "Running OpenCode via npx..."
    exec npx --yes opencode-ai@latest "$@"
}

# Option 2: Use Docker if available
run_with_docker() {
    if command -v docker &> /dev/null; then
        echo "Running OpenCode via Docker..."
        docker run -it --rm \
            -v "$PWD:/workspace" \
            -w /workspace \
            -v "$HOME/.config/opencode:/root/.config/opencode" \
            node:20-alpine \
            sh -c "npm install -g opencode-ai@latest && opencode $*"
    else
        echo "Docker not found, falling back to npx..."
        run_with_npx "$@"
    fi
}

# Option 3: Create a temporary Node.js project
run_in_temp_project() {
    TEMP_DIR=$(mktemp -d)
    trap "rm -rf $TEMP_DIR" EXIT
    
    echo "Setting up temporary OpenCode environment..."
    cd "$TEMP_DIR"
    npm init -y >/dev/null 2>&1
    npm install opencode-ai@latest >/dev/null 2>&1
    
    # Run OpenCode with original working directory
    cd "$OLDPWD"
    exec "$TEMP_DIR/node_modules/.bin/opencode" "$@"
}

# Main execution
case "${OPENCODE_METHOD:-npx}" in
    docker)
        run_with_docker "$@"
        ;;
    temp)
        run_in_temp_project "$@"
        ;;
    *)
        run_with_npx "$@"
        ;;
esac