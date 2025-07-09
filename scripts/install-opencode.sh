#!/usr/bin/env bash

# Install OpenCode locally (similar to how Claude is installed)

set -e

OPENCODE_DIR="$HOME/.opencode/local"

echo "Installing OpenCode to $OPENCODE_DIR..."

# Create directory structure
mkdir -p "$OPENCODE_DIR"

# Navigate to the directory
cd "$OPENCODE_DIR"

# Create package.json
cat > package.json << 'EOF'
{
  "name": "opencode-local",
  "version": "0.0.1",
  "private": true,
  "dependencies": {
    "opencode-ai": "latest"
  }
}
EOF

# Install OpenCode
echo "Running npm install..."
npm install

# Create wrapper script
cat > opencode << 'EOF'
#!/bin/bash
exec "$HOME/.opencode/local/node_modules/.bin/opencode" "$@"
EOF

chmod +x opencode

echo "OpenCode installed successfully!"
echo ""
echo "To use OpenCode, add this to your PATH:"
echo "  export PATH=\$HOME/.opencode/local:\$PATH"
echo ""
echo "Or run directly with:"
echo "  ~/.opencode/local/opencode"