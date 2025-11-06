# Parallels Clipboard Fix - Beach Ball Prevention

## Summary of Changes

I've implemented comprehensive fixes to prevent beach balls when switching between your Parallels VM and macOS (especially with Slack). The key improvements include:

### 1. **Force X11 Mode for prlcp**
- Added environment variables to force X11 backend
- Prevents Wayland compatibility crashes that were causing segfaults

### 2. **Clipboard Size Limits**
- Reduced maximum clipboard size to 256KB
- Enabled plain text only mode to avoid format conversion issues
- Active monitoring that clears oversized clipboard content

### 3. **Focus Management**
- Created a focus guard service that monitors window focus changes
- Automatically clears clipboard on rapid focus changes
- Prevents the clipboard sync from hanging during VM/host switches

### 4. **Service Optimization**
- Added memory limits (256MB) and CPU quotas (50%)
- Implemented automatic restart with backoff
- Better error handling and recovery

## Available Commands

### Quick Fix (When Experiencing Beach Balls)
```bash
# Keyboard shortcut: Super+Shift+X
fix-clipboard

# Or run directly:
fix-parallels-clipboard
```

### Health Check
```bash
# Check status of all Parallels services
/home/cipher/nixos-config/scripts/parallels-health-check.sh

# Auto-fix any issues found
/home/cipher/nixos-config/scripts/parallels-health-check.sh --fix
```

### Manual Service Management
```bash
# Restart clipboard services
systemctl --user restart prlcp parallels-clipboard-monitor

# Check service status
systemctl --user status prlcp
systemctl --user status parallels-clipboard-monitor
```

## Configuration

The configuration is in `/home/cipher/nixos-config/machines/vm-aarch64-prl.nix`:
- `plainTextOnly = true` - Forces plain text clipboard (more stable)
- `maxSize = 262144` - 256KB limit to prevent hangs
- `monitor = true` - Active monitoring enabled

## Logs

Check logs if issues persist:
- `/tmp/prlcp-wrapper.log` - prlcp wrapper logs
- `/tmp/parallels-clipboard-monitor.log` - Clipboard monitor logs
- `/tmp/parallels-focus-guard.log` - Focus guard logs
- `/tmp/parallels-health-check.log` - Health check results

## Tips

1. If you still experience beach balls, use the keyboard shortcut `Super+Shift+X` to quickly fix
2. The system now automatically clears large clipboard content
3. Focus changes are monitored to prevent sync issues
4. Consider disabling "Preserve text formatting" in Parallels Desktop preferences for additional stability