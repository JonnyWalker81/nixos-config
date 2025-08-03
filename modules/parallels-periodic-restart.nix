{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.hardware.parallels.periodicRestart;
  
  # Enhanced health check script with memory tracking and service age
  enhancedHealthCheck = pkgs.writeShellScript "parallels-enhanced-health-check" ''
    #!/usr/bin/env bash
    
    set -euo pipefail
    
    LOG_FILE="/tmp/parallels-health-check.log"
    STATE_FILE="/tmp/parallels-service-state"
    ISSUES_FOUND=0
    
    log() {
        echo "[$(date)] $1" | tee -a "$LOG_FILE"
    }
    
    # Track service uptime
    get_service_uptime() {
        local service="$1"
        local active_since=$(systemctl --user show -p ActiveEnterTimestamp "$service" 2>/dev/null | cut -d= -f2)
        if [ -n "$active_since" ] && [ "$active_since" != "n/a" ]; then
            local uptime_seconds=$(( $(date +%s) - $(date -d "$active_since" +%s) ))
            echo "$uptime_seconds"
        else
            echo "0"
        fi
    }
    
    # Check if service needs restart based on uptime
    check_service_age() {
        local service="$1"
        local max_uptime_hours="${toString cfg.restartOnUptimeHours}"
        local max_uptime_seconds=$((max_uptime_hours * 3600))
        
        local uptime=$(get_service_uptime "$service")
        local uptime_hours=$((uptime / 3600))
        
        if [ "$uptime" -gt "$max_uptime_seconds" ]; then
            log "✗ ''${service} has been running for ''${uptime_hours} hours (threshold: ''${max_uptime_hours} hours)"
            ISSUES_FOUND=$((ISSUES_FOUND + 1))
            return 1
        else
            log "✓ ''${service} uptime OK: ''${uptime_hours} hours"
            return 0
        fi
    }
    
    # Track memory growth over time
    track_memory_growth() {
        local process="$1"
        local pid=$(pgrep -f "$process" | head -1)
        
        if [ -n "$pid" ]; then
            local current_mem_kb=$(ps -o rss= -p "$pid" 2>/dev/null || echo 0)
            local state_key="''${process}_memory"
            
            # Read previous memory usage
            local prev_mem_kb=0
            if [ -f "$STATE_FILE" ]; then
                prev_mem_kb=$(grep "^''${state_key}=" "$STATE_FILE" 2>/dev/null | cut -d= -f2 || echo 0)
            fi
            
            # Save current memory usage
            grep -v "^''${state_key}=" "$STATE_FILE" 2>/dev/null > "$STATE_FILE.tmp" || true
            echo "''${state_key}=''${current_mem_kb}" >> "$STATE_FILE.tmp"
            mv "$STATE_FILE.tmp" "$STATE_FILE"
            
            # Check for significant memory growth (>20% increase)
            if [ "$prev_mem_kb" -gt 0 ]; then
                local growth_percent=$(( (current_mem_kb - prev_mem_kb) * 100 / prev_mem_kb ))
                if [ "$growth_percent" -gt 20 ]; then
                    log "✗ ''${process} shows significant memory growth: ''${growth_percent}% increase"
                    ISSUES_FOUND=$((ISSUES_FOUND + 1))
                    return 1
                fi
            fi
        fi
        return 0
    }
    
    # Run built-in health checks
    check_service() {
        local service="$1"
        if systemctl --user is-active "$service" &>/dev/null; then
            log "✓ ''${service} is running"
            return 0
        else
            log "✗ ''${service} is not running"
            ISSUES_FOUND=$((ISSUES_FOUND + 1))
            return 1
        fi
    }
    
    check_process_memory() {
        local process="$1"
        local max_mem_mb="${toString cfg.restartOnMemoryThreshold}"
        
        local pid=$(pgrep -f "$process" | head -1)
        if [ -n "$pid" ]; then
            local mem_kb=$(ps -o rss= -p "$pid" 2>/dev/null || echo 0)
            local mem_mb=$((mem_kb / 1024))
            
            if [ "$mem_mb" -gt "$max_mem_mb" ]; then
                log "✗ ''${process} using too much memory: ''${mem_mb}MB (max: ''${max_mem_mb}MB)"
                ISSUES_FOUND=$((ISSUES_FOUND + 1))
                return 1
            else
                log "✓ ''${process} memory usage OK: ''${mem_mb}MB"
                return 0
            fi
        fi
        return 0
    }
    
    check_clipboard_size() {
        local size=0
        if command -v wl-paste &> /dev/null; then
            size=$(wl-paste 2>/dev/null | wc -c || echo 0)
        elif command -v xclip &> /dev/null; then
            size=$(xclip -selection clipboard -o 2>/dev/null | wc -c || echo 0)
        fi
        
        if [ -z "$size" ]; then
            size=0
        fi
        
        local size_kb=$((size / 1024))
        if [ "$size" -gt ${toString config.hardware.parallels.clipboard.maxSize} ]; then
            log "✗ Clipboard content too large: ''${size_kb}KB"
            ISSUES_FOUND=$((ISSUES_FOUND + 1))
            return 1
        else
            log "✓ Clipboard size OK: ''${size_kb}KB"
            return 0
        fi
    }
    
    # Run basic health checks
    log "=== Basic Health Checks ==="
    check_service "prlcp"
    check_service "parallels-clipboard-monitor"
    check_service "parallels-focus-guard" || true
    check_process_memory "prlcp"
    check_clipboard_size
    
    # Additional checks
    log ""
    log "=== Extended Health Checks ==="
    
    # Check service age
    check_service_age "prlcp"
    check_service_age "parallels-clipboard-monitor" || true
    
    # Check memory growth
    track_memory_growth "prlcp"
    
    # Return number of issues found
    exit "$ISSUES_FOUND"
  '';
  
  # Conditional restart script
  conditionalRestart = pkgs.writeShellScript "parallels-conditional-restart" ''
    #!/usr/bin/env bash
    
    echo "[$(date)] Running conditional restart check..."
    
    # Run health check
    if ! ${enhancedHealthCheck}; then
        echo "[$(date)] Health check failed, restarting services..."
        
        # Stop services
        systemctl --user stop prlcp parallels-clipboard-monitor parallels-focus-guard 2>/dev/null || true
        
        # Kill hanging processes
        pkill -9 prlcp 2>/dev/null || true
        
        # Clear clipboard
        if command -v wl-copy &> /dev/null; then
            echo -n "" | wl-copy
            echo -n "" | wl-copy --primary
        fi
        if command -v xclip &> /dev/null; then
            echo -n "" | xclip -selection clipboard
            echo -n "" | xclip -selection primary
        fi
        
        # Wait
        sleep 2
        
        # Restart services
        systemctl --user start prlcp
        systemctl --user start parallels-clipboard-monitor
        systemctl --user start parallels-focus-guard 2>/dev/null || true
        
        echo "[$(date)] Services restarted due to health check failure"
        
        # Send notification if available
        if command -v notify-send &> /dev/null; then
            notify-send "Parallels Services Restarted" "Services were restarted due to health check failure" -u low
        fi
    else
        echo "[$(date)] Health check passed, no restart needed"
    fi
  '';
  
  # Preventive restart script
  preventiveRestart = pkgs.writeShellScript "parallels-preventive-restart" ''
    #!/usr/bin/env bash
    
    echo "[$(date)] Performing preventive restart of Parallels services..."
    
    # Stop services
    systemctl --user stop prlcp parallels-clipboard-monitor parallels-focus-guard 2>/dev/null || true
    
    # Kill any hanging processes
    pkill -9 prlcp 2>/dev/null || true
    
    # Clear state file to reset memory tracking
    rm -f /tmp/parallels-service-state
    
    # Clear clipboard
    if command -v wl-copy &> /dev/null; then
        echo -n "" | wl-copy
        echo -n "" | wl-copy --primary
    fi
    if command -v xclip &> /dev/null; then
        echo -n "" | xclip -selection clipboard
        echo -n "" | xclip -selection primary
    fi
    
    # Wait
    sleep 3
    
    # Restart services
    systemctl --user start prlcp
    systemctl --user start parallels-clipboard-monitor
    systemctl --user start parallels-focus-guard 2>/dev/null || true
    
    echo "[$(date)] Preventive restart completed"
    
    # Send notification if available
    if command -v notify-send &> /dev/null; then
        notify-send "Parallels Services Restarted" "Daily preventive restart completed" -u low
    fi
  '';

in {
  options.hardware.parallels.periodicRestart = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable periodic restart of Parallels services to prevent issues";
    };
    
    healthCheckInterval = mkOption {
      type = types.str;
      default = "1h";
      description = "How often to run health checks (systemd time format)";
    };
    
    preventiveRestartTime = mkOption {
      type = types.str;
      default = "03:00";
      description = "Time of day to perform preventive restart";
    };
    
    restartOnMemoryThreshold = mkOption {
      type = types.int;
      default = 512;
      description = "Restart service if memory usage exceeds this value (MB)";
    };
    
    restartOnUptimeHours = mkOption {
      type = types.int;
      default = 12;
      description = "Restart service if it has been running for more than this many hours";
    };
    
    enablePreventiveRestart = mkOption {
      type = types.bool;
      default = true;
      description = "Enable daily preventive restart";
    };
    
    enableNotifications = mkOption {
      type = types.bool;
      default = true;
      description = "Send desktop notifications when services are restarted";
    };
  };
  
  config = mkIf (config.hardware.parallels.enable && cfg.enable) {
    
    # Health monitor timer and service
    systemd.user.timers.parallels-health-monitor = {
      description = "Parallels Health Monitor Timer";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "5m";
        OnUnitActiveSec = cfg.healthCheckInterval;
        Unit = "parallels-health-monitor.service";
      };
    };
    
    systemd.user.services.parallels-health-monitor = {
      description = "Parallels Health Monitor";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${conditionalRestart}";
        StandardOutput = "journal";
        StandardError = "journal";
      };
    };
    
    # Preventive restart timer and service
    systemd.user.timers.parallels-preventive-restart = mkIf cfg.enablePreventiveRestart {
      description = "Parallels Preventive Restart Timer";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = cfg.preventiveRestartTime;
        Persistent = true;
        Unit = "parallels-preventive-restart.service";
      };
    };
    
    systemd.user.services.parallels-preventive-restart = mkIf cfg.enablePreventiveRestart {
      description = "Parallels Preventive Restart";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${preventiveRestart}";
        StandardOutput = "journal";
        StandardError = "journal";
      };
    };
    
    # Add commands to system packages
    environment.systemPackages = [
      (pkgs.writeShellScriptBin "parallels-health-status" ''
        echo "=== Parallels Periodic Restart Status ==="
        echo ""
        echo "Health Monitor Timer:"
        systemctl --user status parallels-health-monitor.timer
        echo ""
        echo "Last Health Check:"
        systemctl --user status parallels-health-monitor.service
        echo ""
        if [ "${toString cfg.enablePreventiveRestart}" = "1" ]; then
          echo "Preventive Restart Timer:"
          systemctl --user status parallels-preventive-restart.timer
          echo ""
          echo "Next preventive restart:"
          systemctl --user list-timers parallels-preventive-restart.timer
        fi
      '')
    ];
  };
}