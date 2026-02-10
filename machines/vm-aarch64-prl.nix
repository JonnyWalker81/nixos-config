{
  config,
  pkgs,
  lib,
  modulesPath,
  ...
}:
let
  curl = "${pkgs.curl}/bin/curl";
  jq = "${pkgs.jq}/bin/jq";
  date = "${pkgs.coreutils}/bin/date";
  hwclock = "${pkgs.util-linux}/bin/hwclock";
  systemd-cat = "${pkgs.systemd}/bin/systemd-cat";
  systemctl = "${pkgs.systemd}/bin/systemctl";
  sleep-bin = "${pkgs.coreutils}/bin/sleep";

  # Detects Parallels VM freeze/unfreeze by monitoring for clock jumps.
  # When the VM is frozen by the host (sleep/suspend), time stops advancing
  # inside the guest. On unfreeze, the monotonic clock will show a gap.
  # This script sleeps in a loop and triggers sync-time when a jump > 60s is detected.
  clockSkewDetector = pkgs.writeShellScript "clock-skew-detector" ''
    LOG_TAG="clock-skew-detector"
    CHECK_INTERVAL=10
    SKEW_THRESHOLD=60

    log() {
      echo "$1" | ${systemd-cat} -t "$LOG_TAG" -p "''${2:-info}"
    }

    log "Clock skew detector started (interval=''${CHECK_INTERVAL}s, threshold=''${SKEW_THRESHOLD}s)"

    LAST_REALTIME=$(${date} +%s)

    while true; do
      ${sleep-bin} $CHECK_INTERVAL

      NOW_REALTIME=$(${date} +%s)
      ELAPSED=$((NOW_REALTIME - LAST_REALTIME))
      SKEW=$((ELAPSED - CHECK_INTERVAL))

      # Make skew absolute
      if [ $SKEW -lt 0 ]; then
        SKEW=$((-SKEW))
      fi

      if [ $SKEW -gt $SKEW_THRESHOLD ]; then
        log "Clock skew detected: expected ~''${CHECK_INTERVAL}s elapsed, got ''${ELAPSED}s (skew=''${SKEW}s). Triggering time sync." "warning"
        ${systemctl} start sync-time.service || true
      fi

      LAST_REALTIME=$NOW_REALTIME
    done
  '';

  syncTimeScript = pkgs.writeShellScript "sync-time" ''
    LOG_TAG="sync-time"
    MAX_RETRIES=8
    RETRY_DELAY=5
    NETWORK_WAIT=30

    log() {
      echo "$1" | ${systemd-cat} -t "$LOG_TAG" -p "''${2:-info}"
    }

    # Wait for network connectivity before attempting time sync
    # This is critical after Parallels VM freeze/unfreeze where the
    # network interface needs time to re-establish connectivity
    wait_for_network() {
      log "Waiting for network connectivity (up to ''${NETWORK_WAIT}s)..."
      for i in $(seq 1 $NETWORK_WAIT); do
        if ${curl} -s --max-time 3 -o /dev/null -w "" "https://timeapi.io" 2>/dev/null; then
          log "Network is reachable after ''${i}s"
          return 0
        fi
        sleep 1
      done
      log "Network not reachable after ''${NETWORK_WAIT}s" "warning"
      return 1
    }

    # Try to fetch unix timestamp from timeapi.io
    # Returns the timestamp on stdout, or empty string on failure
    fetch_time() {
      local UNIX_TIME=""

      UNIX_TIME=$(${curl} -s --max-time 10 -X 'GET' 'https://timeapi.io/api/v1/time/current/unix' -H 'accept: */*' 2>/dev/null | ${jq} -r '.unix_timestamp // empty' 2>/dev/null)
      if [[ -n "$UNIX_TIME" ]]; then
        echo "$UNIX_TIME"
        return 0
      fi

      return 1
    }

    wait_for_network

    for i in $(seq 1 $MAX_RETRIES); do
      UNIX_TIME=$(fetch_time)

      if [[ -n "$UNIX_TIME" ]]; then
        OLD_TIME=$(${date})

        if ${date} -u -s "@''${UNIX_TIME}"; then
          # Persist to hardware clock
          ${hwclock} --systohc 2>/dev/null || true
          log "Time synced: ''${OLD_TIME} -> $(${date})"
          exit 0
        else
          log "date -s failed with exit code $? for timestamp ''${UNIX_TIME}" "err"
        fi
      fi

      log "Attempt $i/$MAX_RETRIES: failed to fetch or set time, retrying in ''${RETRY_DELAY}s..." "warning"
      sleep $RETRY_DELAY
    done

    log "Failed to sync time after $MAX_RETRIES attempts" "err"
    exit 1
  '';
in
{
  imports = [
    # Parallels is qemu under the covers. This brings in important kernel
    # modules to get a lot of the stuff working.
    (modulesPath + "/profiles/qemu-guest.nix")

    ../hardware/vm-aarch64-prl.nix
    ../modules/parallels-guest.nix
    # ../modules/parallels-clipboard-working.nix  # Old: Wayland-only, no VM-host sync
    # ../modules/parallels-clipboard-hybrid.nix   # Old: Attempted Wayland with prlcp
    ../modules/parallels-clipboard-x11-bridge.nix # New: wl-clipboard-x11 bridge for prlcp
    ./vm-shared.nix
  ];

  services.ntp.enable = true;
  # The official parallels guest support does not work currently.
  # https://github.com/NixOS/nixpkgs/pull/153665
  disabledModules = [ "virtualisation/parallels-guest.nix" ];
  hardware.parallels = {
    enable = true;
    autoMountShares = true; # Re-enabled with patched prlfsmountd (fixed /etc/fstab read-only issue)
    package = (
      pkgs.callPackage ../pkgs/parallels-tools/default.nix {
        kernel = config.boot.kernelPackages.kernel;
      }
    );
  };

  # Interface is this on my M1
  networking.interfaces.enp0s5.useDHCP = true;

  # Time sync from web APIs (fallback for when NTP/timesyncd are insufficient)
  # Uses timeapi.io to fetch current unix timestamp
  # Runs on resume from sleep and periodically every 30 minutes
  systemd.services.sync-time = {
    description = "Sync system clock from web time API";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${syncTimeScript}";
      Restart = "on-failure";
      RestartSec = "30s";
    };
  };

  # Detect Parallels VM freeze/unfreeze via clock skew monitoring.
  # Parallels does not trigger systemd suspend/hibernate targets when the
  # host sleeps — it simply freezes the VM process. This long-running
  # service detects the resulting time jump and triggers sync-time.
  systemd.services.sync-time-resume = {
    description = "Detect VM freeze/unfreeze and trigger time sync";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${clockSkewDetector}";
      Restart = "always";
      RestartSec = "5s";
    };
  };

  # Periodic fallback timer (every 30 minutes + on boot)
  systemd.timers.sync-time = {
    description = "Periodic time sync fallback";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "1min";
      OnUnitActiveSec = "30min";
      Persistent = true;
    };
  };

  # Lots of stuff that uses aarch64 that claims doesn't work, but actually works.
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = true;
}
