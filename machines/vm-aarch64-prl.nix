{ config, pkgs, lib, modulesPath, ... }:
let
  curl = "${pkgs.curl}/bin/curl";
  jq = "${pkgs.jq}/bin/jq";
  date = "${pkgs.coreutils}/bin/date";
  hwclock = "${pkgs.util-linux}/bin/hwclock";
  systemd-cat = "${pkgs.systemd}/bin/systemd-cat";

  syncTimeScript = pkgs.writeShellScript "sync-time" ''
    LOG_TAG="sync-time"
    MAX_RETRIES=5
    RETRY_DELAY=3

    log() {
      echo "$1" | ${systemd-cat} -t "$LOG_TAG" -p "''${2:-info}"
    }

    # Try to fetch unix timestamp from a time API
    # Returns the timestamp on stdout, or empty string on failure
    fetch_time() {
      local UNIX_TIME=""

      # Source 1: worldtimeapi.org (UTC endpoint - more reliable than timezone-specific)
      UNIX_TIME=$(${curl} -s --max-time 10 http://worldtimeapi.org/api/timezone/Etc/UTC 2>/dev/null | ${jq} -r '.unixtime // empty' 2>/dev/null)
      if [[ -n "$UNIX_TIME" ]]; then
        echo "$UNIX_TIME"
        return 0
      fi

      # Source 2: worldclockapi.com (parse ISO datetime to unix timestamp)
      local DATETIME=""
      DATETIME=$(${curl} -s --max-time 10 http://worldclockapi.com/api/json/utc/now 2>/dev/null | ${jq} -r '.currentDateTime // empty' 2>/dev/null)
      if [[ -n "$DATETIME" ]]; then
        UNIX_TIME=$(${date} -d "$DATETIME" +%s 2>/dev/null)
        if [[ -n "$UNIX_TIME" ]]; then
          echo "$UNIX_TIME"
          return 0
        fi
      fi

      return 1
    }

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
in {
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
    autoMountShares =
      true; # Re-enabled with patched prlfsmountd (fixed /etc/fstab read-only issue)
    package = (pkgs.callPackage ../pkgs/parallels-tools/default.nix {
      kernel = config.boot.kernelPackages.kernel;
    });
  };

  # Interface is this on my M1
  networking.interfaces.enp0s5.useDHCP = true;

  # Time sync from web APIs (fallback for when NTP/timesyncd are insufficient)
  # Uses worldtimeapi.org (UTC) with worldclockapi.com as backup
  # Runs on resume from sleep and periodically every 30 minutes
  systemd.services.sync-time = {
    description = "Sync system clock from web time API";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${syncTimeScript}";
    };
  };

  # Trigger time sync after resume from sleep/hibernate
  systemd.services.sync-time-resume = {
    description = "Sync time after resume from sleep";
    after = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" ];
    wantedBy = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.systemd}/bin/systemctl start sync-time.service";
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
