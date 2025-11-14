{ config, lib, pkgs, ... }:

with lib;

let
  prl-tools = config.hardware.parallels.package;
  aarch64 = pkgs.stdenv.hostPlatform.system == "aarch64-linux";

in {

  options = {
    hardware.parallels = {

      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          This enables Parallels Tools for Linux guests, along with provided
          video, mouse and other hardware drivers.
        '';
      };

      autoMountShares = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Control prlfsmountd service. When this service is running, shares can not be manually
          mounted through `mount -t prl_fs ...` as this service will remount and trample any set options.
          Recommended to enable for simple file sharing, but extended share use such as for code should
          disable this to manually mount shares.
        '';
      };

      package = mkOption {
        type = types.nullOr types.package;
        default = config.boot.kernelPackages.prl-tools;
        defaultText = "config.boot.kernelPackages.prl-tools";
        example = literalExpression "config.boot.kernelPackages.prl-tools";
        description = ''
          Defines which package to use for prl-tools. Override to change the version.
        '';
      };
    };

  };

  config = mkIf config.hardware.parallels.enable {
    services.udev.packages = [ prl-tools ];

    # Create a wrapper for mount.fuse that sets PATH
    environment.systemPackages = [
      prl-tools
      (pkgs.runCommand "mount-fuse-wrapper" {} ''
        mkdir -p $out/bin
        cat > $out/bin/mount.fuse << 'EOF'
#!/bin/sh
export PATH="/usr/bin:/run/current-system/sw/bin:''${PATH:-}"
exec ${pkgs.fuse}/bin/mount.fuse "$@"
EOF
        chmod +x $out/bin/mount.fuse
      '')
    ];

    # For driverless Parallels Tools 26.1+, extraModulePackages may be empty
    boot.extraModulePackages = [ prl-tools ];

    # Load kernel modules:
    # - virtio-vsock modules for driverless Parallels Tools 26.1+
    # - prl_notifier only on aarch64 (for older versions if modules exist)
    boot.kernelModules = [
      "vhost_vsock"
      "vmw_vsock_virtio_transport_common"
    ] ++ optional aarch64 "prl_notifier";

    services.timesyncd.enable = false;

    # Create /usr/bin/prlfsmountd symlink for prltoolsd to find it
    # Create /usr/bin/prl_fsd symlink for mount.fuse to find it (mount.fuse executes 'bash -c prl_fsd')
    # Create /sbin/mount.fuse.prl_fsd for mount command to find the FUSE helper
    # prltoolsd has hardcoded paths to /usr/bin/prlfsmountd
    systemd.tmpfiles.rules = [
      "L+ /usr/bin/prlfsmountd - - - - ${prl-tools}/sbin/prlfsmountd"
      "L+ /usr/bin/prl_fsd - - - - ${prl-tools}/bin/prl_fsd"
      "d /sbin 0755 root root -"
      "L+ /sbin/mount.fuse.prl_fsd - - - - ${prl-tools}/bin/prl_fsd"
    ];

    systemd.services.prltoolsd = {
      description = "Parallels Tools' service";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${prl-tools}/bin/prltoolsd -f";
        PIDFile = "/var/run/prltoolsd.pid";
        Environment = "PATH=${prl-tools}/bin:${prl-tools}/sbin:/run/current-system/sw/bin";
      };
    };

    systemd.services.prlfsmountd =
      mkIf config.hardware.parallels.autoMountShares {
        description = "Parallels Shared Folders Daemon";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          ExecStart = "${prl-tools}/sbin/prlfsmountd -f";
          ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p /media";
          ExecStopPost = "${prl-tools}/sbin/prlfsmountd -u";
          PIDFile = "/run/prlfsmountd.pid";
        };
      };

    systemd.services.prlshprint = {
      description = "Parallels Shared Printer Tool";
      wantedBy = [ "multi-user.target" ];
      bindsTo = [ "cups.service" ];
      serviceConfig = {
        Type = "forking";
        ExecStart = "${prl-tools}/bin/prlshprint";
      };
    };

    systemd.user.services = {
      prlcc = {
        description = "Parallels Control Center";
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = { ExecStart = "${prl-tools}/bin/prlcc"; };
      };
      prldnd = {
        description = "Parallels Control Center";
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = { ExecStart = "${prl-tools}/bin/prldnd"; };
      };
      prlcp = {
        description = "Parallels CopyPaste Tool";
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
          ExecStart = "${prl-tools}/bin/prlcp";
          Restart = "always";
        };
      };
      # prlsga = {
      #   description = "Parallels Shared Guest Applications Tool";
      #   wantedBy = [ "graphical-session.target" ];
      #   serviceConfig = { ExecStart = "${prl-tools}/bin/prlsga"; };
      # };
      prlshprof = {
        description = "Parallels Shared Profile Tool";
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = { ExecStart = "${prl-tools}/bin/prlshprof"; };
      };
    };

  };
}
