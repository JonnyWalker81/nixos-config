{ config, lib, pkgs, ... }:

{
  imports = [ ./vm-shared.nix ];

  virtualisation.vmware.guest.enable = true;

  # Shared folder to host works on Intel
  fileSystems."/host" = {
    fsType = "fuse./run/current-system/sw/bin/vmhgfs-fuse";
    device = ".host/";
    options = [
      "umask=22"
      "uid=1000"
      "gid=1000"
      "allow_other"
      "auto_unmount"
      "defaults"
    ];
  };
}
