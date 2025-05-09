# This contains various packages we want to overlay. Note that the
# other ".nix" files in this directory are automatically loaded.
final: prev: {
  # waypoint = final.callPackage ../pkgs/waypoint.nix { };
  terraform = final.callPackage ../pkgs/terraform-bin.nix { };
  # picom = prev.picom.overrideAttrs (old: {
  #   src = prev.fetchFromGitHub {
  #     owner = "jonaburg";
  #     repo = old.pname;
  #     rev = "e3c19cd7d1108d114552267f302548c113278d45";
  #     sha256 = "VBnIzisg/7Xetd/AWVHlnaWXlxX+wqeYTpstO6+T5cE=";
  #   };
  # });
}
