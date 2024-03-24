{
  pkgs,
  ...
}:
{
  services.greetd = {
    enable = true;
    settings = {
    #  default_session.command = ''
    #   ${pkgs.greetd.tuigreet}/bin/tuigreet \
    #     --time \
    #     --asterisks \
    #     --user-menu \
    #     --cmd sway
    # '';
    default_session.command = ''
    Hyprland
    '';
    };
  };

  # environment.etc."greetd/environments".text = ''
  #   sway
  # '';
}
