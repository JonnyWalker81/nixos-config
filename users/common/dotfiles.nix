{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.file.".doom.d" = {
    source = ../doom.d;
    recursive = true;
  };

  # Symlink .emacs.d to chemacs2 profile switcher
  home.file.".emacs.d" = {
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.config/chemacs";
  };

  home.file.".elisp" = {
    source = ../elisp;
    recursive = true;
  };

  home.file.".config/zls.json" = {
    source = ../zls.json;
  };

  home.file.".config/rainfrog/rainfrog_config.toml" = {
    source = ../rainfrog/rainfrog_config.toml;
  };

  home.file."scripts" = {
    source = ../scripts;
    recursive = true;
  };

  home.file.".psqlrc".text = ''
     --  set PROMPT1 '[%m] '
     -- set PROMPT2 '[%m:trx] '
    -- select split_part(:'HOST','.',1) = 'komodo' as is_prod gset
    -- if :is_prod
    --    SET SESSION CHARACTERISTICS AS TRANSACTION READ ONLY;
    --    echo '**************************************************************************************************'
    --    echo 'Connected to the PRODUCTION DB. UPDATE WITH CAUTION.'
    --    echo 'Session READ ONLY by default. Change with: SET SESSION CHARACTERISTICS AS TRANSACTION READ WRITE;'
    --    echo '**************************************************************************************************'
    -- endif
    -- 	iming
  '';

  home.file.".config/gitui/key_bindings.ron" = {
    source = ../gitui/key_bindings.ron;
  };

  home.file.".config/dune/config" = {
    source = ../dune/config;
    recursive = true;
  };

  home.file.".config/nyxt/config.lisp" = {
    source = ../nyxt;
    recursive = true;
  };
}
