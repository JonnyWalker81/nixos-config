# Consolidated vim plugin fixes -- all vimPlugins overrides in one place
# to avoid clobbering from multiple overlays using `prev.vimPlugins // { ... }`
final: prev: {
  vimPlugins = prev.vimPlugins // {
    # Fix for CopilotChat.nvim requiring fzf-lua check
    CopilotChat-nvim = prev.vimPlugins.CopilotChat-nvim.overrideAttrs (old: {
      doCheck = false;
      nvimRequireCheck = "";
      buildPhase = ''
        runHook preBuild
        runHook postBuild
      '';
      installCheckPhase = ''
        runHook preInstallCheck
        runHook postInstallCheck
      '';
    });

    # Skip tests for neotest and its dependencies
    neotest = prev.vimPlugins.neotest.overrideAttrs (oldAttrs: {
      doCheck = false;
      checkPhase = "true";
      propagatedBuildInputs = (oldAttrs.propagatedBuildInputs or [ ])
        ++ (with prev.vimPlugins;
          [
            (nvim-nio.overrideAttrs (old: {
              doCheck = false;
              checkPhase = "true";
            }))
          ]);
    });

    # Also override nvim-nio directly
    nvim-nio = prev.vimPlugins.nvim-nio.overrideAttrs (oldAttrs: {
      doCheck = false;
      checkPhase = "true";
    });
  };

  # Fix fzf-lua tests
  luaPackages = prev.luaPackages // {
    fzf-lua = prev.luaPackages.fzf-lua.overrideAttrs (old: {
      doCheck = false;
      checkPhase = "echo skipping fzf-lua tests";
    });
  };
}
