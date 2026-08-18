{ config, lib, pkgs, ... }:

let
  # Darwin evaluates against home-manager master (nix-darwin follows
  # nixpkgs-unstable), NixOS against release-25.05. Master folded git's
  # userName/aliases/extraConfig into a single `settings` attrset and split
  # delta and difftastic into their own modules, so the option names below have
  # to be chosen per version. The values themselves are shared.
  hmNew = lib.versionAtLeast config.home.version.release "26.05";

  gitUserName = "Jonathan Rothberg";

  gitAliases = {
    bump =
      "!git checkout $1; git pull origin $1; git rebase \${2:-'main'}; git push origin; git checkout \${2:-'main'}";
  };

  gitIni = {
    pull.rebase = false;
    init.defaultBranch = "main";
    color.ui = true;
    core = {
      askPass = ""; # needs to be empty to use terminal for ask pass
      fsmonitor = true; # enables built-in fsmonitor daemon
      untrackedCache = true; # speeds up scanning untracked files
    };

    credential.helper = "cache --timeout 36000";
    push.default = "current";
  };

  deltaOptions = {
    syntax-theme = "1337";
    plus-color = "#32473d";
    minus-color = "#643632";
    features = "line-numbers";
    whitespace-error-style = "22 reverse";
  };
in {
  # delta and difftastic are separate modules only on master, and mkIf is not
  # enough to hide them from release-25.05 -- the option has to be absent
  # entirely -- so they are merged in with optionalAttrs below.
  programs = {
    git = {
      enable = true;
    } // (if hmNew then {
      settings = gitIni // {
        user.name = gitUserName;
        alias = gitAliases;
      };
    } else {
      userName = gitUserName;
      aliases = gitAliases;
      extraConfig = gitIni;
      difftastic.enable = true;
      delta = {
        enable = false;
        options = deltaOptions;
      };
    });

    ssh = {
      enable = true;

      extraConfig = ''
      HostkeyAlgorithms +ssh-rsa

      # Prevent SSH from trying all available keys for every connection
      IdentitiesOnly yes

      # Increase timeout for SSH agent to reduce password prompts
      PasswordAuthentication no
      PubkeyAuthentication yes

      # SSH over AWS Systems Manager Session Manager
      Host i-* mi-*
        ProxyCommand sh -c "aws ssm start-session --target %h --document-name AWS-StartSSHSession --parameters 'portNumber=%p'"

      # Wildcard pattern for hosts that may use Jenkins buildfarm key
      Host *.jenkins.* *.buildfarm.* jenkins-* buildfarm-*
        IdentitiesOnly no
        AddKeysToAgent yes
        PasswordAuthentication no
        PubkeyAuthentication yes
    '';

  } // (if hmNew then {
    # Master replaced the scalar options and matchBlocks with a freeform
    # settings DAG keyed by upstream ssh_config directive names, and made the
    # implicit "*" defaults opt-in. These reproduce the previous output.
    enableDefaultConfig = false;

    settings = {
      "*" = {
        ForwardAgent = true;
        AddKeysToAgent = "yes";
        Compression = false;
        ServerAliveInterval = 60;
        ServerAliveCountMax = 3;
        HashKnownHosts = true;
        UserKnownHostsFile = "~/.ssh/known_hosts";
        ControlMaster = "auto";
        ControlPath = "/tmp/ssh-%u-%r@%h:%p";
        ControlPersist = "1800";
      };

      github = {
        HostName = "github.com";
        IdentityFile = "~/.ssh/id_ed25519";
        ForwardAgent = true;
        User = "jonnywalker81";
      };

      bluebeam = {
        HostName = "scm.bluebeam.com";
        Port = 7999;
        IdentityFile = "~/.ssh/id_ed25519";
        ForwardAgent = true;
        User = "git";
        PubkeyAcceptedAlgorithms = "+ssh-rsa";
        HostkeyAlgorithms = "+ssh-rsa";
      };
    };
  } else {
    controlMaster = "auto";
    controlPath = "/tmp/ssh-%u-%r@%h:%p";
    controlPersist = "1800";

    forwardAgent = true;
    serverAliveInterval = 60;
    addKeysToAgent = "yes";

    hashKnownHosts = true;
    userKnownHostsFile = "~/.ssh/known_hosts";

    matchBlocks = {
      github = {
        hostname = "github.com";
        identityFile = "~/.ssh/id_ed25519";
        forwardAgent = true;
        user = "jonnywalker81";
      };

      bluebeam = {
        hostname = "scm.bluebeam.com";
        port = 7999;
        identityFile = "~/.ssh/id_ed25519";
        forwardAgent = true;
        user = "git";
        extraOptions = {
          PubkeyAcceptedAlgorithms = "+ssh-rsa";
          HostkeyAlgorithms = "+ssh-rsa";
        };
      };
    };
  });
  } // lib.optionalAttrs hmNew {
    delta = {
      enable = false;
      options = deltaOptions;
    };

    # git.enable must now be set explicitly, and the flags that release-25.05
    # supplied as option defaults have to be spelled out.
    difftastic = {
      enable = true;
      git.enable = true;
      options = {
        color = "auto";
        background = "light";
        display = "side-by-side";
      };
    };
  };
}
