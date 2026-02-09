{ config, lib, pkgs, ... }:

{
  programs.git = {
    enable = true;
    userName = "Jonathan Rothberg";
    extraConfig = {
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

    aliases = {
      bump =
        "!git checkout $1; git pull origin $1; git rebase \${2:-'main'}; git push origin; git checkout \${2:-'main'}";
    };

    difftastic = { enable = true; };

    delta = {
      enable = false;
      options = {
        syntax-theme = "1337";
        plus-color = "#32473d";
        minus-color = "#643632";
        features = "line-numbers";
        whitespace-error-style = "22 reverse";
      };
    };
  };

  programs.ssh = {
    enable = true;

    controlMaster = "auto";
    controlPath = "/tmp/ssh-%u-%r@%h:%p";
    controlPersist = "1800";

    forwardAgent = true;
    serverAliveInterval = 60;
    addKeysToAgent = "yes";

    hashKnownHosts = true;
    userKnownHostsFile = "~/.ssh/known_hosts";

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
  };
}
