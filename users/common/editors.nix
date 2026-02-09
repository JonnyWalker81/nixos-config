{ config, lib, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = if pkgs.stdenv.isDarwin then
      pkgs.emacs-unstable # Emacs 31.x with Cocoa GUI and native-comp support
    else
      pkgs.emacs;
    extraPackages = (epkgs: [ epkgs.vterm epkgs.jinx ]);
    extraConfig = ''
      ;; Set up SSH agent environment based on system type
      (cond
       ;; macOS: Find the launchd SSH agent socket dynamically
       ((eq system-type 'darwin)
        (let ((sock (shell-command-to-string
                     "ls /private/tmp/com.apple.launchd.*/Listeners 2>/dev/null | head -1 | tr -d '\n'")))
          (when (and sock (not (string-empty-p sock)) (file-exists-p sock))
            (setenv "SSH_AUTH_SOCK" sock))))
       ;; Linux: Use systemd user environment or fallback to hardcoded path
       ((eq system-type 'gnu/linux)
        (if (executable-find "systemctl")
            (let ((ssh-auth-sock (shell-command-to-string "systemctl --user show-environment | grep SSH_AUTH_SOCK | cut -d'=' -f2-")))
              (when (and ssh-auth-sock (not (string-empty-p (string-trim ssh-auth-sock))))
                (setenv "SSH_AUTH_SOCK" (string-trim ssh-auth-sock))))
          (setenv "SSH_AUTH_SOCK" "/run/user/1000/ssh-agent"))))

    '';
  };
}
