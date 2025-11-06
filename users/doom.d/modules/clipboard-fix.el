;;; clipboard-fix.el --- Fix clipboard integration for Wayland -*- lexical-binding: t; -*-

;; This file fixes clipboard integration between Emacs and other applications
;; when running under Wayland (e.g., with Hyprland)

(defun my/use-wayland-clipboard ()
  "Configure Emacs to use wl-clipboard for Wayland."
  (when (and (eq system-type 'linux)
             (getenv "WAYLAND_DISPLAY")
             (executable-find "wl-copy")
             (executable-find "wl-paste"))
    
    ;; Create functions for clipboard interaction
    (defvar wl-copy-process nil)
    
    (defun my/wl-copy (text)
      "Copy TEXT to Wayland clipboard using wl-copy."
      (let ((process (make-process :name "wl-copy"
                                   :buffer nil
                                   :command '("wl-copy" "-f" "-n")
                                   :connection-type 'pipe)))
        (process-send-string process text)
        (process-send-eof process)))
    
    (defun my/wl-paste ()
      "Paste from Wayland clipboard using wl-paste."
      (let ((clipboard (shell-command-to-string "wl-paste -n 2>/dev/null")))
        (if (string= clipboard "")
            nil
          clipboard)))
    
    ;; Set the interprogram functions
    (setq interprogram-cut-function #'my/wl-copy)
    (setq interprogram-paste-function #'my/wl-paste)
    
    ;; Enable clipboard
    (setq select-enable-clipboard t)
    (setq select-enable-primary nil) ; Don't use PRIMARY selection
    
    ;; For Doom Emacs evil mode
    (after! evil
      (setq evil-visual-update-x-selection-p nil))
    
    (message "Wayland clipboard integration enabled")))

;; Apply the fix
(my/use-wayland-clipboard)

;; Also fix for terminal Emacs
(unless (display-graphic-p)
  (when (and (eq system-type 'linux)
             (executable-find "wl-copy")
             (executable-find "wl-paste"))
    (defun my/terminal-copy-region (beg end)
      "Copy region to system clipboard in terminal."
      (interactive "r")
      (shell-command-on-region beg end "wl-copy"))
    
    (defun my/terminal-paste ()
      "Paste from system clipboard in terminal."
      (interactive)
      (insert (shell-command-to-string "wl-paste -n")))
    
    ;; Bind keys for terminal
    (global-set-key (kbd "C-c C-w") #'my/terminal-copy-region)
    (global-set-key (kbd "C-c C-y") #'my/terminal-paste)))

(provide 'clipboard-fix)
;;; clipboard-fix.el ends here