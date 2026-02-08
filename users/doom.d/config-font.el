;;; config-font.el --- Font configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; FONT CONFIGURATION
;; ============================================================================
;; Easy switching between programming fonts

;; ----------------------------------------------------------------------------
;; Available Fonts
;; ----------------------------------------------------------------------------
;; Add new fonts here as (name . (family-variants...))
;; Variants handle different naming conventions across systems

(defvar jr/available-fonts
  '(("Cascadia Code" . ("Cascadia Code" "CascadiaCode"))
    ("JetBrains Mono" . ("JetBrains Mono" "JetBrainsMono"))
    ("Fira Code" . ("Fira Code" "FiraCode"))
    ("Victor Mono" . ("Victor Mono" "VictorMono"))
    ("Iosevka" . ("Iosevka" "Iosevka Nerd Font Mono"))
    ("Input Mono" . ("Input Mono" "InputMono")))
  "Alist of available fonts with their possible family name variants.")

;; ----------------------------------------------------------------------------
;; Current Font Setting
;; ----------------------------------------------------------------------------

(defvar jr/current-font "Cascadia Code"
  "The currently selected font name (key from `jr/available-fonts').")

(defvar jr/font-size 16
  "Default font size.")

(defvar jr/font-size-big 24
  "Big font size for presentations/zoom.")

;; ----------------------------------------------------------------------------
;; Font Helper Functions
;; ----------------------------------------------------------------------------

(defun jr/find-available-font-family (font-name)
  "Find the first available font family variant for FONT-NAME."
  (let ((variants (cdr (assoc font-name jr/available-fonts))))
    (cl-find-if (lambda (variant)
                  (member variant (font-family-list)))
                variants)))

(defun jr/set-font (font-name &optional size)
  "Set the doom font to FONT-NAME with optional SIZE."
  (interactive
   (list (completing-read "Select font: "
                          (mapcar #'car jr/available-fonts)
                          nil t nil nil jr/current-font)))
  (let* ((size (or size jr/font-size))
         (family (jr/find-available-font-family font-name)))
    (if family
        (progn
          (setq jr/current-font font-name)
          (setq doom-font (font-spec :family family :size size)
                doom-big-font (font-spec :family family :size jr/font-size-big))
          ;; Apply immediately if in a graphical frame
          (when (display-graphic-p)
            (set-frame-font doom-font nil t))
          (message "Font set to %s (%s) at size %d" font-name family size))
      (message "Font %s not available on this system" font-name))))

(defun jr/set-font-size (size)
  "Set the font SIZE while keeping the current font family."
  (interactive "nFont size: ")
  (setq jr/font-size size)
  (jr/set-font jr/current-font size))

(defun jr/increase-font-size ()
  "Increase font size by 1."
  (interactive)
  (jr/set-font-size (1+ jr/font-size)))

(defun jr/decrease-font-size ()
  "Decrease font size by 1."
  (interactive)
  (jr/set-font-size (1- jr/font-size)))

(defun jr/cycle-font ()
  "Cycle through available fonts."
  (interactive)
  (let* ((font-names (mapcar #'car jr/available-fonts))
         (current-pos (cl-position jr/current-font font-names :test #'string=))
         (next-pos (mod (1+ (or current-pos -1)) (length font-names)))
         (next-font (nth next-pos font-names)))
    (jr/set-font next-font)))

;; ----------------------------------------------------------------------------
;; Initialize Font
;; ----------------------------------------------------------------------------

;; Set the default font on startup
(jr/set-font jr/current-font jr/font-size)

;; ----------------------------------------------------------------------------
;; Keybindings
;; ----------------------------------------------------------------------------

(map! :leader
      (:prefix ("F" . "font")
       :desc "Select font"      "f" #'jr/set-font
       :desc "Set font size"    "s" #'jr/set-font-size
       :desc "Increase size"    "+" #'jr/increase-font-size
       :desc "Decrease size"    "-" #'jr/decrease-font-size
       :desc "Cycle font"       "c" #'jr/cycle-font))

(provide 'config-font)
;;; config-font.el ends here
