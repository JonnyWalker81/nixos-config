;;; config-font.el --- description -*- lexical-binding: t; -*-

;; (set-frame-font "Fira Code")
;; (setq doom-big-font (font-spec :family "Input Mono" :size 19))
;; (setq doom-font (font-spec :family "Input Mono" :size 16))

;; (setq doom-big-font (font-spec :family "Iosevka Nerd Font Mono" :size 19))
;; (setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 16))

;; (setq doom-big-font (font-spec :family "Fantasque Sans Mono" :size 19))
;; (setq doom-font (font-spec :family "Fantasque Sans Mono" :size 16))
;;
;; (setq doom-big-font (font-spec :family "Cascadia Code" :size 19))
;; (setq doom-font (font-spec :family "Cascadia Code" :size 16))

(setq doom-big-font (font-spec :family "JetBrains Mono" :size 19))
(setq doom-font (font-spec :family "JetBrains Mono" :size 16))

;; (setq doom-big-font (font-spec :family "Victor Mono" :size 19))
;; (setq doom-font (font-spec :family "Victor Mono" :size 16))
;; (set-face-attribute 'default nil :family "Fira Code" :height 165 :weight 'medium)
;; (set-face-attribute 'default nil :family "Input Mono" :height 165 :weight 'medium)
;; (set-face-attribute 'default nil :family "Iosevka Nerd Font Mono" :height 200 :weight 'medium)

;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;                (36 . ".\\(?:>\\)")
;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;;                (48 . ".\\(?:x[a-zA-Z]\\)")
;;                (58 . ".\\(?:::\\|[:=]\\)")
;;                (59 . ".\\(?:;;\\|;\\)")
;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                (91 . ".\\(?:]\\)")
;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                (94 . ".\\(?:=\\)")
;;                (119 . ".\\(?:ww\\)")
;;                (123 . ".\\(?:-\\)")
;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                )
;;              ))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))


(provide 'config-font)
;; ;;; config-font.el ends here
