;;; config-uuid.el --- UUID generation utilities -*- lexical-binding: t; -*-

;; ============================================================================
;; UUID GENERATION
;; ============================================================================
;; Generate, insert, copy, and replace UUIDs (v4, v7, and readable variants)

;; ----------------------------------------------------------------------------
;; Basic UUID Generation
;; ----------------------------------------------------------------------------

(defun jr/generate-uuidv4 ()
  "Generate a random UUIDv4 using uuidgen."
  (string-trim (shell-command-to-string "uuidgen -r")))

(defun jr/generate-uuidv7 ()
  "Generate a time-ordered UUIDv7 using uuidgen."
  (string-trim (shell-command-to-string "uuidgen -7")))

(defun jr/insert-uuidv4 ()
  "Insert a UUIDv4 at point."
  (interactive)
  (insert (jr/generate-uuidv4)))

(defun jr/insert-uuidv7 ()
  "Insert a UUIDv7 at point."
  (interactive)
  (insert (jr/generate-uuidv7)))

(defun jr/copy-uuidv4 ()
  "Generate a UUIDv4 and copy it to the kill ring."
  (interactive)
  (let ((uuid (jr/generate-uuidv4)))
    (kill-new uuid)
    (message "UUIDv4 copied: %s" uuid)))

(defun jr/copy-uuidv7 ()
  "Generate a UUIDv7 and copy it to the kill ring."
  (interactive)
  (let ((uuid (jr/generate-uuidv7)))
    (kill-new uuid)
    (message "UUIDv7 copied: %s" uuid)))

;; ----------------------------------------------------------------------------
;; UUID Replacement
;; ----------------------------------------------------------------------------

(defun jr/replace-uuid-at-point-with-v4 ()
  "Replace the UUID at point or in region with a new UUIDv4."
  (interactive)
  (let* ((uuid-regex "[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}")
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (save-excursion
                     (let ((start (progn
                                    (re-search-backward "[^0-9a-f-]" nil t)
                                    (forward-char)
                                    (point)))
                           (end (progn
                                  (re-search-forward "[^0-9a-f-]" nil t)
                                  (backward-char)
                                  (point))))
                       (cons start end))))))
    (when bounds
      (let ((text (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (if (string-match-p uuid-regex text)
            (progn
              (delete-region (car bounds) (cdr bounds))
              (goto-char (car bounds))
              (insert (jr/generate-uuidv4))
              (message "Replaced UUID with UUIDv4"))
          (message "No UUID found at point"))))))

(defun jr/replace-uuid-at-point-with-v7 ()
  "Replace the UUID at point or in region with a new UUIDv7."
  (interactive)
  (let* ((uuid-regex "[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}")
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (save-excursion
                     (let ((start (progn
                                    (re-search-backward "[^0-9a-f-]" nil t)
                                    (forward-char)
                                    (point)))
                           (end (progn
                                  (re-search-forward "[^0-9a-f-]" nil t)
                                  (backward-char)
                                  (point))))
                       (cons start end))))))
    (when bounds
      (let ((text (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (if (string-match-p uuid-regex text)
            (progn
              (delete-region (car bounds) (cdr bounds))
              (goto-char (car bounds))
              (insert (jr/generate-uuidv7))
              (message "Replaced UUID with UUIDv7"))
          (message "No UUID found at point"))))))

;; ----------------------------------------------------------------------------
;; Readable UUID Generation (with pronounceable first segment)
;; ----------------------------------------------------------------------------

(defvar jr/readable-words
  '("facade" "decade" "beaded" "accede" "deface" "backed" "baffed" "beefed"
    "cabbed" "cabled" "cached" "caddie" "cafed0" "caged0" "calced" "called"
    "dabbed" "danced" "decaf0" "decode" "deeded" "defeat" "defied" "ebbed0"
    "edged0" "faced0" "faded0" "failed" "falled" "fedde0" "feed00" "abacab"
    "abase0" "abased" "abated" "abbace" "accede" "access" "acedia" "acned0"
    "badass" "baffle" "baffed" "balled" "beaded" "beadle" "beefed" "cabala"
    "cabale" "cabled" "cadded" "caecal" "ceased" "dabble" "dabbed" "decade"
    "decaff" "deface" "defade" "ebbede" "efface" "fabled" "facade" "facede"
    "fadded" "bedbad" "bedded" "beebee" "beefed" "cabbed" "caffee" "canned"
    "deaded" "deaden" "deadbe" "decade" "decaff" "decede" "accede" "aceded"
    "addeda" "baccab" "baffed" "beefed" "caffed" "daffed" "efface" "faccee"
    "abcdef" "acebad" "accede" "badeaf" "baffed" "bedead" "caffed" "decade"
    "defbad" "efface" "fabade" "fadcab" "acefac" "badfad" "bedbad" "cabfed"
    "dabfad" "deface" "efface" "fabbed" "badfac" "bedfac" "caffac" "dadfac"
    "deafac" "efffac" "f00d00" "facade" "baddad" "beef00" "cafe00" "dead00"
    "deaf00" "decaf0" "decade" "feed00" "babe00" "dada00" "fade00" "face00"
    "abad00" "acace0" "added0" "baaed0" "cabba0" "dabba0" "fabba0" "ebb000"
    "acc000" "add000" "aff000" "baa000" "bad000" "bee000" "cab000" "dab000"
    "dad000" "ebb000" "fad000" "fee000" "abba00" "acdc00" "bada00" "bead00"
    "cafe00" "dada00" "dead00" "deaf00" "fade00" "face00" "feed00" "deed00"
    "aaa000" "bbb000" "ccc000" "ddd000" "eee000" "fff000" "aba000" "aca000"
    "ada000" "afa000" "aea000" "bab000" "bac000" "bad000" "cac000" "cad000"
    "dab000" "dac000" "dad000" "fab000" "fac000" "fad000" "ebb000" "ecc000"
    "abcabc" "defdef" "fedcba" "bcdefa" "cdefab" "acabab" "fadede" "bebebe"
    "cecece" "dedede" "efefef" "afafaf" "bababa" "cacaca" "dadada" "fabfab"
    "deface" "beface" "caface" "daface" "efface" "baface" "acface" "adface")
  "List of readable hex-valid words for UUID prefixes (all exactly 6 chars).")

(defun jr/get-random-readable-word ()
  "Get a random readable word from the list."
  (nth (random (length jr/readable-words)) jr/readable-words))

(defun jr/generate-readable-uuidv4 ()
  "Generate a UUIDv4 with a readable first segment using uuidgen."
  (let* ((readable-word (jr/get-random-readable-word))
         (base-uuid (string-trim (shell-command-to-string "uuidgen -r"))))
    ;; Replace first 6 chars with readable word
    (concat readable-word (substring base-uuid 6))))

(defun jr/generate-readable-uuidv7 ()
  "Generate a UUIDv7 with a readable first segment using uuidgen."
  (let* ((readable-word (jr/get-random-readable-word))
         (base-uuid (string-trim (shell-command-to-string "uuidgen -7"))))
    ;; Replace first 6 chars with readable word
    (concat readable-word (substring base-uuid 6))))

(defun jr/insert-readable-uuidv4 ()
  "Insert a readable UUIDv4 at point."
  (interactive)
  (insert (jr/generate-readable-uuidv4)))

(defun jr/insert-readable-uuidv7 ()
  "Insert a readable UUIDv7 at point."
  (interactive)
  (insert (jr/generate-readable-uuidv7)))

(defun jr/copy-readable-uuidv4 ()
  "Generate a readable UUIDv4 and copy it to the kill ring."
  (interactive)
  (let ((uuid (jr/generate-readable-uuidv4)))
    (kill-new uuid)
    (message "Readable UUIDv4 copied: %s" uuid)))

(defun jr/copy-readable-uuidv7 ()
  "Generate a readable UUIDv7 and copy it to the kill ring."
  (interactive)
  (let ((uuid (jr/generate-readable-uuidv7)))
    (kill-new uuid)
    (message "Readable UUIDv7 copied: %s" uuid)))

(defun jr/replace-uuid-at-point-with-readable-v4 ()
  "Replace the UUID at point or in region with a new readable UUIDv4."
  (interactive)
  (let* ((uuid-regex "[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}")
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (save-excursion
                     (let ((start (progn
                                    (re-search-backward "[^0-9a-f-]" nil t)
                                    (forward-char)
                                    (point)))
                           (end (progn
                                  (re-search-forward "[^0-9a-f-]" nil t)
                                  (backward-char)
                                  (point))))
                       (cons start end))))))
    (when bounds
      (let ((text (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (if (string-match-p uuid-regex text)
            (progn
              (delete-region (car bounds) (cdr bounds))
              (goto-char (car bounds))
              (insert (jr/generate-readable-uuidv4))
              (message "Replaced UUID with readable UUIDv4"))
          (message "No UUID found at point"))))))

(defun jr/replace-uuid-at-point-with-readable-v7 ()
  "Replace the UUID at point or in region with a new readable UUIDv7."
  (interactive)
  (let* ((uuid-regex "[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}")
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (save-excursion
                     (let ((start (progn
                                    (re-search-backward "[^0-9a-f-]" nil t)
                                    (forward-char)
                                    (point)))
                           (end (progn
                                  (re-search-forward "[^0-9a-f-]" nil t)
                                  (backward-char)
                                  (point))))
                       (cons start end))))))
    (when bounds
      (let ((text (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (if (string-match-p uuid-regex text)
            (progn
              (delete-region (car bounds) (cdr bounds))
              (goto-char (car bounds))
              (insert (jr/generate-readable-uuidv7))
              (message "Replaced UUID with readable UUIDv7"))
          (message "No UUID found at point"))))))

;; ----------------------------------------------------------------------------
;; Keybindings
;; ----------------------------------------------------------------------------

;; Using SPC o u (open → UUID) to avoid conflict with SPC i u (insert unicode)
(map! :leader
      (:prefix ("o" . "open")
               (:prefix ("u" . "UUID")
                :desc "Insert UUIDv4" "4" #'jr/insert-uuidv4
                :desc "Insert UUIDv7" "7" #'jr/insert-uuidv7
                (:prefix ("R" . "readable")
                 :desc "Insert readable UUIDv4" "4" #'jr/insert-readable-uuidv4
                 :desc "Insert readable UUIDv7" "7" #'jr/insert-readable-uuidv7)
                (:prefix ("y" . "copy/yank")
                 :desc "Copy UUIDv4" "4" #'jr/copy-uuidv4
                 :desc "Copy UUIDv7" "7" #'jr/copy-uuidv7
                 (:prefix ("R" . "readable")
                  :desc "Copy readable UUIDv4" "4" #'jr/copy-readable-uuidv4
                  :desc "Copy readable UUIDv7" "7" #'jr/copy-readable-uuidv7))
                (:prefix ("r" . "replace")
                 :desc "Replace with UUIDv4" "4" #'jr/replace-uuid-at-point-with-v4
                 :desc "Replace with UUIDv7" "7" #'jr/replace-uuid-at-point-with-v7
                 (:prefix ("R" . "readable")
                  :desc "Replace with readable UUIDv4" "4" #'jr/replace-uuid-at-point-with-readable-v4
                  :desc "Replace with readable UUIDv7" "7" #'jr/replace-uuid-at-point-with-readable-v7)))))

(provide 'config-uuid)
;;; config-uuid.el ends here
