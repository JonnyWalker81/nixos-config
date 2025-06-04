;;; init.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!

(setq comp-speed 2)

(doom! :feature

       :completion
       (corfu
        +icons
        +orderless
        +dabbrev
        )
       ;; (company          ; the ultimate code completion backend
       ;;  +auto
       ;;  +childframe
       ;;  )           ; as-you-type code completion
       ;; (helm             ; the *other* search engine for love and life
       ;; +fuzzy)          ; enable fuzzy search backend for helm
                                        ;ido               ; the other *other* search engine...
       (vertico +icons)           ; the search engine of the future
       ;; (ivy              ; a search engine for love and life
       ;;  +fuzzy
       ;;  ;; +childframe
       ;;  +prescient
       ;;  +icons
       ;;  )          ; enable fuzzy search backend for ivy

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       modeline     ; a snazzy Atom-inspired mode-line
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;; evil-goggles      ; display visual hints when editing in evil
       ophints           ; highlight the region an operation acts on
       ;; fci               ; a `fill-column' indicator
       ;; fill-column
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       modeline
       minimap
       nav-flash         ; blink the current line after jumping
       neotree           ; a project drawer, like NERDTree for vim
       treemacs          ; a project drawer, like neotree but cooler
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;; pretty-code       ; replace bits of code with pretty symbols
       ligatures
                                        ;tabbar            ; FIXME an (incomplete) tab bar for Emacs
                                        ;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
                                        ;parinfer          ; turn lisp into python, sort of
       multiple-cursors  ; editing in many places at once
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       (format +onsave)

       :emacs
       undo
       (dired
        +icons
        ;; +ranger
                                        ; +dirvish
        )            ; making dired pretty [functional]
       ;; ediff             ; comparing files in Emacs
       electric          ; smarter, keyword-based electric-indent
                                        ;  imenu             ; an imenu sidebar and searchable code index
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; the elisp shell that works everywhere
       shell             ; simple shell REPL for Emacs
       term              ; basic terminal emulator for Emacs
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax
       (spell
        +enchant
        +hunspell)

       :tools
       eval              ; run code, run (also, repls)
                                        ;debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
                                        ;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)
                                        ;gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +devdocs         ; ...on devdocs.io online
        +dictionary
        +offline
        +docsets)        ; ...or in Dash docsets locally
       (lsp
        ;; +eglot
        ;;  +peek
        )

       jr
       make              ; run make tasks from Emacs
       (magit
         +forge
        )          ;
                                        ;password-store    ; password manager for nerds
       pdf               ; pdf enhancements
       ;; spellcheck        ; tasing you for misspelling mispelling
       ;; (syntax-checker   ; tasing you for every semicolon you forget
       ;;  +childframe)     ; use childframes for error popups (Emacs 26+ only)
       ;; flycheck        ; tasing you for misspelling mispelling
       ;; flyspell
       ;; (syntax-checker   ; tasing you for every semicolon you forget
       ;;   +childframe)     ; use childframes for error popups (Emacs 26+ only)
                                        ;prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       (terraform
        ;; +lsp
        )
       ;; tree-sitter
                                        ;tmux              ; an API for interacting with tmux
                                        ;upload            ; map local to remote projects via ssh/ftp
                                        ;wakatime

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
                                        ;assembly          ; assembly for fun or debugging
                                        ;(cc +irony +rtags); C/C++/Obj-C madness
                                        ;common-lisp       ; if you've seen one lisp, you've seen them all
                                        ;crystal           ; ruby at the speed of c
                                        ;clojure           ; java with a lisp
       ;; csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
                                        ;erlang            ; an elegant language for a more civilized age
                                        ;elixir            ; erlang done right
       ;; elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
                                        ;ess               ; emacs speaks statistics
       (go
        +lsp
        ;; +tree-sitter
        )
       ;; go
                                        ; the hipster dialect
       graphql
       (haskell
        ;; +lsp
        )  ; a language that's lazier than I am
                                        ;hy                ; readability of scheme w/ speed of python
                                        ;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;; (typescript
       ;;  +lsp
       ;;  ;; +tree-sitter
       ;;  )
                                        ; JS, but good
       (javascript
        +lsp
        ;; +tree-sitter
        )        ; all(hope(abandon(ye(who(enter(here))))))
       (json
        ;; +lsp
        ;; +tree-sitter
        )
                                        ;julia             ; a better, faster MATLAB
                                        ;latex             ; writing papers in Emacs has never been so fun
                                        ;ledger            ; an accounting system in Emacs
       (lua
        ;; +lsp
        )                                        ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
                                        ;nim               ; python + lisp at the speed of c
       (nix
        ;; +lsp
        )               ; I hereby declare "nix geht mehr!"
       (ocaml
        ;; +lsp
        ;; +tree-sitter
        )            ; an objective camel
       (org              ; organize your plain life in plain text
        +pretty
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +present        ; Emacs for presentations
        )
                                        ;perl              ; write code no one else can comprehend
                                        ;php               ; perl's insecure younger brother
       plantuml          ; diagrams for confusing people more
                                        ;purescript        ; javascript, but functional
                                        ;python            ; beautiful is better than ugly
                                        ;qt                ; the 'cutest' gui framework ever
       ;; rest              ; Emacs as a REST client
                                        ;ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust
        ;; +lsp
        )               ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;; (rust
       ;;  +lsp)
                                        ;scala             ; java, but good
       (sh
        ;; +lsp
        )               ; she sells (ba|z)sh shells on the C xor
                                        ;solidity          ; do you need a blockchain? No.
       ;; swift             ; who asked for emoji variables?
       (yaml
        ;; +lsp
        )
       (web
        +lsp
        )               ; the tubes
       zig
       (sql +postgres)

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       ;; (email +gmail)    ; emacs as an email client
                                        ;irc               ; how neckbeards socialize
       (rss +org)        ; emacs as an RSS reader
                                        ;twitter           ; twitter client https://twitter.com/vnought
                                        ;(write            ; emacs as a word processor (latex + org + markdown)
                                        ; +wordnut         ; wordnet (wn) search
                                        ; +langtool)       ; a proofreader (grammar/style check) for Emacs

       :email
       mu4e
       :collab
                                        ;floobits          ; peer programming for a price
                                        ;impatient-mode    ; show off code over HTTP

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
                                        ;literate
       ;; :os
       ;;   (:if IS-MAC macos)  ; improve compatibility with macOS

       ;; The default module set reasonable defaults for Emacs. It also provides
       ;; a Spacemacs-inspired keybinding scheme, a custom yasnippet library,
       ;; and additional ex commands for evil-mode. Use it as a reference for
       ;; your own modules.
       ;; (default +bindings +snippets +evil-commands))
       (default +bindings +evil-commands +smartparens))

;; (remove-hook 'org-load-hook #'(
;;                                +org|setup-ui
;;                                ))
