;;; init-coding.el --- Programming Languages and Compilation

;;; Commentary:

;;; Code:


;; Make our standard LSP client customizable
(defcustom lsp-client-start #'eglot-ensure
  "Command to launch the standard LSP client. By default this launches the
Emacs built-in client `Eglot' through `eglot-ensure', but a popular
alternative is `LSP Mode', using the function `lsp'."
  :type 'function
  :group 'tools)
(defun lsp-launcher ()
  "Hook function for launching LSP client via `lsp-client-start'."
  (funcall lsp-client-start))


;; Tree-sitter automatic mode and grammar management
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
)


;; Eldoc configuration
(require 'eldoc)
;; combine doc texts instead of showing only one; start displaying immediately
(setopt eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
;; make eldoc buffers display on the bottom as dedicated side windows
(add-to-list 'display-buffer-alist
             '("^\\*eldoc" display-buffer-in-side-window
               (dedicated . t)
               (side . bottom)
               (window-height . 5)))
;; if the dedicated eldoc buffer is being shown, don't also print in echo area
(setopt eldoc-echo-area-prefer-doc-buffer 't)  ; less noisy
;; allow resizing message area up to a certain limit
(setopt eldoc-echo-area-use-multiline-p 5)


;; Compilation

(setopt compile-command "make MAKEFLAGS= -k -s -w") ; keep going, don't echo commands, print directories
(setopt compilation-scroll-output 'first-error)
(global-set-key (kbd "C-c c") 'recompile)

(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))


;; Language Servers

;; Eglot (the default language server client)
(use-package eglot
  :config
  (setopt eglot-report-progress nil) ; don't be so noisy
  (add-hook 'eglot-managed-mode-hook
            (function
             (lambda ()
               ;; show all doc texts, and as soon as possible
               ;; (the hook is needed because eglot overrides the global
               ;; setting, changing it to eldoc-documentation-compose)
               (setopt eldoc-documentation-strategy
                       #'eldoc-documentation-compose-eagerly))))
  )


;; LSP mode (more bells and whistles than the built-in Eglot)
(defun lsp-completion-mode-orderless ()
  "Hook function for lsp-completion-mode."
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)) ;; make lsp use orderless for completion
  )
(use-package lsp-mode
  :init
  ;; customize prefix for LSP keybindings (must happen before loading lsp-mode)
  (setopt lsp-keymap-prefix "C-l")
  :config
  (setopt lsp-completion-provider :none)  ; tries to use Company otherwise
  ;; most of these are on by default, but we list them all for easy tweaking
  (setopt lsp-enable-symbol-highlighting t)
  (setopt lsp-symbol-highlighting-skip-current t) ; a bit less noisy
  (setopt lsp-eldoc-enable-hover t)  ; show eldoc in echo area on cursor hover
  (setopt lsp-eldoc-render-all nil)  ; single line only in echo area
  (setopt lsp-lens-enable t)
  (setopt lsp-headerline-breadcrumb-enable t)
  (setopt lsp-modeline-code-actions-enable t)
  (setopt lsp-signature-render-documentation t)  ; also show docs with sig
  (setopt lsp-signature-doc-lines 15)
  :hook
  (lsp-completion-mode . #'lsp-completion-mode-orderless)
  (lsp-mode . yas-minor-mode)  ; needs to be enabled
  )

;; also enable the LSP UI package
(use-package lsp-ui
  :config
  ;; most of these are on by default, but we list them all for easy tweaking
  (setopt lsp-ui-doc-enable t)    ; show docs in popup window on hover
  (setopt lsp-ui-sideline-delay 0.5)
  (setopt lsp-ui-doc-show-with-mouse t)
  (setopt lsp-ui-doc-show-with-cursor nil)  ; slows down cursor movement
  (setopt lsp-ui-sideline-enable t)
  (setopt lsp-ui-sideline-show-code-actions t)
  (setopt lsp-ui-sideline-show-hover t)
  (setopt lsp-ui-sideline-ignore-duplicate t)
  (setopt lsp-ui-sideline-show-diagnostics t)
  ;; we use embark with consult for xref with preview instead of lsp-ui-peek
  ;(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )


;; Flycheck (instead of Flymake)
(use-package flycheck
  :config
  ;; eglot will start flycheck automatically via flycheck-eglot (see below),
  ;; but for non-eglot buffers, it's useful to have flycheck enabled globally
  (add-hook 'after-init-hook 'global-flycheck-mode)
  )

;; enable integration with eglot
(use-package flycheck-eglot
  :config
  ;; if this is not enabled, eglot will choose flymake instead
  (add-hook 'after-init-hook 'global-flycheck-eglot-mode)
  ;; Flycheck-Eglot considers Eglot to be the only syntax check provider, and
  ;; it is assumed that all suitable checkers are plugged into the LSP server;
  ;; this is usually what you want, but you can override it like this:
  ;:custom
  ;(flycheck-eglot-exclusive nil)  ; default is t - set to nil to allow others
  )

;; Show Flycheck diagnostics at the bottom, using 1/4 of the available space
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.25)))


;; Magit (Git)
(use-package magit
  :config
  ;; Use recommended keys C-x g, C-c g, C-c f
  (setopt magit-define-global-key-bindings 'recommended))

;; Git Timemachine
(use-package git-timemachine
  :bind
  ("C-c t" . git-timemachine-toggle))

;; Git Gutters
(use-package git-gutter
  ;:hook (prog-mode . git-gutter-mode) ; if you only want it when coding
  :bind
  ("C-x <up>" . git-gutter:previous-hunk)
  ("C-x <down>" . git-gutter:next-hunk)
  ("C-x v s" . git-gutter:stage-hunk) ; replaces vc-create-tag
  ("C-x v r" . git-gutter:revert-hunk) ; replaces vc-retrieve-tag
  ("C-x v =" . git-gutter:popup-hunk) ; replaces vc-diff
  ("C-x v SPC" . git-gutter:mark-hunk)
  (:repeat-map git-gutter-repeat-map
               ("<up>" . git-gutter:previous-hunk)
               ("<down>" . git-gutter:next-hunk)
               ("s" . git-gutter-stage-hunks)
               ("r" . git-gutter-revert-hunk)
               :exit
               ("=" . git-gutter:popup-hunk)
               ("c" . magit-commit-create))
  :config
  (setopt git-gutter:update-interval 2)
  ;'(git-gutter:modified-sign "  ")
  ;'(git-gutter:added-sign "++")
  ;'(git-gutter:deleted-sign "--"))
  ;(set-face-background 'git-gutter:modified "purple")
  ;(set-face-foreground 'git-gutter:added "green")
  ;(set-face-foreground 'git-gutter:deleted "red")
  (add-hook 'after-init-hook 'global-git-gutter-mode) ; if you want it always on
  )

;; use the fringe instead of the gutter for Git
(use-package git-gutter-fringe
  :config
  (setopt git-gutter-fr:side 'right-fringe)
  ;; styling as in Doom Emacs
  ;; disabled for now - makes scrolling slow down!
  ;(define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  ;(define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  ;(define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  ;; other config
  ;(set-face-foreground 'git-gutter-fr:modified "yellow")
  ;(set-face-foreground 'git-gutter-fr:added    "blue")
  ;(set-face-foreground 'git-gutter-fr:deleted  "white")
  ;(setq-default left-fringe-width  20)
  ;(setq-default right-fringe-width 20)
  )


;; Erlang
(use-package erlang
  :init
  (setq erlang-skel-file "my-erlang-skels")
  :config
  ;; kill Erlang mode's use of important keys like M-+
  (define-key erlang-mode-map (kbd "M-+") nil)
  ;; let source files specify their indentation
  (add-to-list 'safe-local-variable-values '(erlang-indent-level . 2))
  ;; for running standalone Flycheck on Erlang when Eglot/LSP is not enabled
  (setopt flycheck-erlang-include-path '("include" "../include"))
  ;; set options for Erlang when running under Emacs
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  ;; insert a standard module header automatically in new Erlang files
  (add-hook 'erlang-new-file-hook 'tempo-template-erlang-normal-header)
  ;(add-hook 'erlang-mode-hook #'origami-mode))
  (add-hook 'erlang-mode-hook #'lsp-launcher)
  ;; use ELP as Erlang language server if installed, instead of default erlang_ls
  (let ((has-elp (executable-find "elp")))
    (when has-elp
      (add-to-list 'eglot-server-programs
                   '(erlang-mode . ("elp" "server"
                                    :initializationOptions
                                    ;; initial server options may be passed here
                                    ;(:log "debug")  ; see RUST_LOG
                                    )
                                 ))
      ;; pass ELP configuration flags via eglot default workspace config
      (setq-default eglot-workspace-configuration
                    (plist-put eglot-workspace-configuration
                               ':elp '(:signatureHelp (:enable t))))
      ;; settings for using ELP with lsp-mode
      (lsp-register-client
       (make-lsp-client :new-connection (lsp-stdio-connection '("elp" "server"))
                        :major-modes '(erlang-mode)
                        :priority 0 ; take priority over the built-in lsp (-1)
                        :server-id 'erlang-language-platform))
    ))
  ;; auto mode for Erlang sys.config files
  (add-to-list 'auto-mode-alist '("\\.config\\'" . erlang-mode))

  ;; Erlang QuickCheck Emacs Mode
  ;;(add-to-list 'load-path "/opt/erlang/current/lib/erlang/lib/eqc-1.44.1/emacs")
  ;;(autoload 'eqc-erlang-mode-hook "eqc-ext" "EQC Mode" t)
  ;;(add-hook 'erlang-mode-hook #'eqc-erlang-mode-hook)
  ;;(setq eqc-max-menu-length 30)
  ;;(setq eqc-root-dir "/opt/erlang/current/lib/erlang/lib/eqc-1.44.1")

  ;; Wrangler
  ;;(add-to-list 'load-path "~/src/erl/wrangler/elisp")
  ;;(require 'wrangler)
  )


;; Elixir
(use-package elixir-mode
  :config
  ;; the exec name for elixir-ls is `language_server.sh', silly enough,
  ;; but both eglot and lsp-mode are aware about it, so it just works
  (when (executable-find "language_server.sh")
    (add-hook 'elixir-mode-hook #'lsp-launcher))
  )


;; C/C++
(setopt c-default-style
        '((java-mode . "java")
          (awk-mode . "awk")
          (other . "linux"))  ; default is gnu (you never do gnu)
        )
(when (or (executable-find "clangd") (executable-find "ccls"))
  (add-hook 'c-mode-hook #'lsp-launcher))

(use-package cmake-mode)


;; Rust
(use-package rust-mode
  :config
  ;; to enable Clippy
  ;;(add-to-list 'eglot-server-programs
  ;;             '((rust-ts-mode rust-mode) .
  ;;               ("rust-analyzer" :initializationOptions
  ;;                (:check (:command "clippy")))))
  (when (executable-find "rust-analyzer")
    (add-hook 'rust-mode-hook #'lsp-launcher))
  )


;; Swift
(use-package swift-mode)


;; Scala
(use-package scala-mode)


;; gForth
(use-package forth-mode
  :config
  (setq forth-indent-level 4)
  (setq forth-minor-indent-level 2)
  (setq forth-hilight-level 3)
  )


;; BASIC
(use-package basic-mode
  ;; After opening a file, do e.g. `basic-zx81-mode' to select sub-mode.
  ;; You can also put something like 5 REM -*- basic-zx81 -*- in the file.
  )


;; Lua
(use-package lua-mode)


;; Haskell
(use-package haskell-mode)


;; Prolog
;; Emacs builtin; by default registers .prolog extension only
(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))


;; Elm
(use-package elm-mode)


;; PureScript
(use-package purescript-mode
  :config
  (projectile-register-project-type 'purescript '("spago.dhall")
                                    :project-file "spago.dhall"
				    :compile "spago build"
				    :test "spago test"
				    :run "spago run")
  (setq lsp-purescript-formatter "purs-tidy")
  (when (executable-find "pursls")
    (add-hook 'purescript-mode-hook #'lsp-launcher)
    ;(setq lsp-purescript-server-executable "~/bin/purs")
    ;(setq lsp-purescript-server-args '("ide" "server" "--log-level" "all"))
    )
  :hook
  (purescript-mode . turn-on-purescript-indentation)
  )


(provide 'init-coding)
;;; init-coding.el ends here
