;;; init-addons.el --- General enhancements not included by default

;;; Commentary:

;;; Code:


;; Same modeline as Doom Emacs and Spacemacs
(use-package doom-modeline
  :config
  (setopt doom-modeline-icon nil) ; enable if you have nerd-icons installed
  (setopt doom-modeline-minor-modes t)  ; also see minions-mode below
  (setopt doom-modeline-column-zero-based nil)
  (add-hook 'after-init-hook 'doom-modeline-mode))

;;  Collapse minor modes list (when shown) in modeline into a menu icon
(use-package minions
  :config
  (add-hook 'after-init-hook 'minions-mode))


;; Support for .editorconfig files (https://editorconfig.org/)
(use-package editorconfig
  :config
  (setopt editorconfig-override-file-local-variables nil)
  (setopt editorconfig-override-dir-local-variables nil)
  (add-hook 'after-init-hook 'editorconfig-mode))


;; Improved help
(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))


;; Terminal (Eat: Emulate-a-terminal)
;; note: run 'eat-compile-terminfo' if keys are not working properly
(use-package eat
  :config
  (setopt eat-enable-shell-prompt-annotation nil)  ; drop the extra margin
  (global-set-key (kbd "C-c s") 'eat)
  (add-to-list 'eat-semi-char-non-bound-keys [?\e ?o]) ;M-o for ace-window
  (add-to-list 'eat-semi-char-non-bound-keys [?\e ?g]) ;M-g for misc gotos
  (eat-update-semi-char-mode-map)
  (eat-reload) ; necessary
  )


;; Orderless for completion using space separated fragments (as in Helm or Ivy)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  )


;; Quick jumping to items on screen, or acting directly on them
(use-package avy
  :config
  ;; avy-goto-char-timer: start typing a prefix, then after the timeout,
  ;; avy will highlight the matching locations with one or more shortcut
  ;; characters for jumping to that location
  (global-set-key (kbd "M-j") 'avy-goto-char-timer)
  (define-key isearch-mode-map (kbd "M-j") 'avy-isearch)
  (setopt avy-timeout-seconds 0.7)  ; default is 0.5
  )


;; Which-key for reminding you which keys are avaliable
;;
;; disabled in favor of Embark - see embark-prefix-help-command
;; (use-package which-key
;;   :config
;;   (setopt which-key-idle-delay 2.0)
;;   (global-unset-key (kbd "C-h C-h"))  ; make paging work in C-h-...
;;   (add-hook 'after-init-hook 'which-key-mode)
;;   )


;; Ace Window for quick switching between Emacs windows
(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))


;; Origami for folding and unfolding text regions
(use-package origami
  :config
  ;; example key bindings
  (define-key global-map (kbd "C-c o") 'origami-mode-map)
  (define-prefix-command 'origami-mode-map)
  (define-key origami-mode-map (kbd "o") 'origami-open-node)
  (define-key origami-mode-map (kbd "O") 'origami-open-node-recursively)
  (define-key origami-mode-map (kbd "c") 'origami-close-node)
  (define-key origami-mode-map (kbd "C") 'origami-close-node-recursively)
  (define-key origami-mode-map (kbd "a") 'origami-open-all-nodes)
  (define-key origami-mode-map (kbd "A") 'origami-close-all-nodes)
  (define-key origami-mode-map (kbd "n") 'origami-toggle-node)
  (define-key origami-mode-map (kbd "N") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "s") 'origami-show-only-node) ; "solo"
  (define-key origami-mode-map (kbd "<up>") 'origami-previous-fold)
  (define-key origami-mode-map (kbd "<down>") 'origami-forward-fold)
  (define-key origami-mode-map (kbd "x") 'origami-reset)
  ;; probably best to enable this only for specific major modes...
  ;(add-hook 'after-init-hook 'global-origami-mode)
  )


;; Undo-tree mode for improved undo/redo
(use-package undo-tree
  :config
  ;; undo-tree already uses mainly /? for undo/redo, so we only need to ensure
  ;; that the rest of our normal remappings (see above) work also in undo-tree
  (define-key undo-tree-map (kbd "C--") 'undo-tree-undo)
  (define-key undo-tree-map (kbd "C-_") 'undo-tree-redo)
  (define-key undo-tree-map (kbd "C-z") 'undo-tree-undo)
  (define-key undo-tree-map (kbd "M-z") 'undo-tree-undo)
  (define-key undo-tree-map (kbd "S-C-z") 'undo-tree-redo)
  (define-key undo-tree-map (kbd "M-Z") 'undo-tree-redo)
  (setopt undo-tree-auto-save-history nil)
  (add-hook 'after-init-hook 'global-undo-tree-mode))


;; Selecting text incrementally
(use-package expand-region
  :bind
  ;; (C-= is a common suggestion for expand-region, but it's hard to reach
  ;; and in keymaps like SE, it's a dead key which doesn't work for this.)
  ("C-;" . er/expand-region) ; with repeat mode, just keep pressing ; - or 0
  ("C-ö" . er/expand-region) ; make it work in SE keymap as well
  )


;; Easily moving text up/down, with auto-indentation
(defcustom auto-indent-ignore-modes
  '(python-mode yaml-mode)
  "Modes where auto-indenting should not be done, even if prog-mode based."
  :type 'list)
(defcustom auto-indent-extra-modes '(LaTeX-mode TeX-mode)
  "Additional modes (apart from all prog-mode based modes) where auto-
indenting should be done, e.g. when using move-text."
  :type 'list)
(defun indent-region-advice (&rest ignored)
  "Auto indents region after moving text"
  (if (and (not (member major-mode auto-indent-ignore-modes))
           (or (member major-mode auto-indent-extra-modes)
               (derived-mode-p 'prog-mode)))
      (let ((deactivate deactivate-mark))
        (if (region-active-p)
            (indent-region (region-beginning) (region-end))
          (indent-region (line-beginning-position) (line-end-position)))
        (setq deactivate-mark deactivate))
    ))
(use-package move-text
  :config
  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice)
  ;; Note that the default M-up/down bindings make it rather too easy
  ;; to accidentally move a line without even noticing it
  ;(move-text-default-bindings)
  :bind
  ("M-S-<down>" . move-text-down)  ; require shift with meta to move
  ("M-S-<up>" . move-text-up))


;; Multiple cursors
(use-package multiple-cursors
  :config
  ;; define some aliases because mc/... has terrible discoverability
  (defalias 'multiple-cursors-mark-all-dwim 'mc/mark-all-dwim)
  (defalias 'multiple-cursors-mark-next-like-this 'mc/mark-next-like-this)
  (defalias 'multiple-cursors-mark-previous-like-this 'mc/mark-previous-like-this)
  (defalias 'multiple-cursors-edit-lines 'mc/edit-lines)
  ;; note that C-' is also used by mc to hide/show unmatched lines
  (global-set-key (kbd "C-'") 'mc/mark-all-dwim)
  (global-set-key (kbd "M-<down>") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-<up>") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c l") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-c m") 'mc/edit-lines)
  (global-set-key (kbd "C-c r") 'set-rectangular-region-anchor)
  ;; make US key '" work the same in SE layout
  (global-set-key (kbd "C-ä") 'mc/mark-all-dwim)
  (setopt mc/edit-lines-empty-lines 'ignore))


;; Phi-search (isearch compatible with multiple-cursors)
;(use-package phi-search
;  :config
;  (global-set-key (kbd "C-s") 'phi-search)
;  (global-set-key (kbd "C-r") 'phi-search-backward))


;; Enable editing in grep buffers
;; (For consult searches, use embark-export to get a grep buffer to edit,
;; then use C-c C-p to launch wgrep-mode)
(use-package wgrep)


;; Deadgrep for grepping (though consult-ripgrep probably makes this obsolete)
(use-package deadgrep
  :config
  (global-set-key (kbd "M-s d") 'deadgrep))

(use-package wgrep-deadgrep
  :config
  (autoload 'wgrep-deadgrep-setup "wgrep-deadgrep")
  (add-hook 'deadgrep-finished-hook 'wgrep-deadgrep-setup))


;; Yasnippet templating for abbreviation expansion
(use-package yasnippet
  :config
  (setopt yas-use-menu nil)  ; don't clutter the menu
  ;(add-hook 'after-init-hook 'yas-global-mode)
  )


;; Corfu for in-buffer completion popups
(use-package corfu-terminal)  ; enabled in corfu config below
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2) ; not below 2 (default 3)
  :bind
  (:map corfu-map
        ;("RET" . nil) ; unbind RET, use TAB only
        ("S-SPC" . corfu-insert-separator)  ; M-SPC may be in use by the OS
        ;("SPC" . corfu-insert-separator)  ; doesn't work well with auto
        )
  :config
  (unless (display-graphic-p)
    ;; also work in terminals, using popons instead of child frames
    (add-hook 'after-init-hook 'corfu-terminal-mode))
  (add-hook 'after-init-hook 'corfu-popupinfo-mode) ; extra info popup
  (add-hook 'after-init-hook 'global-corfu-mode))


;; Vertico for selecting among alternatives
(use-package vertico
  :config
  ; (setq vertico-count 20)
  ; (setq vertico-scroll-margin 0)
  ; (setq vertico-resize t)
  ; (setq vertico-cycle t) ; cycle vertico-next/vertico-previous
  (vertico-multiform-mode) ; enable switching layout (M-G, M-B, M-F, M-V, ...)
  (add-hook 'after-init-hook 'vertico-mode))


;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer. To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind
  (:map minibuffer-local-map ("M-A" . marginalia-cycle))

  :init
  ;; Marginalia must be activated in the :init section of use-package such
  ;; that the mode gets enabled right away. Note that this forces loading
  ;; the package.
  (marginalia-mode))


;; Embark for actions (uses Marginalia if enabled)
(use-package embark
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)  ; replaces `xref-find-definitions'
  ("C-h b" . embark-bindings) ; replaces `describe-bindings'

  :init
  ;; Replace prefix help (C-x C-h, C-c C-h, etc.) with an embark menu
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Use compact help key layout (requires enabling vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  ;; Show the Embark target at point via Eldoc (can be noisy)
  ;(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; also set this globally, but preferably in the eldoc configuration section:
  ;;(setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;(setopt embark-cycle-key ";")  ; not needed if using C-. for embark-act
  ;(setopt embark-help-key "?")  ; leave C-h free
  (setopt embark-mixed-indicator-delay 1.5)
  (setopt embark-verbose-indicator-display-action
          ;; show at bottom like which-key instead of in other window
          '(display-buffer-in-side-window (side . bottom)))

  ;; Use the minimal indicator by default and press embark-help-key for menu
  (setopt embark-indicators
          '(embark-minimal-indicator  ; default is embark-mixed-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Custom actions
  (define-key embark-file-map (kbd "f") #'consult-fd)
  (define-key embark-identifier-map (kbd "g") #'consult-ripgrep)
  (define-key embark-identifier-map (kbd "G") #'consult-git-grep)
  (define-key embark-symbol-map (kbd "g") #'consult-ripgrep)
  (define-key embark-symbol-map (kbd "G") #'consult-git-grep)
  (define-key embark-variable-map (kbd "g") #'consult-ripgrep)
  (define-key embark-variable-map (kbd "G") #'consult-git-grep)
  )


;; Consult for candidate completion and selection with previews
(use-package consult
  :hook
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Improve register preview for `consult-register',
  ;; `consult-register-load', `consult-register-store' and the Emacs
  ;; built-ins.
  (setopt register-preview-delay 0.5
          register-preview-function #'consult-register-format)

  ;; This adds thin lines, sorting, and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

  :config
  (setopt consult-narrow-key "<")

  ;; Delay preview for some commmands (default is immediate update)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  :bind
  (
   ;; C-c bindings in `mode-specific-map'
   ("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)

   ;; C-x bindings in `ctl-x-map'
   ("C-x b" . consult-buffer)                ; replaces switch-to-buffer
   ("C-x C-r" . consult-recent-file)         ; requires recentf-mode; replaces find-file-read-only (use C-x 4 r instead)
   ("C-x 4 b" . consult-buffer-other-window) ; replaces switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ; replaces switch-to-buffer-other-frame
   ("C-x t b" . consult-buffer-other-tab)    ; replaces switch-to-buffer-other-tab
   ("C-x r b" . consult-bookmark)            ; replaces bookmark-jump
   ("C-x p b" . consult-project-buffer)      ; replaces project-switch-to-buffer
   ("C-x M-:" . consult-complex-command)     ; replaces repeat-complex-command

   ;; Other rebindings
   ("M-y" . consult-yank-pop)                ; replaces yank-pop

   ;; M-g bindings in `goto-map'
   ("M-g g" . consult-goto-line)             ; replaces goto-line
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flycheck)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-g m" . consult-mark)
   ("M-g M" . consult-global-mark)
   ("M-g o" . consult-outline)
   ("M-g h" . consult-org-heading)

   ;; M-s bindings in `search-map'
   ("M-s f" . consult-fd)    ; alt. consult-find if you don't have fd
   ("M-s F" . consult-find)  ; fallback if fd not available
   ("M-s g" . consult-ripgrep)
   ("M-s G" . consult-git-grep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)

   ;; Isearch integration
   ("M-s i" . consult-isearch-history)  ; (M-s h is in use as a prefix)

   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ; replaces isearch-edit-string
   ("M-s i" . consult-isearch-history)       ; replaces isearch-edit-string
   ("M-s l" . consult-line)                  ; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ; needed by consult-line to detect isearch

   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)    ; replaces next-matching-history-element
   ("M-r" . consult-history)    ; replaces previous-matching-history-element
   )

   ;; Custom bindings
   ;; ("M-#" . consult-register-load)
   ;; ("M-'" . consult-register-store)  ; replaces abbrev-prefix-mark (unrelated)
   ;; ("C-M-#" . consult-register)
  )

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; Projectile for improved navigating and managing of projects
(use-package projectile
  ;; (Projectile integrates with Emacs' default project.el to make it use
  ;; Projectile's project lookup function (projectile-project-root) and
  ;; project file lookup function (projectile-project-files) whenever
  ;; projectile-mode is enabled; see the project-find-functions hook.)
  :config
  ;; tweak the project identification search order
  ;; to check bottom-up for .git etc only after having checked top-down first
  ;; note:  "bottom-up" = innermost wins, so top-down should mean outermost!
  (setopt projectile-project-root-functions
        '(projectile-root-local  ; check if 'projectile-project-root' var already set, e.g. by '.dir-locals.el'
          projectile-root-marked  ; search for '.projectile' marker file ('projectile-dirconfig-file') - innermost wins (defined via projectile-root-bottom-up)
          projectile-root-bottom-up ; search for files in 'projectile-project-root-files-bottom-up' - .git, .hg, etc - innermost wins - PROBLEM: this before project files in default order means .git always wins over pom.xml, mix.exs, etc., even if it's a submodule of a main project
          projectile-root-top-down  ; search for files in 'projectile-project-root-files' - GTAGS, configure.in, etc. - innermost wins = BUG!; projectile-register-project-type adds project-file to this list, like pom.xml, mix.exs
          projectile-root-top-down-recurring  ; search for nested files in 'projectile-project-root-files-top-down-recurring' - Makefile, .svn, CVS - outermost of innermost sequence wins
          ))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-hook 'projectile-after-switch-project-hook
            ;; note: only triggers on explicit project switch,
            ;; not by simply opening a file
            (function (lambda ()
                        (message "Running projectile-after-switch-project-hook")
                        (let* ((dir (projectile-project-root))
                               (file (concat dir  ".emacs.desktop")))
                          (when (file-exists-p file) (desktop-change-dir dir)))
                        )))
  (add-hook 'after-init-hook 'projectile-mode))


;; File explorer in side window
(use-package treemacs
  :config
  (treemacs-resize-icons 16)
  (setopt treemacs-text-scale -2)
  (setopt treemacs-width 24)
  :bind
  ("C-c e" . treemacs)
  )


;; Zen mode
(use-package writeroom-mode
  :config
  :bind
  ("C-c z" . writeroom-mode)
  )


(use-package treemacs-projectile)  ; additional integration with projectile


(provide 'init-addons)
;;; init-addons.el ends here
