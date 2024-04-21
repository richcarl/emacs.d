;;  Emacs startup script
;; (see also early-init.el)

;; Ensure the initial frame is on top and in focus (esp. needed on Mac OS)
(select-frame-set-input-focus (selected-frame))

;; Add lisp and site-lisp subdirectories to load-path
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
  )

;; Load any local pre-loading initialization
(setq local-preload-init-file (locate-user-emacs-file "init-preload-local.el"))
(when (file-exists-p local-preload-init-file) (load local-preload-init-file))

;; Load generic, system-independent settings
(require 'init-main)

;; Load any local initialization
(setq local-init-file (locate-user-emacs-file "init-local.el"))
(when (file-exists-p local-init-file) (load local-init-file))

;; Use a separate file for Emacs customization (must be explicitly loaded)
(setq custom-file (locate-user-emacs-file "custom.el")) ; use setq for this
(when (file-exists-p custom-file) (load custom-file))

;; Avoid thrashing your personal custom-file by always saving
;; customizations to a separate file (ignored on restart). Manually move
;; settings that you want to keep into your actual custom-file.
(setq custom-file (expand-file-name "custom-dump.el" user-emacs-directory))
