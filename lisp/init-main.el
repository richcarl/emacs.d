;;; init-main.el --- Generic (system-independent) Emacs configuration

;;; Commentary:

;;; Code:


;; Shuddup already.
(setopt inhibit-startup-message t)
(setopt initial-scratch-message nil)


;; Ensure exec path is the same as a normal shell path on Mac OS
;; (Necessary because GUI-launched apps do not read .profile etc.
;; Not needed when using Emacs-plus).)
(when (and (eq system-type 'darwin)
           (not (boundp 'ns-system-appearance))) ; check for emacs-plus
  (use-package exec-path-from-shell)
  (exec-path-from-shell-initialize))


;; Start Emacs server after initialization is complete
(if window-system
    (require 'server)
    ;;(setopt server-temp-file-regexp "/tmp/.+")
    ;;(setopt server-window (car (frame-list)))
    (add-hook 'after-init-hook
              (function (lambda () (unless (server-running-p) (server-start)))))
  )


;; Package management

;; Add MELPA to package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Set up use-package to automate package installation
(unless (package-installed-p 'use-package)
  ;; first of all ensure that use-package itself is installed
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setopt use-package-always-ensure t))

(use-package auto-package-update
  :config
  (setopt auto-package-update-delete-old-versions t)
  (setopt auto-package-update-hide-results t)
  (setopt auto-package-update-interval 14)
  (auto-package-update-maybe))


;; Load the rest of the configuration organized into categories

(require 'init-keys)
(require 'init-basics)
(require 'init-addons)
(require 'init-fileformats)
(require 'init-coding)
(require 'init-misc)


(provide 'init-main)
;;; init-main.el ends here
