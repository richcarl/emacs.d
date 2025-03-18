;;; init-misc.el --- A place for various home made hacks and other stuff

;;; Commentary:


;; Inserting a chunk of example text
(defun lorem-ipsum ()
  "Insert the standard block of Lorem Ipsum text."
  (interactive)
  (insert
"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
enim ad minim veniam, quis nostrud exercitation ullamco laboris
nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in
reprehenderit in voluptate velit esse cillum dolore eu fugiat
nulla pariatur. Excepteur sint occaecat cupidatat non proident,
sunt in culpa qui officia deserunt mollit anim id est laborum.")
)

;; Sonic Pi
;(require 'sonic-pi)  ; not use-package, for now - not compatible with recent v.
(add-to-list 'auto-mode-alist '("\\.spi\\'" . sonic-pi-mode))
;; Set these paths per installation in your init-preload-local.el instead:
;(setq sonic-pi-path "~/SONIC-PI-INSTALL-DIR/") ; Must end with "/"
;(add-to-list 'load-path "~/.emacs.d/site-lisp/sonic-pi-mode")

(provide 'init-misc)
;;; init-misc.el ends here
