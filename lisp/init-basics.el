;;; init-basics.el --- Configuration of built-in Emacs features

;;; Commentary:

;;; Code:


;; General behaviour
(setopt visible-bell t)    ; be quiet
(setopt frame-title-format (concat  "Emacs@" (system-name) "- %b"))
(setopt confirm-kill-emacs 'yes-or-no-p)  ; don't close Emacs by accident
(setopt use-short-answers t)  ; y or n instead of yes or no
(setopt use-dialog-box nil)  ; call me old fashioned, but...
(setopt use-file-dialog nil)
(setopt kill-buffer-query-functions
        ;; don't ask before killing buffer with running process
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))
(setopt version-control nil)  ; no numbers on backup files unless already existing
(setopt delete-old-versions t)  ; clear out old backup files
(setopt help-window-select t)  ; jump to the help window when it pops up
(setopt even-window-sizes nil) ; don't resize windows willy-nilly
(setopt initial-major-mode 'text-mode)  ; sane default for the scratch buffer
(setopt wdired-allow-to-change-permissions t)

;; Fix default behaviour when Emacs wants to show a buffer (e.g. Help).
;; First try to reuse an existing window for that buffer; if none exists, pop
;; up a new window instead (or replace this with `display-buffer-same-window' if
;; you prefer to stay in the current window and not get new unwanted windows).
(setopt display-buffer-base-action
        '((display-buffer-reuse-window display-buffer-pop-up-window)
          (reusable-frames . t)))


;; Look-and-feel
(setopt text-scale-mode-step 1.075)  ; default step is too large (1.2)
(setopt window-resize-pixelwise t)
;(setopt frame-resize-pixelwise t)
(setq max-mini-window-height 0.33)  ; default is too small (0.25)
(setopt truncate-partial-width-windows 32) ; don't wrap lines if window too narrow
(set-face-background 'glyphless-char "red") ; show zero-width space etc in red
;(setopt mode-line-position-line-format '(" L%l")) ; typically no need to change this
(setopt mode-line-position-column-format '(" C%C")) ; 1-based column numbers
(setopt mode-line-position-column-line-format '(" (%l,%C)")) ; 1-based column numbers

;; Scrolling
(setopt scroll-preserve-screen-position t)
(setopt scroll-conservatively 8)
(setopt scroll-margin 2)

;; Mouse
(setopt mouse-yank-at-point t)  ; don't move point on mouse-yank

;; Text editing
(setopt sentence-end-double-space nil)
(setopt fill-column 75)

;; TAB behaviour (indentation and completion)
(setopt indent-tabs-mode nil)   ; don't insert tab characters
(setopt tab-always-indent 'complete)  ; make TAB both indent and complete
(setopt tab-first-completion 'word-or-paren-or-punct)  ; be conservative
;(setopt completion-styles '(basic partial-completion flex emacs22)) ; use Orderless instead

;; Searching and regexps
(setopt search-whitespace-regexp "[-_ \t\n]+") ; make search treat _-\n as space

(require 're-builder)
(setopt reb-re-syntax 'string)  ; don't require double escapes in re-builder

;; EDiff
(setopt ediff-window-setup-function 'ediff-setup-windows-plain) ; no popup frame
(setopt ediff-split-window-function 'split-window-horizontally)

;; Saving desktop sessions
(require 'desktop)
(add-to-list 'desktop-path '".")
;; If there is a desktop session file in the current dir when Emacs starts,
;; we disable checking for a global session file (in ~/.emacs.d/ or ~/),
;; restoring the local session and enabling the desktop-save-mode. Otherwise,
;; we do nothing, requiring the user to run 'desktop-read' manually.
(when (file-exists-p "./.emacs.desktop")
  (setopt desktop-path '("."))  ; only look in current dir for saved sessions
  (setopt desktop-file-name-format 'local)  ; make the session dir relocatable
  (desktop-save-mode 1) ; load session and enable session auto-save
  )

;; Org-mode (note taking, .org)
(require 'org)
(setq org-directory "~/notes")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")
                          (sequence "CHECK" "BUG" "FIXED")
                          ))
(setq org-startup-indented 1) ; less visual clutter
(add-hook 'org-mode-hook #'visual-line-mode)  ; wrap lines nicely
;(setq org-blank-before-new-entry '((heading . always) (plain-list-item . auto)))


;; Global modes

;; Disabled modes

(set-scroll-bar-mode nil)  ; no damn scrollbars (see also early-init.el)
(tool-bar-mode -1)    ; disable the abominable toolbar (see also early-init.el)
;(menu-bar-mode -1)   ; the menu bar can stay - it's often useful
(tooltip-mode -1)  ; just use echo area instead of puny popups
(setopt tooltip-resize-echo-area t)  ; show the full tooltip message

;; Enabled modes (postpone enabling until end of init when possible)

(add-hook 'after-init-hook 'pixel-scroll-precision-mode)  ; no choppy scrolling

;(add-hook 'after-init-hook 'global-display-fill-column-indicator-mode)  ; show fill column on the right

(add-hook 'after-init-hook 'global-display-line-numbers-mode)  ; show line numbers on the left
(setopt display-line-numbers-width 4)
(set-face-attribute 'line-number nil :background "gray97")  ; white looks bad
(set-face-attribute 'line-number nil :height (round (* 0.75 (face-attribute 'default :height)))) ; smaller line numbers

(add-hook 'after-init-hook 'line-number-mode)  ; show line number in modeline
(add-hook 'after-init-hook 'column-number-mode)  ; show column number in modeline

(add-hook 'after-init-hook 'show-paren-mode)  ; visualize matching parens

(add-hook 'after-init-hook 'delete-selection-mode)  ; typing replaces selection

(add-hook 'after-init-hook 'electric-pair-mode)  ; be smart about parens

(add-hook 'after-init-hook 'global-whitespace-mode) ; add before adding font-lock-mode
(setopt whitespace-line-column 80)
(with-eval-after-load 'whitespace
  ;; this font doesn't exist until whitespace-mode has loaded
  (set-face-attribute 'whitespace-line nil :background "gray94" :foreground 'unspecified)  ; just a subtle shading
  )
(setopt whitespace-style (list 'face 'trailing 'lines-tail 'empty
                             'space-before-tab))

(add-hook 'after-init-hook 'global-hl-line-mode)  ; highlight the current line

(add-hook 'after-init-hook 'global-font-lock-mode)  ; put on makeup

(add-hook 'after-init-hook 'recentf-mode)  ; remember recent files
(setopt recentf-auto-cleanup 'never)  ; auto cleanup interacts poorly with Tramp
(setopt recentf-max-menu-items 10)
(setopt recentf-max-saved-items 20)


(provide 'init-basics)
;;; init-basics.el ends here
