;;; init-keys.el --- Key bindings

;;; Commentary:

;;; Code:


;; On Mac OS, use Cmd as Meta key, make Right Opt into an extra Ctrl,
;; and pass left Opt on unchanged to Mac OS for alternative characters
(setopt mac-command-modifier 'meta)
(setopt mac-option-modifier 'none)
(setopt mac-right-option-modifier 'ctrl)

;; Enable the built-in Windmove (Shift + arrow keys to move between windows)
(windmove-default-keybindings)

;; Winner mode - use C-c Left/Right to undo/redo the window configuration
(add-hook 'after-init-hook 'winner-mode)

;; Repeat mode - allow pressing just the last key to repeat some combos
(add-hook 'after-init-hook 'repeat-mode)


;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Some necessary Keyboard remappings for sanity
(global-unset-key "\C-z")  ; Don't let C-z iconify the frame
(global-unset-key "\C-l")  ; Disable recentering to avoid a nervous tic
;(global-set-key (kbd "C-.") 'set-mark-command)    ; alternative to C-SPC
;(global-set-key (kbd "C-x C-.") 'pop-global-mark) ; alternative to C-x SPC
(global-set-key (kbd "C-c SPC") 'just-one-space)  ; since M-SPC may be in use by the OS
(global-set-key (kbd "C-x k") 'kill-current-buffer)  ; don't pop up a buffer list
(global-set-key (kbd "C-x K") 'kill-buffer)  ; use with Shift-K to get the list


;; Make SE keyboard use same physical keys as US layout for some things
;; (only use M-- for negative-argument, since we remap C-- to undo)
;; this makes US key /? work as the main undo key also in SE keymap (labeled -_)
(global-set-key (kbd "C--") 'undo)       ; (C-- defaults to negative-argument)
(global-set-key (kbd "C-_") 'undo-redo)  ; (C-_ defaults to undo)
; this makes US key - work as negative-argument also in SE keymap (labeled +)
(global-set-key (kbd "M-+") 'negative-argument)
;; this makes keys labeled + or - work for text scale in both SE and US
;; (increase/decrease text scale get reversed in SE layout, but that's ok;
;; also note that since ´` is a dead key pressing C-´ yields just ´)
(global-set-key (kbd "C-x ´") (function (lambda () (interactive) (text-scale-adjust -1))))
; this makes US key 6^ work as join-line and enlarge-window also in SE keymap (labeled 6&)
(global-set-key (kbd "M-&") 'delete-indentation)  ; (M-& defaults to async-shell-command)
(global-set-key (kbd "C-x &") 'enlarge-window)
; this makes US key \| work as normal also in SE keymap (labeled '*)
(global-set-key (kbd "C-M-'") 'indent-region)
(global-set-key (kbd "M-*") 'shell-command-on-region)
; dead keys may be made to work with C or M but not with a prefix like C-x
(global-set-key (kbd "C-<dead-diaeresis>") 'abort-recursive-edit) ; (C-])
(global-set-key (kbd "M-S-<dead-circumflex>") 'forward-paragraph) ; (M-})
(global-set-key (kbd "M-Å") 'backward-paragraph) ; (M-{)

;; Move some US-centric bindings to match SE keyboard legends, mainly + and -
;; (in particular, US =+ and ]} are dead keys ´` and ^¨ in SE, so avoid them)
(global-set-key (kbd "C-x !") 'shrink-window-if-larger-than-buffer) ; (C-x -)
(global-set-key (kbd "C-x C-/") 'text-scale-adjust) ; (C-x C--)
(global-set-key (kbd "C-x -") 'balance-windows) ; (C-x +)
(global-set-key (kbd "C-x /") 'balance-windows) ; (C-x +)

;; Make C-z and M-z also work as undo
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "S-C-z") 'undo-redo)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-Z") 'undo-redo) ; note: M-Z, not S-M-z (doesn't work)

;; Move zap-to-char to C-,
(global-set-key (kbd "C-,") 'zap-to-char)


;; Other useful custom key bindings (keep it short)
(global-set-key (kbd "M-n") 'forward-to-indentation)
(global-set-key (kbd "M-p") 'backward-to-indentation)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-c a") 'align-current)
(global-set-key (kbd "C-c d") 'picture-mode)  ; "draw"
(global-set-key (kbd "C-c z") 'toggle-frame-fullscreen) ; better than F11


;; Bind duplicate-dwim to a key and configure repeat mode for it
(global-set-key (kbd "C-x j") 'duplicate-dwim)
(defvar-keymap duplicate-dwim-repeat-map
  :doc "Keymap to repeat `duplicate-dwim'.  Used in `repeat-mode'."
  :repeat t
  "j" #'duplicate-dwim)


;; Make delete-horizontal-space cycle through spacings instead, and allow
;; pressing \ to keep cycling if repeat-mode is enabled
(global-set-key [remap delete-horizontal-space] 'cycle-spacing)
; make US key \| work as cycle-spacing also in SE keymap
(global-set-key (kbd "M-'") 'cycle-spacing) ; (M-' defaults to abbrev-prefix-mark)
(defvar-keymap cycle-spacing-repeat-map
  :doc "Keymap to repeat `cycle-spacing' commands.  Used in `repeat-mode'."
  :repeat t
  ;"SPC" #'cycle-spacing
  "\\" #'cycle-spacing)

; Make M-/ use the builtin hippie-expand instead of plain old dabbrev-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)


;; Changing frame width (defaults to doubling/halving)
(defun change-frame-width (width)
  "Resize the frame horizontally.
Either resizes by prefix arg WIDTH or by doubling or halving the
current size depending on the current width."
  (interactive "P")
  (if window-system
      (let ((current (frame-width)))
        (set-frame-width (selected-frame)
                         ;; don't halve unless result is wide enough
                         (or width (if (< current 112) (* current 2)
                                     (/ current 2)))))
    (error "Cannot resize frame horizontally: is a text terminal")))
(global-set-key (kbd "C-c w") 'change-frame-width)


(provide 'init-keys)
;;; init-keys.el ends here
