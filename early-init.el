;;  Emacs early-startup script
;; (note that any messages at this stage get printed to the console)

;; keep GC at a minimum during startup, restore as last point after init
(setq gc-cons-threshold most-positive-fixnum)
;(profiler-start 'cpu) ; uncomment to profile, run profiler-report after
(add-hook 'after-init-hook
          (function (lambda ()
                      ;(profiler-stop) ; uncomment to profile startup
                      (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
                      )))

(when (eq system-type 'windows-nt)
  ;; Emacs JIT seems broken on Windows right now
  (setq native-comp-jit-compilation nil)
  (setq native-comp-enable-subr-trampolines nil)
  )

;; Don't read system defaults.
(setopt inhibit-default-init t)

;; Avoid loading .elc bytecode older than the .el file
(setopt load-prefer-newer t)

;; Set up basic frame properties (modify in early-init-local.el as needed)
(setq initial-frame-alist '( (top . 32) (left . 32)))
(modify-all-frames-parameters
 '((width . 84)
   (height . 42)
   (vertical-scroll-bars . nil)  ; right, left, or nil
   (tool-bar-lines . 0)  ; 0 disables
   ))

;; Load any local early initialization (suppress the load message)
(setq local-early-init-file (locate-user-emacs-file "early-init-local.el"))
(when (file-exists-p local-early-init-file) (load local-early-init-file nil t))
