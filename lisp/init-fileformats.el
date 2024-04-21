;;; init-fileformats.el --- Configuration for editing various file formats

;;; Commentary:

;;; Code:


;; Yaml
(use-package yaml-mode)


;; AsciiDoc
(use-package adoc-mode)


;; Graphviz DOT files
(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))


;; GNUPlot
(use-package gnuplot-mode
  :config
  ;; gnuplot-mode automatically adds suffixes .gp and .gnuplot
  (add-to-list 'auto-mode-alist '("\\.plt\\'" . gnuplot-mode))
  (add-to-list 'auto-mode-alist '("\\.gnu\\'" . gnuplot-mode))
  )


;; CSV files
(use-package csv-mode)


;; Docker files
(use-package dockerfile-mode)


;; OpenTofu/Terraform
(use-package terraform-mode
  ;:custom (terraform-indent-level 4)
  :config
  ; ;; if you want to use outline-minor-mode
  ; (defun my-terraform-mode-init () (outline-minor-mode 1))
  ; (add-hook 'terraform-mode-hook 'my-terraform-mode-init)
  )


;; TeX/LaTeX
(use-package tex
  :ensure auctex
  ;;:hook ((LaTeX-mode . prettify-symbols-mode)
  ;;       (TeX-mode . prettify-symbols-mode))
  )


(provide 'init-fileformats)
;;; init-fileformats.el ends here
