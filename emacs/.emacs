; (add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory  "~/.emacs.d/"))
  (normal-top-level-add-to-load-path
   '("markdown-mode"
     "async"
     "helm"
     "helm-swoop")))


; helm
(require 'helm-config)
(require 'helm-swoop)

; markdown-mode
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing Github-Flavored Markdown files" t)

;; enable gfm-mode on markdown files
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
