
(let ((default-directory  "~/.emacs.d/"))
  (normal-top-level-add-to-load-path
   '("markdown-mode"
     "async"
     "helm")))

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(require 'helm-config)


;; enable markdown mode on markdown files
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
