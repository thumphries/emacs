
(let ((default-directory  "~/.emacs.d/"))
  (normal-top-level-add-to-load-path
    '("markdown-mode")))

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)






;; markdown mode on markdown files
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
