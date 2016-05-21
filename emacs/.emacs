; (add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory  "~/.emacs.d/"))
  (normal-top-level-add-to-load-path
   '("markdown-mode"
     "async"
     "helm"
     "helm-swoop"
     "dash"
     "projectile"
     "haskell-mode")))


; helm
(require 'helm-config)
(require 'helm-swoop)


; markdown-mode
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing Github-Flavored Markdown files" t)

;; enable gfm-mode on markdown files
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))


; projectile
(require 'projectile)
(projectile-global-mode)


; haskell-mode
(require 'haskell-mode-autoloads)



;; common stuff that probably shouldn't be here

;; Disable the ridiculous and frustrating electric-indent
(electric-indent-mode 0)
