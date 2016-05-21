; (add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory  "~/.emacs.d/"))
  (normal-top-level-add-to-load-path
   '("markdown-mode"
     "async"
     "helm"
     "helm-ag"
     "helm-swoop"
     "dash"
     "projectile"
     "haskell-mode")))


; helm
(require 'helm-config)
(require 'helm-ag)
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
(cua-mode t)


;; lifted and modified from load-dir package
(setq load-dir-loaded '())
(defun load-dir-one (dir)
  "Load all Emacs Lisp files in DIR."
  (let ((suffixes (get-load-suffixes)))
    (dolist (f (and (file-exists-p dir)
                    (file-directory-p dir)
                    (directory-files dir t)))
      (when (and (not (file-directory-p f))
                 (member (file-name-extension f t) suffixes))
        (setq f (file-name-sans-extension f))
        (unless (member f load-dir-loaded)
          (load f)
          (add-to-list 'load-dir-loaded f))))))
