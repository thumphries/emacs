;;; mafia.el

(defgroup mafia nil
  "Complete development mode for Haskell"
  :group 'haskell)


;; not sure which of these is needed
(require 'flycheck)
(require 'json)
(require 'warnings)
(require 'cl-lib)
(require 'company)
(require 'comint)
(require 'widget)
(require 'eldoc)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-local variables/state

(defvar-local mafia-project-root nil
  "The project root of the current buffer.")

(defvar-local mafia-package-name nil
  "The name of the current package.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTALL

(defun mafia-installed-p ()
  "Return non-nil if intero (of the right version) is installed in the stack environment."
  (redisplay)
  (with-temp-buffer
    (if (= 0 (call-process "mafia" nil t nil "--version"))
        'installed
        'not-installed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECT ROOTS

(defun mafia-cabal-find-file (&optional dir)
  "Search for package description file upwards starting from DIR.
If DIR is nil, `default-directory' is used as starting point for
directory traversal.  Upward traversal is aborted if file owner
changes.  Uses `mafia-cabal-find-pkg-desc' internally."
  (let ((use-dir (or dir default-directory)))
    (while (and use-dir (not (file-directory-p use-dir)))
      (setq use-dir (file-name-directory (directory-file-name use-dir))))
    (when use-dir
      (catch 'found
        (let ((user (nth 2 (file-attributes use-dir)))
              ;; Abbreviate, so as to stop when we cross ~/.
              (root (abbreviate-file-name use-dir)))
          ;; traverse current dir up to root as long as file owner doesn't change
          (while (and root (equal user (nth 2 (file-attributes root))))
            (let ((cabal-file (mafia-cabal-find-pkg-desc root)))
              (when cabal-file
                (throw 'found cabal-file)))

            (let ((proot (file-name-directory (directory-file-name root))))
              (if (equal proot root) ;; fix-point reached?
                  (throw 'found nil)
                (setq root proot))))
          nil)))))

(defun mafia-cabal-find-pkg-desc (dir &optional allow-multiple)
  "Find a package description file in the directory DIR.
Returns nil if none or multiple \".cabal\" files were found.  If
ALLOW-MULTIPLE is non nil, in case of multiple \".cabal\" files,
a list is returned instead of failing with a nil result."
  ;; This is basically a port of Cabal's
  ;; Distribution.Simple.Utils.findPackageDesc function
  ;;  http://hackage.haskell.org/packages/archive/Cabal/1.16.0.3/doc/html/Distribution-Simple-Utils.html
  ;; but without the exception throwing.
  (let* ((cabal-files
          (cl-remove-if 'file-directory-p
                        (cl-remove-if-not 'file-exists-p
                                          (directory-files dir t ".\\.cabal\\'")))))
    (cond
     ((= (length cabal-files) 1) (car cabal-files)) ;; exactly one candidate found
     (allow-multiple cabal-files) ;; pass-thru multiple candidates
     (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPL

(defconst mafia-prompt-regexp "^\4 ") ;; WAT

(defvar-local mafia-repl-previous-buffer nil
  "Records the buffer to which `mafia-repl-switch-back' should jump.
This is set by `mafia-repl-buffer', and should otherwise be nil.")

(defun mafia-repl-clear-buffer ()
  "Clear the current REPL buffer."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))


(defun mafia-repl-buffer (prompt-options &optional store-previous)
  "Start the REPL buffer.
If PROMPT-OPTIONS is non-nil, prompt with an options list.  When
STORE-PREVIOUS is non-nil, note the caller's buffer in
`mafia-repl-previous-buffer'."
  (let* ((root (mafia-project-root))
         (package-name (mafia-package-name))
         (name (format "*mafia:%s:%s:repl*"
                       (file-name-nondirectory root)
                       package-name))
         (initial-buffer (current-buffer))
         (backend-buffer (mafia-buffer 'backend)))
    (with-current-buffer
        (if (get-buffer name)
            (get-buffer name)
          (with-current-buffer
              (get-buffer-create name)
            (cd root)
            (mafia-repl-mode)
            (mafia-repl-mode-start backend-buffer
                                   (buffer-local-value 'intero-targets backend-buffer)
                                   prompt-options)
            (current-buffer)))
      (progn
        (when store-previous
          (setq mafia-repl-previous-buffer initial-buffer))
        (current-buffer)))))

(defun mafia-buffer (worker)
  "Get the WORKER buffer for the current directory."
  (let ((buffer (mafia-get-buffer-create worker)))
    (if (get-buffer-process buffer)
        buffer
      (mafia-get-worker-create worker nil (current-buffer)))))

(defun mafia-get-worker-create (worker &optional targets source-buffer)
  "Start the given WORKER.
If provided, use the specified TARGETS and SOURCE-BUFFER."
  (let* ((buffer (mafia-get-buffer-create worker)))
    (if (get-buffer-process buffer)
        buffer
      (let ((install-status (mafia-installed-p)))
        (if (eq install-status 'installed)
            (mafia-start-process-in-buffer buffer targets source-buffer)
            (mafia-auto-install buffer install-status targets source-buffer))))))

(defun intero-get-buffer-create (worker)
  "Get or create the stack buffer for WORKER.
Uses the directory of the current buffer for context."
  (let* ((root (intero-project-root))
         (cabal-file (intero-cabal-find-file))
         (package-name (if cabal-file
                           (intero-package-name cabal-file)
                         ""))
         (buffer-name (intero-buffer-name worker))
         (default-directory (if cabal-file
                                (file-name-directory cabal-file)
                              root)))
    (with-current-buffer
        (get-buffer-create buffer-name)
      (setq intero-package-name package-name)
      (cd default-directory)
      (current-buffer))))







(provide 'mafia)

;;; mafia.el ends here
