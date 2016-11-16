;;; mafia.el

;; not sure which of these is needed
;;(require 'json)
;;(require 'warnings)
;;(require 'cl-lib)
;;(require 'company)
;;
;;(require 'widget)
;;(require 'eldoc)

(require 'comint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defgroup mafia nil
  "Complete development mode for Haskell"
  :group 'haskell)

(defcustom mafia-debug nil
  "Show debug output."
  :group 'mafia
  :type 'boolean)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-local variables/state

(defvar-local mafia-project-root nil
  "The project root of the current buffer.")

(defvar-local mafia-package-name nil
  "The name of the current package.")

(defvar-local mafia-source-buffer (list)
  "Buffer from which Mafia was first requested to start.")

(defvar-local mafia-arguments (list)
  "Arguments used to call the mafia process.")

(defvar-local mafia-targets (list)
  "Targets used for the mafia process.")

(defvar-local mafia-starting nil
  "When non-nil, indicates that the mafia process is starting.")

(defvar-local mafia-deleting nil
  "The process of the buffer is being deleted.")

(defvar-local mafia-give-up nil
  "When non-nil, give up trying to start the backend.
A true value indicates that the backend could not start, or could
not be installed.  The user will have to manually run
`mafia-restart' or `mafia-targets' to destroy the buffer and
create a fresh one without this variable enabled.")

(defvar-local mafia-callbacks (list)
  "List of callbacks waiting for output. LIST is a FIFO.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTALL

(defun mafia-installed-p ()
  "Return non-nil if mafia (of the right version) is installed in the stack environment."
  (redisplay)
  (with-temp-buffer
    (if (= 0 (call-process "mafia" nil t nil "--version"))
        'installed
        'not-installed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECT ROOTS

(defun mafia-find-project-root (&optional dir)
  (file-name-directory (mafia-cabal-find-file dir)))

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

(defun mafia-package-name (&optional cabal-file)
  "Get the current package name from a nearby .cabal file.
If there is none, return an empty string.  If specified, use
CABAL-FILE rather than trying to locate one."
  (or mafia-package-name
      (setq mafia-package-name
            (let ((cabal-file (or cabal-file
                                  (mafia-cabal-find-file))))
              (if cabal-file
                  (replace-regexp-in-string
                   ".cabal$" ""
                   (file-name-nondirectory cabal-file))
                "")))))

(defun mafia-project-root (&optional dir)
  (or mafia-project-root
      (setq mafia-project-root
        (mafia-find-project-root dir))))

(defun mafia-targets ()
  (or mafia-targets
      (setq mafia-targets
        (list (buffer-file-name (current-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPL

(defconst mafia-prompt-regexp "^\4 ") ;; this is repeated throughout, it matters, don't change it

(defvar-local mafia-repl-previous-buffer nil
  "Records the buffer to which `mafia-repl-switch-back' should jump.
This is set by `mafia-repl-buffer', and should otherwise be nil.")

(defun mafia-repl-clear-buffer ()
  "Clear the current REPL buffer."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun mafia-repl (&optional prompt-options)
  "Start up the REPL for this buffer.
If PROMPT-OPTIONS is non-nil, prompt with an options list."
  (interactive "P")
  (switch-to-buffer-other-window (mafia-repl-buffer prompt-options t)))

(defun mafia-repl-buffer (prompt-options &optional store-previous)
  "Start the REPL buffer.
If PROMPT-OPTIONS is non-nil, prompt with an options list.  When
STORE-PREVIOUS is non-nil, note the caller's buffer in
`mafia-repl-previous-buffer'."
  (let* ((root (mafia-project-root))
         (package-name (mafia-package-name))
         (targets (mafia-targets))
         (name (format "*mafia:%s:%s:repl*"
                       (file-name-nondirectory root)
                       package-name))
         (initial-buffer (current-buffer))
         (backend-buffer (mafia-buffer 'backend targets)))
    (with-current-buffer
        (if (get-buffer name)
            (get-buffer name)
          (with-current-buffer
              (get-buffer-create name)
            (cd root)
            (mafia-repl-mode)
            (mafia-repl-mode-start backend-buffer
                                   (buffer-local-value 'mafia-targets backend-buffer)
                                   prompt-options)
            (current-buffer)))
      (progn
        (when store-previous
          (setq mafia-repl-previous-buffer initial-buffer))
        (current-buffer)))))

(define-derived-mode mafia-repl-mode comint-mode "Mafia-REPL"
  "Interactive prompt for Mafia."
  (when (and (not (eq major-mode 'fundamental-mode))
             (eq this-command 'mafia-repl-mode))
    (error "You probably meant to run: M-x mafia-repl"))
  (setq-local comint-prompt-regexp mafia-prompt-regexp)
  (setq-local warning-suppress-types (cons '(undo discard-info) warning-suppress-types))
  ;; (add-hook 'comint-output-filter-functions ;; comint - makes buffer hyperlinks from repl output
  ;;           'intero-linkify-process-output
  ;;           t)
  (setq-local comint-prompt-read-only t))
  ;; (add-to-list (make-local-variable 'company-backends) 'company-intero)
  ;; (company-mode))


(defun mafia-repl-mode-start (backend-buffer targets prompt-options)
  "Start the process for the repl in the current buffer.
BACKEND-BUFFER is used for options.
TARGETS is the targets to load.
If PROMPT-OPTIONS is non-nil, prompt with an options list."
;;  (when prompt-options
;;    (intero-repl-options backend-buffer)) ;; TODO open files other than current
      (let ((process (get-buffer-process (apply #'make-comint-in-buffer "mafia" (current-buffer) "mafia" nil "quick"))))
           (when (process-live-p process)
             (set-process-query-on-exit-flag process nil)
             (message "Started Mafia process for REPL."))))

(font-lock-add-keywords
 'mafia-repl-mode
 '(("\\(\4\\)"
    (0 (prog1 ()
         (compose-region (match-beginning 1)
                         (match-end 1)
                         ?λ))))))

(defun mafia-buffer (worker &optional targets)
  "Get the WORKER buffer for the current directory."
  (let ((buffer (mafia-get-buffer-create worker)))
    (if (get-buffer-process buffer)
        buffer
      (mafia-get-worker-create worker targets (current-buffer)))))

(defun mafia-buffer-name (worker)
  "For a given WORKER, create a buffer name."
  (let* ((root (mafia-project-root))
         (package-name (mafia-package-name)))
    (concat " mafia:"
            (format "%s" worker)
            ":"
            package-name
            " "
            root)))

(defun mafia-get-worker-create (worker &optional targets source-buffer)
  "Start the given WORKER.
If provided, use the specified TARGETS and SOURCE-BUFFER."
  (let* ((buffer (mafia-get-buffer-create worker)))
    (if (get-buffer-process buffer)
        buffer
      (let ((install-status (mafia-installed-p)))
        (if (eq install-status 'installed)
            (mafia-start-process-in-buffer buffer targets source-buffer)
            (throw 'uninstalled t))))))

(defun mafia-get-buffer-create (worker)
  "Get or create the stack buffer for WORKER.
Uses the directory of the current buffer for context."
  (let* ((root (mafia-project-root))
         (cabal-file (mafia-cabal-find-file))
         (package-name (if cabal-file
                           (mafia-package-name cabal-file)
                         ""))
         (buffer-name (mafia-buffer-name worker))
         (default-directory (if cabal-file
                                (file-name-directory cabal-file)
                              root)))
    (with-current-buffer
        (get-buffer-create buffer-name)
      (setq mafia-package-name package-name)
      (cd default-directory)
      (current-buffer))))

(defun mafia-start-process-in-buffer (buffer &optional targets source-buffer)
  "Start an mafia worker in BUFFER, for the default or specified TARGETS.
Automatically performs initial actions in SOURCE-BUFFER, if specified."
  (if (buffer-local-value 'mafia-give-up buffer)
      buffer
    (let*
      ((options (mafia-make-options-list targets))
       (process (with-current-buffer buffer
                  (when mafia-debug
                    (message "Mafia arguments: %s" (combine-and-quote-strings options)))
                  (message "Booting up mafia ...")
                  (apply #'start-process "mafia" buffer "mafia" (cons "quick" options)))))
      (set-process-query-on-exit-flag process nil)
      (process-send-string process ":set -fobject-code\n")  ;; quicker reloads
      (process-send-string process ":set prompt \"\\4\"\n") ;; lines up with repl regex
      (with-current-buffer buffer
        (erase-buffer)
        (setq mafia-targets targets)
        (setq mafia-source-buffer source-buffer)
        (setq mafia-arguments options)
        (setq mafia-starting t)
        (setq mafia-callbacks
              (list (list (cons source-buffer
                                buffer)
                          (lambda (buffers _msg)
                            (let ((source-buffer (car buffers))
                                  (process-buffer (cdr buffers)))
                              (with-current-buffer process-buffer
                                (setq-local mafia-starting nil)))
;; FLYCHECK
;;                              (when source-buffer
;;                                (with-current-buffer source-buffer
;;                                  (when flycheck-mode
;;                                    (run-with-timer 0 nil
;;                                                    'intero-call-in-buffer
;;                                                    (current-buffer)
;;                                                    'intero-flycheck-buffer)))))
                            (message "Booted up mafia!"))))))
      (set-process-filter
       process
       (lambda (process string)
         (when mafia-debug
           (message "[Mafia] <- %s" string))
         (when (buffer-live-p (process-buffer process))
           (with-current-buffer (process-buffer process)
             (goto-char (point-max))
             (insert string)
             (when mafia-starting (message "Booting up mafia ..."))
             (mafia-read-buffer)))))
      (set-process-sentinel process 'mafia-sentinel)
      buffer)))

(defun mafia-make-options-list (targets)
  "Make the mafia quick options list."
  targets)

(defun mafia-read-buffer ()
  "In the process buffer, we read what's in it."
  (let ((repeat t))
    (while repeat
      (setq repeat nil)
      (goto-char (point-min))
      (when (search-forward "\4" (point-max) t 1) ;; lines up with repl regex
        (let* ((next-callback (pop mafia-callbacks))
               (state (nth 0 next-callback))
               (func (nth 1 next-callback)))
          (let ((string (strip-carriage-returns (buffer-substring (point-min) (1- (point))))))
            (if next-callback
                (progn (with-temp-buffer
                         (funcall func state string))
                       (setq repeat t))
              (when mafia-debug
                (mafia--warn "Received output but no callback in `mafia-callbacks': %S"
                      string)))))
        (delete-region (point-min) (point))))))

(defun mafia-sentinel (process change)
  "Handle when PROCESS reports a CHANGE.
This is a standard process sentinel function."
  (when (buffer-live-p (process-buffer process))
    (when (and (not (process-live-p process)))
      (let ((buffer (process-buffer process)))
        (if (with-current-buffer buffer mafia-deleting)
          (message "Mafia process deleted.")
          ;; intero tries to build all dependencies here, but our error messages will be different
          ;; see intero-unsatisfied-package-p, it's just regex
            (progn (with-current-buffer buffer (setq-local mafia-give-up t))
                   (mafia-show-process-problem process change)))))))

(defun mafia-show-process-problem (process change)
  "Report to the user that PROCESS reported CHANGE, causing it to end."
  (switch-to-buffer (process-buffer process))
  (goto-char (point-max))
  (insert "\n---\n\n")
  (insert
   (propertize
    (concat
     "This is the buffer where Emacs talks to Mafia. It's normally hidden,
but a problem occcured.

TROUBLESHOOTING

It may be obvious if there is some text above this message
indicating a problem.

The process ended. Here is the reason that Emacs gives us:

"
     "  " change
     "\n"
     "For troubleshooting purposes, here are the arguments used to launch mafia:

"
     (format "  mafia quick %s"
             (combine-and-quote-strings mafia-arguments)))
    'face 'compilation-error)))


(defun mafia--warn (message &rest args)
  "Display a warning message made from (format MESSAGE ARGS...).
Equivalent to 'warn', but label the warning as coming from mafia."
  (display-warning 'mafia (apply 'format message args) :warning))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; regex

(defun strip-carriage-returns (string)
  "Strip the \\r from Windows \\r\\n line endings in STRING."
  (replace-regexp-in-string "\r" "" string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mafia)

;;; mafia.el ends here
