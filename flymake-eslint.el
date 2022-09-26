;;; flymake-eslint.el --- A Flymake backend for Javascript using eslint  -*- lexical-binding: t; -*-

;; Version: 1.6.0
;; Author: Dan Orzechowski
;; Contributor: Terje Larsen
;; URL: https://github.com/orzechowskid/flymake-eslint
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, tools

;;; Commentary:

;; A backend for Flymake which uses eslint.  Enable it with M-x
;; flymake-eslint-enable RET.  Alternately, configure a mode-hook for your
;; Javascript major mode of choice:

;; (add-hook 'some-js-major-mode-hook #'flymake-eslint-enable)

;; A handful of configurable options can be found in the flymake-eslint
;; customization group: view and modify them with the M-x customize-group RET
;; flymake-eslint RET.

;; License: MIT

;;; Code:

;;;; Customization

(defgroup flymake-eslint nil
  "Flymake checker for Javascript using eslint."
  :group 'programming
  :prefix "flymake-eslint-")

(defcustom flymake-eslint-executable-name "eslint"
  "Name of executable to run when checker is called.
Must be present in variable `exec-path'."
  :type 'string
  :group 'flymake-eslint)

(defcustom flymake-eslint-executable-args nil
  "Extra arguments to pass to eslint."
  :type '(choice string (repeat string))
  :group 'flymake-eslint)

(defcustom flymake-eslint-show-rule-name t
  "When non-nil show eslint rule name in flymake diagnostic."
  :type 'boolean
  :group 'flymake-eslint)

(defcustom flymake-eslint-defer-binary-check nil
  "Defer the eslint binary presence check.
When non-nil, the initial check, which ensures that eslint binary
is present, is disabled.  Instead, this check is performed during
backend execution.

Useful when the value of variable `exec-path' is set dynamically
and the location of eslint might not be known ahead of time."
  :type 'boolean
  :group 'flymake-eslint)

(defcustom flymake-eslint-project-root nil
  "Buffer-local.
Set to a filesystem path to use that path as the current working
directory of the linting process."
  :type 'string
  :group 'flymake-eslint)

;;;; Variables

(defvar flymake-eslint--message-regex "^[[:space:]]*\\([0-9]+\\):\\([0-9]+\\)[[:space:]]+\\(warning\\|error\\)[[:space:]]+\\(.+?\\)[[:space:]]\\{2,\\}\\(.*\\)$"
  "Regexp to match eslint messages.")

(defvar-local flymake-eslint--process nil
  "Handle to the linter process for the current buffer.")

;;;; Functions

;;;;; Public

;;;###autoload
(defun flymake-eslint-enable ()
  "Enable Flymake and flymake-eslint.
Add this function to some js major mode hook."
  (interactive)
  (unless flymake-eslint-defer-binary-check
    (flymake-eslint--ensure-binary-exists))
  (make-local-variable 'flymake-eslint-project-root)
  (flymake-mode t)
  (add-hook 'flymake-diagnostic-functions 'flymake-eslint--checker nil t))

;;;;; Private

(defun flymake-eslint--executable-args ()
  "Get additional arguments for `flymake-eslint-executable-name'.
Return `flymake-eslint-executable-args' value and ensure that
this is a list."
  (if (listp flymake-eslint-executable-args)
      flymake-eslint-executable-args
    (list flymake-eslint-executable-args)))

(defun flymake-eslint--ensure-binary-exists ()
  "Ensure that `flymake-eslint-executable-name' exists.
Otherwise, throw an error and tell Flymake to disable this
backend if `flymake-eslint-executable-name' can't be found in
variable `exec-path'"
  (unless (executable-find flymake-eslint-executable-name)
    (error (message "can't find '%s' in exec-path - try M-x set-variable flymake-eslint-executable-name maybe?" flymake-eslint-executable-name))))

(defun flymake-eslint--report (eslint-stdout-buffer source-buffer)
  "Create Flymake diag messages from contents of ESLINT-STDOUT-BUFFER.
They are reported against SOURCE-BUFFER.  Return a list of
results."
  (with-current-buffer eslint-stdout-buffer
    ;; start at the top and check each line for an eslint message
    (goto-char (point-min))
    (if (looking-at-p "Error:")
        (let ((diag (flymake-make-diagnostic source-buffer (point-min) (point-max) :error (thing-at-point 'line t))))
          ;; ehhhhh point-min and point-max here are of the eslint output buffer
          ;; containing the error message, not source-buffer
          (list diag))
      (let ((results '()))
        (while (not (eobp))
          (when (looking-at flymake-eslint--message-regex)
            (let* ((row (string-to-number (match-string 1)))
                   (column (string-to-number (match-string 2)))
                   (type (match-string 3))
                   (msg (match-string 4))
                   (lint-rule (match-string 5))
	           (msg-text (if flymake-eslint-show-rule-name
                                 (format "%s: %s [%s]" type msg lint-rule)
                               (format "%s: %s" type msg)))
                   (type-symbol (if (string-equal "warning" type) :warning :error))
                   (src-pos (flymake-diag-region source-buffer row column)))
              ;; new Flymake diag message
              (push (flymake-make-diagnostic source-buffer
                                             (car src-pos)
                                             ;; buffer might have changed size
                                             (min (buffer-size source-buffer) (cdr src-pos))
                                             type-symbol
                                             msg-text
                                             (list :rule-name lint-rule))
                    results)))
          (forward-line 1))
        results))))

;; Heavily based on the example found at
;; https://www.gnu.org/software/emacs/manual/html_node/flymake/An-annotated-example-backend.html
(defun flymake-eslint--create-process (source-buffer callback)
  "Create linter process for SOURCE-BUFFER.
CALLBACK is invoked once linter has finished the execution.
CALLBACK accepts a buffer containing stdout from linter as its
argument."
  (when (process-live-p flymake-eslint--process)
    (kill-process flymake-eslint--process))
  (let ((default-directory (or flymake-eslint-project-root default-directory)))
    (setq flymake-eslint--process
          (make-process
           :name "flymake-eslint"
           :connection-type 'pipe
           :noquery t
           :buffer (generate-new-buffer " *flymake-eslint*")
           :command `(,flymake-eslint-executable-name "--no-color" "--no-ignore" "--stdin" "--stdin-filename" ,(buffer-file-name source-buffer) ,@(flymake-eslint--executable-args))
           :sentinel (lambda (proc &rest ignored)
                       ;; do stuff upon child process termination
                       (when (and (eq 'exit (process-status proc))
                                  ;; make sure we're not using a deleted buffer
                                  (buffer-live-p source-buffer)
                                  ;; make sure we're using the latest lint process
                                  (with-current-buffer source-buffer (eq proc flymake-eslint--process)))
                         ;; read from eslint output then destroy temp buffer when done
                         (let ((proc-buffer (process-buffer proc)))
                           (funcall callback proc-buffer)
                           (kill-buffer proc-buffer))))))))

(defun flymake-eslint--check-and-report (source-buffer flymake-report-fn)
  "Internal function.
Run eslint against SOURCE-BUFFER and use FLYMAKE-REPORT-FN to report results."
  (if flymake-eslint-defer-binary-check
      (flymake-eslint--ensure-binary-exists))
  (flymake-eslint--create-process
   source-buffer
   (lambda (eslint-stdout)
     (funcall flymake-report-fn (flymake-eslint--report eslint-stdout source-buffer))))
  (with-current-buffer source-buffer
    (process-send-string flymake-eslint--process (buffer-string))
    (process-send-eof flymake-eslint--process)))

(defun flymake-eslint--checker (flymake-report-fn &rest ignored)
  "Internal function.
Run eslint on the current buffer, and report results using FLYMAKE-REPORT-FN.  All other parameters are currently IGNORED."
  (flymake-eslint--check-and-report (current-buffer) flymake-report-fn))

;;;; Footer

(provide 'flymake-eslint)

;;; flymake-eslint.el ends here
