;;; flymake-eslint.el --- An eslint Flymake backend    -*- lexical-binding: t; -*-

;;; Version: 1.0.0

;;; Author: Dan Orzechowski

;;; URL: https://github.com/orzechowskid/flymake-eslint

;;; Package-Requires: ((emacs "26.0"))

;;; Commentary:
;; A backend for Flymake which uses eslint.  Enable flymake-mode with `M-x flymake-mode [RET]', then enable this backend with `M-x flymake-eslint-enable [RET]'.
;; Alternately, configure a mode-hook for your Javascript major mode of choice:
;;
;; (add-hook 'some-js-major-mode-hook
;;   (lambda () (flymake-mode) (flymake-eslint-enable))
;;
;; A handful of configurable options can be found in the `flymake-eslint' customization group: view and modify them with the command `M-x customize-group [RET] flymake-eslint [RET]'.

;;; License: MIT

;;; Code:


;; our own customization group


(defgroup flymake-eslint nil
  "Flymake checker for Javascript using eslint"
  :group 'programming
  :prefix "flymake-eslint-")


;; useful variables


(defcustom flymake-eslint-executable-name "eslint"
  "Name of executable to run when checker is called.  Must be present in variable `exec-path'."
  :type 'string
  :group 'flymake-eslint)


(defcustom flymake-eslint-executable-args ""
  "Extra arguments to pass to eslint."
  :type 'string
  :group 'flymake-eslint)


;; internal variables


(defvar flymake-eslint--filename ".##flymake-eslint.js"
  "Internal variable.
Name of the temporary file on which to run eslint.")


(defvar flymake-eslint--message-regex "^[[:space:]]*\\([0-9]+\\):\\([0-9]+\\)[[:space:]]+\\(warning\\|error\\)[[:space:]]+\\(.+?\\)[[:space:]]\\{2,\\}\\(.*\\)$"
  "Internal variable.
Regular expression definition to match eslint messages.")


;; internal functions


(defun flymake-eslint--ensure-binary-exists ()
  "Internal function.
Throw an error if `flymake-eslint-executable-name' can't be found on variable `exec-path'"
  (unless (executable-find flymake-eslint-executable-name)
    (error (message "can't find '%s' in exec-path - try M-x set-variable flymake-eslint-executable-name maybe?" flymake-eslint-executable-name))))


(defun flymake-eslint--create-temp-file-from-buffer (source-buffer)
  "Internal function.
Create a temporary file containing contents of SOURCE-BUFFER, and return its name."
  ;; TODO: there's probably a better way to do all this
  (with-current-buffer source-buffer
    ;; save the contents of `source-buffer' as a string
    (let ((buffer-text (buffer-string))
          (temp-file-name flymake-eslint--filename))
      ;; create the new temp file
      (with-temp-file temp-file-name
        (insert buffer-text))
      ;; return the name of the temp file
      (identity temp-file-name))))


(defun flymake-eslint--report (source-buffer)
  "Internal function.
Create Flymake diag messages from contents of current buffer, as reported against SOURCE-BUFFER."
  ;; start at the top and check each line for an eslint message
  (goto-char (point-min))
  (if (looking-at "Error:")
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
	         (msg-text (format "%s: %s [%s]" type msg lint-rule))
                 (type-symbol (if (string-equal "warning" type) :warning :error))
                 (src-pos (flymake-diag-region source-buffer row column)))
            ;; new Flymake diag message
            (push (flymake-make-diagnostic source-buffer (car src-pos) (cdr src-pos) type-symbol msg-text) results)))
        (forward-line 1))
      results)))


(defun flymake-eslint--check-and-report (source-buffer flymake-report-fn)
  "Internal function.
Run eslint against SOURCE-BUFFER and use FLYMAKE-REPORT-FN to report results."
  (with-temp-buffer
    ;; eslint might report incorrect row/column numbers for unsaved buffers, so we
    ;; write the current buffer to a temp file and process that instead.  Use a file
    ;; in the current directory, not the system's temp directory, in case .eslintrc or
    ;; other path-sensitive tools like babel are applied to this file
    ;; TODO I think that can be solved with --stdin and --stdin-filename ?
    (let ((temp-file-name (flymake-eslint--create-temp-file-from-buffer source-buffer)))
      (call-process flymake-eslint-executable-name nil t t "--no-ignore" "--no-color" temp-file-name)
      (funcall flymake-report-fn (flymake-eslint--report source-buffer))
      (delete-file temp-file-name))))


(defun flymake-eslint--checker (flymake-report-fn &rest ignored)
  "Internal function.
Run eslint on the current buffer, and report results using FLYMAKE-REPORT-FN.  All other parameters are currently IGNORED."
  (flymake-eslint--ensure-binary-exists)
  (flymake-eslint--check-and-report (current-buffer) flymake-report-fn))


;; module entry point


(defun flymake-eslint-enable ()
  "Enable `flymake-mode', and add flymake-eslint as a buffer-local Flymake backend."
  (interactive)
  (flymake-mode t)
  (add-hook 'flymake-diagnostic-functions 'flymake-eslint--checker nil t))


(provide 'flymake-eslint)


;;; flymake-eslint.el ends here
