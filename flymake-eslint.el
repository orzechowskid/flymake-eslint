;;; flymake-eslint.el --- An eslint Flymake backend    -*- lexical-binding: t; -*-
;;; Summary:
;;; Commentary:
;;; License: MIT
;;; Code:

;; our own customization group

(defgroup flymake-eslint nil
  "Flymake checker for Javascript using eslint"
  :group 'programming
  :prefix "flymake-eslint-")


;; some useful variables

(defcustom flymake-eslint-executable-name "eslint_d"
  "Name of executable to run when checker is called.  Must be present in `exec-path`."
  :type 'string
  :group 'flymake-eslint)


;; some internal variables

(defvar flymake-eslint--filename "##flymake-eslint.js"
  "Internal variable.  Name of the temporary file on which to run eslint.")


;; some internal functions

(defun flymake-eslint--ensure-binary-exists ()
  "Internal function.  Throws an error if `flymake-eslint-executable-name` can't be found on `exec-path`."

  (unless (executable-find flymake-eslint-executable-name)
    (error (message "can't find '%s' in exec-path - try M-x set-variable flymake-eslint-executable-name maybe?" flymake-eslint-executable-name))))


(defun flymake-eslint--create-temp-file-from-buffer (source-buffer)
  "Internal function.  Create a temporary file containing contents of SOURCE-BUFFER, and return its name."

  ;; TODO: there's probably a better way to do all this
  (with-current-buffer source-buffer
    ;; save the contents of `source-buffer` as a string
    (let ((buffer-text (buffer-string))
          (temp-file-name flymake-eslint--filename))
      ;; create the new temp file
      (with-temp-file temp-file-name
        (insert buffer-text))
      ;; return the name of the temp file
      (identity temp-file-name))))


(defun flymake-eslint--check (source-buffer destination-buffer)
  "Internal function.  Run eslint on contents of SOURCE-BUFFER and write the results to DESTINATION-BUFFER."

  ;; eslint might report incorrect row/column numbers for unsaved buffers, so we write
  ;; the current buffer to a temp file and process that instead.  Use a file in the
  ;; current directory, not the system's temp directory, in case .eslintrc or other
  ;; path-sensitive tools like babel are applied to this file
  (let* ((temp-file-name (flymake-eslint--create-temp-file-from-buffer source-buffer))
         (process-args (format "--ignore-pattern '!%s' %s" temp-file-name temp-file-name)))
    (call-process flymake-eslint-executable-name nil destination-buffer t process-args)))


(defun flymake-eslint--report (source-buffer report-buffer flymake-report-fn)
  "Internal function.  Create Flymake diag messages from contents of REPORT-BUFFER, and report them against SOURCE-BUFFER via FLYMAKE-REPORT-FN."

  (let ((results '()))
    (with-current-buffer report-buffer
      ;; start at the top and check each line for an eslint message
      (goto-char (point-min))
      (while (not (eobp))
        (if (re-search-forward "^[[:space:]]*\\([0-9]+\\):\\([0-9]+\\)[[:space:]]+\\(warning\\|error\\)[[:space:]]+\\(.*\\)$" nil t)
            (let* ((row (string-to-number (match-string 1)))
                   (column (string-to-number (match-string 2)))
                   (src-pos (flymake-diag-region source-buffer row column))
                   (type (if (string-match "^warning" (match-string 3))
                             :warning
                           :error))
                   (msg (match-string 4)))
              ;; new Flymake diag message
              (push (flymake-make-diagnostic source-buffer (car src-pos) (cdr src-pos) type msg) results)))
        (forward-line 1))
      ;; report
      (funcall flymake-report-fn results))))


(defun flymake-eslint--check-and-report (source-buffer flymake-report-fn)
  "Internal function.  Run eslint against SOURCE-BUFFER and use FLYMAKE-REPORT-FN to report results."

  (with-temp-buffer
    (flymake-eslint--check source-buffer (current-buffer))
    (flymake-eslint--report source-buffer (current-buffer) flymake-report-fn)))


(defun flymake-eslint--run (flymake-report-fn &rest ignored)
  "Call eslint and pass results to FLYMAKE-REPORT-FN.  All other parameters are currently IGNORED."

  (flymake-eslint--ensure-binary-exists)
  (flymake-eslint--check-and-report (current-buffer) flymake-report-fn))


;; module entry point

(defun flymake-eslint-enable ()
  "Add flymake-eslint as a buffer-local Flymake backend."
  (interactive)
  (add-hook 'flymake-diagnostic-functions 'flymake-eslint--run nil t))

(provide 'flymake-eslint)

;;; flymake-eslint.el ends here
