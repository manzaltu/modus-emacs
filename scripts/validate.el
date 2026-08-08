;;; validate.el --- Validate the Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Batch validation driver used by scripts/validate.  Byte compiles each file
;; given on the command line in a clean environment matching flycheck's
;; emacs-lisp checker, prints all warnings and errors to stdout, and exits
;; non-zero if there are any.

;;; Code:

(require 'bytecomp)

;; Match the environment of flycheck's emacs-lisp checker
(setq package-user-dir (expand-file-name "~/.emacs.d/elpa")
      no-native-compile t
      enable-local-eval nil
      enable-local-variables :safe)
(with-demoted-errors "Error during package initialization: %S"
  (package-initialize))

(defvar mo-validate--repo
  (expand-file-name ".." (file-name-directory load-file-name))
  "Repository root directory.")

(defun mo-validate--compile-findings (file)
  "Byte compile FILE and return a list of warning and error lines."
  (let* ((dest (make-temp-file "mo-validate-"))
         (byte-compile-root-dir mo-validate--repo)
         (byte-compile-dest-file-function (lambda (_source) dest))
         (findings nil))
    (with-current-buffer (get-buffer-create byte-compile-log-buffer)
      (erase-buffer))
    (condition-case err
        (let ((inhibit-message t))
          (byte-compile-file file))
      (error (push (format "%s: Error: %s" file (error-message-string err))
                   findings)))
    (ignore-errors (delete-file dest))
    (with-current-buffer byte-compile-log-buffer
      (goto-char (point-min))
      (while (re-search-forward
              "^.*:[0-9]+:[0-9]+: \\(?:Warning\\|Error\\):.*$" nil t)
        (push (match-string 0) findings)))
    (nreverse findings)))

(let ((findings (mapcan #'mo-validate--compile-findings command-line-args-left)))
  (dolist (finding findings)
    (princ (concat finding "\n")))
  (kill-emacs (if findings 1 0)))

;;; validate.el ends here
