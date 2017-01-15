(require 'f)
(require 'ert)
(require 'el-mock)
(eval-when-compile
    (require 'cl)) ;; for el-mock

;;; code coverage
(when (require 'undercover nil t)
  (undercover "*.el"))

;; helper function
(defun get-line-at-pos (pos)
  "Get line string at POS"
  (interactive "n")
  (save-excursion
    (goto-char pos)
    (buffer-substring (line-beginning-position) (line-end-position))))

(defvar assembla-support-path (f-dirname load-file-name))
(defvar assembla-features-path (f-parent assembla-support-path))
(defvar assembla-root-path (f-parent assembla-features-path))
(require 'assembla (f-expand "assembla" assembla-root-path))

(Before
 (makunbound 'foo)
 (setq fixture-spaces (read (f-read "test-fixtures/spaces.el"))))
