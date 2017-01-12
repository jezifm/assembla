(require 'f)
(require 'ert)
(require 'el-mock)
(eval-when-compile
    (require 'cl)) ;; for el-mock

;;; code coverage
(when (require 'undercover nil t)
  (undercover "*.el"))

(defvar assembla-support-path (f-dirname load-file-name))
(defvar assembla-features-path (f-parent assembla-support-path))
(defvar assembla-root-path (f-parent assembla-features-path))

(require 'assembla (f-expand "assembla" assembla-root-path))

(Before
 (makunbound 'foo))
