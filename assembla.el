;;; assembla.el --- Support for the Assembla

;; Copyright (C) 2010-2016 Jezrael Arciaga

;; Author: Jezrael Arciaga <jezarciaga@gmail.com>
;; Keywords: languages
;; Version: 1.0
;; Package-Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defconst assembla-api "https://api.assembla.com/v1")
(defconst assembla-api-spaces "https://api.assembla.com/v1/spaces")

(defcustom assembla-buffer-name "*assembla*"
  "Name of Assembla buffer"
  :group 'assembla)

(defvar assembla-mode-hook nil
  "Mode hook for `assembla-mode'.")

(defvar assembla-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'assembla-quit)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'forward-line)
    (define-key map (kbd "<return>") 'assembla-get-tickets)
    map)
  "Keymap for `assembla-mode'.")


(defun assembla-quit ()
  "Quit assembla buffer"
  (interactive)
  (kill-buffer assembla-buffer-name))

(defun assembla-get-name (space)
  "Extract name from space object"
  (cdr (assoc 'name space)))

(defun assembla-get-spaces ()
  "Retrieve assembla spaces"
  (interactive)
  (let ((url-request-method "GET")
	(url-request-extra-headers
	 `(("X-Api-Key" . ,assembla-api-key)
	   ("X-Api-Secret" . ,assembla-api-secret)
	   ("Content-Type" . "json"))))
    (with-current-buffer (url-retrieve-synchronously assembla-api-spaces)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun assembla-get-tickets ()
  "Get tickets on buffer"
  (interactive)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "Implement me"))

(defun assembla-spaces-to-buffer ()
  "Populate buffer with assembla spaces"
  (interactive)
  (with-current-buffer assembla-buffer-name
    (let ((spaces (assembla-get-spaces)))
      (erase-buffer)
      (insert (mapconcat 'assembla-get-name spaces "\n")))))

;;;###autoload
(defun assembla ()
  "Start Assembla mode."
  (interactive)
  (switch-to-buffer (get-buffer-create assembla-buffer-name))
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (assembla-spaces-to-buffer)
  (setq buffer-read-only t)
  (setq mode-name "Assembla")
  (setq major-mode 'assembla-mode)
  (use-local-map assembla-mode-map)
  (run-mode-hooks 'assembla-mode-hook))

(provide 'assembla)

;;; assembla.el ends here
