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
    (define-key map (kbd "<return>") 'assembla-trigger-return)
    map)
  "Keymap for `assembla-mode'.")

(defun assembla-trigger-return ()
  "Trigger the CALLBACK attached to :on-return key"
  (interactive)
  (let ((callback (get-text-property (point) ':on-return)))
    (funcall callback)))

(defun assembla-quit ()
  "Quit assembla buffer"
  (interactive)
  (kill-buffer assembla-buffer-name))

(defun assembla-get-name (space)
  "Extract name from space object"
  (plist-get space ':name))

(defun assembla-get-resource (path)
  "Get list of resource"
  (interactive)
  (let ((json-object-type 'plist)
	(json-array-type 'list)
	(url-request-method "GET")
	(url-request-extra-headers
	 `(("X-Api-Key" . ,assembla-api-key)
	   ("X-Api-Secret" . ,assembla-api-secret)
	   ("Content-Type" . "json"))))
    (with-current-buffer (url-retrieve-synchronously (format "%s%s" assembla-api path))
      (goto-char (point-min))
      (re-search-forward "^$")
      (json-read))))

(defun assembla-get-spaces ()
  "Retrieve assembla spaces"
  (interactive)
  (assembla-get-resource "/spaces"))

(defun assembla-get-ticket-desc (ticket)
  "Return description of ticket"
  (plist-get ticket ':summary))

(defun assembla-get-tickets ()
  "Get tickets on buffer"
  (interactive)
  (setq buffer-read-only nil)
  (save-excursion
    (let* ((space (text-properties-at (point)))
	   (space-id (plist-get space ':id))
	   (tickets (assembla-get-resource (format "/spaces/%s/tickets" space-id))))
      (erase-buffer)
      (dolist (ticket tickets nil) (assembla-insert-ticket ticket)))))

(defun assembla-load-ticket ()
  "Load TICKET"
  (let ((ticket-space_id (get-text-property (point) ':space_id))
	(ticket-number (get-text-property (point) ':number)))
    (browse-url (format "https://app.assembla.com/spaces/%s/tickets/%s" ticket-space_id ticket-number))))

(defun assembla-insert-ticket (ticket)
  "Insert TICKET to buffer"
  (let ((summary (plist-get ticket ':summary)))
    (insert summary)
    (let ((beg (line-beginning-position))
	  (end (line-end-position)))
      (set-text-properties beg end ticket)
      (add-text-properties beg end '(:on-return assembla-load-ticket)))
    (insert "\n")))

(defun assembla-insert-space (space)
  "Insert SPACE to buffer"
  (let ((name (plist-get space ':name)))
    (insert name)
    (let ((beg (line-beginning-position))
	  (end (line-end-position)))
      (set-text-properties beg end space)
      (add-text-properties beg end '(:on-return assembla-get-tickets)))
    (insert "\n")))

(defun assembla-spaces-to-buffer ()
  "Populate buffer with assembla spaces"
  (interactive)
  (with-current-buffer assembla-buffer-name
    (save-excursion
      (let ((spaces (assembla-get-spaces)))
	(erase-buffer)
	(dolist (space spaces nil) (assembla-insert-space space))))))

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
