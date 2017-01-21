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

;; Constants
(defconst assembla-api "https://api.assembla.com/v1")
(defconst assembla-api-spaces "https://api.assembla.com/v1/spaces")

(defcustom assembla-buffer-name "*assembla*"
  "Name of Assembla buffer"
  :group 'assembla)

;; Variables
(defvar assembla-current-view nil "...")
(defvar assembla-last-resource nil "...")
(defvar assembla-mode-hook nil "Mode hook for `assembla-mode'.")

;; Keymaps
(defvar assembla-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'assembla-quit)
    (define-key map (kbd "p") 'assembla-prev-line)
    (define-key map (kbd "n") 'assembla-next-line)
    (define-key map (kbd "<return>") 'assembla-trigger-return)
    map)
  "Keymap for `assembla-mode'.")

(defun assembla-prev-line ()
  "Move to previous line maintaing cursor at beginning of line"
  (interactive)
  (previous-line)
  (goto-char (line-beginning-position)))

(defun assembla-next-line ()
  "Move to next line maintaing cursor at beginning of line"
  (interactive)
  (next-line)
  (goto-char (line-beginning-position)))

(defun assembla-list-view (elements)
  "Display list view of RESOURCE"
  (erase-buffer)
  (save-excursion
    (dolist (item elements nil)
      (setq line-text (format "%s\n" (plist-get item :line-text)))
      (add-text-properties 0 (length line-text) item line-text)
      (insert line-text))))

(defun assembla-trigger-return ()
  "Trigger the CALLBACK attached to :on-return key"
  (interactive)
  (funcall (get-text-property (point) ':on-return)))

(defun assembla-quit ()
  "Quit assembla buffer"
  (interactive)
  (previous-buffer))

(defun assembla-get-resource (path)
  "Get list of resource"
  (interactive)
  (setq assembla-last-resource path)
  (let ((json-object-type 'plist)
	(json-array-type 'list)
	(url-request-method "GET")
	(url-request-extra-headers
	 `(("X-Api-Key" . ,assembla-api-key)
	   ("X-Api-Secret" . ,assembla-api-secret)
	   ("Content-Type" . "json")))
	(url (format "%s%s" assembla-api path)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (json-read))))

(defun assembla-get-spaces ()
  "Retrieve assembla spaces"
  (interactive)
  (assembla-get-resource "/spaces"))

(defun assembla-get-tickets (space)
  "Get tickets on buffer"
  (interactive)
  (let* ((space-id (plist-get space ':id))
	 (tickets-path (format "/spaces/%s/tickets?per_page=100" space-id)))
    (assembla-get-resource tickets-path)))

(defun assembla-load-ticket ()
  "Open up a browser loading ticket's URL"
  (let* ((space-id (get-text-property (point) ':space_id))
	 (ticket-number (get-text-property (point) ':number))
	 (ticket-url-template "https://app.assembla.com/spaces/%s/tickets/%s")
	 (ticket-url (format ticket-url-template space-id ticket-number)))
    (browse-url ticket-url)))

(defun assembla-tickets-to-buffer ()
  "..."
  (let* ((buffer-read-only nil)
	 (space (text-properties-at (point)))
	 (space-name (plist-get space ':name))
	 (buffer-name (format "%s: %s Tickets" assembla-buffer-name space-name))	 
	 (tickets (assembla-get-tickets space))
	 (tickets (mapcar (lambda (ticket)
			    (let ((line-text (plist-get ticket ':summary)))
			      (plist-put ticket ':line-text line-text)
			      (plist-put ticket ':on-return 'assembla-load-ticket)))
			  tickets)))
    (switch-to-buffer (get-buffer-create buffer-name))
    (assembla-list-view tickets)
    (use-local-map assembla-mode-map)))

(defun assembla-spaces-to-buffer ()
  "Populate buffer with assembla spaces"
  (interactive)
  (let* ((buffer-read-only nil)
	 (spaces (assembla-get-spaces))
	 (spaces (mapcar (lambda (space)
			   (let ((line-text (plist-get space ':name)))
			     (plist-put space ':line-text line-text)
			     (plist-put space ':on-return 'assembla-tickets-to-buffer)))
			 spaces)))
    (assembla-list-view spaces)))

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
