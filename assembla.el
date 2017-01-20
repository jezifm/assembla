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

(defvar assembla-current-view nil "...")
(defvar assembla-last-resource nil "...")
(defvar assembla-mode-hook nil "Mode hook for `assembla-mode'.")
(defvar assembla-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'assembla-quit)
    (define-key map (kbd "p") 'assembla-previous-line)
    (define-key map (kbd "n") 'assembla-forward-line)
    (define-key map (kbd "<return>") 'assembla-trigger-return)
    map)
  "Keymap for `assembla-mode'.")

(defun assembla-previous-line ()
  "Move to previous line maintaing cursor at beginning of line"
  (interactive)
  (previous-line)
  (goto-char (line-beginning-position)))

(defun assembla-next-line ()
  "Move to next line maintaing cursor at beginning of line"
  (interactive)
  (next-line)
  (goto-char (line-beginning-position-position)))

(defun assembla-pluck (property plists)
  "Pluck PROPERTY on each element in the LIST"
  (mapcar (lambda (plist) (plist-get plist property)) plists))

(defun assembla-merge (property value plists)
  "..."
  (mapcar (lambda (plist) (plist-put plist property value)) plists))

(defun assembla-add-text-properties-to-line (plist &optional pos)
  "..."
  (unless pos (setq pos (point)))
    (save-excursion
      (goto-char pos)
      (let ((beg (line-beginning-position))
	    (end (1+ (line-end-position))))
	(add-text-properties beg end plist))))

(defun assembla-list-view (collection)
  "Display list view of RESOURCE"
  (interactive "sResource path: ")
  (erase-buffer)
  (save-excursion 
    (dolist (item collection nil)
      (insert (format "%s\n" (s-join " " (list
					  (plist-get item
						     (cond
						      ((plist-member item ':name) ':name)
						      ((plist-member item ':summary) ':summary)
						      (t "Unable to find description")))))))
      (save-excursion
	(previous-line)
	(assembla-add-text-properties-to-line item)))))


(defun assembla-goto-detail-view ()
  "..."
  (interactive)
  (setq buffer-read-only nil)
  (let* ((resource (format "%s/%s" assembla-last-resource (get-text-property (point) ':id)))
	 (collection (assembla-get-resource (format "%s/tickets" resource))))
    (assembla-list-view collection)))

(defun assembla-trigger-return ()
  "Trigger the CALLBACK attached to :on-return key"
  (interactive)
  (let* ((callback (plist-get (text-properties-at (point)) ':on-return)))
    (funcall callback))

)

(defun assembla-quit ()
  "Quit assembla buffer"
  (interactive)
  (kill-buffer assembla-buffer-name))

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
	   ("Content-Type" . "json"))))
    (with-current-buffer (url-retrieve-synchronously (format "%s%s" assembla-api path))
      (goto-char (point-min))
      (re-search-forward "^$")
      (json-read))))

(defun assembla-get-spaces ()
  "Retrieve assembla spaces"
  (interactive)
  (assembla-get-resource "/spaces"))

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
    (save-excursion
      (insert (format "%s\n" name)))
    (assembla-add-text-properties-to-line space)))

(defun assembla-tickets-to-buffer ()
  "..."
  (let* ((space-id (get-text-property (point) ':space_id))
	 (ticket-number (get-text-property (point) ':number))
	 (collection (assembla-get-resource (format "/spaces/%s/tickets/%s" space-id ticket-number)))
	 (collection (assembla-merge ':on-return 'assembla-load-ticket collection))
	 (buffer-read-only nil))
    (assembla-list-view collection)))

(defun assembla-spaces-to-buffer ()
  "Populate buffer with assembla spaces"
  (interactive)
  (let* ((collection (assembla-get-resource "/spaces"))
	 (collection (assembla-merge ':on-return 'assembla-tickets-to-buffer collection)))
    (assembla-list-view collection)))

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
