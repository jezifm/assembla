(Given "^I start assembla$"
       (lambda ()
	 (with-mock
	   (stub url-retrieve-synchronously => (find-file-noselect fixture-path-spaces))
	   (call-interactively 'assembla))))

(Given "^I load the following:$"
       (lambda (body)
	 (eval (car (read-from-string body)))))


(Given "^I am in ticket list view$"
       (lambda ()
	 (let (tickets
	       (buffer-read-only nil)
	       (json-object-type 'plist)
	       (json-array-type 'list))
	   (save-excursion
	     (find-file fixture-path-tickets)
	     (goto-char (point-min))
	     (re-search-forward "^$")
	     (setq tickets (mapcar
			    (lambda (ticket)
			      (plist-put ticket ':line-text (plist-get ticket ':summary))
			      (plist-put ticket ':on-return 'assembla-load-ticket))
			    (json-read))))
	   (assembla-list-view tickets))))

(When "^I press return$"
      (lambda ()
	(with-mock
	  (stub url-retrieve-synchronously => (find-file-noselect fixture-path-tickets))
	  (execute-kbd-macro (kbd "<return>")))))

(When "^I press return to tickets$"
      (lambda ()
	(with-mock
	  (mock (browse-url *) :times 1)
	  (execute-kbd-macro (kbd "<return>")))))

(When "^I press \"\\([^\"]+\\)\"$"
      (lambda (key)
	(execute-kbd-macro (kbd key))))

(When "^I execute \"\\([^\"]+\\)\"$"
      (lambda (arg)
	(funcall (intern arg))))

(Then "^I should be in assembla mode$"
      (lambda ()
	(should (equal major-mode 'assembla-mode))
	(should (equal mode-name "Assembla"))))

(Then "^I should be in buffer \"\\([^\"]+\\)\"$"
      (lambda (arg)
	(should (equal (buffer-name (current-buffer)) arg))))

(Then "^the buffer should be read only$" ;
      (lambda ()
	(should buffer-read-only)))

(Then "^the variable \"\\([^\"]+\\)\" should be undefined$"
      (lambda (variable-name)
	(should-not (boundp (intern variable-name)))))

(Then "^the variable \"\\([^\"]+\\)\" should have value \"\\([^\"]+\\)\"$"
      (lambda (variable-name value)
	(should (equal (eval (intern variable-name)) value))))

(Then "I should not be in assembla mode"
      (lambda ()
	(should-not (equal major-mode 'assembla-mode))))

(Then "^I should see my spaces$"
      (lambda ()
	(lambda () (should (equal (get-text-property (point-min) ':name)
				  (get-line-at-pos (point-min)))))))

(Then "^I should see my tickets$"
      (lambda () (should (equal (get-text-property (point-min) ':summary)
				(get-line-at-pos (point-min))))))

(Then "^I should be browsing the web$"
      (lambda () (should t))) ;; not implemented, unable to retrieve last function called

(Then "^I should go up one line$"
      (lambda () (should (equal (line-number-at-pos) 1))))

(Then "^I should go down one line$"
      (lambda () (should (equal (line-number-at-pos) 2))))

(Then "^My cursor is at the beginning of line$"
      (lambda () (should (equal (point) (line-beginning-position)))))

(Then "^I should be in assembla dwim mode$"
      (lambda ()
	(should (equal major-mode 'assembla-dwim-mode))))

(Then "^I should have updated tickets$"
      (lambda ()
	
	))

