(Given "^I start assembla$"
       (lambda ()
	 (with-mock
	   (stub assembla-get-spaces => fixture-spaces)
	   (call-interactively 'assembla))))

(Given "^I load the following:$"
       (lambda (body)
	 (eval (car (read-from-string body)))))

(When "^I press \"\\([^\"]+\\)\"$"
      (lambda (key)
	(execute-kbd-macro (kbd key))))

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

(Then "I should see my spaces"
      (lambda ()
	(should (equal (progn
			 (goto-char (point-min))
			 (buffer-substring (point-min) (line-end-position)))
		       "Test Space"))))
