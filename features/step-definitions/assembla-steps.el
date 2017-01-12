;; fix (undefined) - Symbol's function definition is void: with-mock
(require 'el-mock)
(eval-when-compile
  (require 'cl))

(Given "^I start assembla$"
       (lambda ()
	 (with-mock
	   (stub assembla-get-spaces => (read (f-read "fixture-spaces.el")))
	   (call-interactively 'assembla))))

(Given "^I load the following:$"
       (lambda (body)
	 (eval (car (read-from-string body)))))


(When "^I press \"\\([^\"]+\\)\"$"
      (lambda (key)
	(execute-kbd-macro key)))

(Then "^I should be in assembla mode$"
      (lambda ()
	(cl-assert (equal major-mode 'assembla-mode))
	(cl-assert (equal mode-name "Assembla"))))

(Then "^I should be in buffer \"\\([^\"]+\\)\"$"
      (lambda (arg)
	(cl-assert (equal (buffer-name (current-buffer)) arg))))

(Then "^the buffer should be read only$" ;
      (lambda ()
	(cl-assert buffer-read-only)))

(Then "^the variable \"\\([^\"]+\\)\" should be undefined$"
      (lambda (variable-name)
	(cl-assert (not (boundp (intern variable-name))))))

(Then "^the variable \"\\([^\"]+\\)\" should have value \"\\([^\"]+\\)\"$"
      (lambda (variable-name value)
	(cl-assert (equal (eval (intern variable-name)) value) value)))

(Then "I should not be in assembla mode"
      (lambda ()
	(cl-assert (not (equal major-mode 'assembla-mode)))))

(Then "I should see my spaces"
      (lambda ()
	(cl-assert (not (equal (count-lines (point-min) (point-max)) 4)))))
