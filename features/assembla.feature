Feature: Assembla

Scenario: Assembla mode
Given I start assembla
Then I should be in assembla mode

Scenario: Assembla buffer
Given I start assembla
Then I should be in buffer "*assembla*"
And the buffer should be read only

Scenario: Mode hook
Given I load the following:
  """
  (add-hook 'assembla-mode-hook
    (lambda () (setq foo "bar")))
  """
Then the variable "foo" should be undefined
When I start assembla
Then the variable "foo" should have value "bar"

Scenario: Quit Assembla mode
Given I start assembla
Then I should be in assembla mode
When I press "q"
Then I should not be in assembla mode

Scenario: Spaces List
Given I start assembla
Then I should see my spaces

Scenario: Ticket List
Given I start assembla
When I press return
Then I should see my tickets
When I press return to tickets
Then I should be browsing the web
