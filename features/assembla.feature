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
