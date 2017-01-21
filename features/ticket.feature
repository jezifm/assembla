Scenario: Ticket List
Given I start assembla
When I press return
Then I should see my tickets
And the buffer should be read only
When I press return to tickets
Then I should be browsing the web
