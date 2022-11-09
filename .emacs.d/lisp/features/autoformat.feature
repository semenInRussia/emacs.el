Feature: autoformat in the org
  there are tests for autoformat-mode in the org-mode major-mode

  Scenario: type text at the beginning of a buffer
    Given a org-mode buffer
    And I activate the insert mode
    When I type "any sentence"
    Then I should see "Any"
    And I activate the command mode
  Scenario: type a new sentence
    Given a org-mode buffer
    And I activate the insert mode
    When I type "Luke, i your father.  noooo!!!!!"
    Then I should see "Noooo"
    And I activate the command mode
  Scenario: type a multi-line sentence
    Given a org-mode buffer
    And I activate the insert mode
    When I type "Skylller, i think you don't understood who I.  I am not in the danger"
    And I press "RET"
    And I type "i, am the danger, Skyller"
    Then I should see:
      """
      Skylller, i think you don't understood who I.  I am not in the danger
      i, am the danger, Skyller
      """
    And I activate the command mode
  Scenario: type a org-mode list
    Given a org-mode buffer
    And I activate the insert mode
    And I disable modern-org-mode
    When I type "Type of me:"
    And I press "RET"
    And I type "- aggressive worker"
    And I press "M-RET"
    And I type "super indolent"
    And I press "M-RET"
    And I type "[x] stupid"
    Then I should see "Aggressive"
    And I should see "Super"
    And I should see "Stupid"
    And I activate the command mode

  Scenario: type a org-mode list with label +
    Given a org-mode buffer
    And I activate the insert mode
    And I disable modern-org-mode
    When I type "+ a"
    And I press "M-RET"
    And I type "b"
    Then I should see "A"
    And I should see "B"
    And I activate the command mode

  Scenario: type org-mode heading
    Given a org-mode buffer
    And I activate the insert mode
    And I disable modern-org-mode
    When I type "* book"
    And I press "RET"
    And I type "** introduction"
    And I press "M-RET"
    And I type "main part"
    And I press "M-RET"
    And I type "conclusion"
    Then I should see "Introduction"
    And I should see "Main part"
    And I should see "Conclusion"
    And I activate the insert mode

  Scenario: type a sentence after line with org-mode heading
    Given a org-mode buffer
    And I activate the insert mode
    And I disable modern-org-mode
    When I type "* Heading"
    And I press "RET"
    And I type "first sentence.  second sentence"
    Then I should see "First"
    And I should see "Second"

  Scenario: type a text after org-mode properties
    Given a org-mode buffer
    And I disable modern-org-mode
    And I activate the insert mode
    When I type "* Heading"
    And I press "RET"
    And I create id for org-mode heading
    And I type "first sentence.  second sentece"
    Then I should see "First"
    And I should see "Second"
    And I activate the command mode

  Scenario: type a text after a list item of a nested list
    Given a org-mode buffer
    And I disable modern-org-mode
    And I activate the insert mode
    When I type "- Heading"
    And I press "RET"
    And I type "+ a"
    Then I should see "A"
    And I press "M-RET"
    And I type "b"
    And I should see "B"
    And I activate the command mode

Feature: autoformat in markdown mode
  there are tests for autoformat-mode in the markdown-mode major-mode

  Scenario: type markdown heading, first letter should be capitalizated
    Given a markdown-mode buffer
    And I activate the insert mode
    When I type "# heading"
    Then I should see "# Heading"
    When I press "RET"
    And I type "## subheading"
    Then I should see "## Subheading"
    And I activate the command mode
