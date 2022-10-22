
Feature: autoformat in the org
  there are tests for autoformat-mode in the org-mode major-mode

  Scenario: type text at the beginning of a buffer, first letter should be capitalizated
    Given a org-mode buffer
    And I activate the insert mode
    When I type "george"
    And I activate the command mode
    Then I should see "George"

  Scenario: type a new sentence, first letter should be capitalizated
    Given a org-mode buffer
    And I activate the insert mode
    When I type "so, I already ready for read.  ready!"
    And I activate the command mode
    Then I should see "So, I already ready for read.  Ready!"

  Scenario: type a multi-line sentence, first letter of line shouldn't be capitalizated
    Given a org-mode buffer
    And I activate the insert mode
    When I type "for example, this is dog, this explained as the theory of"
    And I press "RET"
    And I type "the Newton third (nigerian king), because "
    And I activate the command mode
    Then I should see:
      """
      For example, this is dog, this explained as the theory of the Newton
      third (nigerian king), because 
      """

  Scenario: type a org-mode list, first letter of item should be capitalizated
    Given a org-mode buffer
    And I activate the insert mode
    And I disable modern-org-mode
    When I type "Presidents:"
    And I press "RET"
    And I type "- bush"
    And I press "M-RET"
    And I type "obama"
    And I press "M-RET"
    And I press "tramp"
    And I press "M-RET"
    And I press "[x] biden"
    And I activate the command mode
    Then I should see:
      """
      Presidents:
      - Bush
      - Obama
      - Tramp
      - [x]Biden
      """

  Scenario: type a org-mode list with label +,
    Given a org-mode buffer
    And I activate the insert mode
    And I disable modern-org-mode
    And I type "+ a"
    And I press "M-RET"
    And I type "b"
    And I activate the command mode
    Then I should see:
      """
      + A
      + B
      """

  Scenario: type org-mode heading, first letter should be capitalizated
    Given a org-mode buffer
    And I activate the insert mode
    And I disable modern-org-mode
    When I press "M-RET"
    And I type "main config"
    Then I should see "* Main config"
    And I press "RET"
    And I type "** config of a shit feature"
    Then I should see "** Config of a shit feature"
    When I press "M-RET"
    And I type "TODO config of me"
    Then I should see "Config of me"
    
