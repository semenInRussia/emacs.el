Feature: automatically insertion of spaces in a LaTeX math environment
  Scenario: type a LaTeX command
    Given a LaTeX-mode buffer
    And I activate the insert mode
    And I activate the my-latex-math-spaces-mode
    And I insert a LaTeX math environment
    When I type "2\LaTeX"
    Then I should see "2 \LaTeX"
    And I activate the command mode

  Scenario: type \\
    Given a LaTeX-mode buffer
    And I activate the insert mode
    And I activate the my-latex-math-spaces-mode
    When I insert a LaTeX math environment
    And I type "1+1=2\\"
    Then I should see "\\"
    And I activate the command mode

  Scenario: binary operation
    Given a LaTeX-mode buffer
    And I activate the insert mode
    And I activate the my-latex-math-spaces-mode
    And I insert a LaTeX math environment
    When I type "a+b-x\pm1\cdot2\times6=c"
    Then I should see "a + b - x \pm 1 \cdot 2 \times 6 = c"
    And I activate the command mode

  Scenario: parens
    Given a LaTeX-mode buffer
    And I activate the insert mode
    And I activate the my-latex-math-spaces-mode
    And I insert a LaTeX math environment
    When I type "a(a+b)"
    Then I should see "a (a + b)"
    And I activate the command mode

  Scenario: type backslash with initial spaces
    Given a LaTeX-mode buffer
    And I activate the insert mode
    And I activate the my-latex-math-spaces-mode
    And I insert a LaTeX math environment
    When I type "2      \LaTeX"
    Then I should see "2 \LaTeX"
    And I activate the command mode
