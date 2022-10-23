Feature: fast insertion of a new defclass field

  Scenario: i already have typed some defclass fields and want to add a new
    Given a emacs-lisp-mode buffer
    When I insert:
      """
      (defclass band ()
        ((name :initarg :name :accessor band-name)
         (tracks-at-top :initarg :tracks-at-top :accessor band-tracks-at-top))
        "The structure for rock band.")
      """
    And I go to beginning of buffer
    And I go to end of line
    And I press "M-RET"
    And I activate the insert mode
    And I type "rating"
    Then I should see "(rating :initarg :rating :accessor band-rating)"
    And I activate the command mode
  Scenario: i have type defclass without docstring and any fields
    Given a emacs-lisp-mode buffer
    When I insert:
      """
      (defclass teacher
      )
      """
    And I go to beginning of buffer
    And I go to end of line
    And I press "M-RET"
    And I activate the insert mode
    And I type "homework-size"
    Then I should see "(homework-size :initarg :homework-size :accessor teacher-homework-size)"
    And I activate the command mode
