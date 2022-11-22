(Then "^I insert a LaTeX math environment$"
  (lambda ()
    (insert "\\begin{equation}")
    (newline)
    (newline)
    (insert "\\end{equation}")
    (forward-line -1)))

(And "^I activate the my-latex-math-spaces-mode$"
  (lambda () (my-latex-math-spaces-mode 1)))
