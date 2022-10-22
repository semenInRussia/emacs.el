
(Given "^a org-mode buffer$"
  (lambda ()
    (switch-to-buffer (get-buffer-create "*org-mode*"))
    (erase-buffer)
    (org-mode)))

(And "^I activate the insert mode$"
  (lambda () (xah-fly-insert-mode-activate)))

(And "^I activate the command mode$"
  (lambda () (xah-fly-command-mode-activate)))

(And "^I print the buffer content$"
  (lambda () (message "---\n%s\n---" (buffer-string))))

(Given "^I disable modern-org-mode$"
  (lambda () (org-modern-mode 0)))
