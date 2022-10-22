
(Given "^a \\(.*\\) buffer$"
  (lambda (mode)
    (switch-to-buffer (get-buffer-create (concat "*" mode "*")))
    (erase-buffer)
    (funcall (intern mode))))

(And "^I activate the insert mode$"
  (lambda () (xah-fly-insert-mode-activate)))

(And "^I activate the command mode$"
  (lambda () (xah-fly-command-mode-activate)))

(And "^I print the buffer content$"
  (lambda () (message "---\n%s\n---" (buffer-string))))

(Given "^I disable modern-org-mode$"
  (lambda () (org-modern-mode 0)))
