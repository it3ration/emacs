;; Let's use auto-fill-mode here and use a width
;; that's reasonable for things like journaling.
(defun my-org-mode-hook ()
  (set-fill-column 65)
  (turn-on-auto-fill))

(add-hook 'org-mode-hook 'my-org-mode-hook)

(provide 'setup-org)
