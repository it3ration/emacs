;; Our csharp-specific setup.
(defun my-csharp-mode-hook ()
  ;; (local-set-key (kbd "{") 'c-electric-brace))
  (electric-pair-mode 1))
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(provide 'setup-csharp)
