;; my specific csharp-mode settings
(defun it3ration-csharp-style ()
  (local-set-key (kbd "{") 'c-electric-brace))

(add-hook 'csharp-mode-hook 'it3ration-csharp-style)

(provide 'setup-csharp)
