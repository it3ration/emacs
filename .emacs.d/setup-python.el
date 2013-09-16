(add-hook 
 'python-mode-hook
 (lambda ()
   (setq tab-width 4)
   (setq python-indent 4)))

(provide 'setup-python)
