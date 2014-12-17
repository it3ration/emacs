;; my custom gams emacs theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(condition-case nil 
    (load-theme 'gams t)
  (wrong-number-of-arguments (load-theme 'gams)))

(provide 'setup-theme)

