(defun it3ration-graphviz-style ())
  ;; (electric-indent-mode -1))

(add-hook 'graphviz-dot-mode-hook 'it3ration-graphviz-style)

(provide 'setup-graphviz)
