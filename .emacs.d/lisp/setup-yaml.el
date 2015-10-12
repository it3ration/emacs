(require 'yaml-mode)

;; Associate the following files with yaml-mode.
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(provide 'setup-yaml)
