(require 'go-mode)

(defun my-go-setup ()
  (setq-default indent-tabs-mode 1))

;; Turn on tabs mode.
(add-hook 'go-mode-hook 'my-go-setup t)

;; Reformat the file before saving.
(add-hook 'before-save-hook #'gofmt-before-save)

(provide 'setup-go)
