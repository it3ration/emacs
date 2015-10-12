;; Start robe for ruby files.
(add-hook 'ruby-mode-hook 'robe-mode)

;; Make company-mode robe-aware.
(eval-after-load 'company
  '(push 'company-robe company-backends))

(provide 'setup-robe)
