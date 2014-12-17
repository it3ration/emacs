(require 'yasnippet)

;; only use our own snippets
(setq yas-snippet-dirs "~/.emacs.d/snippets")

;; use snippets everywhere
(yas-global-mode 1)

;; disable dropdowns
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

(provide 'setup-yasnippet)
