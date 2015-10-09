(require 'yasnippet)

(setq site-lisp-dir (expand-file-name "lisp" user-emacs-directory))

;; Point to our snippets.
(setq yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))

;; Use snippets everywhere.
(yas-global-mode 1)

;; Disable dropdowns.
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

(provide 'setup-yasnippet)
