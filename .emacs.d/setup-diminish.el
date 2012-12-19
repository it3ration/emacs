(require 'diminish)

;; minor modes
(diminish 'yas-minor-mode "yas")
(diminish 'undo-tree-mode "ut")
(diminish 'auto-complete-mode "ac")
(diminish 'abbrev-mode)

;; todo: fix these!
;; (diminish 'auto-fill-mode)
;; (diminish 'visual-line-mode)
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (diminish 'org-indent-mode))
;;           t)

;; major modes
(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "el")))
(add-hook 'c++-mode-hook (lambda() (setq mode-name "c++")))
(add-hook 'lua-mode-hook (lambda() (setq mode-name "lua")))
(add-hook 'org-mode-hook (lambda() (setq mode-name "org")))

(provide 'setup-diminish)
