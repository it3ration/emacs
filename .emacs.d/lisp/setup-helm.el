(require 'helm)
(require 'helm-config)
(helm-mode 1)

;; use C-c h as helm's prefix
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; use helm's meta-x
(global-set-key (kbd "M-x") 'helm-M-x)

;; use helm's kill ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; let's use helm-mini
(global-set-key (kbd "C-x b") 'helm-mini)

;; open helm in the current pane only
(setq helm-split-window-in-side-p t)

;; make sure we can access man pages
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; projectile
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(provide 'setup-helm)
