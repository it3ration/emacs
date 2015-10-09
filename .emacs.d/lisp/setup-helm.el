(require 'helm)
(require 'helm-config)

;; Turn it on.
(helm-mode 1)

;; Use C-c h as helm's prefix.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; Use helm's M-x.
(global-set-key (kbd "M-x") 'helm-M-x)

;; Use helm's kill ring.
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Let's use helm-mini.
(global-set-key (kbd "C-x b") 'helm-mini)

;; Open helm in the current pane only.
(setq helm-split-window-in-side-p t)

;; Make sure we can access man pages.
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; ;; projectile
;; (setq projectile-completion-system 'helm)
;; (helm-projectile-on)

;; ;; colors
;; (set-face-attribute 'helm-selection nil :background "firebrick4")

(provide 'setup-helm)
