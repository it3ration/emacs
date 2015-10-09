;; Turn it on globally.
(projectile-global-mode)

;; Set the default projectile keybinding.
(global-set-key (kbd "C-x p") 'projectile-find-file)
(setq projectile-indexing-method 'alien)

(provide 'setup-projectile)
