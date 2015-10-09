(require 'company)

;; Turn company on globally.
(add-hook 'after-init-hook 'global-company-mode)

;; No delay please.
(setq company-idle-delay 0)

(provide 'setup-company)
