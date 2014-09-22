(require 'company)

;; no delay
(setq company-idle-delay 0)

;; turn company on globally
(add-hook 'after-init-hook 'global-company-mode)

(provide 'setup-company)
