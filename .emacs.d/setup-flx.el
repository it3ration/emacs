(require 'flx-ido)

;; turn on flx mode
(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(provide 'setup-flx)
