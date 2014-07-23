(require 'ido)
(require 'ido-ubiquitous)

;; turn on ido-mode
(ido-mode t)

;; use ido-mode everywhere
(ido-ubiquitous-mode 1)

;; enable flex matching
(setq ido-enable-flex-matching t)

(provide 'setup-ido)
