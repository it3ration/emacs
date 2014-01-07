;; defaults
(setq-default c-default-style "k&r")
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; my specific cc-mode settings
(defun it3ration-cc-style ()
  (setq c-default-style "k&r")
  (c-set-style "k&r")
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (c-set-offset 'inline-open '0)
  (c-set-offset 'case-label '4))

(add-hook 'c-mode-common-hook 'it3ration-cc-style)

(provide 'setup-cc)
