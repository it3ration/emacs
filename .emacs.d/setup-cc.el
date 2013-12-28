(defun it3ration-cc-style ()
  "My specific cc-mode style."
  (c-set-style "k&r")
  (c-set-offset 'inline-open '0)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

(add-hook 'c-mode-common-hook 'it3ration-cc-style)

(provide 'setup-cc)
