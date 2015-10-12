(require 's)

(defun my-mode-line-source-control-string ()
  "Builds a source control string or nil."
  (when vc-mode
    `("["
      ,(s-trim (substring-no-properties vc-mode))
      "] ")))

;; My custom mode-line-format setup.
(setq-default
 mode-line-format
 '(
   "[%&] "
   (:eval (propertize "%b" 'face '(:foreground "red" :background "yellow" :weight bold)))
   " (%l, %c) [%m] "
   (:eval (my-mode-line-source-control-string))
   ))

(provide 'setup-modeline)
