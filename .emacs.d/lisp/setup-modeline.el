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

;; If we're on a gui version of emacs,
;; fix the modeline height / box.
(when is-gui
  (progn
    (set-face-attribute 'mode-line nil :box nil :font it3ration-font)
    (set-face-attribute 'mode-line-inactive nil :box nil :font it3ration-font)))

(provide 'setup-modeline)
