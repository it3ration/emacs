(require 'guide-key)

;; Specify which key sequences it pops up for.
(setq guide-key/guide-key-sequence '("C-c" "C-c h" "C-c p" "C-x r" "C-h" "C-x 4"))

;; Turn it on.
(guide-key-mode 1)

;; Use the recursive option.
(setq guide-key/recursive-key-sequence-flag t)

;; Show it at the bottom, not the right.
(setq guide-key/popup-window-position 'bottom)

;; Make it happen instantly.
(setq guide-key/idle-delay 0)

(provide 'setup-guide-key)
