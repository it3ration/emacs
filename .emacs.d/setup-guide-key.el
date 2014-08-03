(require 'guide-key)

;; specify which key sequences it pops up for
(setq guide-key/guide-key-sequence '("C-x r" "C-h" "C-x 4"))

;; turn it on
(guide-key-mode 1)

;; use the recursive option
(setq guide-key/recursive-key-sequence-flag t)

;; show it at the bottom, not the right
(setq guide-key/popup-window-position 'bottom)

;; let's not have a long delay
(setq guide-key/idle-delay 0)

(provide 'setup-guide-key)
