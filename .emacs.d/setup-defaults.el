;; no splash screen
(setq inhibit-splash-screen t)

;; no startup message
(setq inhibit-startup-message t)

;; make the initial scratch buffer empty
(setq initial-scratch-message "")

;; no menu/tool/scroll bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; show column number
(column-number-mode t)

;; only require 'y' and 'n', not 'yes' and 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight matching parentheses
(show-paren-mode t)

;; highlight the line that point is on
(global-hl-line-mode t)

;; enable subword mode globally
(global-subword-mode 1)

;; reload files if they change
(global-auto-revert-mode 1)

;; backups
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . "~/.emacs.backups/")))
(setq auto-save-list-file-prefix "~/.emacs.backups/")
(setq auto-save-file-name-transforms `((".*" "~/.emacs.backups/" t)))

;; text must be 80 characters wide
(setq-default fill-column 80)

;; makes vertical split more likely
(setq split-height-threshold 100)

;; compilation
(setq compilation-scroll-output 'first-error)

;; tab settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(provide 'setup-defaults)
