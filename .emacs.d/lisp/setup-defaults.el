;; Auto refresh buffers.
(global-auto-revert-mode 1)

;; Auto refresh dired.
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Highlight the line that point is on.
(global-hl-line-mode t)

;; Answering 'y' or 'n' will do.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use utf8 encoding please.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Always show line and column numbers.
(setq line-number-mode t)
(setq column-number-mode t)

;; Subword mode is the cat's meow.
(global-subword-mode 1)

;; Highlight matching parentheses.
(show-paren-mode t)

;; We're not in the 80's anymore.
(setq gc-cons-threshold 20000000)

;; Shoot for an 80 character width.
(setq fill-column 80)
(setq-default fill-column 80)

;; Make a vertical split more likely.
(setq split-height-threshold 100)

;; Save minibuffer history.
(savehist-mode 1)
(setq history-length 1000)

;; Decrease minibuffer echo time.
(setq echo-keystrokes 0.1)

;; If inserting text, replace active region.
(delete-selection-mode 1)

(provide 'setup-defaults)
