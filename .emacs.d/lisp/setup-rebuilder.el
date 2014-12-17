;; re-builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; this function resizes the re-builder window as you type
(add-hook 'reb-mode-hook (defun
           setup-reb-mode () (add-hook 'post-command-hook (defun
           reb-mode-resize-window-after-change () (save-excursion
           (fit-window-to-buffer) (enlarge-window 1) (recenter -1))) nil t)))

(provide 'setup-rebuilder)
