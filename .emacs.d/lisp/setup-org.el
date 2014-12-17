;; all org files should be indented
(setq org-startup-indented t)

;; enable wrapping in org files
(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode t)
            (auto-fill-mode t))
          t)

(provide 'setup-org)
