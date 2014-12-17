(require 'omnisharp)

;; point to the omnisharp exe
;; (setq omnisharp-server-executable-path (expand-file-name "~/repos/omnisharpserver/OmniSharp/bin/Debug/OmniSharp.exe"))

;; turn this on for c# 
;; (add-hook 'csharp-mode-hook 'omnisharp-mode)

;; (setq omnisharp--curl-executable-path (expand-file-name "/usr/bin/curl"))
;; (setq omnisharp-auto-complete-popup-help-delay 0)
;; (setq omnisharp-server-executable-path (expand-file-name "~/dev/opensource/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe"))

;; (add-hook 'omnisharp-mode-hook
;;   (lambda()
;;     (local-set-key  (kbd "M-.") 'omnisharp-auto-complete)
;;     (local-set-key  (kbd ".") 'omnisharp-add-dot-and-auto-complete)))

(provide 'setup-omnisharp)
