(require 'smex)
(smex-initialize)

;; use smex for M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; regular M-x still accessible
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'setup-smex)
