(require 'breadcrumb)

(global-set-key (kbd "C-c b") 'bc-set)
(global-set-key (kbd "C-c n") 'bc-next)
(global-set-key (kbd "C-c p") 'bc-previous)
(global-set-key (kbd "C-c l") 'bc-list)
(global-set-key (kbd "C-c c") 'bc-clear)
(global-set-key (kbd "C-c j") 'bc-goto-current)
(global-set-key (kbd "C-c a") 'bc-local-previous)
(global-set-key (kbd "C-c e") 'bc-local-next)

(provide 'setup-breadcrumb)
