(require 'find-file)

;; find other file
(global-set-key (kbd "C-c o") (lambda () (interactive)
                                (ff-find-other-file t)))

;; never try to create files if not found
(setq ff-always-try-to-create nil)

(provide 'setup-ffip)
