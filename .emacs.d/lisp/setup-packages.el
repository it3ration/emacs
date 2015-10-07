(require 'package)

;; We're only going to use melpa.
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defun packages-install (packages)
  (mapc
   (lambda (package)
     (when (not (package-installed-p package))
       (package-install package)))
   packages)
  (delete-other-windows))

(provide 'setup-packages)
