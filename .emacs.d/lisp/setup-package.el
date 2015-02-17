(require 'package)

(defvar melpa '("melpa" . "http://melpa.milkbox.net/packages/"))
(defvar marmalade '("marmalade" . "http://marmalade-repo.org/packages/"))
(defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))

;; marmalade
(add-to-list 'package-archives melpa)
(add-to-list 'package-archives marmalade t)
(add-to-list 'package-archives gnu t)

(package-initialize)

;; refresh package list if you never have
(unless (and (file-exists-p "~/.emacs.d/elpa/archives/melpa")
             (file-exists-p "~/.emacs.d/elpa/archives/marmalade")
             (file-exists-p "~/.emacs.d/elpa/archives/gnu"))
  (package-refresh-contents))

;; courtesy of magnars: install a list of packages
(defun packages-install (&rest packages)
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (package-initialize)
                (package-install name)))))
        packages)
  (package-initialize)
  (delete-other-windows))

(provide 'setup-package)
