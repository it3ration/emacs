;; -------------------------
;; Kevin Depue (2012)
;; Emacs configuration file.
;; -------------------------

;; load path
(add-to-list 'load-path "~/.emacs.d/")

;; setup package
(require 'setup-package)

;; install missing extensions
(defun init--install-packages ()
  (packages-install
   (cons 'undo-tree marmalade)
   (cons 'lua-mode marmalade)
   (cons 'auto-complete melpa)
   (cons 'yasnippet melpa)
   (cons 'multi-term melpa)
   (cons 'p4 marmalade)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; setup customizations
(require 'setup-defaults)
(require 'setup-theme)
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'org '(require 'setup-org))
(require 'setup-ido)
(require 'setup-uniquify)
(require 'setup-ffip)
(require 'setup-rebuilder)
(require 'setup-clipboard)

;; setup extensions
(eval-after-load 'lua-mode '(require 'setup-lua))
(require 'setup-undo-tree)
(require 'setup-auto-complete)
(require 'setup-yasnippet)
(require 'setup-xcode)
(require 'setup-p4)

;; map file extensions to modes
(require 'mode-mappings)
