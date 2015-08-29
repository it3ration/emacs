;; -------------------------
;; Kevin Depue (2014)
;; Emacs configuration file.
;; -------------------------

;; load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; setup package
(require 'setup-package)

;; install missing extensions
(defun init--install-packages ()
  (packages-install
   (cons 'undo-tree marmalade)
   (cons 'lua-mode marmalade)
   (cons 'company melpa)
   (cons 'yasnippet melpa)
   (cons 'multi-term melpa)
   (cons 'diminish melpa)
   (cons 'csharp-mode melpa)
   (cons 'magit melpa)
   (cons 'haskell-mode melpa)
   (cons 'ag melpa)
   (cons 'graphviz-dot-mode melpa)
   (cons 'projectile melpa)
   (cons 'helm melpa)
   (cons 'helm-projectile melpa)
   (cons 'helm-ag melpa)
   (cons 'haskell-mode melpa)
   (cons 'go-mode melpa)
   (cons 'guide-key melpa)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; setup customizations
(require 'setup-defaults)
(require 'setup-theme)
(require 'setup-cc)
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'org '(require 'setup-org))
(require 'setup-uniquify)
(require 'setup-projectile)
(require 'setup-helm)
(require 'setup-guide-key)
(require 'setup-multi-term)
(require 'setup-ffip)
(require 'setup-rebuilder)
(require 'setup-clipboard)
(require 'setup-python)
(require 'setup-csharp)
(require 'setup-graphviz)
(require 'setup-haskell)
(require 'setup-go)

;; setup extensions
(eval-after-load 'lua-mode '(require 'setup-lua))
(require 'setup-undo-tree)
(require 'setup-company)
(require 'setup-yasnippet)
(require 'setup-xcode)
(require 'setup-diminish)

;; final tweaks
(require 'setup-mappings)
(require 'setup-aliases)
