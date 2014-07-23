;; -------------------------
;; Kevin Depue (2012)
;; Emacs configuration file.
;; -------------------------

;; load path
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/packages/")

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
   (cons 'diminish melpa)
   (cons 'smex melpa)
   (cons 'csharp-mode melpa)
   (cons 'magit melpa)
   (cons 'haskell-mode melpa)
   (cons 'ag melpa)
   (cons 'graphviz-dot-mode melpa)
   (cons 'flymake melpa)
   (cons 'ido-ubiquitous marmalade)))

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
(require 'setup-ido)
(require 'setup-ido-ubiquitous)
(require 'setup-smex)
(require 'setup-uniquify)
(require 'setup-multi-term)
(require 'setup-ffip)
(require 'setup-rebuilder)
(require 'setup-clipboard)
(require 'setup-python)
(require 'setup-csharp)
(require 'setup-graphviz)

;; setup extensions
(eval-after-load 'lua-mode '(require 'setup-lua))
(require 'setup-lazypile)
(require 'setup-undo-tree)
(require 'setup-auto-complete)
(require 'setup-yasnippet)
(require 'setup-xcode)
(require 'setup-diminish)

;; final tweaks
(require 'setup-mappings)
(require 'setup-aliases)

