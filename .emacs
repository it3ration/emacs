;; Turn these off immediately to avoid ever seeing them.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen / startup message.
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Make the scratch buffer empty.
(setq initial-scratch-message "")

;; Setup our load path.
(setq site-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)

;; Put backup files in their own directory.
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Setup packages.
(require 'setup-package)

;; This method installs packages.
(defun install-packages ()
  (packages-install
   '(company
     magit
     )))

;; Attempt to install packages.
(condition-case nil
    (install-packages)
  (error
   (package-refresh-contents)
   (install-packages)))


;; (defun init--install-packages ()
;;   (packages-install
;;    '(magit
;;      paredit
;;      move-text
;;      gist
;;      htmlize
;;      visual-regexp
;;      markdown-mode
;;      fill-column-indicator
;;      flycheck
;;      flycheck-pos-tip
;;      flycheck-clojure
;;      flx
;;      f
;;      flx-ido
;;      dired-details
;;      css-eldoc
;;      yasnippet
;;      smartparens
;;      ido-vertical-mode
;;      ido-at-point
;;      simple-httpd
;;      guide-key
;;      nodejs-repl
;;      restclient
;;      highlight-escape-sequences
;;      whitespace-cleanup-mode
;;      elisp-slime-nav
;;      dockerfile-mode
;;      clojure-mode
;;      clojure-mode-extra-font-locking
;;      groovy-mode
;;      prodigy
;;      cider
;;      yesql-ghosts
;;      string-edit
;;      )))

;; ;; install missing extensions
;; (defun init--install-packages ()
;;   (packages-install
;;    (cons 'undo-tree marmalade)
;;    (cons 'lua-mode marmalade)
;;    (cons 'company melpa)
;;    (cons 'yasnippet melpa)
;;    (cons 'multi-term melpa)
;;    (cons 'diminish melpa)
;;    (cons 'csharp-mode melpa)
;;    (cons 'magit melpa)
;;    (cons 'haskell-mode melpa)
;;    (cons 'ag melpa)
;;    (cons 'graphviz-dot-mode melpa)
;;    (cons 'projectile melpa)
;;    (cons 'helm melpa)
;;    (cons 'helm-projectile melpa)
;;    (cons 'helm-ag melpa)
;;    (cons 'haskell-mode melpa)
;;    (cons 'go-mode melpa)
;;    (cons 'guide-key melpa)))

;; ;; setup customizations
;; (require 'setup-defaults)
;; (require 'setup-theme)
;; (require 'setup-cc)
;; (eval-after-load 'dired '(require 'setup-dired))
;; (eval-after-load 'org '(require 'setup-org))
;; (require 'setup-uniquify)
;; (require 'setup-projectile)
;; (require 'setup-helm)
;; (require 'setup-guide-key)
;; (require 'setup-multi-term)
;; (require 'setup-ffip)
;; (require 'setup-rebuilder)
;; (require 'setup-clipboard)
;; (require 'setup-python)
;; (require 'setup-csharp)
;; (require 'setup-graphviz)
;; (require 'setup-haskell)
;; (require 'setup-go)

;; ;; setup extensions
;; (eval-after-load 'lua-mode '(require 'setup-lua))
;; (require 'setup-undo-tree)
;; (require 'setup-company)
;; (require 'setup-yasnippet)
;; (require 'setup-xcode)
;; (require 'setup-diminish)

;; ;; final tweaks
;; (require 'setup-mappings)
;; (require 'setup-aliases)

