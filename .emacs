;; Turn these off immediately to avoid ever seeing them.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 0))

;; No splash screen / startup message.
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Make the scratch buffer empty.
(setq initial-scratch-message "")

;; Setup our load path.
(setq site-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)

;; Setup platform.
(require 'setup-platform)

;; Setup backups.
(require 'setup-backups)

;; Setup font.
(require 'setup-font)

;; Setup themes.
(require 'setup-themes)

;; Setup packages.
(require 'setup-packages)

;; Installs our packages.
(defun install-packages ()
  (packages-install
   '(s
     f
     dash
     magit
     guide-key
     helm
     projectile
     ag
     helm-projectile
     helm-ag
     company
     yasnippet
     undo-tree
     haskell-mode
     go-mode
     web-mode
     csharp-mode
     yaml-mode
     restclient
     dockerfile-mode
     shader-mode
     )))

;; Attempts to install our packages.
(condition-case nil
    (install-packages)
  (error
   (package-refresh-contents)
   (install-packages)))

;; Setup sane defaults.
(require 'setup-defaults)

;; Stupid temporary fix
;; for csharp-mode.
(require 'cl)

;; Setup various packages.
;; Order matters here!
(require 'setup-modeline)
(require 'setup-mappings)
(require 'setup-clipboard)
(require 'setup-uniquify)
(require 'setup-org)
(require 'setup-guide-key)
(require 'setup-helm)
(require 'setup-eshell)
(require 'setup-projectile)
(require 'setup-helm-projectile)
(require 'setup-company)
(require 'setup-yasnippet)
(require 'setup-undo-tree)
(require 'setup-cc)
(require 'setup-erc)

;; Setup language-specific modes.
;; Only run if the mode is loaded.
(eval-after-load 'haskell-mode '(require 'setup-haskell))
(eval-after-load 'go-mode '(require 'setup-go))
(require 'setup-js)
(require 'setup-web)
(require 'setup-csharp)
(require 'setup-yaml)
(require 'setup-shaders)

;; Setup your packages.
(require 'setup-diary)

;; (require 'setup-diminish)

;; Must haves ..
;; smartparens
;; paredit
;; rainbow-mode
;; rainbow-delimiters
;; helm-spotify

;; Modes to checkout ..
;; highlight-escape-sequences
;; jump-char
;; expand-region
;; gist
;; move-text
;; string-edit

;; Things to look into ..
;; rx.el
;; eshell [aliases / initial directory]
