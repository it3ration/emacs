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

;; Backup files even if we're using source control.
(setq vc-make-backup-files t)

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
     )))

;; Attempts to install our packages.
(condition-case nil
    (install-packages)
  (error
   (package-refresh-contents)
   (install-packages)))

;; Setup sane defaults.
(require 'setup-defaults)

;; ;; setup customizations
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

