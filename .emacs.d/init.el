;; Kevin Depue (2017)
;; My personal emacs configuration. I used to use the
;; package setup from Magnars, but now I'm going with
;; use-package. Always a work in progress.

;; TODO
;; * Navigate to the *scratch* buffer and close
;;   all other windows once we are done loading.
;; * Checkout some other emacs themes here:
;;   https://pawelbx.github.io/emacs-theme-gallery/

;;
;; Basic configuration
;;

;; I don't ever want to see this.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 0))

;; No splash screen / startup message.
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Clear the minibuffer please.
(defun display-startup-echo-area-message ()
  (message ""))

;; Make the scratch buffer empty.
(setq initial-scratch-message "")

;; Auto refresh buffers.
(global-auto-revert-mode 1)

;; Auto refresh dired.
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Highlight the line that point is on.
(global-hl-line-mode t)

;; Answering 'y' or 'n' will do.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use utf8 encoding please.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Always show line and column numbers.
(setq line-number-mode t)
(setq column-number-mode t)

;; Subword mode is the cat's meow.
(global-subword-mode 1)

;; Highlight matching parentheses.
(show-paren-mode t)

;; We're not in the 80's anymore.
(setq gc-cons-threshold 20000000)

;; Shoot for an 80 character width.
(setq fill-column 80)
(setq-default fill-column 80)

;; Make a vertical split more likely.
(setq split-height-threshold 100)

;; Save minibuffer history.
(savehist-mode 1)
(setq history-length 1000)

;; Decrease minibuffer echo time.
(setq echo-keystrokes 0.1)

;; If inserting text, replace active region.
(delete-selection-mode 1)

;; If loading code from a byte-compiled elc
;; file, prefer raw code from an el file if
;; it is newer.
(setq load-prefer-newer t)

;; If we open a help buffer in
;; any way, navigate to it.
(setq help-window-select t)

;;
;; Platform
;;

;; Define some platform-specific variables.
(setq is-gui (equal window-system 'ns))
(setq is-mac (equal system-type 'darwin))

;;
;; Backups
;;

;; TODO
;; Find a better way to deal with backups.

;; Put backup files in their own directory.
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Backup files even if we're using source control.
(setq vc-make-backup-files t)

;;
;; Custom
;;

;; Keep custom settings in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;
;; Clipboard
;;

;; Copy / paste on mac.
(when is-mac
  ;; Copy from the clipboard.
  (defun mac-copy ()
    (shell-command-to-string "pbpaste"))

  ;; Paste from the clipboard.
  (defun mac-paste (text &optional push)
    (let ((process-connection-type nil)) 
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  ;; Install the commands.
  (setq interprogram-paste-function 'mac-copy)
  (setq interprogram-cut-function 'mac-paste))

;;
;; Theme
;;

;; Make sure we set this for gui emacs.
(defconst it3ration-font "Menlo Regular-18" "My emacs font.")
(when is-gui (add-to-list 'default-frame-alist `(font . ,it3ration-font)))

;; Setup our theme path.
(setq custom-theme-directory (concat user-emacs-directory "themes"))

;; Use the it3ration theme for now.
(condition-case nil 
    (load-theme 'it3ration t)
  (wrong-number-of-arguments (load-theme 'it3ration)))

;;
;; Styles
;;

;; Style defaults.
(setq-default c-default-style "k&r")
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; Specific style settings.
(defun it3ration-cc-style ()
  (setq c-default-style "k&r")
  (c-set-style "k&r")
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (c-set-offset 'inline-open '0)
  (c-set-offset 'case-label '4)
  (electric-pair-mode 1))

;; Install the settings.
(add-hook 'c-mode-common-hook 'it3ration-cc-style)

;;
;; Package management
;;

;; Setup our load path.
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)

;; Setup packages.
(require 'package)
(setq package-enable-at-startup nil)

;; Setup package repositories.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
 
;; Initialize packages.
(package-initialize)

;; Make sure use-package is installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable diminish / bind variants.
(eval-when-compile
  (require 'use-package))
(require 'diminish)                
(require 'bind-key)                

;;
;; Libraries
;;

;; The string library.
(use-package s
  :ensure t
  :defer t)

;; The file library.
(use-package f
  :ensure t
  :defer t)

;; The list library.
(use-package dash
  :ensure t
  :defer t)

;;
;; uniquify
;;

(require 'uniquify)

;; Customize the look of duplicate values.
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")

;;
;; company
;;

;; This mode enables make-
;; believe intellisense.
(use-package company
  :ensure t
  :init
  (progn
    ;; No delay please.
    (setq company-idle-delay 0)

    ;; Turn company on globally.
    (add-hook 'after-init-hook 'global-company-mode)))

;;
;; which-key
;;

;; The replacement for guide-key. Given a key 
;; sequence, shows what commands are available.
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (progn
    ;; No delay please.
    (setq which-key-idle-delay 0.0)

    ;; Show count / total on the modeline.
    (setq which-key-show-remaining-keys t)

    ;; Allow 50% of the frame to display keys.
    (setq which-key-side-window-max-height 0.5)
    
    ;; Open it at the bottom.
    (which-key-setup-side-window-bottom)

    ;; Turn it on.
    (which-key-mode)))

;;
;; helm
;;

;; TODO
;; * Customize this package.
;; * You should probably map helm-occur
;;   to something that's easier to type.
;; * Bind the various ag commands to
;;   useful keys, you use them a lot.
;; * Add support for helm-swoop!

;; The best completion package ever in my
;; humble opinion. The initialization order
;; is important here due to some global key
;; bindings - we start with the config.
(use-package helm
  :ensure t
  :demand t
  :diminish helm-mode
  :bind
  (("C-x C-f" . helm-find-files)
   ("M-x" . helm-M-x)
   ("C-x b" . helm-mini)
   ("C-x C-b" . helm-buffers-list)
   ("M-y" . helm-show-kill-ring)
   ("C-h a" . helm-apropos))
  :init
  (progn
    ;; We need this now.
    (use-package helm-config)

    ;; Open helm in the current window.
    (setq helm-split-window-in-side-p t)

    ;; Remap C-c h to helm's command map.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)

    ;; Disable C-x c, it's too close to
    ;; C-x C-c which closes emacs. :/
    (global-unset-key (kbd "C-x c"))

    ;; Add occur mode to the command map. I'm
    ;; surprised it's not already in there.
    (define-key helm-command-map (kbd "o") 'helm-occur))
  :config
  (progn
    ;; Support for the silver searcher.
    (use-package helm-ag
      :ensure t)
     
    ;; For inspecting bindings.
    (use-package helm-descbinds
      :ensure t
      :bind ("C-h b" . helm-descbinds))))

;;
;; projectile
;;

;; TODO
;; * Add a keybinding for projectile-switch-project.

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-find-file projectile-switch-project)
  :bind
  (("C-x p" . projectile-find-file))
  :init
  (progn
    ;; Use helm as projectile's completion system.
    (setq projectile-completion-system 'helm)

    ;; Enable caching.
    (setq projectile-enable-caching t)
  
    ;; Set our indexing mode.
    (setq projectile-indexing-method 'alien))
  :config
  (progn
    ;; Turn projectile on globally.
    (projectile-mode)

    ;; Helm integration? Yes please!
    (use-package helm-projectile
      :ensure t
      :config
      (helm-projectile-on))))

;;
;; magit
;;

;; TODO
;; * Open the status buffer in the current window.
;; * Add a keymap for magit so we get which-key
;;   support.
;; * Customize this package.

;; The best git interface ever.
(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status))
  :config
  (progn
    ;; Open the status buffer in the current window and select it.
    (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)))

;;
;; undo-tree
;;

(use-package undo-tree
  :ensure t
  :bind
  (("C-x u" . undo-tree-visualize))
  :config
  (progn
    ;; Let's use this everywhere.
    (global-undo-tree-mode)))

;;
;; try
;;

;; Allows you to try packages
;; without installing them.
(use-package try
  :ensure t
  :commands (try try-and-refresh))




;;    '(ag
;;      yasnippet
;;      haskell-mode
;;      go-mode
;;      web-mode
;;      csharp-mode
;;      yaml-mode
;;      restclient
;;      dockerfile-mode
;;      shader-mode
;;      )))

;; ;; Stupid temporary fix
;; ;; for csharp-mode.
;; (require 'cl)

;; ;; Setup various packages.
;; ;; Order matters here!
;; (require 'setup-modeline)
;; (require 'setup-mappings)
;; (require 'setup-clipboard)
;; (require 'setup-uniquify)
;; (require 'setup-org)
;; (require 'setup-guide-key)
;; (require 'setup-helm)
;; (require 'setup-eshell)
;; (require 'setup-projectile)
;; (require 'setup-helm-projectile)
;; (require 'setup-company)
;; (require 'setup-yasnippet)
;; (require 'setup-undo-tree)
;; (require 'setup-cc)
;; (require 'setup-erc)

;; ;; Setup language-specific modes.
;; ;; Only run if the mode is loaded.
;; (eval-after-load 'haskell-mode '(require 'setup-haskell))
;; (eval-after-load 'go-mode '(require 'setup-go))
;; (require 'setup-js)
;; (require 'setup-web)
;; (require 'setup-csharp)
;; (require 'setup-yaml)
;; (require 'setup-shaders)

;; ;; (require 'setup-diminish)

;; ;; Must haves ..
;; ;; smartparens
;; ;; paredit
;; ;; rainbow-mode
;; ;; rainbow-delimiters
;; ;; helm-spotify

;; ;; Modes to checkout ..
;; ;; highlight-escape-sequences
;; ;; jump-char
;; ;; expand-region
;; ;; gist
;; ;; move-text
;; ;; string-edit

;; ;; Things to look into ..
;; ;; rx.el
;; ;; eshell [aliases / initial directory]

