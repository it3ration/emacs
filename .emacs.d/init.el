;; Kevin Depue (2017)
;; My personal emacs configuration. I used to use the
;; package setup from Magnars, but now I'm going with
;; use-package. Always a work in progress.

;; TODO
;; * Navigate to the *scratch* buffer and close
;;   all other windows once we are done loading.
;; * Checkout some other emacs themes here:
;;   https://pawelbx.github.io/emacs-theme-gallery/
;; * Figure out a way to copy the thing under point.
;; * Figure out how to diminish all major modes.

;; PACKAGES TO EXPLORE
;; * slime
;; * paredit
;; * rainbow-mode
;; * rainbow-delimiters
;; * ace-jump-mode
;; * helm-spotify
;; * highlight-escape-sequences
;; * jump-char
;; * expand-region
;; * gist
;; * move-text
;; * string-edit

;;
;; Custom
;;

;; Put the special emacs "customize" settings in their own
;; file. We load it first because we may want to override
;; these settings in our config below.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

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
;; (show-paren-mode t)

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

;; Don't automatically debug on error.
;; We enable this by hand if desired.
(setq debug-on-error nil)

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

;; Setup our theme path.
(setq custom-theme-directory (concat user-emacs-directory "themes"))

;; Use the it3ration theme for now.
(condition-case nil 
    (load-theme 'it3ration t)
  (wrong-number-of-arguments (load-theme 'it3ration)))

;;
;; C-style languages
;;

;; TODO
;; * See if we can initialize this stuff
;;   using the use-package macro.

;; Open header files in c++ mode.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Style defaults.
(setq-default c-default-style "k&r")
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; The style.
(add-hook
 'c-mode-common-hook
 (lambda ()
   (setq c-default-style "k&r")
   (c-set-style "k&r")
   (setq tab-width 4)
   (setq indent-tabs-mode nil)
   (setq c-basic-offset 4)
   (c-set-offset 'inline-open '0)
   (c-set-offset 'case-label '4)
   (electric-pair-mode 1)))

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
  :ensure t)

;; The file library.
(use-package f
  :ensure t)

;; The list library.
(use-package dash
  :ensure t)

;;
;; uniquify
;;

(require 'uniquify)

;; Customize the look of duplicate values.
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")

;;
;; smartparens
;;

;; This package kicks ass.
(use-package smartparens
  :ensure t
  :init
  (progn
    ;; Use the default configuration.
    (use-package smartparens-config)

    ;; Show matching delimiters.
    (show-smartparens-global-mode t)
    
    ;; Use it everywhere.
    (smartparens-global-mode 1)))

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

;; The best completion package ever in my
;; humble opinion. The initialization order
;; is important here due to some global key
;; bindings - we start with the config.
(use-package helm
  :ensure t
  :demand t
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

    ;; Remap helm's command map from C-x c to C-c h,
    ;; C-x c is too close to C-x C-c which closes emacs.
    (global-unset-key (kbd "C-x c"))
    (global-set-key (kbd "C-c h") 'helm-command-prefix)

    ;; Add a few extensions to helm's command map.
    (define-key helm-command-map (kbd "d") 'helm-dash)
    (define-key helm-command-map (kbd "o") 'helm-occur))
  :config
  (progn
    ;; Turn it on.
    (helm-mode t)

    ;; For spell checking.
    (use-package helm-flyspell
      :ensure t)

    ;; For docsets.
    (use-package helm-dash
      :ensure t)
    
    ;; Swoop mode ftw.
    (use-package helm-swoop
      :ensure t
      :bind (("M-i" . helm-swoop)
             ("C-c M-i" . helm-multi-swoop))
      :init
      (progn
        ;; Start with no search string.
        (setq helm-swoop-pre-input-function (lambda () ""))

        ;; Split vertically please.
        (setq helm-swoop-split-direction 'split-window-horizontally))
      :config
      (progn
        ;; Move up and down using isearch keys.
        (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
        (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
        (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
        (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)))
    
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

(use-package projectile
  :ensure t
  :demand t
  :commands (projectile-find-file projectile-switch-project)
  :bind
  (("C-x p" . projectile-find-file))
  :init
  (progn
    ;; Use helm as projectile's completion system.
    (setq projectile-completion-system 'helm)
  
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
      (helm-projectile-toggle 1))))

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
;; rainbow-mode
;;

(use-package rainbow-mode
  :ensure t
  :config
  (progn
    ;; Only style hex colors please.
    (setq rainbow-ansi-colors nil)
    (setq rainbow-latex-colors nil)
    (setq rainbow-html-colors nil)
    (setq rainbow-x-colors nil)))

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

;;
;; csharp-mode
;;

;; The c# language.
(use-package csharp-mode
  :ensure t
  :mode "\\.cs$"
  :init
  (progn
    ;; The style.
    (add-hook
     'csharp-mode-hook
     (lambda ()
       (electric-pair-mode 1)))))

;;
;; shader-mode
;;

;; For editing shaders.
(use-package shader-mode
  :ensure t
  :mode ("\\.shader$" "\\.cginc$"))

;;
;; clojure-mode
;;

;; TODO
;; * Customize these packages.

;; The clojure language.
(use-package clojure-mode
  :ensure t
  :mode "\\.clj$"
  :config
  (progn
    ;; This mode adds a clojure repl.
    (use-package cider
      :ensure t)

    ;; Helm integration? Yes please!
    (use-package helm-cider
      :ensure t
      :init
      (progn
        ;; Don't snap to the bottom.
        (setq cider-repl-scroll-on-output nil))
      :config
      (helm-cider-mode 1))))

;;
;; yaml-mode
;;

;; The yaml language.
(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

;;
;; web-mode
;;

;; Follow conventions please.
(setq js-indent-level 2)

;; For web-based languages.
(use-package web-mode
  :ensure t
  :mode ("\\.js$"
         "\\.jsx$"
         "\\.css$"
         "\\.scss$"
         "\\.html"
         "\\.php")
  :init
  (progn
    (add-hook
     'web-mode-hook
     (lambda ()
       ;; Indenting offsets.
       (setq web-mode-markup-indent-offset 2)
       (setq web-mode-css-indent-offset 2)
       (setq web-mode-code-indent-offset 2)))))

;;
;; go-mode
;;

(use-package go-mode
  :ensure t
  :mode "\\.go$"
  :init
  (progn
    ;; The style.
    (add-hook
     'go-mode-hook
     (lambda ()
       (setq-default indent-tabs-mode 1)))

    ;; Reformat the file before saving.
    (add-hook 'before-save-hook #'gofmt-before-save)))

;;
;; solidity-mode
;;

(use-package solidity-mode
  :ensure t
  :mode "\\.sol$")

;;
;; org-mode
;;

;; The style.
(add-hook
 'org-mode-hook
 (lambda ()
   ;; Set a reasonable width.
   (set-fill-column 65)
   
   ;; Auto fill please.
   (turn-on-auto-fill)

   ;; Only show rightmost stars.
   (org-indent-mode)))

;;
;; org-journal
;;

(use-package org-journal
  :ensure t
  :config
  (progn
    ;; Various settings.
    (setq org-journal-dir "~/repos/journal")
    (setq org-journal-file-format "%Y-%m-%d.org")
    (setq org-journal-date-format "%F (%A)")
    (setq org-journal-time-format"%I:%M %p | ")
    (setq org-journal-find-file 'find-file)))

;;
;; haskell-mode
;;

(use-package haskell-mode
  :ensure t
  :mode "\\.hs$"
  :config
  (progn
    (electric-pair-mode 1)))

;;
;; markdown-mode
;;

(use-package markdown-mode
  :ensure t
  :mode "\\.md$")

;;
;; eshell
;;

;; TODO
;; * Look into eshell alises, not having "la" sucks.
;; * Check to make sure the initial directory is set.

;; The emacs shell.
(use-package eshell
  :ensure t
  :commands eshell
  :config
  (progn
    ;; We need these.
    (require 's)
    (require 'f)

    ;; History size.
    (setq eshell-history-size 100000)

    ;; Make our prompt look like the terminal.
    (setq
     eshell-prompt-function
     (lambda ()
       (let ((color-yellow "#ffff87")
             (color-green "#00af00")
             (color-red "#d75f5f"))
         (concat
          (propertize "[" 'face `(:foreground ,color-yellow :weight bold))
          (propertize
           (concat
            (user-login-name)
            "@"
            (s-trim (shell-command-to-string "hostname -s")))
           'face `(:foreground ,color-green :weight bold))
          " "
          (propertize
           (f-short (s-trim (shell-command-to-string "pwd")))
           'face `(:foreground ,color-red :weight bold))
          (propertize "]" 'face `(:foreground ,color-yellow :weight bold))
          "\n"
          (propertize "$" 'face `(:foreground ,color-red :weight bold))
          " "))))

    ;; Fix the prompt regex.
    (setq eshell-prompt-regexp "^[$] ")))

;;
;; erc
;;

(use-package erc
  :ensure t
  :init
  (progn
    ;; Fill chat messages based on window width.
    (make-variable-buffer-local 'erc-fill-column)
    (add-hook
     'window-configuration-change-hook 
     '(lambda ()
        (save-excursion
          (walk-windows
           (lambda (w)
             (let ((buffer (window-buffer w)))
               (set-buffer buffer)
               (when (eq major-mode 'erc-mode)
                 (setq erc-fill-column (- (window-width w) 2)))))))))

    ;; Let's ignore the notice prefix.
    (setq erc-notice-prefix nil)

    ;; Don't rename used nicknames.
    (setq erc-try-new-nick-p nil)

    ;; Don't leak our name.
    (setq erc-user-full-name nil)

    ;; Don't leak your username.
    (setq erc-email-userid "user")

    ;; Don't leak your system name.
    (setq erc-system-name "emacs")

    ;; Always show timestamps.
    (setq erc-timestamp-only-if-changed-flag nil)
    
    ;; Show timestamps on the left.
    (setq erc-insert-timestamp-function 'erc-insert-timestamp-left)

    ;; Make the format non-military.
    (setq erc-timestamp-format "%I:%M ")

    ;; Add an indention prefix.
    (setq erc-fill-prefix "    + ")
    
    ;; Let's use a sane prompt please.
    (setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))))

(defun chat-load-settings (file)
  "Tries to load nickname / password as association list."
  (when (file-exists-p file)
    (let ((lines (-map 's-trim (s-lines (f-read file 'utf-8)))))
      (list (nth 0 lines) (nth 1 lines)))))

(defun chat-connect (file server port)
  "Tries to load credentials from file, connect to server / port."
  (let ((settings (chat-load-settings file)))
    (if (listp settings)
        (erc :server server
             :port port
             :nick (nth 0 settings)
             :password (nth 1 settings))
      (erc :server server
           :port port))))

(defun chat ()
  "Main erc chat."
  (interactive)
  (chat-connect "~/.erc" "irc.freenode.net" 6667))

;;
;; restclient
;;

;; TODO
;; * Customize this package.

(use-package restclient
  :ensure t
  :commands restclient-mode)

;;
;; 2048
;;

(use-package 2048-game
  :ensure t)
