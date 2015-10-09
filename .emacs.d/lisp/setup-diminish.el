;; (require 'diminish)

;; ;; minor modes
;; (diminish 'yas-minor-mode "yas")
;; (diminish 'undo-tree-mode "ut")
;; (diminish 'abbrev-mode)

;; ;; major modes
;; (add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "el")))
;; (add-hook 'c++-mode-hook (lambda() (setq mode-name "c++")))
;; (add-hook 'lua-mode-hook (lambda() (setq mode-name "lua")))
;; (add-hook 'org-mode-hook (lambda() (setq mode-name "org")))

;; (eval-after-load "anus" '(diminish 'lisp-mode))

;; (diminish 'lisp-mode "el")

;; (require 'delight)

;; (delight '((abbrev-mode " Abv" abbrev)
;;             (smart-tab-mode " \\t" smart-tab)
;;             (eldoc-mode nil "eldoc")
;;             (rainbow-mode)
;;             (emacs-lisp-mode "Elisp" :major)))

;; (delight
;;  '((emacs-lisp-mode "anus" :major)))
           
(provide 'setup-diminish)


;; (require 'diminish)
;; (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
;; (eval-after-load "eldoc" '(diminish 'eldoc-mode))
;; (eval-after-load "paredit" '(diminish 'paredit-mode))
;; (eval-after-load "tagedit" '(diminish 'tagedit-mode))
;; (eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
;; (eval-after-load "skewer-mode" '(diminish 'skewer-mode))
;; (eval-after-load "skewer-css" '(diminish 'skewer-css-mode))
;; (eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
;; (eval-after-load "smartparens" '(diminish 'smartparens-mode))
;; (eval-after-load "guide-key" '(diminish 'guide-key-mode))
;; (eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))
;; (eval-after-load "subword" '(diminish 'subword-mode))

;; (defmacro rename-modeline (package-name mode new-name)
;;   `(eval-after-load ,package-name
;;      '(defadvice ,mode (after rename-modeline activate)
;;         (setq mode-name ,new-name))))

;; (rename-modeline "js2-mode" js2-mode "JS2")
;; (rename-modeline "clojure-mode" clojure-mode "Clj")
