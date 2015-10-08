;; (setq custom-theme-directory (concat user-emacs-directory "themes"))


;; use my custom it3ration theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (condition-case nil 
;;     (load-theme 'it3ration t)
;;   (wrong-number-of-arguments (load-theme 'it3ration)))

;; Setup our theme path.
(setq custom-theme-directory (concat user-emacs-directory "themes"))

;; Use the it3ration theme for now.
(load-theme 'it3ration)

(provide 'setup-theme)

