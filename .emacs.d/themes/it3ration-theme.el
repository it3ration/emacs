(deftheme it3ration "A theme that's easier on the eyes.")

(custom-theme-set-faces
 'it3ration

 ;; the defaults
 '(default ((t (:background "grey20" :foreground "white"))))
 '(font-lock-comment-face ((t (:foreground "wheat4"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "wheat4" :inherit font-lock-comment-face))))
 '(font-lock-doc-face ((t (:foreground "wheat4"))))
 '(font-lock-preprocessor-face ((t (:foreground "IndianRed3"))))
 '(font-lock-string-face ((t (:foreground "khaki1"))))
 '(font-lock-keyword-face ((t (:foreground "IndianRed3" :bold t))))
 '(font-lock-function-name-face ((t (:foreground "green3"))))
 '(font-lock-constant-face ((t (:foreground "purple" :bold t :weight bold))))
 '(font-lock-type-face ((t (:foreground "cyan3"))))
 '(font-lock-variable-name-face ((t (:foreground "dark orange"))))
 '(font-lock-builtin-face ((t (:foreground "purple"))))
 '(font-lock-warning-face ((t (:foreground "red" :weight bold))))
 '(show-paren-match ((t (:background "dodger blue"))))
 '(show-paren-mismatch ((t (:background "green"))))
 '(isearch ((t (:background "sienna4"))))
 '(highlight ((t (:background "firebrick4"))))
 '(lazy-highlight ((t (:background "sienna4"))))
 '(region ((t (:background "gray10"))))
 '(cursor ((t (:background "DarkSlateGray3"))))

 ;; magit
 '(magit-diff-add ((t (:foreground "green"))))
 '(magit-diff-del ((t (:foreground "IndianRed3"))))
 '(magit-item-highlight ((t (:background "grey26"))))
 )

(provide-theme 'it3ration)
