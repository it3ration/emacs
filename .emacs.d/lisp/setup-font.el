(defconst it3ration-font "Menlo Regular-18" "My emacs font.")

;; Make sure we set this for gui emacs.
(when is-gui (add-to-list 'default-frame-alist `(font . ,it3ration-font)))

(provide 'setup-font)
