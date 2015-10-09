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

 ;;
 ;; magit
 ;;

 ;; rebase
 ;; git-rebase-hash
 ;; git-rebase-description
 ;; git-rebase-killed-action

 ;; bisect
 ;; magit-bisect-good
 ;; magit-bisect-skip
 ;; magit-bisect-bad

 ;; blame
 ;; magit-blame-heading
 ;; magit-blame-summary
 ;; magit-blame-hash
 ;; magit-blame-name
 ;; magit-blame-date

 ;; diff
 ;; '(magit-diff-file-heading ((t (:foreground "" :background ""))))
 ;; '(magit-diff-file-heading-highlight ((t (:foreground "" :background ""))))
 ;; '(magit-diff-file-heading-selection ((t (:foreground "" :background ""))))
 '(magit-diff-hunk-heading ((t (:foreground "grey70" :background "grey35"))))
 '(magit-diff-hunk-heading-highlight ((t (:foreground "grey70" :background "grey35"))))
 ;; '(magit-diff-hunk-heading-selection ((t (:foreground "" :background ""))))
 ;; '(magit-diff-lines-heading ((t (:foreground "" :background ""))))
 ;; '(magit-diff-lines-boundary ((t (:foreground "" :background ""))))
 ;; '(magit-diff-conflict-heading ((t (:foreground "" :background ""))))
 '(magit-diff-added ((t (:foreground "green"))))
 '(magit-diff-removed ((t (:foreground "IndianRed3"))))
 ;; '(magit-diff-our ((t (:foreground "" :background ""))))
 ;; '(magit-diff-base ((t (:foreground "" :background ""))))
 ;; '(magit-diff-their ((t (:foreground "" :background ""))))
 ;; '(magit-diff-context ((t (:foreground "" :background ""))))
 '(magit-diff-added-highlight ((t (:foreground "green" :background "grey26"))))
 '(magit-diff-removed-highlight ((t (:foreground "IndianRed3" :background "grey26"))))
 ;; '(magit-diff-our-highlight ((t (:foreground "" :background ""))))
 ;; '(magit-diff-base-highlight ((t (:foreground "" :background ""))))
 ;; '(magit-diff-their-highlight ((t (:foreground "" :background ""))))
 ;; '(magit-diff-context-highlight ((t (:foreground "" :background ""))))
 ;; '(magit-diff-whitespace-warning ((t (:foreground "" :background ""))))
 ;; '(magit-diffstat-added ((t (:foreground "" :background ""))))
 ;; '(magit-diffstat-removed ((t (:foreground "" :background ""))))

 ;; log
 ;; magit-log-graph
 ;; magit-log-author
 ;; magit-log-date
 ;; magit-reflog-commit
 ;; magit-reflog-amend
 ;; magit-reflog-merge
 ;; magit-reflog-checkout
 ;; magit-reflog-reset
 ;; magit-reflog-rebase
 ;; magit-reflog-cherry-pick
 ;; magit-reflog-remote
 ;; magit-reflog-other

 ;; process
 ;; magit-process-ok
 ;; magit-process-ng

 ;; sequence
 ;; magit-sequence-pick
 ;; magit-sequence-stop
 ;; magit-sequence-part
 ;; magit-sequence-head
 ;; magit-sequence-drop
 ;; magit-sequence-done
 ;; magit-sequence-onto

 ;; section
 ;; magit-section-highlight
 ;; magit-section-heading
 ;; magit-section-secondary-heading
 ;; magit-section-heading-selection

 ;; magit
 ;; magit-header-line
 ;; magit-dimmed
 ;; magit-hash
 ;; magit-tag
 ;; magit-branch-remote
 ;; magit-branch-local
 ;; magit-branch-current
 ;; magit-head
 ;; magit-refname
 ;; magit-refname-stash
 ;; magit-refname-wip
 ;; magit-signature-good
 ;; magit-signature-bad
 ;; magit-signature-untrusted
 ;; magit-cherry-unmatched
 ;; magit-cherry-equivalent
 ;; magit-filename
 )

(provide-theme 'it3ration)

;; ;; colors
;; (set-face-attribute 'helm-selection nil :background "firebrick4")
