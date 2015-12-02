(deftheme it3ration "A theme that's easier on the eyes.")

(let ((face-default        '((t (:background "#303030" :foreground "#e5e5e5"))))
      (face-paren-match    '((t (:background "#0087ff"))))
      (face-paren-mismatch '((t (:background "#00cd00"))))
      (face-highlight      '((t (:background "#870000"))))
      (face-region         '((t (:background "#1c1c1c"))))
      (face-search-current '((t (:background "#005fff"))))
      (face-search         '((t (:background "#005f00"))))
      (face-string         '((t (:foreground "#ffff87"))))
      (face-comment        '((t (:foreground "#87875f"))))
      (face-keyword        '((t (:foreground "#d75f5f" :bold t))))
      (face-function       '((t (:foreground "#00af00" :bold t))))
      (face-constant       '((t (:foreground "#af87ff" :bold t))))
      (face-type           '((t (:foreground "#00cdcd" :bold t))))
      (face-variable       '((t (:foreground "#ff8700" :bold t))))
      )
  (custom-theme-set-faces
   'it3ration

   ;;
   ;; defaults
   ;;

   `(default ,face-default)
   `(show-paren-match ,face-paren-match)
   `(show-paren-mismatch ,face-paren-mismatch)
   `(highlight ,face-highlight)
   `(region ,face-region)
   `(isearch ,face-search-current)
   `(query-replace ,face-search-current)
   `(lazy-highlight ,face-search)

   ;; '(cursor ((t (:background ""))))

   ;;
   ;; font lock
   ;;
   
   `(font-lock-string-face ,face-string)
   `(font-lock-comment-face ,face-comment)
   `(font-lock-comment-delimiter-face ,face-comment)
   `(font-lock-doc-face ,face-comment)
   `(font-lock-keyword-face ,face-keyword)
   `(font-lock-preprocessor-face ,face-keyword)
   `(font-lock-function-name-face ,face-function)
   `(font-lock-constant-face ,face-constant)
   `(font-lock-builtin-face ,face-constant)
   `(font-lock-type-face ,face-type)
   `(font-lock-variable-name-face ,face-variable)

   ;;
   ;; helm
   ;;

   ;; helm-M-x-key                            
   ;; helm-action                              
   ;; helm-ag-edit-deleted-line                
   ;; helm-bookmark-addressbook                
   ;; helm-bookmark-directory                  
   ;; helm-bookmark-file                       
   ;; helm-bookmark-gnus                       
   ;; helm-bookmark-info                       
   ;; helm-bookmark-man                        
   ;; helm-bookmark-w3m                        
   ;; helm-buffer-directory                    
   ;; helm-buffer-file                         
   ;; helm-buffer-not-saved                    
   ;; helm-buffer-process                      
   ;; helm-buffer-saved-out                    
   ;; helm-buffer-size                         
   ;; helm-candidate-number                    
   ;; helm-etags-file                          
   ;; helm-ff-directory                        
   ;; helm-ff-dotted-directory                 
   ;; helm-ff-dotted-symlink-directory         
   ;; helm-ff-executable                       
   ;; helm-ff-file                             
   ;; helm-ff-invalid-symlink                  
   ;; helm-ff-prefix                           
   ;; helm-ff-symlink                          
   ;; helm-grep-cmd-line                       
   ;; helm-grep-file                           
   ;; helm-grep-finish                         
   ;; helm-grep-lineno                         
   ;; helm-grep-match                          
   ;; helm-header                              
   ;; helm-header-line-left-margin             
   ;; helm-helper                              
   ;; helm-history-deleted                     
   ;; helm-history-remote                      
   ;; helm-lisp-completion-info                
   ;; helm-lisp-show-completion                
   ;; helm-locate-finish                       
   ;; helm-match                               
   ;; helm-match-item                          
   ;; helm-moccur-buffer                       
   ;; helm-prefarg                             
   ;; helm-resume-need-update                  
   ;; helm-selection                           
   ;; helm-selection-line                      
   ;; helm-separator                           
   ;; helm-source-header                       
   ;; helm-visible-mark                        
   
   ;;
   ;; web-mode
   ;;

   ;; web-mode-error-face
   ;; web-mode-warning-face
   ;; web-mode-preprocessor-face
   ;; web-mode-block-delimiter-face
   ;; web-mode-block-control-face
   ;; web-mode-builtin-face
   ;; web-mode-symbol-face
   ;; web-mode-doctype-face
   '(web-mode-html-tag-face ((t (:foreground "green3" :weight bold))))
   '(web-mode-html-tag-custom-face ((t (:foreground "green3" :weight bold))))
   ;; web-mode-html-tag-bracket-face
   '(web-mode-html-attr-name-face ((t (:foreground "purple" :weight bold))))
   ;; web-mode-html-attr-custom-face
   ;; web-mode-html-attr-engine-face
   ;; web-mode-html-attr-equal-face
   ;; web-mode-html-attr-value-face
   ;; web-mode-block-attr-name-face
   ;; web-mode-block-attr-value-face
   ;; web-mode-variable-name-face
   ;; web-mode-css-selector-face
   ;; web-mode-css-pseudo-class-face
   ;; web-mode-css-at-rule-face
   ;; web-mode-css-property-name-face
   ;; web-mode-css-color-face
   ;; web-mode-css-priority-face
   ;; web-mode-css-function-face
   ;; web-mode-css-variable-face
   ;; web-mode-function-name-face
   ;; web-mode-filter-face
   ;; web-mode-function-call-face
   ;; web-mode-string-face
   ;; web-mode-block-string-face
   ;; web-mode-part-string-face
   ;; web-mode-javascript-string-face
   ;; web-mode-css-string-face
   ;; web-mode-json-key-face
   ;; web-mode-json-context-face
   ;; web-mode-json-string-face
   ;; web-mode-comment-face
   ;; web-mode-block-comment-face
   ;; web-mode-part-comment-face
   ;; web-mode-json-comment-face
   ;; web-mode-javascript-comment-face
   ;; web-mode-css-comment-face
   ;; web-mode-constant-face
   ;; web-mode-type-face
   ;; web-mode-keyword-face
   ;; web-mode-param-name-face
   ;; web-mode-whitespace-face
   ;; web-mode-inlay-face
   ;; web-mode-block-face
   ;; web-mode-part-face
   ;; web-mode-script-face
   ;; web-mode-style-face
   ;; web-mode-folded-face
   ;; web-mode-bold-face
   ;; web-mode-italic-face
   ;; web-mode-underline-face
   ;; web-mode-current-element-highlight-face
   ;; web-mode-current-column-highlight-face
   ;; web-mode-comment-keyword-face
   ;; web-mode-sql-keyword-face
   ;; web-mode-html-entity-face
   
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
   ))

(provide-theme 'it3ration)
