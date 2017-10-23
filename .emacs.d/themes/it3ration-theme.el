(deftheme it3ration "A theme that's easier on the eyes.")

(let ((c-default "#303030")
      (c-text "#e5e5e5")
      (c-highlight "#870000")
      (c-region "#1c1c1c")
      (c-search "#005f00")
      (c-search-current "#005fff")
      (c-paren-match "#0087ff")
      (c-paren-mismatch "#00cd00")
      (c-string "#ffff87")
      (c-comment "#87875f")
      (c-keyword "#d75f5f")
      (c-function "#00af00")
      (c-constant "#af87ff")
      (c-type "#00cdcd")
      (c-variable "#ff8700"))
  
  ;; old
  ;; (face-header         '((t (:background "#005faf" :bold t))))
  ;; (face-diff-added     '((t (:foreground "#00af00" :bold t))))
  ;; (face-diff-removed   '((t (:foreground "#d75f5f" :bold t)))))
  (custom-theme-set-faces
   'it3ration

   ;;
   ;; defaults
   ;;

   `(default ((t (:background ,c-default :foreground ,c-text))))
   `(highlight ((t (:background ,c-highlight))))
   `(region ((t (:background ,c-region))))
   `(lazy-highlight ((t (:background ,c-search))))
   `(isearch ((t (:background ,c-search-current))))
   `(query-replace ((t (:background ,c-search-current))))
   `(show-paren-match ((t (:background ,c-paren-match))))
   `(show-paren-mismatch ((t (:background ,c-paren-mismatch))))

   ;;
   ;; font lock
   ;;

   `(font-lock-string-face ((t (:foreground ,c-string))))
   `(font-lock-comment-face ((t (:foreground ,c-comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,c-comment))))
   `(font-lock-doc-face ((t (:foreground ,c-comment))))
   `(font-lock-keyword-face ((t (:foreground ,c-keyword :bold t))))
   `(font-lock-preprocessor-face ((t (:foreground ,c-keyword :bold t))))
   `(font-lock-function-name-face ((t (:foreground ,c-function :bold t))))
   `(font-lock-constant-face ((t (:foreground ,c-constant :bold t))))
   `(font-lock-builtin-face ((t (:foreground ,c-constant :bold t))))
   `(font-lock-type-face ((t (:foreground ,c-type :bold t))))
   `(font-lock-variable-name-face ((t (:foreground ,c-variable :bold t))))

   ;;
   ;; helm
   ;;

   ;; helm-M-x-key                             
   ;; helm-action                              
   ;; helm-ag-edit-deleted-line                
   ;; helm-bookmark-addressbook                
   ;; helm-bookmark-directory                  
   ;; helm-bookmark-file                       
   ;; helm-bookmark-file-not-found             
   ;; helm-bookmark-gnus                       
   ;; helm-bookmark-info                       
   ;; helm-bookmark-man                        
   ;; helm-bookmark-w3m                        
   ;; helm-buffer-directory                    
   ;; helm-buffer-file                         
   ;; helm-buffer-modified                     
   ;; helm-buffer-not-saved                    
   ;; helm-buffer-process                      
   ;; helm-buffer-saved-out                    
   ;; helm-buffer-size                         
   ;; helm-candidate-number                    
   ;; helm-candidate-number-suspended          
   ;; helm-etags-file                          
   ;; helm-ff-directory                        
   ;; helm-ff-dirs                             
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
   ;; helm-non-file-buffer                     
   ;; helm-prefarg                             
   ;; helm-resume-need-update                  
   ;; helm-selection                           
   ;; helm-selection-line                      
   ;; helm-separator                           
   ;; helm-source-header                       
   ;; helm-swoop-line-number-face              
   ;; helm-swoop-target-line-block-face        
   ;; helm-swoop-target-line-face              
   ;; helm-swoop-target-word-face              
   ;; helm-visible-mark                        

   ;; `(helm-ff-directory ,face-keyword)
   ;; `(helm-ff-dotted-directory ,face-keyword)
   ;; `(helm-ff-dotted-symlink-directory ,face-string)
   ;; `(helm-ff-executable ,face-type)
   ;; `(helm-ff-file ,face-text)
   ;; `(helm-ff-invalid-symlink ,face-string)
   ;; `(helm-ff-prefix ,face-text)
   ;; `(helm-ff-symlink ,face-string)
   ;; `(helm-selection ,face-highlight)
   ;; `(helm-source-header ,face-header)
   ;; `(helm-visible-mark ,face-region)

   ;;
   ;; eshell
   ;;

   ;; eshell-ls-archive
   ;; eshell-ls-backup
   ;; eshell-ls-clutter
   ;; eshell-ls-directory
   ;; eshell-ls-executable
   ;; eshell-ls-missing
   ;; eshell-ls-product
   ;; eshell-ls-readonly
   ;; eshell-ls-special
   ;; eshell-ls-symlink
   ;; eshell-ls-unreadable
   ;; eshell-prompt
   
   ;;
   ;; web-mode
   ;;

   ;; ;; web-mode-error-face
   ;; ;; web-mode-warning-face
   ;; ;; web-mode-preprocessor-face
   ;; ;; web-mode-block-delimiter-face
   ;; ;; web-mode-block-control-face
   ;; ;; web-mode-builtin-face
   ;; ;; web-mode-symbol-face
   ;; ;; web-mode-doctype-face
   ;; `(web-mode-html-tag-face ,face-keyword)
   ;; `(web-mode-html-tag-custom-face ,face-keyword)
   ;; ;; web-mode-html-tag-bracket-face
   ;; `(web-mode-html-attr-name-face ,face-constant)
   ;; ;; web-mode-html-attr-custom-face
   ;; ;; web-mode-html-attr-engine-face
   ;; ;; web-mode-html-attr-equal-face
   ;; ;; web-mode-html-attr-value-face
   ;; ;; web-mode-block-attr-name-face
   ;; ;; web-mode-block-attr-value-face
   ;; ;; web-mode-variable-name-face
   ;; ;; web-mode-css-selector-face
   ;; ;; web-mode-css-pseudo-class-face
   ;; ;; web-mode-css-at-rule-face
   ;; ;; web-mode-css-property-name-face
   ;; ;; web-mode-css-color-face
   ;; ;; web-mode-css-priority-face
   ;; ;; web-mode-css-function-face
   ;; ;; web-mode-css-variable-face
   ;; ;; web-mode-function-name-face
   ;; ;; web-mode-filter-face
   ;; ;; web-mode-function-call-face
   ;; ;; web-mode-string-face
   ;; ;; web-mode-block-string-face
   ;; ;; web-mode-part-string-face
   ;; ;; web-mode-javascript-string-face
   ;; ;; web-mode-css-string-face
   ;; ;; web-mode-json-key-face
   ;; ;; web-mode-json-context-face
   ;; ;; web-mode-json-string-face
   ;; ;; web-mode-comment-face
   ;; ;; web-mode-block-comment-face
   ;; ;; web-mode-part-comment-face
   ;; ;; web-mode-json-comment-face
   ;; ;; web-mode-javascript-comment-face
   ;; ;; web-mode-css-comment-face
   ;; ;; web-mode-constant-face
   ;; ;; web-mode-type-face
   ;; ;; web-mode-keyword-face
   ;; ;; web-mode-param-name-face
   ;; ;; web-mode-whitespace-face
   ;; ;; web-mode-inlay-face
   ;; ;; web-mode-block-face
   ;; ;; web-mode-part-face
   ;; ;; web-mode-script-face
   ;; ;; web-mode-style-face
   ;; ;; web-mode-folded-face
   ;; ;; web-mode-bold-face
   ;; ;; web-mode-italic-face
   ;; ;; web-mode-underline-face
   ;; ;; web-mode-current-element-highlight-face
   ;; ;; web-mode-current-column-highlight-face
   ;; ;; web-mode-comment-keyword-face
   ;; ;; web-mode-sql-keyword-face
   ;; ;; web-mode-html-entity-face
   
   ;;
   ;; magit
   ;;

   ;; ;; rebase
   ;; ;; git-rebase-hash
   ;; ;; git-rebase-description nil
   ;; ;; git-rebase-killed-action
   ;; ;; git-rebase-comment-hash
   ;; ;; git-rebase-comment-heading

   ;; ;; bisect
   ;; ;; magit-bisect-good
   ;; ;; magit-bisect-skip
   ;; ;; magit-bisect-bad

   ;; ;; blame
   ;; ;; magit-blame-heading
   ;; ;; magit-blame-summary
   ;; ;; magit-blame-hash
   ;; ;; magit-blame-name
   ;; ;; magit-blame-date

   ;; ;; diff
   ;; ;; magit-diff-file-heading
   ;; ;; magit-diff-file-heading-highlight
   ;; ;; magit-diff-file-heading-selection
   ;; ;; magit-diff-hunk-heading
   ;; ;; magit-diff-hunk-heading-highlight
   ;; ;; magit-diff-hunk-heading-selection
   ;; ;; magit-diff-lines-heading
   ;; ;; magit-diff-lines-boundary
   ;; ;; magit-diff-conflict-heading
   ;; `(magit-diff-added ,face-diff-added)
   ;; `(magit-diff-removed ,face-diff-removed)
   ;; ;; magit-diff-our
   ;; ;; magit-diff-base
   ;; ;; magit-diff-their
   ;; ;; magit-diff-context
   ;; `(magit-diff-added-highlight ,face-diff-added)
   ;; `(magit-diff-removed-highlight ,face-diff-removed)
   ;; ;; magit-diff-our-highlight
   ;; ;; magit-diff-base-highlight
   ;; ;; magit-diff-their-highlight
   ;; ;; magit-diff-context-highlight
   ;; ;; magit-diff-whitespace-warning
   ;; ;; magit-diffstat-added
   ;; ;; magit-diffstat-removed
   
   ;; ;; log
   ;; ;; magit-log-graph
   ;; ;; magit-log-author
   ;; ;; magit-log-date
   ;; ;; magit-reflog-commit
   ;; ;; magit-reflog-amend 
   ;; ;; magit-reflog-merge 
   ;; ;; magit-reflog-checkout
   ;; ;; magit-reflog-reset 
   ;; ;; magit-reflog-rebase
   ;; ;; magit-reflog-cherry-pireen
   ;; ;; magit-reflog-remote
   ;; ;; magit-reflog-other

   ;; ;; process
   ;; ;; magit-process-ok
   ;; ;; magit-process-ng

   ;; ;; section
   ;; ;; magit-section-highlight
   ;; ;; magit-section-heading
   ;; ;; magit-section-secondary-heading
   ;; ;; magit-section-heading-selection

   ;; ;; sequence
   ;; ;; magit-sequence-pick
   ;; ;; magit-sequence-stop
   ;; ;; magit-sequence-part
   ;; ;; magit-sequence-head
   ;; ;; magit-sequence-drop
   ;; ;; magit-sequence-done
   ;; ;; magit-sequence-onto

   ;; ;; magit
   ;; ;; magit-header-line
   ;; ;; magit-dimmed
   ;; ;; magit-hash
   ;; ;; magit-tag
   ;; ;; magit-branch-remote
   ;; ;; magit-branch-local
   ;; ;; magit-branch-current
   ;; ;; magit-head
   ;; ;; magit-refname
   ;; ;; magit-refname-stash
   ;; ;; magit-refname-wip
   ;; ;; magit-signature-good
   ;; ;; magit-signature-bad
   ;; ;; magit-signature-untrusted
   ;; ;; magit-cherry-unmatched
   ;; ;; magit-cherry-equivalent
   ;; ;; magit-filename
   ))

(provide-theme 'it3ration)
