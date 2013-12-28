(lazypile/init)

;; elisp
(lazypile/make 
 "elisp" 
 (lambda ()
   (append 
    (list ".emacs")
    (lazypile/find-files "find -E '.emacs.d/' -type f -depth 1 -regex '.*\.el' -print"))))


