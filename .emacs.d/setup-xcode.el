; compile xcode
(defun xcode-compile ()
  (interactive)
  (if (directory-files "." nil ".*\.xcodeproj$" nil)
      (compile "xcodebuild -target \"Mac\" -configuration \"Debug\"")
    (progn
      (cd "../")
      (xcode-compile))))

;; follow compilation output
(setq compilation-scroll-output t)

;; bind xcode build to f5
(global-set-key (kbd "<f5>") 'xcode-compile)

(provide 'setup-xcode)
