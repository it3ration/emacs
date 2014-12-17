;; the following allows you to open all marked 
;; files by typing 'F' when in dired mode
(define-key dired-mode-map "F" 'my-dired-find-file)
(defun my-dired-find-file (&optional arg)
  "Open each of the marked files, or the file under the point, or when prefix arg, the next N files."
  (interactive "P")
  (let* ((fn-list (dired-get-marked-files nil arg)))
    (mapc 'find-file fn-list)))

(provide 'setup-dired)
