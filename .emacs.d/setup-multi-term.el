(defun new-term (name &optional dir)
  "This function allows you to open a terminal as a named buffer in a specific directory."
  (interactive "sname: ")
  (let ((default-name "*terminal<1>*")
        (old-dir (file-truename default-directory)))
    (when (stringp dir) (cd dir))
    (multi-term)
    (save-excursion
      (set-buffer default-name)
      (rename-buffer name))
    (when (stringp dir) 
      (message "back: %s" old-dir)
      (cd old-dir))))

(provide 'setup-multi-term)
