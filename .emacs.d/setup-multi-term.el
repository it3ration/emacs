(defun new-term (path name)
  "This function allows you to open a terminal as a named buffer in a specific directory."
  (interactive (list 
                (if (and (featurep 'ido) ido-mode)
                    (ido-read-directory-name "path: ")
                  (read-directory-name "path: "))
                (read-string "name: ")))
  (let ((buffer-name "*terminal<1>*")
        (default-directory path))
    (multi-term)
    (save-excursion
      (set-buffer buffer-name)
      (rename-buffer name))))

(provide 'setup-multi-term)
