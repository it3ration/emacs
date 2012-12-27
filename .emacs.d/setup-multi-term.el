(defun new-term (name dir)
  "This function allows you to open a terminal as a named buffer in a specific directory."
  (interactive "sname: \nsdir: ")
  (let ((buffer-name "*terminal<1>*")
        (default-directory dir))
    (multi-term)
    (save-excursion
      (set-buffer buffer-name)
      (rename-buffer name))))

(provide 'setup-multi-term)
