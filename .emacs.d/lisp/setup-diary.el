(require 'f)

(defvar diary/root
  "~/repos/diary/"
  "Where journal entries reside.")

(defvar diary/template
  "template.org"
  "Root-relative template for new entries.")

(defun diary/make-date-today ()
  "Returns today's date."
  (format-time-string "%Y-%m-%d"))

(defun diary/make-filename (date)
  "Returns the path for a diary entry."
  (concat diary/root date ".org"))

(defun diary/today ()
  "Opens today's diary entry."
  (interactive)
  (let ((path (diary/make-filename (diary/make-date-today))))
    (unless (f-exists? path)
      (f-copy (f-join diary/root diary/template) path))
    (find-file path)))
  
(provide 'setup-diary)
