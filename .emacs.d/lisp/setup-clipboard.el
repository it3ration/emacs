(when is-mac
  ;; copy from the clipboard
  (defun mac-copy ()
    (shell-command-to-string "pbpaste"))

  ;; paste from the clipboard
  (defun mac-paste (text &optional push)
    (let ((process-connection-type nil)) 
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  ;; install the commands
  (setq interprogram-paste-function 'mac-copy)
  (setq interprogram-cut-function 'mac-paste))

(provide 'setup-clipboard)
