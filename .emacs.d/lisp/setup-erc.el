(require 'f)
(require 's)
(require 'dash)

(defun chat-load-settings (file)
  "Tries to load nickname / password as association list."
  (when (file-exists-p file)
    (let ((lines (-map 's-trim (s-lines (f-read file 'utf-8)))))
      (list (nth 0 lines) (nth 1 lines)))))

(defun chat-connect (file server port)
  "Tries to load credentials from file, connect to server / port."
  (let ((settings (chat-load-settings file)))
    (if (listp settings)
        (erc :server server
             :port port
             :nick (nth 0 settings)
             :password (nth 1 settings))
      (erc :server server
           :port port))))

(defun chat ()
  "Main erc chat."
  (interactive)
  (chat-connect "~/.erc" "irc.freenode.net" 6667))

(provide 'setup-erc)
