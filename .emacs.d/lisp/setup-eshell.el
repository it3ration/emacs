(require 's)
(require 'f)

;; Setup our path.
(setenv "PATH"
 (concat
  "~/bin" ":"
  "/usr/local/bin" ":"
  "/usr/local/sbin" ":"
  "/usr/bin" ":"
  "/usr/sbin" ":"
  "/bin" ":"
  "/sbin" ":"
  "./node_modules/.bin"))

;; History size.
(setq eshell-history-size 100000)

;; Make our prompt look like the terminal.
(setq eshell-prompt-function
      (lambda ()
        (let ((color-yellow "#ffff87")
              (color-green "#00af00")
              (color-red "#d75f5f"))
          (concat
           (propertize "[" 'face `(:foreground ,color-yellow :weight bold))
           (propertize
            (concat
             (user-login-name)
             "@"
             (s-trim (shell-command-to-string "hostname -s")))
            'face `(:foreground ,color-green :weight bold))
           " "
           (propertize
            (f-short (s-trim (shell-command-to-string "pwd")))
            'face `(:foreground ,color-red :weight bold))
           (propertize "]" 'face `(:foreground ,color-yellow :weight bold))
           "\n"
           (propertize "$" 'face `(:foreground ,color-red :weight bold))
           " "))))

;; Fix the prompt regex.
(setq eshell-prompt-regexp "^[$] ")

(provide 'setup-eshell)
