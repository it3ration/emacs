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
  "/sbin"))

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

;; Aliases.
(defalias 'la "ls -lha")
(defalias 'll "ls -lha")

(provide 'setup-eshell)

;; # ls aliases
;; alias ls='CLICOLOR_FORCE=1 ls'
;; alias la='ls -lha'
;; alias ll='ls -lha'

;; # make rm, cp, mv safe
;; alias rm='rm -i'
;; alias cp='cp -i'
;; alias mv='mv -i'

;; # tar alias
;; alias tar="COPYFILE_DISABLE=true tar"

;; # du/df
;; alias du="du -h"
;; alias df="df -h"

;; # more / less
;; alias more="more -R"
;; alias less="less -R"

;; # ag
;; alias ag='ag -f --color'

;; # grep
;; alias grep="grep --color=always"
;; alias egrep="egrep --color=always"

;; # perforce
;; export P4CONFIG=.p4settings
;; export P4IGNORE=.p4ignore

;; # start in ~ please
;; cd ~
