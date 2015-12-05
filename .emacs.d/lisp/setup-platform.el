;; Are we on a gui version of emacs?
(setq is-gui (equal window-system 'ns))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

(provide 'setup-platform)
