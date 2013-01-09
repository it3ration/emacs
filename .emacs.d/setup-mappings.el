;; lua
(autoload 'lua-mode "lua-mode")
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; header files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(provide 'mode-mappings)
