;; (setq
;;  mode-line-format
;;  '("%e"
;;    mode-line-front-space
;;    mode-line-mule-info
;;    mode-line-client
;;    mode-line-modified
;;    mode-line-remote
;;    mode-line-frame-identification
;;    mode-line-buffer-identification
;;    "   "
;;    mode-line-position
;;    (vc-mode vc-mode)
;;    "  "
;;    mode-line-modes
;;    mode-line-misc-info
;;    mode-line-end-spaces))

;; (require 'dash)
(require 's)

(defun my-source-control-string ()
  "Builds a source control string or nil."
  (when vc-mode
    `(" ["
      ,(s-trim (substring-no-properties vc-mode))
      "]")))
    
(setq-default
 mode-line-format
 '(
   "%b (%l, %c) [%m]"
   (:eval (my-source-control-string))
   ))

;; A string is printed verbatim in the mode line except for %-constructs:
;;   %b -- print buffer name.      %f -- print visited file name.
;;   %F -- print frame name.
;;   %* -- print %, * or hyphen.   %+ -- print *, % or hyphen.
;; 	%& is like %*, but ignore read-only-ness.
;; 	% means buffer is read-only and * means it is modified.
;; 	For a modified read-only buffer, %* gives % and %+ gives *.
;;   %s -- print process status.   %l -- print the current line number.
;;   %c -- print the current column number (this makes editing slower).
;;         To make the column number update correctly in all cases,
;; 	`column-number-mode' must be non-nil.
;;   %i -- print the size of the buffer.
;;   %I -- like %i, but use k, M, G, etc., to abbreviate.
;;   %p -- print percent of buffer above top of window, or Top, Bot or All.
;;   %P -- print percent of buffer above bottom of window, perhaps plus Top,
;;         or print Bottom or All.
;;   %n -- print Narrow if appropriate.
;;   %t -- visited file is text or binary (if OS supports this distinction).
;;   %z -- print mnemonics of keyboard, terminal, and buffer coding systems.
;;   %Z -- like %z, but including the end-of-line format.
;;   %e -- print error message about full memory.
;;   %@ -- print @ or hyphen.  @ means that default-directory is on a
;;         remote machine.
;;   %[ -- print one [ for each recursive editing level.  %] similar.
;;   %% -- print %.   %- -- print infinitely many dashes.
;; Decimal digits after the % specify field width to which to pad.

(provide 'setup-modeline)
