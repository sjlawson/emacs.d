;;; importmagic-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "importmagic" "importmagic.el" (0 0 0 0))
;;; Generated autoloads from importmagic.el

(autoload 'importmagic-mode "importmagic" "\
A mode that lets you autoimport unresolved Python symbols.

This is a minor mode.  If called interactively, toggle the
`importmagic mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `importmagic-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "importmagic" '("importmagic-"))

;;;***

;;;### (autoloads nil nil ("importmagic-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; importmagic-autoloads.el ends here
