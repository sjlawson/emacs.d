;;; eslint-rc-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eslint-rc" "eslint-rc.el" (0 0 0 0))
;;; Generated autoloads from eslint-rc.el

(autoload 'eslint-rc-mode "eslint-rc" "\
Runs eslint on file save using local rc rules when this mode is turned on.

This is a minor mode.  If called interactively, toggle the
`Eslint-RC mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `eslint-rc-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "eslint-rc" '("eslint-rc"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; eslint-rc-autoloads.el ends here
