;;; flymake-jslint-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flymake-jslint" "flymake-jslint.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from flymake-jslint.el

(defvar flymake-jslint-detect-trailing-comma t "\
Whether or not to report warnings about trailing commas.")

(custom-autoload 'flymake-jslint-detect-trailing-comma "flymake-jslint" t)

(defvar flymake-jslint-command "jsl" "\
Whether or not to report warnings about trailing commas.")

(custom-autoload 'flymake-jslint-command "flymake-jslint" t)

(autoload 'flymake-jslint-load "flymake-jslint" "\
Configure flymake mode to check the current buffer's javascript syntax.

This function is designed to be called in `js-mode-hook' or
equivalent; it does not alter flymake's global configuration, so
function `flymake-mode' alone will not suffice." t nil)

(register-definition-prefixes "flymake-jslint" '("flymake-jslint-"))

;;;***

;;;### (autoloads nil nil ("flymake-jslint-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flymake-jslint-autoloads.el ends here
