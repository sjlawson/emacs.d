;;; flymake-css-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flymake-css" "flymake-css.el" (0 0 0 0))
;;; Generated autoloads from flymake-css.el

(defvar flymake-css-lint-command "csslint" "\
Name (and optionally full path) of csslint executable.")

(custom-autoload 'flymake-css-lint-command "flymake-css" t)

(autoload 'flymake-css-load "flymake-css" "\
Configure flymake mode to check the current buffer's css syntax." t nil)

(register-definition-prefixes "flymake-css" '("flymake-css-"))

;;;***

;;;### (autoloads nil nil ("flymake-css-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flymake-css-autoloads.el ends here
