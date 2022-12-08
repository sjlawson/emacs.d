
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(add-to-list 'load-path "~/.emacs.d/lisp")

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (package-initialize)


(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(transient-mark-mode 1) ; makes the region visible
(line-number-mode 1)    ; makes the line number show up
(column-number-mode 1)  ; makes the column number show up
(global-set-key [(control tab)] 'indent-region)
(global-set-key [(control \,)] 'goto-line)
(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-width 4)
(setq c-basic-indent 4)

(defun clean-php-mode ()
  (interactive)
  (php-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq tab-width 4)
  (setq c-basic-indent 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode 1)
  (setq fill-column 78)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-close 'c-lineup-arglist-operators)
)

(c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
(c-set-offset 'arglist-cont-nonempty 'c-lineup-math)
(show-paren-mode)


(defun ktw-buffer-clean ()
;  "Will untabify the buffer and remove trailing whitespace."
  (interactive "*")
  (save-excursion
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))))

(setq delete-trailing-whitespace t)
(add-hook 'before-save-hook 'ktw-buffer-clean)

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))
(global-set-key [(control c) (p)] 'forward-or-backward-sexp)

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

(add-to-list 'load-path "/Users/samuellawson/.emacs.d/elpy")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(toxi))
 '(custom-safe-themes
   '("854f83f6a13c588984dc57d800101ee292b95918d3794e60f51a45190c38e2ab" default))
 '(js-indent-level 2)
 '(package-selected-packages
   '(ts es-mode hacker-typer eslint-rc lsp-jedi company auto-complete jedi importmagic browse-url-dwim tabbar typescript-mode markdown-mode ##))
 '(warning-suppress-types '((lsp-mode) (lsp-mode) (lsp-mode))))

(load "elpy")
(load "elpy-rpc")
(load "elpy-shell")
(load "elpy-profile")
(load "elpy-refactor")
(load "elpy-django")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(treemacs-file-face ((t (:foreground "#aabbff")))))

(setq default-frame-alist
       '((height . 55)
         (width . 174)
         (left . 100)
         (top . 50)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (tool-bar-lines . 1)))

;; (when window-system (set-frame-size (selected-frame) 200 47))
;; (add-to-list 'default-frame-alist '(height . 47))
;; (add-to-list 'default-frame-alist '(width . 200))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(load "elpy")
(elpy-enable)

;; (let ((filename "~/.emacs.d/startup.txt"))
;;   (when (file-exists-p filename)
;;     (setq initial-buffer-choice filename)))

;; (require 'blacken)
;; (setq blacken-line-length 88)

(set-frame-font
    "-outline-Courier New-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1")

(setq mac-command-modifier 'meta)

(eval-after-load "elpy"
  '(cl-dolist (key '("M-<left>" "M-<right>"))
     (define-key elpy-mode-map (kbd key) nil)))

(require 'treemacs)
(tabbar-mode)
(run-with-timer 1 nil (lambda () (save-selected-window (treemacs))))

(defun set-tabbar-nav-keys ()
  (global-set-key (kbd "C-c <right>") 'tabbar-forward-tab)
  (global-set-key (kbd "C-c <left>") 'tabbar-backward-tab)
  )
(run-with-timer 2 nil (lambda() (set-tabbar-nav-keys)))

(require 'use-package)
(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

(setq lsp-jedi-workspace-extra-paths
  (vconcat lsp-jedi-workspace-extra-paths
           ["/Users/samuellawson/workspace/venv_res/lib/python3.8/site-packages"]))
