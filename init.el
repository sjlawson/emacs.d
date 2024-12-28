
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/codeium.el")
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?$" . typescript-mode))


(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (package-initialize)

(setq warning-minimum-level :error)
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
    (unless (eq major-mode 'markdown-mode)
      (delete-trailing-whitespace)
      )
    (unless (eq major-mode 'makefile-gmake-mode)
      (untabify (point-min) (point-max)))
    )
  )

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

(add-to-list 'load-path "~/.emacs.d/elpy")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(codeium/metadata/api_key "a563aa8b-5f09-4fec-bb59-5770d6140652")
 '(custom-enabled-themes '(wheatgrass))
 '(custom-safe-themes
   '("bb8f8516765a86eae482d32ec956479719217c2c5322f18652a7126dc82a8241" "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "854f83f6a13c588984dc57d800101ee292b95918d3794e60f51a45190c38e2ab" default))
 '(elpy-syntax-check-command "flake8")
 '(js-indent-level 2)
 '(package-selected-packages
   '(rjsx-mode ess flymake-eslint flymake-python-pyflakes flymake-jslint flymake-css flymake yaml-mode dockerfile-mode nodejs-repl flycheck-pycheckers elisp-lint eldoc-box eldoc pydoc blacken s company-anaconda company-jedi xml-format magit prettier ts es-mode hacker-typer eslint-rc lsp-jedi company auto-complete jedi importmagic browse-url-dwim tabbar typescript-mode markdown-mode ##))
 '(typescript-indent-level 2)
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
       '((height . 50)
         (width . 174)
         (top . 1)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (tool-bar-lines . 1)))

(when window-system (set-frame-size (selected-frame) 174 50))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 174))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(load "elpy")
(elpy-enable)
(setq elpy-rpc-python-command "python")
;; (let ((filename "~/.emacs.d/startup.txt"))
;;   (when (file-exists-p filename)
;;     (setq initial-buffer-choice filename)))

;; (require 'blacken)
;; (setq blacken-line-length 110)

;; (setq mac-command-modifier 'meta)

(eval-after-load "elpy"
  '(cl-dolist (key '("M-<left>" "M-<right>"))
     (define-key elpy-mode-map (kbd key) nil)))

(require 'treemacs)
(tabbar-mode)
;; (run-with-timer 1 nil (lambda () (save-selected-window (treemacs))))

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

;; python/pydoc bindings
(add-hook 'python-mode-hook
      (lambda ()
        (local-set-key (kbd "C-h f") 'pydoc-at-point)))

(defun pydoc-at-point ()
  "Run `pydoc' on the word around point."
  (interactive)
  (pydoc (python-eldoc--get-symbol-at-point)))

(defun pydoc (word)
  "Run `pydoc' on WORD."
  (interactive
   (list (let* ((default-entry (python-eldoc--get-symbol-at-point))
                (input (read-string
                        (format "pydoc entry%s: "
                                (if (string= default-entry "")
                                    ""
                                  (format " (default %s)" default-entry))))))
           (if (string= input "")
               (if (string= default-entry "")
                   (error "No pydoc args given")
                 default-entry)
             input))))
  (require 'man)
  (let* ((case-fold-search nil)
     (Man-switches nil)
     (manual-program "pydoc"))
    (Man-getpage-in-background word)))

;; we recommend using use-package to organize your init.el
(use-package codeium
    ;; if you use straight
    ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
    ;; otherwise, make sure that the codeium.el file is on load-path

    :init
    ;; use globally
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))
