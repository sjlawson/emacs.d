(deftheme sjlawson
  "Created 2023-12-13.")

(custom-theme-set-variables
 'sjlawson
 '(codeium/metadata/api_key "a563aa8b-5f09-4fec-bb59-5770d6140652")
 '(elpy-syntax-check-command "flake8")
 '(js-indent-level 2)
 '(package-selected-packages '(flymake-eslint flymake-python-pyflakes flymake-jslint flymake-css flymake yaml-mode dockerfile-mode nodejs-repl flycheck-pycheckers elisp-lint eldoc-box eldoc pydoc blacken s company-anaconda company-jedi xml-format magit prettier ts es-mode hacker-typer eslint-rc lsp-jedi company auto-complete jedi importmagic browse-url-dwim tabbar typescript-mode markdown-mode ##))
 '(typescript-indent-level 2)
 '(warning-suppress-types '((lsp-mode) (lsp-mode) (lsp-mode)))
 '(ispell-dictionary nil)
 '(custom-safe-themes '("2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "854f83f6a13c588984dc57d800101ee292b95918d3794e60f51a45190c38e2ab" default)))

(custom-theme-set-faces
 'sjlawson
 '(treemacs-file-face ((t (:foreground "#aabbff")))))

(provide-theme 'sjlawson)
