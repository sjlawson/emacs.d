;;; lsp-jedi.el --- Lsp client plugin for Python Jedi Language Server    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Fred Campos

;; Author: Fred Campos <fred.tecnologia@gmail.com>
;; Maintainer: Fred Campos
;; Version: 0.0.1
;; Package-Version: 1.2.2
;; Package-Commit: 5e3eb3e160c2d38b8bd2b5cd3b86fa4f823f9330
;; Package-Requires: ((emacs "25.1") (lsp-mode "6.0"))
;; Homepage: http://github.com/fredcamps/lsp-jedi
;; Keywords: language-server, tools, python, jedi, ide

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A Emacs client for Python Jedi Language Server (LSP plugin for lsp-mode Emacs)

;;; Code:
(require 'lsp-mode)

(defgroup lsp-jedi nil
  "LSP support for Python, using Jedi Python Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/fredcamps/lsp-jedi"))

(defcustom lsp-jedi-enable t
  "If non-nil enable jedi-language-server."
  :type 'boolean
  :group 'lsp-jedi)

(defcustom lsp-jedi-executable-command "jedi-language-server"
  "Specify your jedi-language-server executable."
  :type 'string
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (lsp-dependency 'lsp-jedi
                         `(:system ,value))
         (set-default symbol value))
  :group 'lsp-jedi)

(defcustom lsp-jedi-executable-args []
  "Specify the args list passed to your executable."
  :type 'lsp-string-vector
  :group 'lsp-jedi)

(defcustom lsp-jedi-startup-message nil
  "If non-nil enables jedi-language-server's message on startup.."
  :type 'boolean
  :group 'lsp-jedi)

(defcustom lsp-jedi-markup-kind-preferred nil
  "Type of markup."
  :type  '(choice (const :tag "Plain text" "plaintext")
                  (const :tag "Markdown" "markdown")
                  (other :tag "None" nil))
  :group 'lsp-jedi)

(defcustom lsp-jedi-trace-server "verbose"
  "Trace server."
  :type '(choice (const :tag "Disabled" "off")
                 (const :tag "Messages" "messages")
                 (const :tag "Verbose" "verbose"))
  :group 'lsp-jedi)

(defcustom lsp-jedi-diagnostics-enable nil
  "If non-nil enables diagnostics provided by Jedi."
  :type 'boolean
  :group 'lsp-jedi)

(defcustom lsp-jedi-diagnostics-did-open nil
  "When diagnostics are enabled, run on document open."
  :type 'boolean
  :group 'lsp-jedi)

(defcustom lsp-jedi-diagnostics-did-change nil
  "When diagnostics are enabled.
Run on in-memory document change (eg, while you're editing, without needing to save to disk)."
  :type 'boolean
  :group 'lsp-jedi)

(defcustom lsp-jedi-diagnostics-did-save nil
  "When diagnostics are enabled, run on document save (to disk)."
  :type 'boolean
  :group 'lsp-jedi)

(defcustom lsp-jedi-hover-enable t
  "Enable (or disable) all hover text.  If set to false, will cause the hover method not to be registered to the language server."
  :type 'boolean
  :group 'lsp-jedi)

(defcustom lsp-jedi-hover-disable-keyword-all nil
  "Don't hover on keywords."
  :type 'boolean
  :group 'lsp-jedi)

(defcustom lsp-jedi-hover-disable-keyword-names []
  "Don't hover on keywords based on names list."
  :type 'lsp-string-vector
  :group 'lsp-jedi)

(defcustom lsp-jedi-hover-disable-keyword-full-names []
  "Don't hover on keywords based on full names list."
  :type 'lsp-string-vector
  :group 'lsp-jedi)

(defcustom lsp-jedi-workspace-extra-paths []
  "Add additional paths for Jedi's analysis.  Useful with vendor directories, packages in a non-standard location, etc."
  :type 'lsp-string-vector
  :group 'lsp-jedi)

(defcustom lsp-jedi-completion-disable-snippets nil
  "If your language client supports CompletionItem snippets but
you don't like them, disable them by setting this option to a
non-nil value."
  :type 'boolean
  :group 'lsp-jedi)

(defcustom lsp-jedi-auto-import-modules []
  "Modules that will not be analyzed but imported. Improves
autocompletion performance but loses goto definition."
  :type 'lsp-string-vector
  :group 'lsp-jedi)

(defcustom lsp-jedi-python-library-directories '("/usr/")
  "List of directories which will be considered to be libraries."
  :risky t
  :type '(repeat directory)
  :group 'lsp-jedi)

(defcustom lsp-jedi-case-insensitive-completion t
  "Completions are by default case insensitive."
  :type 'boolean
  :group 'lsp-jedi)

(defcustom lsp-jedi-debug nil
  "Print jedi debugging messages to stderr."
  :type 'boolean
  :group 'lsp-jedi)

(defcustom lsp-jedi-code-action-name-extract-function "jls_extract_def"
  "Function name generated by the 'extract_function' codeAction."
  :type 'string
  :group 'lsp-jedi)

(defcustom lsp-jedi-code-action-name-extract-variable "jls_extract_var"
  "Variable name generated by the 'extract_variable' codeAction."
  :type 'string
  :group 'lsp-jedi)

(defcustom lsp-jedi-completion-resolve-eagerly nil
  "Return all completion results in initial completion request."
  :type 'boolean
  :group 'lsp-jedi)

(defcustom lsp-jedi-workspace-symbols-max-symbols 20
  "Maximum number of symbols returned by a call to `workspace/symbols'."
  :type 'integer
  :group 'lsp-jedi)

(defcustom lsp-jedi-workspace-symbols-ignore-folders
  [".nox" ".tox" ".venv" "__pycache__" "venv"]
  "Performance optimization that sets names of folders that are ignored for the workspace symbols action."
  :type 'lsp-string-vector
  :group 'lsp-jedi)

(lsp-register-custom-settings
 '(("jedi.enable" lsp-jedi-enable)
   ("jedi.startupMessage" lsp-jedi-startup-message)
   ("jedi.markupKindPreferred" lsp-jedi-markup-kind-preferred)
   ("jedi.trace.server" lsp-jedi-trace-server)
   ("jedi.jediSettings.autoImportModules" lsp-jedi-auto-import-modules)
   ("jedi.jediSettings.caseInsensitiveCompletion" lsp-jedi-case-insensitive-completion t)
   ("jedi.jediSettings.debug" lsp-jedi-debug)
   ("jedi.executable.command" lsp-jedi-executable-command)
   ("jedi.executable.args" lsp-jedi-executable-args)
   ("jedi.codeAction.nameExtractFunction" lsp-jedi-code-action-name-extract-function)
   ("jedi.codeAction.nameExtractVariable" lsp-jedi-code-action-name-extract-variable)
   ("jedi.completion.disableSnippets" lsp-jedi-completion-disable-snippets t)
   ("jedi.completion.resolveEagerly" lsp-jedi-completion-resolve-eagerly t)
   ("jedi.diagnostics.enable" lsp-jedi-diagnostics-enable t)
   ("jedi.diagnostics.didOpen" lsp-jedi-diagnostics-did-open t)
   ("jedi.diagnostics.didChange" lsp-jedi-diagnostics-did-change t)
   ("jedi.diagnostics.didSave" lsp-jedi-diagnostics-did-save t)
   ("jedi.hover.enable" lsp-jedi-hover-enable)
   ("jedi.hover.disable.keyword.all" lsp-jedi-hover-disable-keyword-all t)
   ("jedi.hover.disable.keyword.names" lsp-jedi-hover-disable-keyword-names)
   ("jedi.hover.disable.keyword.fullNames" lsp-jedi-hover-disable-keyword-full-names)
   ("jedi.workspace.extraPaths" lsp-jedi-workspace-extra-paths)
   ("jedi.workspace.symbols.maxSymbols" lsp-jedi-workspace-symbols-max-symbols)
   ("jedi.workspace.symbols.ignoreFolders" lsp-jedi-workspace-symbols-ignore-folders)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda () lsp-jedi-executable-command))
  :major-modes '(python-mode cython-mode)
  :priority -1
  :server-id 'jedi
  :library-folders-fn (lambda (_workspace) lsp-jedi-python-library-directories)
  :initialization-options (lambda () (gethash "jedi" (lsp-configuration-section "jedi")))))

(provide 'lsp-jedi)
;;; lsp-jedi.el ends here
