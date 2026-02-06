;;; graphix-mode.el --- Major mode for Graphix programming language -*- lexical-binding: t -*-

;; Copyright (C) 2024 Graphix Authors

;; Author: Graphix Authors
;; Keywords: languages graphix
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; A major mode for editing Graphix source files (.gx)
;;
;; Installation:
;; 1. Place this file in your load-path
;; 2. Add to your init.el:
;;    (require 'graphix-mode)
;;
;; For LSP support with Eglot:
;;    (add-to-list 'eglot-server-programs '(graphix-mode . ("graphix" "lsp")))
;;
;; For LSP support with lsp-mode:
;;    (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-stdio-connection '("graphix" "lsp"))
;;                      :major-modes '(graphix-mode)
;;                      :server-id 'graphix-lsp))

;;; Code:

(defvar graphix-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments: // to end of line
    (modify-syntax-entry ?/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    ;; Brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    ;; Operators
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?~ "." table)
    (modify-syntax-entry ?$ "." table)
    ;; Underscore is part of identifiers
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table for `graphix-mode'.")

(defvar graphix-keywords
  '("let" "rec" "mod" "use" "type" "fn" "select" "if" "try" "catch"
    "cast" "any" "with" "where")
  "Graphix keywords.")

(defvar graphix-builtins
  '("true" "false" "null" "ok")
  "Graphix built-in constants.")

(defvar graphix-types
  '("bool" "string" "bytes" "i8" "u8" "i16" "u16"
    "i32" "u32" "v32" "z32" "i64" "u64" "v64" "z64"
    "f32" "f64" "decimal" "datetime" "duration"
    "Any" "Array" "Map")
  "Graphix built-in types.")

(defvar graphix-font-lock-keywords
  `(;; Documentation comments
    ("^\\s-*///.*$" . font-lock-doc-face)
    ;; Keywords
    (,(regexp-opt graphix-keywords 'words) . font-lock-keyword-face)
    ;; Built-in constants
    (,(regexp-opt graphix-builtins 'words) . font-lock-constant-face)
    ;; Built-in types
    (,(regexp-opt graphix-types 'words) . font-lock-type-face)
    ;; Type identifiers (capitalized)
    ("\\b[A-Z][a-zA-Z0-9_]*\\b" . font-lock-type-face)
    ;; Type variables
    ("'[a-z][a-zA-Z0-9_]*" . font-lock-type-face)
    ;; Labeled parameters
    ("#[a-z][a-zA-Z0-9_]*" . font-lock-variable-name-face)
    ;; Variants
    ("`[A-Z][a-zA-Z0-9_]*" . font-lock-constant-face)
    ;; Operators
    ("<-\\|=>\\|::\\|->\\|==\\|!=\\|<=\\|>=\\|&&\\|||" . font-lock-builtin-face))
  "Font lock keywords for `graphix-mode'.")

;;;###autoload
(define-derived-mode graphix-mode prog-mode "Graphix"
  "Major mode for editing Graphix source files.

\\{graphix-mode-map}"
  :syntax-table graphix-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+ *")
  (setq-local font-lock-defaults '(graphix-font-lock-keywords))
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gx\\'" . graphix-mode))

(provide 'graphix-mode)
;;; graphix-mode.el ends here
