;;; graphix-mode.el --- Major mode for Graphix programming language -*- lexical-binding: t -*-

;; Copyright (C) 2024 Graphix Authors

;; Author: Graphix Authors
;; Keywords: languages graphix
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Major modes for editing Graphix source files (.gx).
;;
;; Two modes are provided:
;;
;;   `graphix-mode'    — regex-based fallback for Emacs < 29 or missing grammar
;;   `graphix-ts-mode' — tree-sitter mode for Emacs 29+ (requires graphix grammar)
;;
;; When tree-sitter is available and the graphix grammar is installed,
;; `graphix-ts-mode' is used automatically.  Install the grammar with
;; M-x graphix-ts-mode-install-grammar.
;;
;; LSP support (eglot and lsp-mode) is configured automatically for both modes.

;;; Code:

;;; ---- Shared utilities ----

(defgroup graphix nil
  "Support for the Graphix programming language."
  :group 'languages
  :prefix "graphix-")

(defcustom graphix-indent-offset 4
  "Number of spaces for each indentation level in Graphix mode."
  :type 'integer
  :group 'graphix)

(defcustom graphix-lsp-server '("graphix" "lsp")
  "Command to start the Graphix LSP server."
  :type '(repeat string)
  :group 'graphix)

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
  "Syntax table for `graphix-mode' and `graphix-ts-mode'.")

;;; ---- Regex-based fallback mode ----

(defvar graphix-font-lock-keywords
  (let ((keywords '("let" "rec" "mod" "use" "type" "fn" "select" "if" "try"
                     "catch" "cast" "any" "with" "where" "throws" "as"))
        (builtins '("true" "false" "null" "ok"))
        (types '("bool" "string" "bytes" "i8" "u8" "i16" "u16"
                 "i32" "u32" "v32" "z32" "i64" "u64" "v64" "z64"
                 "f32" "f64" "decimal" "datetime" "duration"
                 "Any" "Array" "Map")))
    `(("^\\s-*///.*$" . font-lock-doc-face)
      (,(regexp-opt keywords 'words) . font-lock-keyword-face)
      (,(regexp-opt builtins 'words) . font-lock-constant-face)
      (,(regexp-opt types 'words) . font-lock-type-face)
      ("\\b[A-Z][a-zA-Z0-9_]*\\b" . font-lock-type-face)
      ("'[a-z][a-zA-Z0-9_]*" . font-lock-type-face)
      ("#[a-z][a-zA-Z0-9_]*" . font-lock-variable-name-face)
      ("`[A-Z][a-zA-Z0-9_]*" . font-lock-constant-face)
      ("<-\\|=>\\|::\\|->\\|==\\|!=\\|<=\\|>=\\|&&\\|||" . font-lock-builtin-face)))
  "Font lock keywords for `graphix-mode'.")

;;;###autoload
(define-derived-mode graphix-mode prog-mode "Graphix"
  "Major mode for editing Graphix source files (regex-based).

Falls back to this mode when tree-sitter is not available.

\\{graphix-mode-map}"
  :syntax-table graphix-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+ *")
  (setq-local font-lock-defaults '(graphix-font-lock-keywords))
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width graphix-indent-offset))

;;; ---- Tree-sitter mode (Emacs 29+) ----

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))

  (defvar graphix--treesit-font-lock-rules
    (treesit-font-lock-rules
     ;; Level 1: comments
     :language 'graphix
     :feature 'comment
     '((line_comment) @font-lock-comment-face
       (doc_comment) @font-lock-doc-face)

     ;; Level 1: keywords
     :language 'graphix
     :feature 'keyword
     '(["mod" "use" "let" "rec" "type" "fn" "select" "if" "try" "catch"
        "cast" "any" "with" "where" "throws" "as"] @font-lock-keyword-face)

     ;; Level 1: strings
     :language 'graphix
     :feature 'string
     '((string) @font-lock-string-face
       (raw_string) @font-lock-string-face
       (string_content) @font-lock-string-face
       (raw_string_content) @font-lock-string-face
       (escape_sequence) @font-lock-escape-face)

     ;; Level 2: types
     :language 'graphix
     :feature 'type
     '((type_identifier) @font-lock-type-face
       (primitive_type) @font-lock-builtin-face
       (type_variable) @font-lock-type-face
       (type_def name: (type_identifier) @font-lock-type-face))

     ;; Level 2: functions
     :language 'graphix
     :feature 'function
     '((apply function: (reference (module_path (identifier) @font-lock-function-name-face)))
       (builtin_ref (identifier) @font-lock-function-name-face))

     ;; Level 2: constants
     :language 'graphix
     :feature 'constant
     '((boolean) @font-lock-constant-face
       (null) @font-lock-constant-face)

     ;; Level 2: numbers
     :language 'graphix
     :feature 'number
     '((number) @font-lock-number-face)

     ;; Level 3: variables
     :language 'graphix
     :feature 'variable
     '((let_binding
        pattern: (structure_pattern
                  (pattern_bind name: (identifier) @font-lock-variable-name-face)))
       (pattern_bind name: (identifier) @font-lock-variable-name-face)
       (reference (module_path (identifier) @font-lock-variable-name-face)))

     ;; Level 3: parameters
     :language 'graphix
     :feature 'parameter
     '((labeled_param (identifier) @font-lock-variable-name-face)
       (labeled_param_with_default (identifier) @font-lock-variable-name-face)
       (labeled_arg (identifier) @font-lock-variable-name-face)
       (labeled_arg_shorthand (identifier) @font-lock-variable-name-face)
       (variadic_param (identifier) @font-lock-variable-name-face))

     ;; Level 3: properties
     :language 'graphix
     :feature 'property
     '((struct_field name: (identifier) @font-lock-property-name-face)
       (struct_ref (identifier) @font-lock-property-name-face)
       (struct_pattern_field name: (identifier) @font-lock-property-name-face)
       (struct_type_field name: (identifier) @font-lock-property-name-face))

     ;; Level 3: modules
     :language 'graphix
     :feature 'module
     '((module name: (identifier) @font-lock-constant-face)
       (module_path . (identifier) @font-lock-constant-face))

     ;; Level 3: constructors (variants)
     :language 'graphix
     :feature 'constructor
     '((variant (type_identifier) @font-lock-type-face)
       (variant_pattern (type_identifier) @font-lock-type-face)
       (variant_type (type_identifier) @font-lock-type-face))

     ;; Level 4: operators
     :language 'graphix
     :feature 'operator
     '(["+" "-" "*" "/" "%" "==" "!=" "<" ">" "<=" ">=" "&&" "||"
        "!" "~" "?" "$" "&" "<-" "=>" "=" "->" ":" "::" "." "," ";" "@" "`" "#"]
       @font-lock-operator-face)

     ;; Level 4: brackets
     :language 'graphix
     :feature 'bracket
     '(["(" ")" "[" "]" "{" "}" "|"] @font-lock-bracket-face)

     ;; Level 4: string interpolation brackets
     :language 'graphix
     :feature 'interpolation
     '((interpolation "[" @font-lock-misc-punctuation-face
                      "]" @font-lock-misc-punctuation-face)))
    "Tree-sitter font-lock rules for Graphix.")

  (defvar graphix--treesit-indent-rules
    `((graphix
       ((node-is "}") parent-bol 0)
       ((node-is "]") parent-bol 0)
       ((node-is ")") parent-bol 0)
       ((parent-is "module_body") parent-bol ,graphix-indent-offset)
       ((parent-is "do_block") parent-bol ,graphix-indent-offset)
       ((parent-is "select") parent-bol ,graphix-indent-offset)
       ((parent-is "struct") parent-bol ,graphix-indent-offset)
       ((parent-is "map") parent-bol ,graphix-indent-offset)
       ((parent-is "array") parent-bol ,graphix-indent-offset)
       ((parent-is "tuple") parent-bol ,graphix-indent-offset)
       ((parent-is "struct_type") parent-bol ,graphix-indent-offset)
       ((parent-is "tuple_type") parent-bol ,graphix-indent-offset)
       ((parent-is "signature") parent-bol ,graphix-indent-offset)
       ((parent-is "apply_args") parent-bol ,graphix-indent-offset)
       ((parent-is "lambda") parent-bol ,graphix-indent-offset)
       ((parent-is "match_arm") parent-bol ,graphix-indent-offset)
       (no-node parent-bol 0)))
    "Tree-sitter indentation rules for Graphix.")

  (defvar graphix--treesit-grammar-recipe
    '(graphix "https://github.com/estokes/graphix"
              nil                        ; tag
              "ide/tree-sitter-graphix/src") ; source dir
    "Tree-sitter grammar recipe for Graphix.")

  (defun graphix-ts-mode-install-grammar ()
    "Install the Graphix tree-sitter grammar.
Downloads and compiles the grammar from the graphix repository."
    (interactive)
    (add-to-list 'treesit-language-source-alist graphix--treesit-grammar-recipe)
    (treesit-install-language-grammar 'graphix))

  ;;;###autoload
  (define-derived-mode graphix-ts-mode prog-mode "Graphix"
    "Major mode for editing Graphix source files using tree-sitter.

Requires Emacs 29+ and the graphix tree-sitter grammar.
Install the grammar with \\[graphix-ts-mode-install-grammar].

\\{graphix-ts-mode-map}"
    :syntax-table graphix-mode-syntax-table
    (unless (treesit-ready-p 'graphix)
      (error "Graphix tree-sitter grammar not available; install with M-x graphix-ts-mode-install-grammar"))

    ;; Comments
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip "//+ *")

    ;; Indentation
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width graphix-indent-offset)
    (setq-local treesit-simple-indent-rules graphix--treesit-indent-rules)

    ;; Font-lock
    (setq-local treesit-font-lock-settings graphix--treesit-font-lock-rules)
    (setq-local treesit-font-lock-feature-list
                '((comment keyword string)
                  (type function constant number)
                  (variable parameter property module constructor)
                  (operator bracket interpolation)))

    ;; Navigation
    (setq-local treesit-defun-type-regexp
                (rx (or "module" "lambda" "let_binding" "type_def")))
    (setq-local treesit-thing-settings
                `((graphix
                   (sentence ,(rx (or "let_binding" "type_def" "use" "connect")))
                   (sexp ,(rx (or "module" "lambda" "do_block" "select" "if"
                                  "try_catch" "apply" "struct" "map" "array"
                                  "tuple" "reference" "number" "string"
                                  "boolean" "null"))))))

    (treesit-major-mode-setup))

  ;; Auto-remap to tree-sitter mode when grammar is available
  (when (treesit-ready-p 'graphix t)
    (add-to-list 'major-mode-remap-alist '(graphix-mode . graphix-ts-mode))))

;;; ---- File association ----

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gx\\'" . graphix-mode))

;;; ---- LSP integration ----

;; Eglot (built-in Emacs 29+)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((graphix-ts-mode graphix-mode) . ,graphix-lsp-server)))

;; lsp-mode (external package)
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(graphix-mode . "graphix"))
  (add-to-list 'lsp-language-id-configuration '(graphix-ts-mode . "graphix"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection graphix-lsp-server)
    :major-modes '(graphix-mode graphix-ts-mode)
    :server-id 'graphix-lsp)))

(provide 'graphix-mode)
;;; graphix-mode.el ends here
