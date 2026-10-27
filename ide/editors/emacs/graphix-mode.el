;;; graphix-mode.el --- Major mode for Graphix programming language -*- lexical-binding: t -*-

;; Copyright (C) 2024 Graphix Authors

;; Author: Graphix Authors
;; Keywords: languages graphix
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Major modes for editing Graphix source files (.gx and .gxi).
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

(defcustom graphix-format-on-save t
  "Format the buffer through the LSP before each save."
  :type 'boolean
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
    ;; Strings. Only `"` — `'` opens a type variable ('a) or a builtin
    ;; reference ('array_map), and `\`` opens a variant (`Foo); making
    ;; either a string delimiter would swallow the rest of the buffer
    ;; up to the next one, and invites electric-pair to close it.
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "_" table)
    (modify-syntax-entry ?\` "_" table)
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
                     "catch" "cast" "never" "any" "with" "throws" "as"
                     "pub" "trait" "impl" "mut" "val" "for"
                     "seq" "seqq" "until" "live" "abort" "flush"
                     "dynamic" "sandbox" "whitelist" "blacklist"
                     "unrestricted" "sig" "source"
                     "self" "super" "package"))
        (builtins '("true" "false" "null"))
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

;; Raw strings (`r"..."`, `r#"..."#`) have no escapes and a triple string
;; ("""...""") may hold bare quotes, so the syntax table alone mis-pairs
;; them: their delimiters are string fences and a backslash inside is
;; punctuation.
(defun graphix--raw-hashes (at)
  "The hashes before the quote at AT when it opens a raw string, else nil."
  (save-excursion
    (goto-char at)
    (skip-chars-backward "#")
    (let ((hashes (buffer-substring-no-properties (point) at)))
      (and (eq (char-before) ?r)
           (progn (backward-char) (looking-at-p "\\_<"))
           hashes))))

(defun graphix--syntax-propertize (start end)
  "Mark the raw and triple-quoted strings between START and END.
Each quote and backslash is judged alone from the state before it, so a
pass may start anywhere: an opening quote in code and the first quote of
a closer are string fences, and a backslash inside is punctuation."
  (goto-char start)
  (while (re-search-forward "[\"\\]" end t)
    (let* ((at (match-beginning 0))
           (ppss (save-excursion (syntax-ppss at))))
      (cond
       ((eq (nth 3 ppss) t)
        (if (eq (char-after at) ?\\)
            (put-text-property at (1+ at) 'syntax-table (string-to-syntax "."))
          (let* ((hashes (graphix--raw-hashes (nth 8 ppss)))
                 (close (if hashes (concat "\"" hashes) "\"\"\"")))
            (when (save-excursion (goto-char at) (looking-at-p (regexp-quote close)))
              (put-text-property at (1+ at) 'syntax-table (string-to-syntax "|"))
              (goto-char (min end (+ at (length close))))))))
       ((or (nth 3 ppss) (nth 4 ppss) (eq (char-after at) ?\\)))
       (t
        (let ((raw (graphix--raw-hashes at)))
          (when (or raw (save-excursion (goto-char at) (looking-at-p "\"\"\"")))
            (put-text-property at (1+ at) 'syntax-table (string-to-syntax "|"))
            (unless raw
              (goto-char (min end (+ at 3)))))))))))

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
  (setq-local syntax-propertize-function #'graphix--syntax-propertize)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width graphix-indent-offset))

;;; ---- Tree-sitter mode (Emacs 29+) ----

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))

  ;; `treesit-font-lock-rules' lives in treesit.el. Autoloads cover most
  ;; entry points but not the macros/functions used here at load time,
  ;; so require explicitly to avoid `void-function' on first load.
  (require 'treesit)

  (defvar graphix--treesit-font-lock-rules
    (treesit-font-lock-rules
     ;; Level 1: comments
     :language 'graphix
     :feature 'comment
     '((line_comment) @font-lock-comment-face
       (doc_comment) @font-lock-doc-face
       (shebang) @font-lock-preprocessor-face)

     ;; Level 1: attributes
     :language 'graphix
     :feature 'attribute
     '((attribute ["#[" "]"] @font-lock-preprocessor-face)
       (attribute name: (identifier) @font-lock-preprocessor-face))

     ;; Level 1: keywords
     :language 'graphix
     :feature 'keyword
     '(["mod" "use" "let" "rec" "type" "fn" "select" "seq" "seqq" "until" "abort" "flush(" "if" "catch"
        "cast" "any" "with" "throws" "as" "trait" "impl" "for" "pub" "try" "never" "live" "val"]
       @font-lock-keyword-face)

     ;; Level 1: strings
     :language 'graphix
     :feature 'string
     '((string) @font-lock-string-face
       (raw_string) @font-lock-string-face
       (triple_string) @font-lock-string-face
       (value_string) @font-lock-string-face
       (string_content) @font-lock-string-face
       (value_string_content) @font-lock-string-face
       (escape_sequence) @font-lock-escape-face
       (template_escape) @font-lock-escape-face)

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
     ;; a call's last path segment, as highlights.scm: a string, for the
     ;; `.` anchor (see the module rule below)
     "(apply (reference (module_path (identifier) @font-lock-function-name-face .)))
      (builtin_ref (identifier) @font-lock-function-name-face)"

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
     ;; a path's last segment; the segments before it are namespaces
     "(pattern_bind name: (identifier) @font-lock-variable-name-face)
      (connect target: (module_path (identifier) @font-lock-variable-name-face .))
      (reference (module_path (identifier) @font-lock-variable-name-face .))"

     ;; Level 3: parameters
     :language 'graphix
     :feature 'parameter
     '((labeled_param (identifier) @font-lock-variable-name-face)
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

     ;; Level 3: modules.
     ;; String form: the `.` anchor in `(module_path . (identifier) ...)`
     ;; is a tree-sitter query operator (first-child anchor), but Emacs's
     ;; Lisp reader would parse `.` inside a quoted list as a cons-cell
     ;; dot and bail with "expected )". String queries skip the reader.
     :language 'graphix
     :feature 'module
     "(module name: (identifier) @font-lock-constant-face)
      (module_path (identifier) @font-lock-constant-face . \"::\")"

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
        "!" "~" "~!" "?" "$" "&" "<-" "=>" "=" "->" ":" "::" "." "," ";" "@" "`" "#"]
       @font-lock-operator-face)

     ;; Level 4: brackets
     :language 'graphix
     :feature 'bracket
     '(["(" ")" "[" "]" "{" "}" "|"] @font-lock-bracket-face)

     ;; Level 4: string interpolation brackets
     :language 'graphix
     :feature 'interpolation
     '((interpolation "[" @font-lock-misc-punctuation-face
                      "]" @font-lock-misc-punctuation-face)
       (template_splice "\\[" @font-lock-misc-punctuation-face
                        "]" @font-lock-misc-punctuation-face)))
    "Tree-sitter font-lock rules for Graphix.")

  (defvar graphix--treesit-indent-rules
    `((graphix
       ((node-is "}") parent-bol 0)
       ((node-is "]") parent-bol 0)
       ((node-is ")") parent-bol 0)
       ;; a list's `>]` is a hidden token, no node to match
       (,(lambda (_node _parent bol &rest _)
           (save-excursion (goto-char bol) (looking-at-p ">\\]")))
        parent-bol 0)
       ((parent-is "sig_block") parent-bol ,graphix-indent-offset)
       ((parent-is "block") parent-bol ,graphix-indent-offset)
       ((parent-is "select") parent-bol ,graphix-indent-offset)
       ((parent-is "seq_block") parent-bol ,graphix-indent-offset)
       ((parent-is "try_with") parent-bol ,graphix-indent-offset)
       ((parent-is "list") parent-bol ,graphix-indent-offset)
       ((parent-is "struct") parent-bol ,graphix-indent-offset)
       ((parent-is "map") parent-bol ,graphix-indent-offset)
       ((parent-is "array") parent-bol ,graphix-indent-offset)
       ((parent-is "tuple") parent-bol ,graphix-indent-offset)
       ((parent-is "struct_type") parent-bol ,graphix-indent-offset)
       ((parent-is "tuple_type") parent-bol ,graphix-indent-offset)
       ((parent-is "apply_args") parent-bol ,graphix-indent-offset)
       ((parent-is "lambda") parent-bol ,graphix-indent-offset)
       ((parent-is "match_arm") parent-bol ,graphix-indent-offset)
       ((parent-is "trait_def") parent-bol ,graphix-indent-offset)
       ((parent-is "impl_def") parent-bol ,graphix-indent-offset)
       ((parent-is "use_group") parent-bol ,graphix-indent-offset)
       ((parent-is "union_type") parent-bol ,graphix-indent-offset)
       ((parent-is "struct_pattern") parent-bol ,graphix-indent-offset)
       ((parent-is "variant") parent-bol ,graphix-indent-offset)
       ((parent-is "variant_pattern") parent-bol ,graphix-indent-offset)
       ((parent-is "variant_type") parent-bol ,graphix-indent-offset)
       (no-node parent-bol 0)))
    "Tree-sitter indentation rules for Graphix.")

  (defvar graphix--treesit-grammar-recipe
    (let ((root (and load-file-name
                     (expand-file-name "../../.." (file-name-directory
                                                   (file-truename load-file-name))))))
      (list 'graphix
            (if (and root (file-exists-p
                           (expand-file-name "ide/tree-sitter-graphix/grammar.js" root)))
                root
              "https://github.com/graphix-lang/graphix")
            nil                         ; tag
            "ide/tree-sitter-graphix/src")) ; source dir
    "Tree-sitter grammar recipe for Graphix: the checkout this file was
loaded from, so the grammar matches its queries, else GitHub.")

  (defun graphix-ts-mode-install-grammar ()
    "Install the Graphix tree-sitter grammar.
Compiles the grammar of the checkout this file was loaded from, else
downloads it from the graphix repository."
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
    (treesit-parser-create 'graphix)

    ;; Comments
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip "//+ *")

    ;; Indentation
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width graphix-indent-offset)
    (setq-local syntax-propertize-function #'graphix--syntax-propertize)
    (setq-local treesit-simple-indent-rules graphix--treesit-indent-rules)

    ;; Font-lock
    (setq-local treesit-font-lock-settings graphix--treesit-font-lock-rules)
    (setq-local treesit-font-lock-feature-list
                '((comment attribute keyword string)
                  (type function constant number)
                  (variable parameter property module constructor)
                  (operator bracket interpolation)))

    ;; Navigation
    (setq-local treesit-defun-type-regexp
                (rx bos (or "module" "lambda" "let_binding" "type_def") eos))
    (setq-local treesit-thing-settings
                `((graphix
                   (sentence ,(rx bos (or "let_binding" "type_def" "use" "connect") eos))
                   (sexp ,(rx bos (or "module" "lambda" "block" "select" "seq_block"
                                      "try_with" "apply" "struct" "map" "array"
                                      "list" "tuple" "reference" "number" "string"
                                      "boolean" "null")
                              eos)))))

    (treesit-major-mode-setup))

  ;; Auto-remap to tree-sitter mode when grammar is available
  (when (treesit-ready-p 'graphix t)
    (add-to-list 'major-mode-remap-alist '(graphix-mode . graphix-ts-mode))))

;;; ---- File association ----

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gxi?\\'" . graphix-mode))

;;; ---- LSP integration ----

(defun graphix--format-before-save ()
  "Format the buffer through whichever LSP client manages it."
  (when graphix-format-on-save
    (cond ((and (fboundp 'eglot-managed-p) (eglot-managed-p))
           (eglot-format-buffer))
          ((bound-and-true-p lsp-mode)
           (lsp-format-buffer)))))

(defun graphix--setup-format-on-save ()
  (add-hook 'before-save-hook #'graphix--format-before-save nil t))

(add-hook 'graphix-mode-hook #'graphix--setup-format-on-save)
(add-hook 'graphix-ts-mode-hook #'graphix--setup-format-on-save)

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
