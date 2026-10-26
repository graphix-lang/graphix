;;; graphix-mode-tests.el --- Tests for graphix-mode -*- lexical-binding: t -*-

;; Run: emacs --batch -l ide/editors/emacs/graphix-mode-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(defconst graphix-mode-tests--dir
  (file-name-directory (file-truename (or load-file-name buffer-file-name))))

(load (expand-file-name "graphix-mode.el" graphix-mode-tests--dir) nil t)

(defconst graphix-mode-tests--strings
  (concat "let a = \"\"\"x \"Q\" \\ [1]\n"
          (mapconcat (lambda (i) (format "line %d \"quoted\"\n" i)) (number-sequence 1 30) "")
          "Y\"\"\";\n"
          "let b = r#\"a\"B\\\"#;\n"
          "let c = r\"C\\\";\n"
          "let d = \"e\\\"F\";\n"
          "// a \"\"\" in a comment\n"
          "let e = 1\n")
  "Fenced strings of each kind: each capital letter is inside a string.")

(defun graphix-mode-tests--states (chunk order)
  "The string state at each marker, propertized in CHUNK-sized passes.
ORDER is `up', `down' or `middle': where the first queries land."
  (with-temp-buffer
    (insert graphix-mode-tests--strings)
    (graphix-mode)
    (let* ((syntax-propertize-chunk-size chunk)
           (n (point-max))
           (queries (pcase order
                      ('up (number-sequence 1 n chunk))
                      ('down (number-sequence n 1 (- chunk)))
                      ('middle (list (/ n 2) n)))))
      (dolist (q queries) (syntax-ppss q))
      (let (states (case-fold-search nil))
        (goto-char (point-min))
        (while (re-search-forward "[A-Z]\\|^let\\|^line" nil t)
          (let ((m (match-string 0)))
            (push (cons m (and (nth 3 (save-excursion (syntax-ppss (match-beginning 0)))) t))
                  states)))
        (nreverse states)))))

(ert-deftest graphix-mode-fenced-strings-any-pass ()
  "Every capital is in a string and every `let' is code, however the
buffer is cut into passes."
  (dolist (chunk '(1 2 3 5 8 13 500))
    (dolist (order '(up down middle))
      (dolist (s (graphix-mode-tests--states chunk order))
        (should (equal (list chunk order (car s) (cdr s))
                       (list chunk order (car s)
                             (not (string-prefix-p "let" (car s))))))))))

(ert-deftest graphix-mode-grammar-recipe-is-this-checkout ()
  "Loaded from a checkout, the grammar is built from that checkout."
  (skip-unless (boundp 'graphix--treesit-grammar-recipe))
  (should (equal (file-name-as-directory (nth 1 graphix--treesit-grammar-recipe))
                 (expand-file-name "../../../" graphix-mode-tests--dir))))

;;; graphix-mode-tests.el ends here
