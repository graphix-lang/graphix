;; -*- lexical-binding: t -*-
;; Title: graphix-ts-mode leaves `mut`, `dynamic`, `sig` and `source` uncolored,
;;        which highlights.scm colors @keyword for Helix, Zed and Neovim
;;
;; Command (from the repo root; needs the grammar built as
;; libtree-sitter-graphix.so in DIR, e.g. by `tree-sitter build` on a copy
;; of ide/tree-sitter-graphix):
;;   GRAPHIX_TS_DIR=DIR emacs --batch -l design/review-2026-11-16/repro/ide-tooling-emacs-ts-keywords.el
;;
;; Expected: every word printed with font-lock-keyword-face.
;; Observed (Emacs 31.1, treesit-font-lock-level 4):
;;   let      font-lock-keyword-face
;;   mut      nil
;;   dynamic  nil
;;   sig      nil
;;   val      font-lock-keyword-face
;;   source   nil
;;   seq      font-lock-keyword-face
(setq treesit-extra-load-path (list (getenv "GRAPHIX_TS_DIR")))
(load (expand-file-name "ide/editors/emacs/graphix-mode.el") nil t)
(with-temp-buffer
  (insert "let x = 1;\nlet r = &mut x;\nmod d dynamic {\n  sig { val y: i64 };\n  source \"let y = 1\"\n};\nlet s = seq x { 1 };\nr\n")
  (graphix-ts-mode)
  (setq-local treesit-font-lock-level 4)
  (treesit-font-lock-recompute-features)
  (font-lock-ensure)
  (dolist (w '("let" "mut" "dynamic" "sig" "val" "source" "seq"))
    (goto-char (point-min))
    (search-forward-regexp (concat "\\_<" w "\\_>"))
    (princ (format "%-8s %S\n" w (get-text-property (match-beginning 0) 'face)))))
