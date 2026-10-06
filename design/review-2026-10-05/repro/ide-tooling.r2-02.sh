#!/usr/bin/env bash
# ide-tooling.r2-02: graphix-ts-mode never creates its tree-sitter parser,
# so with the grammar installed a .gx buffer has no highlighting and TAB
# signals an error.
#
# ide/editors/emacs/graphix-mode.el:289-329 checks treesit-ready-p and
# calls treesit-major-mode-setup, but never (treesit-parser-create
# 'graphix); treesit-major-mode-setup does not create one ("Make sure
# necessary parsers are created for the current buffer before calling this
# function"). Line 332 remaps graphix-mode to graphix-ts-mode whenever the
# grammar loads, so every .gx file opens in the broken mode.
#
# The script compiles ide/tree-sitter-graphix/src into a temp dir, puts it
# on treesit-extra-load-path, loads the mode, visits a small .gx file and
# reports the parser list, font-lock, indentation and C-M-a. It runs twice:
# the mode as shipped, and a copy with the one missing line added.
#
# command (from the repo root; needs an Emacs built with tree-sitter, and cc):
#   bash design/review-2026-10-05/repro/ide-tooling.r2-02.sh
#
# expected: shipped behaves like the patched copy: a parser, faces after
# font-lock-ensure, TAB indents.
#
# observed (HEAD c722befe, GNU Emacs 31.1, libtree-sitter 0.26):
#   == shipped
#   major-mode=graphix-ts-mode parsers=nil
#   font-lock-ensure: (wrong-type-argument treesit-parser-p nil)
#   TAB on line 3: (wrong-type-argument treesit-node-p nil)
#   == with (treesit-parser-create 'graphix) added
#   major-mode=graphix-ts-mode parsers=(#<treesit-parser for graphix>)
#   font-lock-ensure: ok, face at 1 = font-lock-keyword-face
#   TAB on line 3: ok
#   C-M-a 4:12 -> 4:0, defun at line 4 = module_path "y"
#   seq body after indent-region: "let a = 1;" at column 0
# The last two lines are what the parser fix exposes: the defun/sexp
# regexps are unanchored ("module" matches module_path, so every reference
# is a defun; with (rx bos (or ...) eos) C-M-a goes to `let y` at 3:0), and
# the indent rules have no seq_block / try_with / list entry.
set -euo pipefail
REPO=$(cd "$(dirname "$0")/../../.." && pwd)
SRC=$REPO/ide/tree-sitter-graphix/src
MODE=$REPO/ide/editors/emacs/graphix-mode.el
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
cc -shared -fPIC -O1 -I "$SRC" "$SRC/parser.c" "$SRC/scanner.c" \
   -o "$T/libtree-sitter-graphix.so"
cat > "$T/x.gx" <<'EOF'
let f = |x| {
    let h = |z| z + 1;
let y = x * 2;
    y + h(x)
};
let r = seq {
let a = 1;
a + 1
};
f(3)
EOF
mkdir -p "$T/fixed"
sed 's|^    ;; Comments$|    (treesit-parser-create (quote graphix))\n    ;; Comments|' \
    "$MODE" > "$T/fixed/graphix-mode.el"
cat > "$T/probe.el" <<'EOF'
;; -*- lexical-binding: t -*-
(defun lc () (format "%d:%d" (line-number-at-pos) (current-column)))
(defmacro probe-try (label &rest body)
  `(condition-case err (progn ,@body)
     (error (message "%s: %S" ,label err))))
(setq treesit-extra-load-path (list (getenv "T")))
(load (getenv "MODE") nil t)
(find-file (expand-file-name "x.gx" (getenv "T")))
(message "major-mode=%S parsers=%S" major-mode (treesit-parser-list))
(probe-try "font-lock-ensure"
     (font-lock-ensure)
     (message "font-lock-ensure: ok, face at 1 = %S" (get-text-property 1 'face)))
(goto-char (point-min)) (forward-line 2)
(probe-try "TAB on line 3" (indent-for-tab-command) (message "TAB on line 3: ok"))
(when (treesit-parser-list)
  (goto-char (point-min)) (forward-line 3) (end-of-line)
  (let ((from (lc)))
    (beginning-of-defun)
    (message "C-M-a %s -> %s, defun at line 4 = %s" from (lc)
             (progn (goto-char (point-min)) (forward-line 3) (back-to-indentation)
                    (let ((n (treesit-defun-at-point)))
                      (format "%s %S" (treesit-node-type n) (treesit-node-text n t))))))
  (indent-region (point-min) (point-max))
  (goto-char (point-min)) (forward-line 6)
  (message "seq body after indent-region: %S at column %d"
           (string-trim (thing-at-point 'line t)) (progn (back-to-indentation) (current-column))))
EOF
echo "== shipped"
T=$T MODE=$MODE emacs --batch -Q -l "$T/probe.el" 2>&1
echo "== with (treesit-parser-create 'graphix) added"
T=$T MODE=$T/fixed/graphix-mode.el emacs --batch -Q -l "$T/probe.el" 2>&1
