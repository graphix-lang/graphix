#!/bin/bash
# Title: graphix-mode's syntax-propertize errors on a raw/template string that
#        runs past the region it is asked to propertize
# Command: bash design/review-2026-10-26/repro/ide-tooling-emacs-multiline-template.sh
# Expected: "let z in-string=nil" (the template closed, the next statement is code)
# Observed: Error: error ("Invalid search bound (wrong side of point)")
#   from graphix--syntax-propertize: the closing """ is searched with no bound,
#   point lands past END, and the loop's next re-search-forward with bound END fails.
#   syntax-ppss at any point inside a multi-line template (jit-lock chunks,
#   show-paren, indentation) hits it, in graphix-mode and graphix-ts-mode alike.
set -euo pipefail
d=$(mktemp -d)
trap 'rm -rf "$d"' EXIT
{ echo 'let y = """'
  for i in $(seq 1 40); do echo "line $i of a template with a \"quote\" in it"; done
  echo '""";'
  echo 'let z = 1'; } > "$d/t.gx"
cat > "$d/t.el" <<'EL'
;;; -*- lexical-binding: t -*-
(load (expand-file-name "ide/editors/emacs/graphix-mode.el" (getenv "REPO")))
(find-file (car command-line-args-left))
(graphix-mode)
(let ((mid (/ (point-max) 2))
      (z (save-excursion (goto-char (point-max)) (re-search-backward "^let z") (point))))
  (syntax-ppss mid)
  (princ (format "let z in-string=%s\n" (nth 3 (syntax-ppss z)))))
EL
REPO="$(cd "$(dirname "$0")/../../.." && pwd)" timeout -s KILL 60 emacs --batch -l "$d/t.el" "$d/t.gx" 2>&1 | grep -v '^Loading'
