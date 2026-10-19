#!/bin/bash
# Title: icon.gxi's `val icon: ImageSource` resolves only through icon.gx's `use`
# Command: bash design/review-2026-10-19/repro/examples-gui-gxi-type-via-impl-use.sh
# Expected: the interface names its own types (module_system.md: a `use` in a
#   .gxi applies to the .gx, not the reverse), so case A should need nothing
#   from the .gx and case B (use in the gxi) is the idiom.
# Observed: A (the book's icon.gxi/icon.gx shape) checks only because the .gx
#   imports ImageSource; C (same gxi, .gx without the use) fails with
#   "undefined type ImageSource in icon at ... icon.gxi"; B checks.
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
T=$(mktemp -d)
mk() { mkdir -p $T/$1; printf "$2" > $T/$1/icon.gxi; printf "$3" > $T/$1/icon.gx
  printf 'mod icon;\nlet x: gui::ImageSource = icon::icon;\nx\n' > $T/$1/main.gx
  echo "== $1"; timeout -s KILL 30 $G --check $T/$1/main.gx; echo "rc=$?"; }
mk A 'val icon: ImageSource\n' 'use gui::ImageSource;\nlet icon: ImageSource = `Svg("x")\n'
mk B 'use gui::ImageSource;\nval icon: ImageSource\n' 'let icon: ImageSource = `Svg("x")\n'
mk C 'val icon: ImageSource\n' 'let icon = `Svg("x")\n'
rm -rf $T
