#!/bin/sh
# Copy the book's first GUI program (book/src/ui/gui/overview.md includes
# examples/gui/hello.gx) into an empty directory, as a reader would, and
# check it: it fails, because every gui example does `mod icon;` and
# `#icon: &icon::icon`, and the book never shows icon.gx.
set -e
G=${GRAPHIX:-graphix}
D=$(mktemp -d)
cp "$(dirname "$0")/../../../book/src/examples/gui/hello.gx" "$D/"
cd "$D"
"$G" --check hello.gx   # Error: module icon could not be found
