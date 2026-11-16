#!/bin/bash
# Title: book/src/modules/interfaces.md "Sub-module Visibility" says "Module and type names
# are visible whatever their order, but values are not"; a module declared below `mod child`
# is visible by name, but none of its contents, types included.
# Command: bash book-modules-udt-later-module-invisible.sh
# Expected per the book: child's `let x: other::Q = 3` checks (a type name, order-free).
# Observed: "undefined type other::Q in p::child"; `other::w` is "other::w not defined";
#   `use super::other;` itself is accepted. The parent's own `type T` declared after
#   `mod child` IS visible in the child (`use super::T` checks).
set -e
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d /home/eric/tmp/review-2026-11-16/work/book-modules-udt/later.XXXX)
mkdir -p $D/p
printf 'mod p;\np::child::x\n' > $D/main.gx
printf 'mod child;\ntype T = i64;\nmod other;\nlet v = 1\n' > $D/p.gx
printf 'type Q = i64;\nlet w = 5\n' > $D/p/other.gx
printf 'use super::other;\nlet x: other::Q = 3\n' > $D/p/child.gx
timeout -s KILL 20 $G --no-cache --check $D/main.gx 2>&1 | tail -1 || true
printf 'use super::T;\nlet x: T = 3\n' > $D/p/child.gx
timeout -s KILL 20 $G --no-cache --check $D/main.gx && echo "type T declared after mod child: ok"
