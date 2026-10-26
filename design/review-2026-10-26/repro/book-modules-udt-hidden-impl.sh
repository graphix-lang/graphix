#!/bin/sh
# Title: book/src/udt/traits.md says an impl is a global fact that "applies
# everywhere the trait is used" once its module loads; a sibling module with an
# interface does not see an impl another module's body adds undeclared, and a
# module with an interface may not decide a type its parent left open. Neither
# rule is in the book.
# Command: sh book-modules-udt-hidden-impl.sh /path/to/graphix
# Expected (per traits.md): b's Show::show(1) uses a's impl for i64.
# Observed: "i64 does not implement Show here: the impl in module /a is not in
#   its interface (declare `impl Show for i64;` there)", and for the second
#   program "the check decides a type the code around the module left open:
#   annotate `cell`".
G=${1:-graphix}
D=$(mktemp -d)
cd "$D" || exit 1
cat > main.gx <<'X'
trait Show { val show: fn(self) -> string };
mod a;
mod b;
b::s
X
printf 'val x: i64;\n' > a.gxi
printf 'use super::Show;\nimpl Show for i64 { let show = |x| "int [x]" };\nlet x = 1\n' > a.gx
printf 'val s: string;\n' > b.gxi
printf 'use super::Show;\nlet s = Show::show(1)\n' > b.gx
"$G" --check main.gx
mkdir -p p2/p
cd p2
printf 'val v: i64;\nmod c;\n' > p.gxi
printf 'let cell = never();\nlet v = 1;\nmod c;\n' > p.gx
printf 'val w: i64;\n' > p/c.gxi
printf 'use super::cell;\nlet w = 2;\ncell <- w\n' > p/c.gx
printf 'mod p;\np::c::w\n' > main.gx
"$G" --check main.gx
rm -rf "$D"
