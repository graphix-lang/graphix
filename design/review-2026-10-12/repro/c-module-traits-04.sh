#!/bin/sh
# c-module-traits-04: a script's interface check (check_script_sig) ignores
# typedefs, traits and impl bounds that a module's check (check_sig) enforces.
#
# Run: sh design/review-2026-10-12/repro/c-module-traits-04.sh [path/to/graphix]
#
# Expected: each script whose main.gxi contradicts main.gx is refused, as the
#   same interface/implementation pair is refused when compiled as `mod m`.
# Observed (HEAD 7324207f):
#   typedef script  -> "a"     (module form: signature mismatch in T, expected
#                               type T = i64, found type T = string)
#   trait script    -> "s3"    (gxi says show returns i64, script returns string)
#   impl script     -> "arr"   (module form: missing constraint 'a: Show in
#                               implementation)
#   --check on each script exits 0.
G=${1:-graphix}
D=$(mktemp -d)
run() { (cd "$1" && timeout -s KILL 30 "$G" --no-cache "$2" 2>&1 | tail -1); }
EXIT='sys::exit(sys::time::after_idle(duration:100.ms, 0));'

mkdir "$D/typedef"
printf 'type T = i64;\nval x: T;\n' > "$D/typedef/main.gxi"
printf 'type T = string;\nlet x: T = "a";\n%s\nx\n' "$EXIT" > "$D/typedef/main.gx"
cp "$D/typedef/main.gxi" "$D/typedef/m.gxi"
printf 'type T = string;\nlet x: T = "a"\n' > "$D/typedef/m.gx"
printf 'mod m;\n%s\nm::x\n' "$EXIT" > "$D/typedef/mm.gx"
echo "typedef script: $(run "$D/typedef" main.gx)"
echo "typedef module: $(run "$D/typedef" mm.gx)"

mkdir "$D/trait"
printf 'trait Show { val show: fn(self) -> i64 };\nimpl Show for i64;\n' > "$D/trait/main.gxi"
printf 'trait Show { val show: fn(self) -> string };\nimpl Show for i64 { let show = |x| "s[x]" };\n%s\nShow::show(3)\n' "$EXIT" > "$D/trait/main.gx"
echo "trait script: $(run "$D/trait" main.gx)"

mkdir "$D/impl"
printf "trait Show { val show: fn(self) -> string };\nimpl<'a: Show> Show for Array<'a>;\n" > "$D/impl/main.gxi"
printf "trait Show { val show: fn(self) -> string };\nimpl Show for i64 { let show = |x| \"int [x]\" };\nimpl<'a> Show for Array<'a> { let show = |xs| \"arr\" };\n%s\nShow::show([1])\n" "$EXIT" > "$D/impl/main.gx"
cp "$D/impl/main.gxi" "$D/impl/m.gxi"
printf "trait Show { val show: fn(self) -> string };\nimpl Show for i64 { let show = |x| \"int [x]\" };\nimpl<'a> Show for Array<'a> { let show = |xs| \"arr\" }\n" > "$D/impl/m.gx"
printf 'mod m;\n%s\nm::Show::show([1])\n' "$EXIT" > "$D/impl/mm.gx"
echo "impl script: $(run "$D/impl" main.gx)"
echo "impl module: $(run "$D/impl" mm.gx)"
rm -rf "$D"
