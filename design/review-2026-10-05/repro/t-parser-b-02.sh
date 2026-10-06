#!/usr/bin/env bash
# t-parser-b-02: `type Error` / `type Abstract` are accepted as typedef
# names, but typ() matches both words as keywords (typexp.rs:427, :429),
# so the bare name never reaches typref(): the type cannot be named
# unqualified, and `Error<..>` silently means the builtin. bound()
# (typexp.rs:78-85) does the same to a user type named Concrete,
# Function, Singleton or OneNumber.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-parser-b-02.sh
#
# expected (design/list_native.md, parser/test.rs list_is_a_reserved_type_name:
# "a user typedef of a compiler-known type name refuses at parse"): every
# typedef below is refused at its definition with "can't use keyword as a
# type name", as case 5 (`type List`) is; or else the user's type is used.
# observed (HEAD c722befe, debug build):
#   1. type Error accepted; `let e: Error = ..` fails at the USE:
#      "Unexpected `=` Expected whitespace or `<`" (line 2, column 14)
#   2. type Error<'a> = {x: 'a}; `let e: Error<i64> = {x: 1}` is the
#      builtin error type: "type mismatch Error<i64> does not contain { x: i64 }"
#   3. type Abstract accepted; `let e: Abstract = 1` fails with
#      "Abstract<..> is legal only as the whole body of a type definition"
#   4. type Concrete = [i64, string]; `'a: Concrete |x: 'a| x` then f(1.5)
#      is ACCEPTED (exit 0: the builtin Concrete bound); the same type named
#      Shape refuses f(1.5) ("'a: unbound within Shape does not contain f64")
#   5. control: type List refused at the definition, "can't use keyword as a type name"
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
check() {
    echo "== $1"
    timeout -s KILL 30 "$GRAPHIX" --check "$dir/$1.gx" 2>&1 | grep -E 'Unexpected|Expected|mismatch|keyword'
    echo "exit=${PIPESTATUS[0]}"
}

cat > "$dir/1_bare_error.gx" <<'EOF'
type Error = [`NotFound, `Denied];
let e: Error = `NotFound;
e
EOF
cat > "$dir/2_error_params.gx" <<'EOF'
type Error<'a> = {x: 'a};
let e: Error<i64> = {x: 1};
e
EOF
cat > "$dir/3_abstract.gx" <<'EOF'
type Abstract = i64;
let e: Abstract = 1;
e
EOF
cat > "$dir/4_bound_concrete.gx" <<'EOF'
type Concrete = [i64, string];
let f = 'a: Concrete |x: 'a| x;
f(1.5)
EOF
cat > "$dir/4_bound_shape.gx" <<'EOF'
type Shape = [i64, string];
let f = 'a: Shape |x: 'a| x;
f(1.5)
EOF
cat > "$dir/5_list_control.gx" <<'EOF'
type List = [`NotFound, `Denied];
`NotFound
EOF

for p in 1_bare_error 2_error_params 3_abstract 4_bound_concrete 4_bound_shape 5_list_control; do
    check "$p"
done
