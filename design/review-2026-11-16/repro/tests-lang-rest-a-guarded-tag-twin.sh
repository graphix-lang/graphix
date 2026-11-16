#!/bin/sh
# Title: undecided_member_beside_a_guarded_tag_refused (select_undecided.rs) has no
#        passing twin, and the annotation its refusal asks for is refused again
# Command: sh design/review-2026-11-16/repro/tests-lang-rest-a-guarded-tag-twin.sh
# Expected: following the refusal ("annotate it, writing for 'z the type of `z`")
#           makes the program pass, and a fixture pins that.
# Observed:
#   a) z annotated with the type it is written: refused again, "the pattern
#      (_, { a, .. }) matches more than one member"
#   b) z left unannotated, every field named, `(_, {a, c: _})`: checks and runs, "j"
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > "$D/a.gx" <<'EOF'
let r = {
    let b = false;
    let g = true;
    let z: {a: string, c: string} = never();
    let v = select b { true => (`K, {a: 1, c: 1}), false => (`J, z) };
    let r = select v { (`J, _) if g => "j", (_, {a, ..}) => "[a]" };
    z <- {a: "s", c: "w"};
    r
};
r
EOF
cat > "$D/b.gx" <<'EOF'
let r = {
    let b = false;
    let g = true;
    let z = never();
    let v = select b { true => (`K, {a: 1, c: 1}), false => (`J, z) };
    let r = select v { (`J, _) if g => "j", (_, {a, c: _}) => "[a]" };
    z <- {a: "s", c: "w"};
    r
};
sys::exit(sys::time::after_idle(duration:100.ms, 0));
r
EOF
echo "== a"; timeout -s KILL 30 "$G" --no-cache --check "$D/a.gx" 2>&1 | tail -1
echo "== b"; timeout -s KILL 30 "$G" --no-cache "$D/b.gx" 2>&1 | tail -1
rm -rf "$D"
