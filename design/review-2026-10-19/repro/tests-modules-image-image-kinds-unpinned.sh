#!/bin/bash
# Title: node kinds program_image.md says program_image_restores covers but
#   whose PROGRAM holds none: connect through a reference, path place
#   references, payload variants, lists
# Command: bash design/review-2026-10-19/repro/tests-modules-image-image-kinds-unpinned.sh
# Expected: the same output with no cache, cold (writing the image) and warm.
# Observed (HEAD 9445a1d21): SAME -- they restore today; nothing in image.rs
#   would notice if one stopped (PROGRAM and BUILTINS hold no `*r <- v`, no
#   `&a[i]`/`&s.f`, no variant with a payload, no list).
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d); cd "$d"
cat > main.gx <<'EOF'
type Shape = [`Circle(f64), `Rect(f64, f64), `Dot];
let shapes: Array<Shape> = [`Circle(1.0), `Rect(2.0, 3.0), `Dot];
let l = [<1, 2, 3>];
let lsum = select l { [<h, t..>] => h + select t { [<a, ..>] => a, _ => 0 }, [<>] => 0 };
let area = |s: Shape| -> f64 select s { `Circle(r) => r * r, `Rect(w, h) => w * h, `Dot => 0.0 };
let st = {name: "n", pt: (1, 2), tags: [<`A, `B>]};
let arr = [10, 20, 30];
let r = &mut arr[1];
*r <- 99;
let sf = &mut st.name;
*sf <- "m";
sys::exit(sys::time::after_idle(duration:200.ms, 0));
(array::map(shapes, area), lsum, st, arr)
EOF
a=$(timeout -s KILL 60 "$G" --no-cache main.gx 2>&1)
b=$(XDG_CACHE_HOME=$d/cache timeout -s KILL 60 "$G" main.gx 2>&1)
c=$(XDG_CACHE_HOME=$d/cache timeout -s KILL 60 "$G" main.gx 2>&1)
echo "$a"; ls "$d"/cache/graphix/registration/*/ | wc -l
if [ "$a" == "$b" ] && [ "$a" == "$c" ]; then echo SAME; else echo "COLD:"; echo "$b"; echo "WARM:"; echo "$c"; fi
rm -rf "$d"
