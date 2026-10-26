#!/bin/sh
# small-pkgs: hbs templates whose closers `shallow` counts but handlebars reads as text pass the depth guard and overflow the stack
# Run: sh design/review-2026-10-26/repro/small-pkgs-hbs-escaped-closer-abort.sh
# Expected: every run answers error:`HbsErr("blocks nest more than 64 deep") (N=100 and N=5000).
# Observed:
#   escape  N=100 renders (100 nested blocks pass the 64 bound); N=5000: "thread 'tokio-rt-worker'
#     has overflowed its stack / fatal runtime error: stack overflow, aborting", exit 134.
#     handlebars reads `\{{{{` as ONE escape (escape = "\\" ~ "{{" ~ "{{"?), so `\{{{{/if}}` is
#     text; `unescaped` skips only the first `{{`, and `shallow` counts `{{/if}}` as a closer.
#   bracket N=5000: the same abort. `{{x [}}{{/if}}]}}` is one expression whose path key
#     `[...]` holds `}}{{/if}}` (path_raw_id); `tag_len` ends the tag at the first `}}`.
set -e
D=${TMPDIR:-/tmp}/small-pkgs-hbs-escape; mkdir -p "$D"
for V in escape bracket; do
for N in 100 5000; do
python3 -I - "$V" "$N" "$D/h$V$N.gx" <<'PY'
import sys
v, n, out = sys.argv[1], int(sys.argv[2]), sys.argv[3]
unit = r'{{#if a}}\{{{{/if}}' if v == 'escape' else '{{#if a}}{{x [}}{{/if}}]}}'
t = unit * n + 'x' + '{{/if}}' * n
open(out, 'w').write('sys::exit(sys::time::after_idle(duration:300.ms, 0));\n'
                     'hbs::render(r"%s", {a: true}) ~ "rendered"\n' % t)
PY
echo "$V N=$N"
timeout -s KILL 60 /home/eric/tmp/review-2026-10-26/bin/graphix --no-cache "$D/h$V$N.gx" 2>&1 | tail -3 | cut -c1-160 || true
done
done
