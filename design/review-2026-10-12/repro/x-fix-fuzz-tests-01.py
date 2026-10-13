#!/usr/bin/env python3
# x-fix-fuzz-tests-01: every `// expect: reject` corpus pin is vacuous.
# build.rs strip_header keeps `// expect: reject` (and any `-v1:` header)
# above the program in REGRESSION_CORPUS; Subject::parse keeps that line in
# the body and wraps it as `use super::*; let result = <body>`, so the marker
# sits after `=` on line 1: a parse error ("a comment stands on a line of its
# own") in every engine, session and the check alone. regress reads that as
# reject == reject for all marked pins whatever their program does.
#
# Run from the repo root:
#   BIN=graphix-fuzz timeout -s KILL 600 python3 -I design/review-2026-10-12/repro/x-fix-fuzz-tests-01.py
# Expected: a valid program marked reject is NOT a refusal; the two pins
#   below are refusals, as outcome.manifest records them.
# Observed:
#   valid_marked: Interp/InLanguage: CompileErr(... Parse error at line: 1,
#     column: 28 Unexpected `/` ... use super::*; let result = // expect: reject)
#   check valid_marked: AGREE (reject)
#   nested-bind-stmt-dead-elim-aug2026/04 (regress text, schedule-v1 header):
#     CompileErr(... line: 1, column: 50 Unexpected `/`)
#   abort-seam-jul2026/01 stripped: AGREE (contained)   (manifest: reject)
#   tvar-lock-jul2026/01 stripped:  AGREE (trace)       (manifest: reject)
import os, re, subprocess, sys, tempfile

BIN = os.environ.get("BIN", "graphix-fuzz")
F = "graphix-fuzz/findings/"


def strip_header(s):
    """graphix-fuzz/build.rs strip_header, line for line."""
    lines, kept, i = s.splitlines(), [], 0
    while i < len(lines):
        t = lines[i].lstrip()
        name = t[3:].split(":", 1)[0] if t.startswith("// ") and ":" in t[3:] else ""
        if (name.endswith("-v1") and " " not in name) or t == "// expect: reject":
            kept.append(t)
        elif not (t.startswith("//") or t == ""):
            break
        i += 1
    prog = "\n".join(lines[i:])
    return prog if not kept or not prog.strip() else "\n".join(kept) + "\n" + prog


def fuzz(cmd, text, last=False):
    with tempfile.NamedTemporaryFile("w", suffix=".gx", delete=False) as f:
        f.write(text)
    out = subprocess.run(["timeout", "-s", "KILL", "175", BIN, cmd, f.name],
                         capture_output=True, text=True).stdout
    os.unlink(f.name)
    lines = [l for l in out.splitlines() if l.strip()]
    return (lines[-1] if last else lines[0])[:300] if lines else "<no output>"


valid = "// expect: reject\n{ let x = 20; x + 22 }\n"
print("valid_marked run:  ", fuzz("run", valid))
print("valid_marked check:", fuzz("check", valid, last=True))
pin = open(F + "nested-bind-stmt-dead-elim-aug2026/04_select_scrutinee_bind.gx").read()
print("v1 pin, regress text:", fuzz("run", strip_header(pin)))
for p in ["abort-seam-jul2026/01_depth_trip_settled_taint.gx",
          "tvar-lock-jul2026/01_constraint_eq_self_deadlock.gx"]:
    prog = "\n".join(l for l in open(F + p).read().splitlines()
                     if not re.match(r"\s*//", l))
    print(p, "stripped:", fuzz("check", prog, last=True))
