# Title: generated seq/seqq ceremonies with an `until` almost never complete a run
# Command: graphix-fuzz gen 600 21 --reactive > r600.txt
#          python3 -I design/review-2026-10-26/repro/fuzz-gen-b-until-stall-count.py r600.txt 80 <scratch dir>
# Expected: a ceremony completes a run on most epochs (the until waits were written to end).
# Observed: {'no-until': [25, 7], 'until+abort': [33, 30], 'no-until+abort': [12, 6], 'until': [10, 9]}
#   ([ceremonies, ceremonies that never printed "run .."]): 39 of the 43 with an until never complete.
import subprocess, sys, re, os
src = open(sys.argv[1]).read()
progs = [p for p in src.split("\n\n") if 'println("run [' in p]
lim = int(sys.argv[2])
out = sys.argv[3]
os.makedirs(out, exist_ok=True)
stats = {}
for i, p in enumerate(progs[:lim]):
    f = os.path.join(out, f"p{i}.gx")
    open(f, "w").write(p.strip() + "\n")
    try:
        r = subprocess.run(["timeout", "-s", "KILL", "60", "/home/eric/tmp/review-2026-10-26/bin/graphix-fuzz", "run", f], capture_output=True, text=True, timeout=70)
    except Exception as e:
        print(i, "ERR", e); continue
    line = next((l for l in r.stdout.splitlines() if l.startswith("Interp/InLanguage:")), "")
    runs = line.count(":run ")
    has_until = "until" in p
    has_abort = "abort(" in p or "flush(" in p
    key = ("until" if has_until else "no-until") + ("+abort" if has_abort else "")
    s = stats.setdefault(key, [0, 0])
    s[0] += 1
    if runs == 0 and "Trace(" in line:
        s[1] += 1
    print(i, key, "runs", runs, "trace" if "Trace(" in line else line[:100])
print(stats)
