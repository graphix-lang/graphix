#!/usr/bin/env python3
"""Check a copy of netidx-admin with the prototype, then the same copy with one
type error put in at a time: every one must be refused.

ANNOTATIONS are the changes the package needs to check under the prototype's
rules; each MUTANT is (file, text, replacement, what the error is).

usage: mutants.py
"""
import os, shutil, subprocess, sys, tempfile

spike = os.environ.get('SPIKE', os.path.expanduser('~/tmp/target/debug/graphix-bounds-spike'))
src = os.path.expanduser('~/proj/netidx/graphix-package-netidx-admin/src/graphix')

ANNOTATIONS = [
    ('tui/local.gx', 'let item = |label, act| {', 'let item = |label, act: Act| {'),
]
MUTANTS = [
    ('tui/local.gx', '[item("Admins", `Admins)]', '[item("Admins", "admins")]', 'a string where Act is wanted'),
    ('tui/local.gx', 'line(it.label)', 'line(it.lable)', 'a misspelt field'),
    ('tui/local.gx', 'let n_items = array::len(items);', 'let n_items = array::len(items) + "s";', 'i64 + string'),
    ('tui/local.gx', '  `Workstation => "Workstation",\n', '', 'an arm removed'),
    ('tui/local.gx', 'desc: desc(act) }', 'desc: 7 }', 'a wrong field type'),
    ('tui/local.gx', 'pad(str::concat(" ", s), n)', 'pad(str::concat(" ", s), n, 1)', 'an extra argument'),
    ('tui/local.gx', 'let row = selected(rows, sel);', 'let row = selected(rows, true);', 'a bool index'),
    ('tui/local.gx', '`Publisher => "Publisher"', '`Publisher => 4', 'an arm of another type'),
    ('tui/local.gx', "let current = opt::map(row, |r| r.install);", "let current = opt::map(row, |r| r.instal);", 'a misspelt field in a callback'),
]

def judge(edits):
    with tempfile.TemporaryDirectory() as tmp:
        root = f'{tmp}/graphix-package-netidx-admin/src/graphix'
        shutil.copytree(src, root)
        for f, old, new in edits:
            text = open(f'{root}/{f}').read()
            if text.count(old) != 1:
                sys.exit(f'{f}: {old!r} is there {text.count(old)} times, not once')
            open(f'{root}/{f}', 'w').write(text.replace(old, new))
        out = subprocess.run([spike, f'{root}/mod.gx'], capture_output=True, text=True).stdout
        return out.strip().split('\t')

base = judge(ANNOTATIONS)
print(f'netidx-admin with {len(ANNOTATIONS)} annotation(s): {base[0]}' + ('' if base[0] == 'ok' else f' {base[1][:300]}'))
missed = 0
for f, old, new, what in MUTANTS:
    got = judge(ANNOTATIONS + [(f, old, new)])
    missed += got[0] != 'no'
    print(f'  {"refused" if got[0] == "no" else "MISSED "}  {what}: {got[1][:150] if got[0] == "no" else ""}  [{got[-1]}]')
print(f'{len(MUTANTS) - missed} of {len(MUTANTS)} refused')
