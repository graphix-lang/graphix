#!/usr/bin/env python3
"""The data sections of design/REVIEW-<date>.md -> STATE/tables.md: numbers,
the design assessments, findings by area, CLAUDE.md drift, duplicates, re-run
tallies. The narrative above them is written by hand (STATE/narrative.md)."""
import collections, json, os, sys
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from cfg import REPRO, STATE

res = json.load(open(STATE + '/results.json'))
crs = {c['id']: c for c in json.load(open(STATE + '/crs.json'))}
excl = json.load(open(STATE + '/crs_excluded.json'))
args = json.load(open(STATE + '/args.json'))
allf = {f['id']: f for r in res.values() for f in r['findings']}
AREAS = [
    ('Design', ('a-',)),
    (f"Re-runs of the {args['prev']['date']} repros", ('x-rerun-',)),
    ('Type checker and types', ('t-', 'x-typecheck', 'x-check-build', 'x-traits-abstract', 'x-diagnostics')),
    ('Compiler nodes and semantics', ('c-', 'x-engine', 'x-sleep-wake', 'x-seq-sampling', 'x-node-contract', 'x-prune',
                                      'x-kind-markers', 'x-recursion', 'x-refs', 'x-dyn-modules', 'x-leaks')),
    ('Fusion and the JIT', ('f-',)),
    ('Parallel evaluation and concurrency', ('x-parallel', 'x-concurrency')),
    ('Images', ('x-image',)),
    ('Robustness: stack, panics, unsafe, security', ('x-stack', 'x-panics', 'x-unsafe', 'x-security')),
    ('Performance', ('x-perf-',)),
    ('Formatter', ('x-formatter',)),
    ('Runtime, shell, REPL, LSP, package manager', ('rt', 'shell', 'lsp', 'package', 'x-lsp-probe', 'x-repl')),
    ('Standard library (Rust)', ('core-', 'collections-str', 'small-pkgs', 'sys-', 'http-', 'db2', 'x-builtin-effects',
                                 'x-async-io')),
    ('Standard library (Graphix)', ('gx-',)),
    ('GUI and TUI', ('gui-', 'tui-')),
    ('Fuzzer', ('fuzz-',)),
    ('Tests', ('tests-',)),
    ('Book and language skill', ('book-', 'lang-skill')),
    ('Examples, IDE, tooling, duplication, docs', ('examples', 'ide-tooling', 'x-dup', 'x-doc-drift')),
]


def area(i):
    for n, p in AREAS:
        if i.startswith(p): return n
    return 'Other'


SEV = {'critical': 0, 'high': 1, 'medium': 2, 'low': 3}


def esc(s): return (s or '').replace('|', '\\|').replace('\n', ' ').strip()


out = []; w = out.append
fs = list(allf.values())
nd = sum(1 for k in res if k.startswith('a-')); nx = sum(1 for k in res if k.startswith('x-'))
w('## Numbers\n')
w(f'- Reviewers: {len(res)} ({len(res) - nd - nx} file units, {nx} cross-cutting, {nd} design). Findings: {len(fs)}; '
  f'{sum(f["evidence"] == "ran" for f in fs)} shown by running a program, {sum(f["evidence"] == "read" for f in fs)} by reading.')
for m in ('fable', 'opus'):
    rs = [v for v in res.values() if v['model'] == m]; mf = [f for v in rs for f in v['findings']]
    hi = sum(f['severity'] in ('critical', 'high') for f in mf)
    w(f'- {m.capitalize()}: {len(rs)} reviewers, {len(mf)} findings ({hi} critical or high, '
      f'{sum(f["evidence"] == "ran" for f in mf)} shown by running).')
w(f'- CRs placed: {len(crs)} ({sum(c["addr"] == "eric" for c in crs.values())} for eric, '
  f'{sum(c["addr"] == "claude" for c in crs.values())} for claude). Merged as duplicates: '
  f'{sum(1 for e in excl if e[1].startswith("dup"))}. CLAUDE.md drift (below, not CRs): '
  f'{sum(1 for e in excl if e[1] == "claude-md")}. Not placed: '
  f'{sum(1 for e in excl if not e[1].startswith("dup") and e[1] != "claude-md")}.')
c = collections.Counter(f"{x['tag']}/{x['sev']}" for x in crs.values())
w('- CRs by tag/severity: ' + ', '.join(
    f'{k} {v}' for k, v in sorted(c.items(), key=lambda kv: (SEV[kv[0].split('/')[1]], kv[0]))) + '.\n')
w('| area | critical | high | medium | low |\n|---|---|---|---|---|')
by = collections.defaultdict(collections.Counter)
for x in crs.values(): by[area(x['id'])][x['sev']] += 1
for n, _ in AREAS + [('Other', ())]:
    if n in by: w(f'| {n} | ' + ' | '.join(str(by[n][s]) for s in SEV) + ' |')
w('')

w('## Design assessments\n')
w('Four reviewers looked at one whole design each, not its lines. Their assessments are printed as written; '
  'their findings are CRs like the others (ids `a-...`).\n')
for k in args['order']:
    if k.startswith('a-') and k in res:
        w(f'### {k}\n\n{res[k]["summary"].strip()}\n')


def row(x):
    f = allf[x['id']]
    rp = f'`{f["repro_path"].split("/")[-1]}`' if f.get('repro_path') else ''
    return (f'| {x["id"]} | {x["tag"]}/{x["sev"]} | {x["addr"]} | `{x["file"]}` | {esc(f["title"])} | '
            f'{f["evidence"]} | {rp} |')


w('## Correctness findings (bug and risk), by area\n')
w(f'`evidence`: `ran` = the reviewer ran a program that shows it; `read` = from the code alone. Repros are in `{REPRO}/`.\n')
for n, _ in AREAS + [('Other', ())]:
    xs = sorted([x for x in crs.values() if area(x['id']) == n and x['tag'] in ('bug', 'risk')],
                key=lambda x: (SEV[x['sev']], x['id']))
    if not xs: continue
    w(f'### {n}\n\n| id | tag/sev | for | file | title | evidence | repro |\n|---|---|---|---|---|---|---|')
    for x in xs: w(row(x))
    w('')
w('## Other findings, by area\n')
for n, _ in AREAS + [('Other', ())]:
    xs = sorted([x for x in crs.values() if area(x['id']) == n and x['tag'] not in ('bug', 'risk')],
                key=lambda x: (SEV[x['sev']], x['tag'], x['id']))
    if not xs: continue
    w(f'### {n}\n\n| id | tag/sev | for | file | title |\n|---|---|---|---|---|')
    for x in xs:
        w(f'| {x["id"]} | {x["tag"]}/{x["sev"]} | {x["addr"]} | `{x["file"]}` | {esc(allf[x["id"]]["title"])} |')
    w('')
w('## CLAUDE.md drift (not written as CRs)\n')
for i, why, p in excl:
    if why != 'claude-md': continue
    f = allf[i]
    w(f'- **{i}** ({f["tag"]}/{f["severity"]}, `{p}:{f.get("cr_line")}`): {esc(f["title"])}. {esc(f["cr_text"])}')
w('\n## Findings with no place for a comment\n')
for i, why, p in excl:
    if why.startswith('dup') or why == 'claude-md': continue
    f = allf[i]
    w(f'- **{i}** ({f["tag"]}/{f["severity"]}, {why}, `{p}`): {esc(f["title"])}. {esc(f["cr_text"])}')
w('\n## Merged duplicates\n')
w(', '.join(f'{i} → {why[7:]}' for i, why, p in excl if why.startswith('dup')) + '.\n')
w('## Re-run tallies (from each x-rerun reviewer)\n')
for k in sorted(res):
    if k.startswith('x-rerun'): w(f'- **{k}**: {esc(res[k]["summary"])}')
open(STATE + '/tables.md', 'w').write('\n'.join(out) + '\n')
print(len(out), 'lines ->', STATE + '/tables.md')
