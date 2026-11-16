#!/usr/bin/env python3
"""STATE/results.json -> crs.json for insert_crs.py, and crs_excluded.json
(duplicates, CLAUDE.md drift, findings with nowhere to put a comment)."""
import json, os, re, sys
from collections import Counter
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from cfg import REPO, STATE

res = json.load(open(STATE + '/results.json'))
TAGS = {'bug', 'risk', 'test-gap', 'doc-drift', 'structure', 'perf', 'readability', 'style', 'dead'}
SEVS = {'critical', 'high', 'medium', 'low'}
# a comment in the grammar's generated sources fails zed_grammar_rev_is_current
GENERATED = ('docs/', 'ide/tree-sitter-graphix/src/', 'graphix-fuzz/findings/')
NO_COMMENTS = ('.json', '.lock', '.manifest', '.txt', '.html', '.css', '.svg', '.png')
crs, excluded = [], []
LINE_REF = re.compile(r'\b([\w./-]+\.(?:rs|gx|gxi|md|toml|py|sh|js|c)):\d+(?:[-:]\d+)?')
for r in res.values():
    for f in r['findings']:
        i = f['id']
        p = (f.get('cr_file') or '').strip()
        if p.startswith(REPO + '/'): p = p[len(REPO) + 1:]
        text = ' '.join((f.get('cr_text') or '').split())
        if not p or not text: excluded.append((i, 'no-placement', p)); continue
        if p.startswith(('../', '/')): excluded.append((i, 'out-of-repo', p)); continue
        if p in ('CLAUDE.md', 'AGENTS.md'): excluded.append((i, 'claude-md', p)); continue
        if p.startswith(GENERATED): excluded.append((i, 'generated', p)); continue
        if p.endswith(NO_COMMENTS): excluded.append((i, 'no-comment-syntax', p)); continue
        if not os.path.isfile(os.path.join(REPO, p)): excluded.append((i, 'missing-file', p)); continue
        text = LINE_REF.sub(r'\1', text)
        crs.append(dict(id=i, tag=f['tag'] if f['tag'] in TAGS else 'risk',
                        sev=f['severity'] if f['severity'] in SEVS else 'medium',
                        addr=f['addressee'] if f['addressee'] in ('eric', 'claude') else 'claude',
                        file=p, line=f.get('cr_line') or 1, anchor=f.get('cr_anchor') or '',
                        text=f'{text} ({i})', title=f['title'], ev=f['evidence']))
# merge duplicates: same file, nearby line, similar title; the kept CR names the others
SEVR = {'critical': 0, 'high': 1, 'medium': 2, 'low': 3}
STOP = set('a an the of to in is and or on for by at its it as be not no with from into that this'.split())


def words(c): return {w for w in re.findall(r'[a-z0-9_:]+', c['title'].lower()) if w not in STOP}


crs.sort(key=lambda c: (SEVR[c['sev']], c['ev'] != 'ran', c['id']))
kept = []
for c in crs:
    w = words(c)
    for k in kept:
        if k['file'] == c['file'] and abs(int(k['line']) - int(c['line'])) <= 8:
            kw = words(k)
            if w and kw and len(w & kw) / len(w | kw) >= 0.35:
                k.setdefault('also', []).append(c['id'])
                excluded.append((c['id'], 'dup-of-' + k['id'], c['file']))
                break
    else:
        kept.append(c)
for k in kept:
    if k.get('also'):
        k['text'] = (k['text'][:-len(f" ({k['id']})")] + ' Also reported as '
                     + ', '.join(f'({a})' for a in k['also']) + f". ({k['id']})")
json.dump(kept, open(STATE + '/crs.json', 'w'), indent=1)
json.dump(excluded, open(STATE + '/crs_excluded.json', 'w'), indent=1)
print('crs', len(kept), 'excluded', len(excluded), Counter(e[1].split('-of-')[0] for e in excluded))
print('by tag/sev', Counter(f"{c['tag']}/{c['sev']}" for c in kept).most_common())
print('by addr', Counter(c['addr'] for c in kept))
