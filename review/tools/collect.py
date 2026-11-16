#!/usr/bin/env python3
"""Read the workflow journal (first result per label wins) -> STATE/results.json:
{key: {model, summary, findings[]}}, each finding with id <key>-NN.
usage: collect.py JOURNAL.jsonl"""
import json, os, sys
from collections import Counter
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from cfg import STATE

fable = set(json.load(open(STATE + '/args.json'))['fable'])
reviews, lab = {}, {}
for l in open(sys.argv[1]):
    d = json.loads(l)
    if d['type'] == 'started': lab[d['agentId']] = d['label']
    elif d['type'] == 'result':
        L = lab.get(d['agentId'], ''); r = d['result']
        if not isinstance(r, dict) or 'findings' not in r or L in reviews: continue
        for i, f in enumerate(r['findings'], 1): f['id'] = f'{L}-{i:02d}'; f['unit'] = L
        reviews[L] = dict(model='fable' if L in fable else 'opus', summary=r.get('summary', ''), findings=r['findings'])
json.dump(reviews, open(STATE + '/results.json', 'w'), indent=1)
fs = [f for v in reviews.values() for f in v['findings']]
order = json.load(open(STATE + '/args.json'))['order']
print('reviews', len(reviews), 'of', len(order), 'findings', len(fs))
print('missing', [k for k in order if k not in reviews])
print(Counter(f"{f['tag']}/{f['severity']}" for f in fs).most_common())
print('evidence', Counter(f['evidence'] for f in fs), 'addr', Counter(f['addressee'] for f in fs))
for m in ('fable', 'opus'):
    rs = [v for v in reviews.values() if v['model'] == m]
    print(m, 'reviewers', len(rs), 'findings', sum(len(v['findings']) for v in rs))
