#!/usr/bin/env python3
"""Move CR blocks in .gx/.gxi files up until the file parses (graphix fmt --stdout).
A Graphix comment is legal only above an expression, an arm, an item; a CR placed
between a doc and its item, inside a parameter list or after a block's last arm is not.
usage: fix_gx_crs.py BIN FILE..."""
import re, subprocess, sys

BIN = sys.argv[1]


def parse_err(path):
    p = subprocess.run(['timeout', '30', BIN, 'fmt', '--stdout', path], capture_output=True, text=True)
    if p.returncode == 0:
        return None
    m = re.search(r'at line: (\d+)', p.stderr)
    return int(m.group(1)) if m else -1


def blocks(lines):
    """[start, end) of each CR block: a `// CR claude` head and its `//` continuation."""
    out, i = [], 0
    while i < len(lines):
        if lines[i].lstrip().startswith('// CR claude for'):
            j = i + 1
            while j < len(lines) and lines[j].lstrip().startswith('//') \
                    and not lines[j].lstrip().startswith('// CR ') and not lines[j].lstrip().startswith('///'):
                j += 1
            out.append((i, j))
            i = j
        else:
            i += 1
    return out


for path in sys.argv[2:]:
    for _ in range(200):
        err = parse_err(path)
        if err is None:
            print('OK', path)
            break
        lines = open(path).read().split('\n')
        bs = blocks(lines)
        cand = [b for b in bs if b[0] <= err - 1] or bs
        if not cand:
            print('NOCR', path, err)
            break
        s, e = max(cand, key=lambda b: b[0])
        if s == 0:
            print('STUCK', path, err)
            break
        # step over the line above (a whole CR block if one sits there)
        above = next((b for b in bs if b[1] == s), None)
        ns = above[0] if above else s - 1
        blk = lines[s:e]
        del lines[s:e]
        lines[ns:ns] = blk
        nxt = lines[ns + len(blk)] if ns + len(blk) < len(lines) else ''
        ind = re.match(r'\s*', nxt).group(0)
        for k in range(ns, ns + len(blk)):
            lines[k] = ind + lines[k].lstrip()
        open(path, 'w').write('\n'.join(lines))
    else:
        print('GAVEUP', path)
