#!/usr/bin/env python3
"""Insert review CRs above their anchor lines.

usage: insert_crs.py crs.json [--root DIR] [--dry-run]

crs.json: [{id, file, line, anchor, tag, sev, addr, text}]. A CR is placed
above the line whose stripped text equals `anchor`, nearest to `line` (within
+-60); failing that, above `line`. It moves up out of a Rust string literal or
block comment, a markdown code fence or front matter, and below a shebang.
Prints one line per CR: OK / OK-FAR / FUZZY / NOANCHOR (+MOVED) and the line.
Run fix_gx_crs.py over the .gx/.gxi files it touched afterwards.
"""
import json, os, re, sys, textwrap

WIDTH = 88
PREFIX = {
    '.rs': '//', '.gx': '//', '.gxi': '//', '.js': '//', '.ts': '//', '.c': '//',
    '.py': '#', '.sh': '#', '.toml': '#', '.el': ';;', '.lua': '--', '.scm': ';',
}


def rust_line_states(lines):
    """For each line, the lexer state at its start: None, ('str', open_line),
    ('raw', hashes, open_line) or ('block', depth, open_line)."""
    states = []
    st = None
    for ln, line in enumerate(lines):
        states.append(st)
        i, n = 0, len(line)
        while i < n:
            if st is None:
                c = line[i]
                if line.startswith('//', i):
                    break
                if line.startswith('/*', i):
                    st = ('block', 1, ln); i += 2; continue
                word_start = i == 0 or not (line[i - 1].isalnum() or line[i - 1] == '_')
                m = re.match(r'(?:b|c)?r(#*)"', line[i:])
                if m and word_start:
                    st = ('raw', len(m.group(1)), ln); i += m.end(); continue
                if c == '"':
                    st = ('str', ln); i += 1; continue
                if c in 'bc' and line.startswith('"', i + 1) and word_start:
                    st = ('str', ln); i += 2; continue
                if c == "'":
                    m = re.match(r"'(\\(x[0-9a-fA-F]{2}|u\{[0-9a-fA-F]+\}|.)|[^\\'])'", line[i:])
                    if m:
                        i += m.end(); continue
                i += 1
            elif st[0] == 'block':
                if line.startswith('/*', i):
                    st = ('block', st[1] + 1, st[2]); i += 2
                elif line.startswith('*/', i):
                    st = None if st[1] == 1 else ('block', st[1] - 1, st[2]); i += 2
                else:
                    i += 1
            elif st[0] == 'str':
                if line[i] == '\\':
                    i += 2
                elif line[i] == '"':
                    st = None; i += 1
                else:
                    i += 1
            else:  # raw
                close = '"' + '#' * st[1]
                j = line.find(close, i)
                if j < 0:
                    break
                st = None; i = j + len(close)
    return states


def md_line_states(lines):
    """For each line, ('fence', open_line) inside a code fence, ('front',
    last_line) inside front matter (the CR goes below it), else None."""
    states, fence = [], None
    front = lines[:1] == ['---'] and next((i for i in range(1, len(lines)) if lines[i].strip() == '---'), None)
    for ln, line in enumerate(lines):
        if front and ln <= front:
            states.append(('front', front + 1)); continue
        states.append(None if fence is None else ('fence', fence))
        t = line.lstrip()
        if t.startswith(('```', '~~~')):
            fence = ln if fence is None else None
    return states


def comment_lines(prefix, indent, head, text):
    width = max(40, WIDTH - len(indent))
    if prefix is None:  # markdown: one HTML comment; mdbook expands {{#include even there
        text = text.replace('{{#', '\\{{#').replace('-->', '->')
        body = textwrap.wrap(f'<!-- {head} {text} -->', width=width,
                             break_long_words=False, break_on_hyphens=False)
        return [indent + l for l in body]
    body = textwrap.wrap(f'{head} {text}', width=width - len(prefix) - 1,
                         break_long_words=False, break_on_hyphens=False)
    return [f'{indent}{prefix} {l}' for l in body]


def main():
    args = sys.argv[1:]
    dry = '--dry-run' in args
    root = args[args.index('--root') + 1] if '--root' in args else '.'
    crs = json.load(open(args[0]))
    by_file = {}
    for c in crs:
        by_file.setdefault(c['file'], []).append(c)
    report = []
    for f, cs in sorted(by_file.items()):
        path = os.path.join(root, f)
        ext = os.path.splitext(f)[1]
        if ext != '.md' and ext not in PREFIX or not os.path.exists(path):
            for c in cs:
                report.append(('NOFILE' if not os.path.exists(path) else 'NOFORMAT', c['id'], f, c.get('line')))
            continue
        prefix = None if ext == '.md' else PREFIX[ext]
        lines = open(path).read().split('\n')
        states = (rust_line_states(lines) if ext == '.rs' else md_line_states(lines) if ext == '.md'
                  else [None] * len(lines))
        placed = []
        for c in cs:
            want = max(1, min(int(c.get('line') or 1), len(lines)))
            anchor = (c.get('anchor') or '').strip()
            status, at = 'NOANCHOR', want
            if anchor:
                cands = [i + 1 for i, l in enumerate(lines) if l.strip() == anchor]
                near = [i for i in cands if abs(i - want) <= 60]
                if near:
                    at = min(near, key=lambda i: abs(i - want)); status = 'OK'
                elif len(cands) == 1:
                    at = cands[0]; status = 'OK-FAR'
                else:
                    key = anchor[:60]
                    fz = [i + 1 for i, l in enumerate(lines) if key in l and abs(i + 1 - want) <= 60]
                    if fz:
                        at = min(fz, key=lambda i: abs(i - want)); status = 'FUZZY'
            st = states[at - 1]
            if st is not None:
                at = st[-1] + 1; status += '+MOVED'
            if at == 1 and lines[0].startswith('#!'):
                at = 2; status += '+MOVED'
            at = min(at, len(lines))
            placed.append((at, status, c))
        for at, status, c in sorted(placed, key=lambda p: -p[0]):
            indent = re.match(r'\s*', lines[at - 1]).group(0)
            head = f"CR claude for {c['addr']}: [{c['tag']}/{c['sev']}]"
            text = ' '.join(c['text'].split())
            lines[at - 1:at - 1] = comment_lines(prefix, indent, head, text)
            report.append((status, c['id'], f, at))
        if not dry:
            open(path, 'w').write('\n'.join(lines))
    for r in sorted(report, key=lambda r: (r[2], r[3] or 0)):
        print(*r)


if __name__ == '__main__':
    main()
