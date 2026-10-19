import re, sys
cols, rows = int(sys.argv[2]), int(sys.argv[3])
data = open(sys.argv[1], encoding='utf-8', errors='replace').read()
scr = [[' '] * cols for _ in range(rows)]
r = c = 0
i = 0
pat = re.compile(r'\x1b\[([0-9;?]*)([A-Za-z])|\x1b[()][A-Z0-9]|\x1b[=>78]')
while i < len(data):
    m = pat.match(data, i)
    if m:
        if m.group(2) == 'H':
            a = [int(x) if x else 1 for x in (m.group(1) or '').split(';')] if m.group(1) else [1, 1]
            r, c = a[0] - 1, (a[1] - 1 if len(a) > 1 else 0)
        elif m.group(2) == 'J' and m.group(1) in ('2', ''):
            scr = [[' '] * cols for _ in range(rows)]
        i = m.end(); continue
    ch = data[i]
    if ch == '\r': c = 0
    elif ch == '\n': r += 1
    elif ch >= ' ':
        if 0 <= r < rows and 0 <= c < cols: scr[r][c] = ch
        c += 1
    i += 1
for line in scr: print(''.join(line).rstrip())
