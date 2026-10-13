import re, sys
data = open(sys.argv[1], 'rb').read().decode('utf-8', 'replace')
W, H = int(sys.argv[2]), int(sys.argv[3])
scr = [[' '] * W for _ in range(H)]
r = c = 0
i = 0
pat = re.compile(r'\x1b\[([0-9;?]*)([A-Za-z])')
while i < len(data):
    m = pat.match(data, i)
    if m:
        if m.group(2) == 'H':
            parts = (m.group(1) or '1;1').split(';')
            r, c = int(parts[0]) - 1, int(parts[1]) - 1
        i = m.end(); continue
    ch = data[i]
    if ch >= ' ' and 0 <= r < H and 0 <= c < W:
        scr[r][c] = ch; c += 1
    i += 1
for row in scr: print(''.join(row).rstrip() + '|')
