# Title: the design docs that hold the rules are not readable as rules.
# Command: python3 design/review-2026-11-16/repro/a-compiler-doc-stats.py   (from the repo root)
# Expected: a rule book a reviewer can find a rule in.
# Observed (HEAD e1be2fbf2):
#   design/tvar_constraints.md: words=37450 paras=113 longest_para=3445 paras>300w=28 sentences=786 median=42 >60w=195 max=833
#   design/architecture.md: words=1939 paras=9 longest_para=568 paras>300w=3 sentences=43 median=27 >60w=11 max=167
#   design/builtins.md: words=988 paras=3 longest_para=966 paras>300w=1 sentences=24 median=26 >60w=3 max=211
#   design/seq_blocks.md: words=9818 paras=64 longest_para=889 paras>300w=8 sentences=302 median=27 >60w=32 max=152
import re
for f in ['design/tvar_constraints.md', 'design/architecture.md', 'design/builtins.md',
          'design/seq_blocks.md', 'design/strict_fusion.md', 'design/bind_driven_bodies.md', 'CLAUDE.md']:
    s = re.sub(r'```.*?```', '', open(f).read(), flags=re.S)
    paras = [p for p in re.split(r'\n\s*\n', s)
             if p.strip() and not p.strip().startswith('|') and not p.strip().startswith('#')]
    pw = [len(p.split()) for p in paras]
    sents = re.split(r'(?<=[.!?])\s+(?=[A-Z`*])', ' '.join(paras))
    sw = sorted(len(x.split()) for x in sents)
    print(f"{f}: words={sum(pw)} paras={len(paras)} longest_para={max(pw)} "
          f"paras>300w={sum(1 for x in pw if x > 300)} sentences={len(sw)} "
          f"median={sw[len(sw)//2]} >60w={sum(1 for x in sw if x > 60)} max={sw[-1]}")
