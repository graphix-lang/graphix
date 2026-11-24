# gdb script: after an interrupt, print the outermost frames of each thread
# that is deep in the old relation, leaving out the relation's own recursion.
#   timeout -s INT 3 gdb -batch -ex run -x hot.py --args <program> <args>
import gdb, re
skip = re.compile(r'conjuncts_admit|cell_constraints_ok|contains_dispatch|contains_int|stacker|psm::|rust_psm|stack::grow|closure|held_by_lower|holds_cell|holds_union|ensure_sufficient|__rust|core::ops|std::|tokio|^\?$')
for t in gdb.selected_inferior().threads():
    t.switch()
    f = gdb.newest_frame()
    deep = 0
    while f is not None and deep < 300:
        f = f.older()
        deep += 1
    if deep < 300:
        continue
    f = gdb.oldest_frame()
    names = []
    while f is not None and len(names) < 70:
        name = f.name() or '?'
        if not skip.search(name):
            names.append(name)
        f = f.newer()
    print('THREAD', t.num)
    for name in names:
        print('   ', name[:170])
