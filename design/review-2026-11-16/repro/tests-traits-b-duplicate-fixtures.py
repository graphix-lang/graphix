# Title: run! fixtures in stdlib/graphix-tests/src/lang/traits.rs whose
#   program text is another fixture's, word for word.
# Command: python3 -I design/review-2026-11-16/repro/tests-traits-b-duplicate-fixtures.py
# Expected: no line printed.
# Observed: three pairs: impl_self_in_a_dynamic_module_signature_names_the_target
#   / ..._refusal_says_self_reads_the_target (same program, the second's
#   message holds the first's), method_result_variable_beside_a_written_member
#   / method_unwritten_type_takes_the_declared_union, and
#   method_result_variable_beside_a_written_member_written /
#   method_written_member_beside_declared_union (program and predicate alike).
import re, collections, os
here = os.path.dirname(os.path.abspath(__file__))
path = os.path.join(here, '../../../stdlib/graphix-tests/src/lang/traits.rs')
src = open(path).read()
progs = collections.defaultdict(list)
for m in re.finditer(r'\nrun!\(\s*(\w+)\s*,', src):
    nxt = src.find('\nrun!(', m.end())
    body = src[m.end(): nxt if nxt != -1 else len(src)]
    key = tuple(re.sub(r'\s+', ' ', s).strip() for s in re.findall(r'r#"(.*?)"#', body, re.S))
    if key:
        progs[key].append((src[:m.start()].count('\n') + 2, m.group(1)))
for v in progs.values():
    if len(v) > 1:
        print(v)
