# sep28j (47f85c52, base 12500M; katana 1 typeflip)

**katana typeflip_000000: a fuzzer false positive.** `label-missing`
dropped `#trigger: let feedback: Any = never()`, an argument that binds a
name the program reads later; the mutant is refused, as it should be, for
`feedback is undefined`, not at the call the family names (MISPLACED).
The family now skips an argument that binds outward (`binds_outward`, as
the extraction families do). Pin: the `label-missing` case in
`must_reject_families_are_refused_where_their_rules_say`.
