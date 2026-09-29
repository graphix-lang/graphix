# sep28i (9123bcf5, base 12400M; aieka 1 typeflip)

**aieka typeflip_000000**: the annotation-variable split of sep28h's
katana typeflip, on a binary before its fix: `let f: fn(x: 'a) -> 'a =
|x| ..` written inline accepted a body the alias form refused. Fixed by
47f85c52 (scoping keeps one copy per cell); both forms refuse, and
`typemorph` on it finds no flip. Pinned by
`findings/rigid-commit-sep2026/01_annotation_variable_split.gx`.
