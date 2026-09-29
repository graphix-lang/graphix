# sep28h (367582d2, base 12300M; katana 1 typeflip, ryouko 1)

Two checker holes, both older than the levels work.

**ryouko divergence_000000: the check accepted what the build refused.**
Inside a definition, a committing `contains` of a concrete type over a
rigid `'r` neither bound the cell (rigid) nor refused, and answered true:
`let f = |x: 'r| g(x)` with `g: fn(a: i64)` checked, and so did
`[i64, 'r]` for `'a: Number`; the instance at `'r = string` failed.
Fixed in `typ/contains.rs`: such a commit takes the rigid verdict (a
conjunct of the cell, or a union member holding it).

**katana typeflip_000000: an annotation's same-named variables split.**
`Type::scope_refs` re-minted every occurrence of a cell, so the two `'a`
of `let f: fn(x: 'a) -> 'a = ..` became two variables and
`buffer::from_string` passed inline while the alias form refused it.
Fixed: scoping maps each cell to one copy.

Pins: `lang::functions::rigid_*`, `annotation_variable_is_one_variable`,
`findings/rigid-commit-sep2026/`.
