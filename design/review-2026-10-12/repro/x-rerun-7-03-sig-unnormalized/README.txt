x-rerun-7-03 (residue of t-tvar-09, its case 7): an interface val checked
against an implementation whose type is a union formed while a member was
open ([y, i64], y written i64 later) is refused by sig_matches, which
compares the unnormalized binding.
command: graphix --check design/review-2026-10-12/repro/x-rerun-7-03-sig-unnormalized/main.gx
expected: exit 0 (x is i64)
observed: val z is declared i64 but implemented '_N: type mismatch:
  signature has i64, implementation has [i64, i64]
