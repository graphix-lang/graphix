# oracle-tier-comment-scan-aug2026

Not an interp/JIT divergence — a hole in the ORACLE that silently
un-gates regression pins.

## The flaw

`oracle_tier` (graphix-fuzz/src/lib.rs:517) decides whether a program's
values may be compared at all, and it decides by substring-scanning the
**whole wrapper text**:

```rust
let excluded = ["rand::", "sys::time", "sys::net", ... ];
if excluded.iter().any(|m| code.contains(m)) {
    return OracleTier::Excluded;
}
```

The wrapper text includes the finding's own write-up comment. So a
header that merely *mentions* one of those APIs turns the whole file
into `OracleTier::Excluded`, and `check` returns `None` — reported as
`AGREE` — no matter what the program does.

## Demonstration (exact, reproducible)

`findings/dyncall-stale-arg-fired-aug2026/01_escape_per_arg_gating.gx`
is a live divergence (interp emits one value, jit emits four). While its
header contained the words `sys::net` and `sys::time` in a sentence
about which packages use `update_diff`:

```
$ graphix-fuzz check 01_escape_per_arg_gating.gx
AGREE — interp and jit produce the same result
```

Removing those two words from the COMMENT — the program body untouched:

```
$ graphix-fuzz check 01_escape_per_arg_gating.gx
DIVERGENCE — fusion/JIT bug (interp != jit)
  interp: Trace([0:"a\\/b"])
  jit:    Trace([0:"a\\/b" 1:"a/\\b" 2:"a/\\b" 3:"a/\\b"])
```

## Corpus files currently affected

Three pre-existing pins mention an excluded API only in their write-up,
so `graphix-fuzz regress` is not actually checking them today — they
would report clean even if their bug came back:

- `homogeneous-arith-jul2026/01_mixed_acc_map_instance.gx` (`sys::time`)
- `source-e-jun2026/06_let_neg_1_select_9223372036854775807_1_n_n_neg.gx` (`sys::time`)
- `source-e-jun2026/07_i64_9223372036854775808_i64_1.gx` (`sys::time`, `sys::net`)

Found with:

```sh
python3 - <<'PY'
import glob
markers = ["rand::","sys::time","sys::net","sys::process::kill","sys::process::pid",
           "tempdir","listener_addr","local_addr","peer_addr",
           "sys::fs::write_all","sys::fs::create_dir","sys::fs::remove_dir",
           "sys::fs::remove_file","sys::fs::read_all","sys::fs::readdir",
           "sys::fs::metadata","sys::fs::is_file","sys::fs::is_dir","sys::fs::watch"]
for f in sorted(glob.glob("graphix-fuzz/findings/*/*.gx")):
    txt = open(f).read()
    body = "\n".join(l for l in txt.splitlines() if not l.lstrip().startswith("//"))
    for m in markers:
        if m in txt and m not in body:
            print(m, f)
PY
```

## Proposed fix

Scan the PROGRAM, not the artifact. `Schedule::parse` and `files::split`
already separate the header, the body and the file sections, so the tier
decision can run on the body + sections with full-line comments stripped
(a `//`-to-end-of-line strip is enough — graphix has no block comments).
Two smaller hardenings worth taking at the same time:

- run the scan after comment-stripping in `corpus`/`regress` too, so a
  future write-up can name any API without consequence;
- make an `Excluded` verdict VISIBLE: `check` should print
  `EXCLUDED — value comparison disabled (matched "sys::time")` rather
  than the indistinguishable `AGREE`. Today the two outcomes look
  identical, which is exactly why this went unnoticed — and an author
  who reads `AGREE` on a program they know diverges has no way to tell
  the oracle simply declined to look.
