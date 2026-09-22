# Compiler CR re-evaluation

Rechecked the six remaining findings against `82be24f7`. **CR01, CR03, CR18,
and CR20 are resolved and their comments have been removed. CR10 and CR25 remain
open**, with updated `CR codex for eric` comments. Across the original review,
25 of 27 findings are closed. This re-evaluation changes comments and this report
only.

## Open findings

| CR | Location | Remaining problem | Evidence |
|---|---|---|---|
| 10 | [node/coretraits.rs:193](src/node/coretraits.rs#L193) | Implementation selection tests type overlap, ignores repeated-variable consistency, and discards declared bounds. | Three reproductions below return the wrong result in both engines. |
| 25 | [node/coretraits.rs:510](src/node/coretraits.rs#L510) | `CachedArgs` invokes a public safe builtin implementation under an unsafe loan while giving it the same mutable context. | Source inspection of the safe trait and its caller; undefined behavior was not exercised. |

## CR10: implementation matching is still too permissive

Carrying `GxAbstract::params` fixes the previous concrete and phantom-parameter
cases. However, `SiteEntry::choose` uses `could_match`, which tests whether types
overlap, rather than whether the implementation applies to the entire concrete
type. It also treats repeated open variables independently. `candidates_for`
replaces declared parameters with unconstrained variables, losing their bounds.

Each program below passes `--check`. Run with the rebuilt shell:

```sh
~/tmp/target/debug/graphix --no-netidx --no-init --no-cache case.gx
~/tmp/target/debug/graphix --no-netidx --no-init --no-cache --no-fusion case.gx
```

Both configurations print **`true` for every case below; each should print
`false`**, using structural equality because the declared implementation does not
apply.

### Overlapping union parameter

`Marker<i64>` does not implement equality for `Marker<[i64, string]>`.

```graphix
type Marker<'a> = Abstract<i64>;
impl Eq for Marker<i64> { let eq = |a, b| true };
let a: Marker<[i64, string]> = Marker(1);
let b: Marker<[i64, string]> = Marker(2);
println(a == b);
sys::exit(sys::time::after_idle(duration:0.01s, 0))
```

The same failure occurs for `Box<'a> = Abstract<'a>` when its constructor receives
an argument statically typed `[i64, string]`: the `Box<i64>` implementation runs
even when the payload is a string.

### Repeated parameter

The two occurrences of `'a` must agree; `Pair<i64, string>` does not match this
implementation head.

```graphix
type Pair<'a, 'b> = Abstract<('a, 'b)>;
impl<'a> Eq for Pair<'a, 'a> { let eq = |a, b| true };
println(Pair((1, "a")) == Pair((2, "b")));
sys::exit(sys::time::after_idle(duration:0.01s, 0))
```

### Unsatisfied bound

`string` has no `Mark` implementation, so the bounded implementation cannot apply
to `Marker<string>`.

```graphix
trait Mark { val mark: fn(self) -> bool };
impl Mark for i64 { let mark = |x| true };
type Marker<'a> = Abstract<i64>;
impl<'a: Mark> Eq for Marker<'a> { let eq = |a, b| true };
let a: Marker<string> = Marker(1);
let b: Marker<string> = Marker(2);
println(a == b);
sys::exit(sys::time::after_idle(duration:0.01s, 0))
```

Match a fresh implementation head against the concrete carried type, preserving
consistent substitutions and checking bounds. Build and cache the hook for the
resulting concrete instantiation. An overlap predicate is insufficient.

## CR25: the unsafe loan still reaches an unrestricted safe callback

The environment snapshots in interpolation, the core printers, and the kernel
remove the previously identified direct shared borrow of `ctx.env` across hook
dispatch. Those changes are accepted.

The remaining problem is the call in
[CachedArgs::update_inner](../stdlib/graphix-package-core/src/lib.rs#L790): it
calls `ev.eval(ctx, cached)` inside `with_value_hooks` and asserts that every eval
obeys the loan contract. But
[EvalCached::eval](../stdlib/graphix-package-core/src/lib.rs#L610) is a public safe
trait method receiving the entire `&mut ExecCtx`. A safe implementation can borrow
`&ctx.env`, pass it to `TVal` while formatting an abstract argument, and read the
environment again afterward. `dispatch_fmt` reconstructs `&mut ExecCtx`, and
`build_site` inserts bindings into that same environment during the formatting.
This violates the loan contract without any unsafe code in the implementation.

The wrapper must separate mutable hook state from the context available to safe
builtin code, or express the required invariant through an audited unsafe
implementation boundary. Auditing today's builtin bodies alone cannot justify
this unrestricted safe extension interface. This is a source-level safety
finding, not a claimed observed crash or sanitizer result.

## Resolved in this re-evaluation

| CR removed | Verification |
|---|---|
| 01 | `graphix_variant_tag_eq` is now unsafe. No safe JIT helper signature takes a raw pointer. |
| 03 | Image expression keys use stable session-owned clones matched by id and contents; the input-address alias table is gone. `image::tests::reused_address_is_not_an_alias` passes. |
| 18 | Each cache entry retains the implementation-list Arc and compares with `Arc::ptr_eq`; removal or clearing invalidates it without allocation-address reuse. Registry mutation paths replace the list. |
| 20 | `polled`, `staged`, and `slots` use pooled Vec storage. `polled` contains `TagValue`, removing the independent tag/optional-value representation. |

Previously closed findings remain closed: 02, 04–09, 11–17, 19, 21–24, 26, 27.

## Validation

- `cargo test -p graphix-compiler -p graphix-tests`: **2,977 passed, two ignored**
  (208 compiler tests and 2,769 language/package tests).
- Rebuilt `graphix-shell`; all eight CLI probes passed `--check` and were run with
  fusion enabled and disabled. The previous sole-specialization case now prints
  `true, false`, and the previous phantom-specialization case prints `false`.
  Generic equality across integer and string instantiations also behaves correctly.
  The four failing probes are the three cases above plus the union-argument Box case.
- `cargo fmt -p graphix-compiler -p graphix-package-core --check` and
  `git diff --check`: passed.
- `cargo fmt --all --check` reports existing differences in sibling netidx files
  `graphix-package-netidx-admin/src/lifecycle.rs` and `netidx-admin/src/lib.rs`.

## Disposition of the third round

| CR | Resolution |
|---|---|
| 10 | `impl_for` matches a fresh head — the declared variables open, carrying their bounds — by containment both ways, so one substitution serves every occurrence (`Pair<'a, 'a>`), a bound is checked (`'a: Mark`) and an overlap is not a match (`Marker<i64>` against `Marker<[i64, string]>`). The hook is cached per concrete instantiation, and its site's signature is the method's instantiated through that substitution (a declared variable settles to ⊥ once the body is checked, which a plain reset carries as a solved fact). Pin: `traits::core_eq_impl_must_apply` (the review's three cases). |
| 25 | The blanket loan is gone. Every loan is exclusive: `with_hooks`/`with_display_hooks` take a closure with no context, and the seam runs its sites over events of its own (`CoreHookSites::{template, spare}`, seeded from the first loan that has an event, `CachedArgs::update` included), so no caller lends a context or an event it still holds; the `unsafe fn` is deleted. `EvalCached::eval` runs unarmed: `fast_eval`/`fast_eval_typed` arm the loan around a fast fn, which sees only its arguments (100 builtins; a `Typed` fn reads an `Env` snapshot when a hook can fire), and the three hand-written evals that compare values (`min`, `max`, `opt::contains`) take `eval_with_hooks` around the comparison; the rule is stated at the trait. `traits::core_sort_min_max_by_ord` caught `min`/`max` during the conversion. |
