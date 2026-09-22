# Compiler review

Reviewed against `e2fe166f` on 2026-09-22. This review adds **21 findings**
(11 P1, 10 P2), marked `CR codex for eric: [CRnn, Pn]` beside the code.
CR03 has markers in both engines; CR21 has markers on the ABI and tag helper.
No implementation changes are included.
The previous review's closed findings remain in git history.

P1 means incorrect results, an invalid program accepted, a valid program broken,
or a process abort. P2 includes narrower correctness defects and material
allocation, lifecycle, structure, and documentation problems. Style and
architecture findings are part of the review, not optional follow-ups.

## Findings

| ID | Priority | Location | Problem | Evidence |
| --- | --- | --- | --- | --- |
| CR01 | P1 | [typ/cast.rs](src/typ/cast.rs#L341) | Abstract predicates ignore generic parameters, allowing invalid narrowing. | Both-engine reproduction; outputs diverge. |
| CR02 | P1 | [node/coretraits.rs](src/node/coretraits.rs#L352) | Binary trait dispatch checks only the left operand's concrete type. | Asymmetric equality in both engines. |
| CR03 | P1 | [node/callsite.rs](src/node/callsite.rs#L1521), [fusion/emit/flow.rs](src/fusion/emit/flow.rs#L357) | Tail rebinds replace bottom with the previous argument value. | Tail/non-tail comparison in both engines. |
| CR04 | P1 | [analysis.rs](src/analysis.rs#L313) | Effect inference discards all but the first instance of a definition. | An unrelated pure call makes an invalid `#[sync]` assertion pass. |
| CR05 | P1 | [expr/resolver.rs](src/expr/resolver.rs#L834) | Cyclic module loading has no active-source guard. | Two files abort `--check` with stack overflow. |
| CR06 | P1 | [expr/format.rs](src/expr/format.rs#L125) | Sorting imports can move an alias use before its definition. | Formatter accepts output that fails compilation. |
| CR07 | P2 | [expr/format.rs](src/expr/format.rs#L57) | Relative configuration discovery misses parents of the working directory. | Subprocess: width 90 instead of project width 37. |
| CR08 | P1 | [node/bind.rs](src/node/bind.rs#L998) | Place references compile and execute address expressions twice. | An index's print runs twice in both engines. |
| CR09 | P1 | [node/bind.rs](src/node/bind.rs#L1074) | A bottom address leaves the previous place registered. | Timer reads the old element after its index becomes bottom. |
| CR10 | P1 | [node/bind.rs](src/node/bind.rs#L1361) | An invalid current place read retains its previous successful value. | Timer reads a removed array element. |
| CR11 | P2 | [node/bind.rs](src/node/bind.rs#L1181) | Place typing subtracts legitimate Error-valued elements. | A reference to an Error-valued field fails typechecking. |
| CR12 | P2 | [node/bind.rs](src/node/bind.rs#L835) | Parentheses detach references from their addressable roots. | Writing through `&(*r).x` does not update the root. |
| CR13 | P1 | [image/mod.rs](src/image/mod.rs#L1222) | Image length queries overstate a derived Pack frame with shared types. | A 15-byte frame advertises 23 bytes and consumes the next value. |
| CR14 | P1 | [typ/tval.rs](src/typ/tval.rs#L94) | Typed rendering exhausts the stack and repeatedly validates subtrees. | Typed-print subprocess aborts after successful `is_a`. |
| CR15 | P2 | [node/coretraits.rs](src/node/coretraits.rs#L334) | Reentrant dispatch hides its cache and drops inner sites without deletion. | Profiler: a new inner instance on every repeated render; lifecycle inspection. |
| CR16 | P2 | [node/coretraits.rs](src/node/coretraits.rs#L402) | Hook lookup allocates abstract wrappers even for a negative lookup. | Allocation path inspection. |
| CR17 | P2 | [expr/resolver.rs](src/expr/resolver.rs#L775) | Whole-subtree prescans make nested module resolution quadratic. | Traversal inspection. |
| CR18 | P2 | [expr/seq.rs](src/expr/seq.rs#L127) | Seq lowering uses fresh allocations for scratch collections. | Collection lifetimes and helper signatures inspected. |
| CR19 | P2 | [node/mod.rs](src/node/mod.rs#L964) | Image codecs copy slices into temporary Vecs for both passes. | Borrowed slice codecs exist; CallSite has the same pattern. |
| CR20 | P2 | [fusion/emit_helpers.rs](src/fusion/emit_helpers.rs#L230) | JIT composite builders allocate a Box around every pooled scratch handle. | One allocation per builder, including each struct field. |
| CR21 | P2 | [fusion/emit/abi.rs](src/fusion/emit/abi.rs#L81), [tval.rs](src/tval.rs#L72) | Tag documentation and an unused helper still assume taint implies stale. | Fresh bottom contradicts the ABI invariant; `with_taint_of` drops firing despite its contract. |

## Reproductions

Graphix examples were run with a freshly built shell:

```sh
~/tmp/target/debug/graphix --no-netidx --no-init --no-cache /tmp/probe.gx
~/tmp/target/debug/graphix --no-netidx --no-init --no-cache --no-fusion /tmp/probe.gx
```

Append this to runtime examples to exit after pending events settle:

```gx
sys::exit(sys::time::after_idle(duration:0.01s, 0))
```

### CR01: incorrect abstract narrowing

```gx
type Box<'a> = Abstract<'a>;
let x: [Box<i64>, Box<string>] = Box("hi");
println(select x {
    Box<i64> as b => `Number(b.0),
    Box<string> as b => `Text(b.0)
});
```

Expected the Text arm. Fusion prints `` `Number(0)``; node-walk prints
`["Number", "hi"]` and reports a mismatch against the inferred result type.
Match the concrete parameters as well as the nominal id, without mutating
inference cells during the runtime predicate.

### CR02: asymmetric equality

```gx
type Box<'a> = Abstract<'a>;
impl Eq for Box<i64> { let eq = |a, b| true };
let a: [Box<i64>, Box<string>] = Box(1);
let b: [Box<i64>, Box<string>] = Box("hi");
println((a == b, b == a));
```

Both engines print `(true, false)`. The first direction invokes the i64-only
implementation with a string-valued second argument; the other falls back to
structural comparison. Fixing CR01 alone will not fix this dispatch. Ord uses
the same one-sided resolution.

### CR03: bottom lost by tail optimization

```gx
let rec f = |n: i64, x: i64| -> i64 select n {
    0 => x,
    _ => f(n - 1, select n { 2 => null$, _ => x })
};
println(f(3, 7));
```

Both engines print `7`. Adding `+ 0` after the recursive call makes it non-tail
and correctly produces no output. The interpreter maps bottom to `None`, then
retains the previous formal; the JIT retains its previous loop value on a
tainted replacement too. Bottom and absence of an argument need separate
representations, with the current production's tag carried through rebinding.

### CR04: effects depend on an unrelated call

```gx
let apply = |f: fn(x: i64) -> i64, x: i64| f(x);
let p = apply(|x| x + 1, 1);
#[sync]
let delayed = |x: i64| apply(|x| sys::time::after_idle(duration:0.001s, x), x);
println((p, delayed(3)));
```

Both engines accept this and print `(2, 3)`. Remove `p` and print only
`delayed(3)`: compilation correctly rejects `#[sync]`. `infer_effects` keeps
one representative body per LambdaId even though callback resolution varies
by instance. These facts also control stateless tail-loop eligibility, so
fix analysis at the instance level and combine facts for definition summaries.

### CR05: import cycle abort

Put `mod b` in `a.gx` and `mod a` in `b.gx`, in the same directory. Checking
`a.gx` aborts with `thread 'tokio-rt-worker' has overflowed its stack`.
The reproduction ran with core dumps disabled. Detect cycles on the current
source-load chain; a global visited set would incorrectly prohibit independent
sibling imports of the same source.

### CR06: the formatter breaks import dependencies

Create `z.gx` containing `mod x`, and `z/x.gx` containing `let v = 1`.

```gx
mod z;
use z::x as a;
use a::v;
println(v);
```

This prints `1`. `graphix fmt --stdout` succeeds but moves `use a::v` before
its alias definition; the result fails with `use: no module a in scope`.
Normalizing both sides of the round-trip comparison masks this change.
Import grouping must respect dependency order and shadowing.

### CR07: relative configuration discovery

Create a project with `graphixfmt.json` containing `{"width":37}`, then run
a subprocess from a child directory. `FormatConfig::discover(Path::new("."))`
returns 90 instead of 37. An absolute starting path reaches the project config.
Keep this regression in a subprocess so parallel tests do not change the
process-wide current directory.

### CR08–CR12: references into containers

Run each separately; both engines exhibit every problem.

CR08 prints `key` twice before `10`:

```gx
let a = [10, 20];
let r = &a[{ println("key"); 0 }];
println(*r);
```

CR09 prints `10` after the key becomes bottom:

```gx
let a = [10, 20];
let k: [i64, null] = 0;
k <- null;
let r = &a[k$];
println(sys::time::timer(duration:0.002s, false) ~ *r);
```

CR10 prints `10` after the element is removed:

```gx
let a = [10, 20];
a <- [];
let r = &a[0];
println(sys::time::timer(duration:0.002s, false) ~ *r);
```

CR11 rejects a valid field reference with a mismatch against `&[]`:

```gx
let a = {x: error(`E)};
let r = &a.x;
let expected: &Error<`E> = r;
println(*expected);
```

CR12 prints `10` instead of `20`:

```gx
let a = {p: {x: 10}};
let r: &{x: i64} = &a.p;
let s = &(*r).x;
*s <- 20;
println(sys::time::timer(duration:0.002s, false) ~ a.p.x);
```

These findings point toward one compiled place description: evaluate the
address once, derive its type from the container, compose paths through
transparent syntax, and represent invalid addresses/targets explicitly.
The duplicate child graph and independently registered path make those
invariants difficult to maintain.

### CR13: image frame consumes the following value

This temporary integration probe failed at the final u64 decode with
`BufferShort`. Measurement and actual writing were both 15 bytes; the frame
header was 23. Appending a value is essential: decoding only the frame can
hide the error because the length-wrapped decoder clamps to available input.

```rust
use graphix_compiler::{
    expr::parser::parse_type,
    image::{DecodeImage, EncodeImage, ImageBuf, ImageDecoder, ImageEncoder},
    typ::Type,
};
use netidx_core::pack::Pack;

#[derive(Debug, netidx_derive::Pack)]
struct Pair {
    a: Type,
    b: Type,
}

let pair = Pair {
    a: parse_type("Array<i64>").unwrap(),
    b: parse_type("i64").unwrap(),
};
let mut enc = ImageEncoder::new();
let measured = EncodeImage::with(&mut enc, || pair.encoded_len());
enc.begin_encode();
let bytes = EncodeImage::with(&mut enc, || {
    let mut buf = ImageBuf::with_capacity(measured);
    pair.encode(&mut buf).unwrap();
    assert_eq!(buf.len(), measured);
    12345_u64.encode(&mut buf).unwrap();
    buf.freeze()
});
let mut dec = ImageDecoder::new(enc.counts());
dec.set_image(bytes.clone());
dec.set_offsets(enc.take_offsets());
DecodeImage::with(&mut dec, || {
    let mut input = &bytes[..];
    let decoded = Pair::decode(&mut input).unwrap();
    assert_eq!(decoded.a, pair.a);
    assert_eq!(decoded.b, pair.b);
    assert_eq!(u64::decode(&mut input).unwrap(), 12345);
});
```

Length queries must simulate sharing throughout the enclosing frame, including
descendants of cached definitions and shared objects across sibling fields.
A cached definition size is not independent of the current encode state.

### CR14: typed formatting aborts

In a child test process, build 1,000 nested `Type::Array(Arc::new(typ))` layers
around i64, and corresponding nested single-element Value arrays around 0.
With `Env::default()`, `typ.is_a(&env, &value)` succeeds. Writing
`TVal { env: &env, typ: &typ, v: &value }` into an `LPooled<String>` then
aborts the standard test thread with stack overflow before formatting returns.
This isolates rendering from Graphix evaluation and deep drops. Core dumps
were disabled. The existing deep-print test covers only `fmt_naked`.

Use explicit traversal state or stack growth. Also avoid revalidating each
entire remaining subtree at every level; stack growth alone leaves quadratic
work in `is_a_with`.

### CR15: reentrant hook pooling

```gx
type Box<'a> = Abstract<'a>;
impl<'a> Display for Box<'a> { let fmt = |a| "<[a.0]>" };
let x = Box(Box(1));
println(x);
println(sys::time::timer(duration:0.002s, 2) ~ x);
```

With `GRAPHIX_PROFILE=1 GRAPHIX_PROFILE_INSTANCES=1`, three renders create
four runtime Display instances: outer and inner initially, then a new inner
instance on each later render. Removing the whole `(trait, AbstractId)` entry
hides the cache from the nested call. The outer reinsertion drops the inner
entry without `site.delete(ctx)`, leaving registered resources behind.
Keep the registry present while loaning individual sites and preserve nested
returns to the pool.

## Scope and validation

Reviewed parser/AST transformations, module and interface loading, formatter
validation, type operations, environment and trait resolution, node evaluation
and lifecycle, function analysis, fusion admission/emission, runtime helper
ABI, and image serialization. Used source tracing, existing tests, both-engine
execution, targeted subprocess failures, and a runtime compilation profile.
This is not a proof that every path is correct. No Miri, sanitizer, long fuzzer
campaign, or slow-test release gate was run. Allocation and traversal findings
are based on code paths, not benchmarks.

- Baseline `cargo test -p graphix-compiler -p graphix-tests`: **2,980 passed,
  two slow tests ignored**.
- Rebuilt `graphix-shell` before executing the examples.
- Three temporary Rust regression probes failed as described for CR07,
  CR13, and CR14; the temporary test file was removed after recording evidence.
- Whole-workspace `cargo test`: **3,417 passed, 12 ignored**, no failures.
  The ignored total includes slow tests and documentation examples.
- `cargo fmt --all --check` and `git diff --check`: passed.

The older Claude CR at `fusion/emit/scalar.rs` is already satisfied by the
signed `sextend` arms. It is not counted as an additional defect here.

## Disposition

Every finding is addressed in this commit; each `CR` marker is now an
`XCR` beside the change. Pins are named by their test.

| ID | Resolution | Pin |
| --- | --- | --- |
| CR01 | The runtime predicate matches the value's instantiation: each declared parameter must contain the constructed one, probed without binding (`contains_with_flags(empty)`). | `lang::traits::abstract_test_matches_parameters` |
| CR02 | A dispatch takes a site only when every operand was constructed at the instantiation the implementation resolved for; a mixed pair is structural, in both directions. | `lang::traits::core_eq_mixed_instantiations_are_structural` |
| CR03 | The tail rebind carries every argument's tag: a bottom bottoms the formal (`None` is only an argument that never produced). The JIT's rebind always replaces the slot; a bottom's placeholder is an owned empty payload and is carried and dropped like a value, so the keep/replace branching is gone. | `lang::functions::tail_rebind_carries_bottom` |
| CR04 | Effects are inferred per instance (`collect_resolved_sites` walks every instance; `infer_effects` keys bodies by instance id; a static target's facts are its instance's). A definition's stored facts are the join over every instance ever analyzed and never improve; the tail-loop collapse is gated by the instance's own facts. | `lang::attributes::sync_on_async_instance`, `tail_recursive_stateful_instance`, `tail_recursive_pure_instances` |
| CR05 | A `LoadChain` of sources on the current load path is carried through resolution; a module whose source is already on it is an `import cycle: a -> b -> a` error. Sibling branches loading one source are unaffected. | `lang::modules::import_cycle_is_an_error`, `sibling_imports_of_one_source` |
| CR06 | A use statement that reads a name an earlier statement of the run bound (or binds a name again, or is a glob of another root) closes the run; statements reorder only within a run. The corpus harness passes over both repos. | `expr::format::tests::uses_merge_at_every_level` (two new cases) |
| CR07 | `discover` walks the ancestors of `std::path::absolute(dir)`. | `expr::format::tests::a_relative_directory_finds_the_project_config` (child process) |
| CR08 | `ByRef` holds one `Referent`: a `Channel` node or a `Place`; a place is compiled once, and the cell mirrors the element read through the same address (`read_path` on the root's production). | `lang::byref::place_key_evaluated_once` |
| CR09 | An undetermined address clears the registered place and the reference delivers bottom (a fresh-bottom key moves the reference); `ConnectDeref` drops its target when the delivered reference resolves to none. | `lang::byref::place_bottom_key` |
| CR10 | A read through a place that does not exist is bottom, tagged by the delivery that found it so. | `lang::byref::place_removed_element` |
| CR11 | The referent type is derived from the container step by step (`Place::elem_type`: the access nodes' own rules, without the access's failure). | `lang::byref::place_error_field` |
| CR12 | Parentheses are transparent in the chain (`unparen`); a dereferenced place's path is composed under the steps (`root_place`). | `lang::byref::place_through_deref` |
| CR13 | In the encode pass a length query descends a definition's contents (its descendants count as met) and what is met stays met until the next write, so a frame's sibling fields see one another; `Slot::def_len` and the query depth are gone. | `image::tests::a_frame_measures_what_it_writes` (the review's probe) |
| CR14 | `TVal` checks the value against the type once, at the root; every level grows the stack as needed (`ensure_sufficient`); a union level walks only when two members admit the value's outer shape (`member_of`). | `typ::tval::test::deep_typed_value_prints` (1,000 nested arrays; a 20,000-deep recursive union under a time bound) |
| CR15 | The registry entry stays in place; a dispatch loans one site out of its slot's pool (`take_site`/`return_site`) and a nested dispatch takes another; a site returns to its slot or is deleted when the entry was rebuilt meanwhile. `Box(Box(Box(1)))` builds two instances, then none. | `lang::traits::core_display_nested_same_tag` |
| CR16 | Resolution reads the borrowed `GxAbstract` (`typ()` clones an `Arc`); only a dispatch that found an implementation wraps its arguments. | covered by the trait suite |
| CR17 | One walk: a node learns from its children whether anything under it changed (`Option<Expr>`), so an unchanged subtree is neither rescanned nor rebuilt; the prescan is deleted. | existing module pins |
| CR18 | The lowering's scratch (`steps`, `visible`, `prelude`, `labels`, `arms`, `body`, `vals`, the queued path's `captures`/`args`) is pooled or inline; `block`/`select` take iterators and are fed by drains. | existing seq pins |
| CR19 | `Block`, `Module` and `CallSite` (a bool tag before the order) use the borrowed-slice codecs. | existing image pins |
| CR20 | Value builders and string builders come from a per-thread spare list of boxes (`Spares`, up to 64), reused across evaluations. | existing fusion pins |
| CR21 | `with_taint_of` is deleted; the `tval` module doc and the ABI's `TAINT`/`STALE` docs describe the two independent bits and their joins. | — |

Noted, not part of this review: a definition with two call sites of a
recursive lambda whose arm is `null$` de-fuses ("select arm type TVar …
doesn't freeze concrete"); it predates these changes and is a fusion
cliff, not a semantics gap.
