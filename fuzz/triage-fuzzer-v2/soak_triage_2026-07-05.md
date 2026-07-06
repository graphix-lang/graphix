# soak-jul05 triage

Campaigns: fuzz/generate/reactive, seeds 70501-3, PAR=16, corpus under
fuzz/soak-jul05/. Launched ~16:15 on the all-bugs-fixed build
(post never-as-⊥). Startup gates 65/65 ×3.

1. **FIXED (2026-07-06) — value-shape DynCall results never went
   STALE** — corpus-fuzz/divergence_000000 (~16:59). Pinned at
   findings/dyncall-valueshape-firing-jul2026/01. jit ExtraFired a
   const `buffer::len(buffer::from_string("hello"))` whenever any
   feeder woke the kernel (probe j3: an infinite counter → fired
   every cycle; interp: once). Root cause exactly as suspected: the
   in-band 2-word return disc from graphix_dyncall is a REAL Value
   disc — no STALE bit — and `emit_dyncall_node`'s value-shape arm
   never folded the args' firing in (the scalar arm always did, via
   `propagate_flags`). FIX: all three non-scalar return arms
   (String / Composite / value-shape) now run `propagate_flags`
   over `arg_taint_discs`, same as the scalar arm — the String and
   Composite const discs had the identical always-fired hole,
   caught by inspection during this fix. Regress pins 01+02 both
   green; item-25's `x <- x` wedge now agrees (both Timeout).

2. **ACCEPTED CLASS (no action)** — corpus-fuzz/crash_000001:
   `array::group(seq(0, i64::MAX), ...)` — the seq-runaway hang,
   Eric's 07-04 ruling. One entry expected per fresh corpus; the
   digit-normalized crash dedup (jul04 harness batch) collapses
   further literal variants into it.

3. **OPEN (runtime/oracle-soundness, fix when campaigns stop) —
   quiescence detection races the deref-echo loop** — corpus-fuzz/
   divergence_000002 (~17:2x):

   ```graphix
   {let f = |x: &i64| *x <- *x * i64:5; let v = i64:41; f(&v);
    array::group(v, |n, _| i64:1 == i64:2)}
   ```

   Semantically an infinite self-multiplying counter through a ref —
   the plain analog (`v <- v * 5`, probe j4) stably Timeouts in BOTH
   modes (correct: infinite counters tick forever). The ref-mediated
   version flips verdicts PER RUN in BOTH modes independently
   (6 samples: 3× both-Timeout, 2× jit-Trace([]), 1× interp-
   Trace([])). Both evaluators acquitted; the shared suspect is
   `trace_wait_idle`'s idle predicate sampling a moment where the
   echo's next var write is in flight through the rt channel (queued,
   not yet a pending cycle) — the deref hop exposes an idle-looking
   gap the direct connect doesn't have. Consequences: (a) the
   recorded divergence's interp/jit assignment is meaningless — a
   coin flip; (b) this is a SELFCHECK-class oracle bug (same mode,
   same program, different verdicts). Fix direction: strengthen the
   runtime idle predicate to count queued-but-undelivered var
   updates (the ToGX/rt channel) as not-idle, or have the tracer
   drain one more cycle after apparent idleness before resolving.
   Also worth re-running selfcheck with a deref-echo seed once fixed.

4. **FIXED with item 1 (same root)** — corpus-fuzz/
   divergence_000003 (~17:4x): `s <- cast<i64>(true)$` SELF-WAKE
   WEDGED the jit (Timeout vs interp's clean quiesce). cast<T>
   returns the 2-word nullable shape via DynCall; the never-STALE
   in-band disc made the consume-always connect write on every
   invocation, and the write woke its own kernel. Pinned as
   findings/dyncall-valueshape-firing-jul2026/02 — green after the
   item-1 fix.

5. **FIXED (2026-07-06) — REAL CRASH (SIGABRT/core dump) —
   find_map/filter_map with a COMPOSITE-returning callback** —
   corpus-fuzz/crash_000004. Minimal (probe j11):

   ```graphix
   {let a = [i64:1, i64:2]; array::find_map(a, |x: i64| a)}
   ```

   ROOT CAUSE (not the direct-path scaffold — its gates were sound
   and correctly de-fused these): the PER-SLOT TEMPLATE kernel
   (`build_lambda_kernel` via `fuse_callsite`). The callback's
   declared rtype `['b, null]` freezes to `AbiKind::Nullable` — the
   in-band 2-word `(disc, payload)` Value return — but a composite
   body emits the COMPOSITE convention (`payload = *mut ValArray`
   box pointer). `emit_kernel_return` passed the raw pair through
   and the runtime's `TagValue::from_raw` decoded the box pointer
   as the in-band ValArray word — allocator SIGABRT / misaligned
   ThinArc deref at trace render. FIX (better than the de-fuse gate
   originally proposed): `emit_return_from_node` — when the kernel's
   return type is value-shape, the body widens through
   `emit_owned_value_operand_node` (the same conversion select arms
   and value operands already used: composites via
   `graphix_value_new_from_array`, strings via
   `graphix_value_new_string`, scalars inline-packed) — so these
   callbacks now FUSE AND RETURN CORRECTLY instead of crashing.
   Covers both `NodeBodyEmitter::emit` (plain bodies) and
   `emit_body_tail` (tail-select arms). Pinned at
   findings/hof-composite-return-jul2026/01-03; run! fixtures
   find_map_captured_array / filter_map_fresh_array /
   find_map_tuple_arm (lib_tests/array.rs) + list_find_map_tuple
   (lib_tests/list.rs). GRAPHIX_DBG_KERNELS=1 (new, permanent)
   prints each built kernel's name + frozen return kind — the tool
   that located this.

6. **ACCEPTED CLASS (item 2 family)** — corpus-fuzz/crash_000005:
   `array::group(seq(MIN, 4), |n,_| n <= 1)` — seq-runaway with a
   structurally different predicate (`<=` vs `==`), so the digit-
   normalized dedup correctly didn't collapse it. Family entries are
   now bounded by operator vocabulary, not literals — a handful per
   soak at worst.

7. **ACCEPTED CLASS (item 2 family) + dedup refinement note** —
   corpus-fuzz/crash_000006: differs from item 2's entry ONLY by the
   `-` on the seq bound — the digit normalization doesn't fold signs,
   so `-N` and `N` key differently. Harness backlog: include an
   optional leading `-` in the digit-run collapse (crash_key), which
   caps this family at one entry per predicate operator.

8. **ACCEPTED CLASS (item 2 family)** — corpus-fuzz/crash_000007:
   seq-runaway, predicate `n > 0`. Third operator variant; as
   predicted the family growth is one-per-operator. The item-7 sign
   fix plus (if it keeps annoying) an operator-insensitive key for
   the group(seq(...)) shape would zero it out.

9. **FIXED (2026-07-06) — FoldQ's empty-array arm re-emitted init
   every cycle (node-walk over-fire)** — corpus-fuzz/
   divergence_000008 (~18:2x). Pinned at
   findings/foldq-empty-overfire-jul2026/01 (probe j14: seq driver →
   interp 6 emits, jit 1). FoldQ::update returned `self.init.clone()`
   unconditionally when nodes.is_empty() && arr_present. FIX exactly
   as diagnosed: a `fired` flag set when the array (either Some arm)
   or the init input fires this cycle gates the empty arm — the
   kernel's zero-iteration loop fires iff src∨init fired, and the
   node-walk now agrees (the non-empty arm always had the discipline
   for free: `inits[i]` resets on a non-firing slot). Same
   fix-the-node-walk verdict as #8; fifth real bug of the round.
   With this the pin corpus is FULLY GREEN — 76/76, the first
   all-green regress of the soak-jul05 fix round. Probes r1-r3
   (clocked empty fold, plain fold, static empty fold) AGREE.

10. **FIXED with item 5 (same root)** — corpus-fuzz/crash_000009:
    `array::filter_map(a, |x| [x, x + i64:1])` — composite-returning
    callback via a FRESH array literal (vs item 5's captured array).
    Pinned at findings/hof-composite-return-jul2026/02; fixture
    filter_map_fresh_array.

11. **FIXED (2026-07-05) — recursive lambda rtype annotation NOT
    enforced against its body; JIT leaks a pointer** — corpus-fuzz/
    divergence_000010. Pinned at
    findings/rec-rtype-unchecked-jul2026/01. `|n, acc| -> i64 select
    n {0 => acc, _ => error(i64:0)}` compiled; fusion built a
    scalar-i64 return slot, an Error Value flowed out, scalar kernel
    leaked the payload ptr. ROOT CAUSE (deeper than the triage
    hypothesis — the def-time check was NEVER swallowed): `TVar::eq`
    calls two DISTINCT unbound cells equal (None == None), and
    `Type::union_int`'s (TVar, TVar) arm collapsed on bare `==` — so
    the select's arm union DROPPED the error arm's not-yet-bound
    rtype cell, and the def-time rtype check bound the lone surviving
    tvar to i64 vacuously. By the time `error(i64:0)` resolved to
    `Error<i64>`, the body type no longer referenced its cell. The
    non-rec twin only "rejected" when arm 0 was concrete; with an
    unannotated second formal BOTH twins compiled. FIX: union_int
    collapses two tvars only same-cell or bound-equal
    (typ/setops.rs). Both twins now reject at def time. run!
    fixtures: rtype_rejects_error_arm, rec_rtype_rejects_error_arm,
    arm_union_keeps_both_tvars (positive control) in
    lang/functions.rs.

12. **FIXED (2026-07-06) — REAL CRASH (SIGABRT/stack overflow) —
    variant-return non-tail recursive lambda overflowed the JIT** —
    corpus-fuzz/crash_000011. Minimal (probe j19):

    ```graphix
    {let rec f = |n| select i64:3 {i64:0 => `Varint(u64:127), _ => n + f(n - i64:1)}; f(i64:3)}
    ```

    The guard-emission hypothesis was WRONG — the crash channel was
    the recursion type-launder: `n + f(...)` where f can return a
    variant is ILL-TYPED, but the pre-knot orphan-cell widening let
    it compile with a laundered signature, and a kernel fused over
    the wrong ABI recursed natively past any guard. The program now
    REJECTS at compile in both modes (the #31 knot + honest unions).
    Fixing the reject's over-strict twin exposed a SECOND pre-existing
    bug: bare variant arms (`` `A `` — payload-empty) classified as
    select WILDCARDS (`is_refutable` is payload-only; the tag test
    lives in the type predicate), so (a) all-bare-variant selects
    BYPASSED exhaustiveness — `select x: [`A,`B] { `A => .. }`
    compiled with a missing tag (a latent soundness hole, probe p7) —
    and (b) with the knot's OPEN scrutinee cell, the first arm's
    narrowing walk greedily bound it to `A alone (spurious dead-arm
    reject of well-typed variant recursion, probe p2). FIX:
    `StructPatternNode::matches_anything` (bind-all test, distinct
    from `is_refutable` whose payload-only contract refutable-`let`
    depends on) now drives the select wildcard classification;
    variant arms join the coverage unions. Well-typed variant-return
    non-tail recursion infers its honest union, DE-FUSES (no native
    path — nothing to guard), and bottoms at the call-depth guard in
    both modes (depth-1M probes p2/p8 agree empty-trace). ALSO
    permanent: GRAPHIX_DBG_BIND=1 traces every tvar bind (the tool
    that found the greedy narrowing). Fixtures
    rec_variant_union_infers + select_variant_nonexhaustive
    (lang/functions.rs); pin variant-rec-depth-jul2026/01.

13. **FIXED with item 5 (same root)** — corpus-fuzz/crash_000012:
    `list::find_map(l, |x| (i64:1, i64:2))` — tuple return, SIGSEGV
    (probe j22, rc=139). The list package has no direct-path
    scaffold, so its callbacks ALWAYS go through the per-slot
    template — the same value-shape-return path as item 5; one fix
    covers array/list/map because the bug was in the shared kernel
    return emission, not the per-package scaffolds (map probe k8
    verified). Pinned at findings/hof-composite-return-jul2026/03;
    fixture list_find_map_tuple.

14. **ACCEPTED CLASS (item 2 family)** — corpus-fuzz/crash_000013:
    seq-runaway, predicate `n != i64:-1`. Fifth operator variant.

15. **FIXED (2026-07-06) — REAL CRASH (SIGABRT) — null ArcStr reached
    graphix_arcstr_drop** — corpus-fuzz/crash_000014. The pending-
    sentinel hypothesis was WRONG (the DynCall pending paths were
    sound); reduced to a timer shape (findings/qop-nested-union-
    jul2026/01) and pinned by CLIF inspection (GRAPHIX_DUMP_CLIF +
    the new GRAPHIX_DBG_INVOKE kernel-invocation trace): the `$`
    node's STATIC type strips EVERY error member of the flattened
    inner union (`[[string, Error<E>], Error<AIE>]$` -> string — the
    node-walk drops ANY error value), but `emit_qop_node` arm-
    selected on the inner's ONE-LAYER option success ([string,
    Error<E>] — value-shape) and minted the value-shape (Null, 0)
    placeholder on the error path. The String-conventioned kernel
    return then force-dropped payload 0 on the not-fired init cycle —
    graphix_arcstr_drop(NULL) abort. FIX: the qop emit arm-selects on
    the node's own frozen static type (result_typ threaded from the
    Qop/OrNever nodes); the passthrough gate stays on the inner. All
    flavors verified (string/scalar/composite nested unions, plain
    OOB `$`, handler-ful `?` — probes q7-q13 AGREE; the original fs
    crasher AGREEs). Eighth real bug of the round, fourth crash.

16. **DUPLICATE of item 9 (FIXED with it) — and NEVER-AS-⊥ EXONERATED**
    — corpus-fuzz/divergence_000015. The window + never-gate +
    count-gate all reduce away: j29 (plain array::push accumulator
    fold, NO never() anywhere) diverges identically — jit misses the
    epoch-1 fire where the empty fold re-emits its init 0.; j30
    (window + array::len, no fold) AGREES. Same FoldQ empty-arm
    over-fire as item 9, interp side. Confirms (a) the never-as-⊥
    commit is CLEAN — the fold firing bug is orthogonal and predates
    it, and (b) item 9's blast radius includes every windowed/gated
    reactive fold, not just the minimal repro. Second regression
    case for the item-9 fix.

17. **FIXED (2026-07-05) — REAL CRASH (SIGABRT) — #21 guard HOLE: rec
    self-call arg mismatch when the FORMAL resolved to the
    recursion's type** — corpus-fuzz/crash_000016. probe j31:
    `|n, acc| select n {0 => acc, _ => sum_to(buffer::to_string(...),
    acc + n)}; sum_to(i64:100, i64:0)`. The self-call passed a String
    for n; interp survived (type-tolerant: acc + "hello" logs an
    arith error, bottoms → Trace([])); the JIT aborted (the marshal
    tried to PARSE "hello" into the i64 slot — "Unexpected h"). My #21
    self_calls_abi_consistent guard checks self-call args vs formals,
    but the FORMAL n resolved to the recursion's type (self-call arg
    == formal, guard passes) and it was the ENTRY call's i64:100 that
    mismatched — a hole the guard structurally can't see.

    FIX (with item 11's union fix as prerequisite): MONOMORPHIC
    RECURSION — the tc0 knot. During the def-time faux body check
    (`Lambda::typecheck0`), a self-call site's `CallSite::typecheck0`
    now unifies against the def's OWN ftype cells instead of a
    `reset_tvars` freshening (`ExecCtx::rec_defs` tracks in-progress
    def checks; the callee ftype's `lambda_ids` identify the self-
    call). Effects: (a) the μ-equation collapses — `'r ⊇ [T, 'r]`
    binds `'r := T` through the same-cell rule, so an unannotated rec
    lambda's return type resolves concretely instead of orphaning a
    cell that `constrain_known` widened to Any (that widening also
    DE-FUSED lazy_no_annotations once the union fix kept the orphan
    alive — the knot restores its fusion); (b) a self-call arg that
    disagrees with the entry narrowing is a DEF-TIME compile error in
    both modes, exactly like the non-rec twin — j31/j48 now reject.
    Pinned at findings/rec-selfcall-argtype-jul2026/01; run! fixture
    rec_selfcall_arg_mismatch in lang/functions.rs. NOTE the language-
    semantics consequence: `let rec` is now monomorphic-recursive (a
    self-call cannot instantiate the lambda at a different type) —
    the ML-family standard; the prior "poly" admission was unsound
    (it widened the signature to Any and crashed the JIT).

18. **FIXED (2026-07-06) — union struct field with a DynCall-produced
    arm leaked the string under the other arm's ABI** — corpus-fuzz/
    divergence_000017. Pinned at findings/union-struct-field-jul2026/
    01. `{y: [i64, string]}` from two select arms (cast<i64>$ vs
    buffer::to_string); jit marshalled the taken string arm as i64 →
    pointer leak. ROOT: the item-11 disease ONE LEVEL UP —
    `union_int`'s composite arms decided collapse via derived
    `Type::eq`, which recurses into `TVar::eq`'s None == None, so
    `{y: 'a} ∪ {y: 'b}` collapsed to ONE struct while both DynCall
    rtype cells were still unresolved; the string arm's type vanished
    and the narrow (wrongly-i64) field froze and fused. Literal arms
    bind at compile → structs differ → honest Set → de-fuse — which
    is why only the DynCall shape leaked. FIX: `union_identical`
    (typ/setops.rs) — Type::eq except two unbound tvars are identical
    only same-cell — now decides every union_int collapse
    (Array/Map/Struct/Tuple/ByRef/Abstract + the item-11 TVar arm
    refactored onto it). The honest `[{y:i64}, {y:string}]` union
    doesn't freeze → the region de-fuses (n1/n3 AGREE); the SAME-type
    two-DynCall control still fuses (normalize's deref-merge collapses
    bound-equal members — n2 fused=2). TENTH real bug of the round.

19. **DUPLICATE of item 18 (FIXED with it; probe n3 AGREES)** — corpus-fuzz/
    divergence_000018: identical `{y: [i64, string]}` union field,
    arm 1 cast<i64>$, arm 2 a string — only the string PRODUCER
    differs (nested `select {0 => "zero", n => "other"}` vs item 18's
    buffer::to_string). jit leaks the string as i64. Confirms the
    trigger is the DynCall (cast$) arm + any string arm, independent
    of the string's producer. Second regression case for item 18.

20. **DUPLICATE of item 3 (no new action)** — corpus-fuzz/
    divergence_000019: `f = |x: &i64| *x <- *x + i64:1; ...;
    array::group(v, ...)` — the ref-echo quiescence race again (`+1`
    vs item 3's `*5`), this run landing interp-Trace([])/jit-Timeout
    (one of the coin-flip outcomes). Same oracle-soundness hole; the
    idle-predicate fix (item 3) covers it. NOTE: this shape will keep
    recurring until item 3 is fixed — it's a corpus magnet. Consider,
    at fix time, an oracle-side skip for ref-write-echo programs
    (like the rand/sys/http divergence exclusion) as a stopgap.

21. **ACCEPTED CLASS (performance asymmetry, no compiler action) +
    two harness notes** — corpus-fuzz/divergence_000020:
    `array::init(i64:500000, |i| i64:0)`. interp empty, jit the full
    500k array. NOT a correctness bug: the interp is O(n) in per-slot
    node-graph instantiation (~1000x the JIT native loop, per the
    bench corpus), so 500k slots exceed the trace oracle's deadline
    and it records empty; at 200k the interp completes and AGREES
    (probe j38). The interp would produce the identical array given
    time. Same finite-perf-asymmetry as the seq-runaway family.
    HARNESS NOTES: (a) the generator should cap array::init/seq/
    window sizes (a large-literal→small-literal remap in the size
    position, or a hard cap) — same fix shape as the seq-span cap;
    (b) this finding SERIALIZED A 5.2MB TRACE into the corpus (the
    full 500k array in the comment header) — record() should cap
    the interp/jit trace repr length in the finding file (truncate
    with an elision marker) so a big-array finding doesn't bloat the
    corpus / OOM a reader.

22. **ACCEPTED CLASS (item 2 family)** — corpus-fuzz/crash_000021:
    seq-runaway, predicate `n < i64:4`. Sixth comparison-operator
    variant (== <= > != <, and one signed bound). The family is now
    demonstrably operator-enumerating and near-exhausted; the item-7
    sign-fold + an operator-normalized key would zero it.

23. **ACCEPTED CLASS (item 2 family)** — corpus-fuzz/crash_000022:
    seq-runaway; the predicate is dressed with a div0/select block but
    the outer shape is the same `array::group(seq(MIN, 4), ...)` hang.
    The predicate body is a red herring (the group never closes because
    seq floods). Different-enough text to dodge the digit dedup; an
    operator/predicate-insensitive key for the group(seq(...)) shape
    (harness backlog) is the real cure.

24. **DUPLICATE of item 3 (no new action) + strengthens its
    diagnosis** — corpus-fuzz/divergence_000023:
    `{let f = |x: &i64| *x <- *x + i64:1; let v = i64:41; f(&v);
    "_test"}`. The const "_test" result makes the flip visible in a
    SINGLE mode: 6 interp-only runs gave 3× Timeout, 2× Trace([_test])
    (1 outer-timeout). Same-mode non-determinism = the item-3
    quiescence race, now proven selfcheck-class (not just interp-vs-
    jit). Mechanism confirmed: trace_wait_idle races the deref-echo
    counter's queued-but-undelivered write — sometimes an idle-looking
    gap (declares quiescence, fires the const), sometimes mid-tick
    (keeps waiting, Timeout). The item-3 idle-predicate fix
    (count queued rt-channel var writes as not-idle) covers this.
    NOTE: this shape should be added to the `selfcheck` corpus once
    fixed — it's a ready-made same-mode-determinism regression test.

25. **FIXED with item 1 (same root; verified both-Timeout agree
    post-fix) + infinite-reschedule wrinkle** —
    corpus-fuzz/divergence_000024:
    `{let x = i64:0; x <- x; buffer::from_string("hello stderr\n")}`.
    buffer::from_string returns Bytes (value-shape 2-word DynCall);
    its disc never goes STALE (item 1), so the jit re-emits the const
    bytes every cycle the kernel wakes. Here the waker is `x <- x` —
    an infinite no-op self-reschedule (schedules x:=x forever), so the
    interp wedges (Timeout) where the jit streams the const. Two
    things tangled: the item-1 value-shape ExtraFire (the real fix
    target) and `x <- x` as an infinite reschedule (accepted infinite-
    loop asymmetry; the interp Timeout is canonical). The item-1 fix
    (AND arg STALE into value-shape DynCall discs) removes the
    ExtraFire; the x<-x wedge then agrees (both infinite). Third
    regression case for item 1.

26. **ACCEPTED CLASS (item 2 family, OOM variant)** — corpus-fuzz/
    crash_000026: `{let m = {"outer" => seq(MIN,4)}; m{"outer"}}` —
    SIGKILL (signal 9) = the OOM killer; the ~2^63 seq allocated until
    killed. Same seq-runaway family, OOM manifestation (vs HANG). The
    generator size-cap (harness backlog) covers it.

27. **ACCEPTED CLASS (item 2 family)** — corpus-fuzz/crash_000027:
    seq-runaway, predicate `n == i64:-1`. Seventh operator variant.

28. **OPEN — real-bug CANDIDATE (needs ASAN, pathological-input-gated),
    investigate carefully when campaigns stop — buffer::encode Pad
    length overflow / probable buffer overrun** — corpus-fuzz/
    divergence_000025: `{let b = buffer::encode([`Pad(u64:MAX)]);
    f64:0.}`. The result is a LITERAL f64:0. yet interp reads it back
    as 5e-324 (smallest subnormal, bit 0x1) and jit emits empty. A
    literal flipping by one bit = adjacent-value corruption, i.e.
    buffer::encode wrote past a buffer whose length calc overflowed on
    the 2^64-1 Pad. DISTINCT from the seq family (that's iteration
    runaway; this is an ALLOCATION-SIZE overflow with memory
    corruption). Pad(4) AGREES (probe j40), so it's the huge-Pad path
    only. DO NOT reproduce the full 2^64 alloc casually (OOM risk);
    investigate under ASAN with a Pad near usize::MAX to find the
    unchecked length in buffer::encode's Pad handling. Likely a
    checked_mul/with_capacity fix + reject/clamp oversized Pad. Also
    generator-cappable (Pad size in the size-cap set). Eleventh real
    bug CANDIDATE of the round (memory-safety, low-priority: needs an
    absurd input, but the corruption is real).

29. **DUPLICATE of item 9 (FIXED with it)** — corpus-fuzz/
    divergence_000028: another windowed/never-gated/count-gated empty
    fold. Reduces (probe j42: `iter` clock + empty fold, NO never/
    window/gate) to the FoldQ empty-arm over-fire — interp re-emits
    init 0. every cycle, jit fires once. Third reactive-dressing
    regression case for item 9; the never-as-⊥ gate is again
    incidental (j42 has none). item 9's blast radius = any empty fold
    under a reactive clock.
