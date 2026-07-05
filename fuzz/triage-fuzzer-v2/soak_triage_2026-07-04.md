# Combined soak triage — 2026-07-04 overnight

Campaigns: fuzz/generate/reactive, seeds 70401-3, PAR=8 each, corpus
dirs under `fuzz/soak-jul04/`. Launched ~17:00 after the bug-queue
clear (all six commits: #20 phases A-E + callee-result flags). Startup
regression gates 57/57 on all three.

## Findings

1. **OPEN (root-caused read-only, fix after campaigns stop) —
   `Deref` per-slot clone wake-up mis-rooted** — corpus-fuzz/
   divergence_000000 (~17:30, minutes into the soak; reproduces 1/1
   uncontended):

   ```graphix
   array::map([(i64:0, i64:2), (i64:3, i64:4)], |(k, v)| {
     let v = i64:41; let r = &v; *r <- *r + i64:1;
     array::group(v, |n, _| n == i64:2)
   })
   ```

   interp: emits every cycle forever (slot-local counter runs — trace
   caps); jit: emits ONCE (cycle 1) then silence — an UNDER-fire.
   Mechanism: `Deref::update` registers its runtime wake-up LAZILY
   (`ctx.rt.ref_var(new_id, self.top_id)` when the ref value first
   arrives) and Deref has NO structural `clone_rebind` override — the
   per-slot HOF template clone takes the default recompile-from-spec
   path, whose top_id is the spec id, not the region root. The slot's
   `*r` read therefore subscribes the WRONG ExprId; the counter's
   writes (ConnectDeref — already fixed, item 20 of the 07-03 triage)
   never wake the clone. Deref is the missed third sibling of the
   Sample + ConnectDeref wake-up family: same fix shape — structural
   clone_rebind override (clone child via clone_rebind, `id: None` so
   the lazy ref_var re-registers under the clone's own update with
   `self.top_id`).

2. **ACCEPTED CLASS (no action) — seq(0, i64::MAX) runaway** —
   corpus-fuzz/crash_000001: `array::group(seq(0, MAX), ...)` child
   HANG on the outer deadline. Eric's 07-04 ruling: can't distinguish
   wedged from slow-but-progressing; live with it. The async seq
   producer floods cycles — not interruptible-by-poll the way pure
   tail loops are.

3. **ACCEPTED CLASS (infinite-tail artifact), with one lead worth a
   look** — corpus-fuzz/divergence_000002: a queuefn arg containing an
   infinite PURE tail recursion (`let rec f = |v| f(v + 1); f(0)`).
   interp: Timeout (wedged cycle); jit: interrupt aborts the native
   loop to bottom, program quiesces, trace `[0:8]`. This is the
   infinite-loop mode-asymmetry family Eric ruled "live with it"
   (07-04). Lead CHECKED and retracted: the interp tail loop DOES poll
   `ctx.interrupted()` per iteration (lambda.rs:259) — the interp
   wedge is elsewhere (queuefn `#count: &depth(=0)` + `#trigger:
   never()` + the infinite arg; or the one-shot interrupt being
   consumed by a different loop). Reproduces under soak load 2/2;
   re-run UNCONTENDED after campaigns stop before investigating —
   Timeout verdicts are load-sensitive (last soak's 000061 was a
   load flake).

4. **Same bucket as 3** — corpus-fuzz/divergence_000003: infinite
   tail rec (`let rec f = |v| f(v+1); f(0)`) under `array::group`;
   interp Timeout, jit quiesces (empty trace). PATTERN: two
   interp-Timeout-vs-jit-recovers findings in the first hour, both
   containing an infinite pure tail loop, both under 24-worker load.
   The 07-03 session verified the interrupt converts these wedges to
   fast AGREEs — suspect the 750ms post-interrupt grace is too short
   under full soak load (the interp child loses the race, the jit
   child wins). Morning job: re-run every interp-Timeout finding
   uncontended; if they all AGREE, consider a load-scaled grace or a
   policy bucket for Timeout-vs-quiesced so the corpus doesn't
   collect this shape all night.
5. **OPEN (root-caused read-only, fix after campaigns stop) — default
   `clone_rebind` recompile resolves captures BY NAME; sibling slot
   clones pollute the shared scope** — corpus-generate/
   divergence_000000 (~20:34; the generate campaign's first finding,
   and the generator's first hit on the audit-bug-3 shadowing family
   it was built to reach). Pinned + hand-minimized at
   `findings/rebind-recompile-jul2026/01` (probes p17–p31):

   ```graphix
   {let x = array::fold([f64:-1.], "hello", |v7, v6| v7);
    let v6: u8 = u8:100;
    array::fold([u8:1], u8:255, |a, b| (try ((v6 /? u8:1))? catch(e) => u8:0))}
   ```

   interp `u8:100`, jit `f64:-1.0` — the inner fold slot's ELEMENT.
   Mechanism (each piece probe-verified): (a) the inner fold has a
   string accumulator, so it does NOT batch-loop-inline — it takes
   per-slot template dispatch, and `remint_bind_id` registers the
   slot's fresh `v6` element bind BY NAME at the HOF's own scope
   (`bind_variable(&scope.lexical, ...)` — pattern.rs:58), shadowing
   `let v6: u8` for every lookup below the block (scalar-acc inner
   fold → batch-inlined → AGREE, p30). (b) The outer fold's callback
   contains try/catch, so its slot clone hits the DEFAULT
   `Update::clone_rebind` (recompile-from-spec, lib.rs:1002 — TryCatch
   is env-managing); the capture-alias pre-pass does
   `if lookup_bind(name).is_none() { alias }` — it finds the inner
   slot's `v6` and SKIPS the alias, so the recompiled try body
   resolves `v6` to the wrong binding (`$`/bare-capture callbacks take
   the structural spine, which remaps by BindId → AGREE, p19/p22/p27).
   Rename the inner param → AGREE (p18); binding order irrelevant
   (p25 — runtime registration, not lexical order). The lib.rs doc
   explicitly accepted this hazard ("env-managing nodes are rare
   inside per-slot templates") — retire that acceptance.

   Fix sketch: make the capture-alias pre-pass UNCONDITIONAL and alias
   into the FRESH hermetic `rbN` scope instead of the shared `scope` —
   the refs walk already knows each external ref's exact original
   BindId, so aliasing name → original id deepest-wins forces the
   recompile to resolve captures by recorded identity regardless of
   same-named env pollution (internal binds still shadow the alias as
   compile registers them). Also stops `alias_variable(&scope.lexical,
   ...)` itself writing into the shared scope — a second pollution
   vector. Caveat to check while fixing: an external name whose
   resolution differs across the subtree's sub-scopes (`use`
   re-pointing mid-subtree).
6. **HARNESS BUG (worked around live, fix in the morning) — `run_pool`
   worker bleed via `continue` in `tokio::select!`** — the fuzz
   campaign EXITED cleanly at ~20:43 after 78444 programs ("done: ...
   3 divergences, 1 crashes"), which `forever` mode must never do.
   Mechanism: the excluded-module paths (rand::/sys::/http:: HANG
   crashes, lib.rs:1135; rand::/sys::/http:: divergences,
   lib.rs:1163) use `continue`, which targets the enclosing `loop` —
   skipping the one-for-one pool refill at lib.rs:1196. Every excluded
   result permanently leaks a worker slot; after PAR=8 leaks the
   JoinSet drains and `else => break` ends the campaign. Only the
   fuzz campaign bleeds (its mutation seeds contain sys::/rand::
   fragments; the generator never emits those modules — the other two
   campaigns were at 210k+ programs unharmed). Fix: restructure so
   exclusion doesn't skip the refill (fold the module check into the
   match arms as a no-op instead of `continue`, or hoist the refill
   above the match). Worked around WITHOUT a rebuild: relaunched the
   fuzz campaign from the same binary at ~20:50, fresh seed 70404
   (70401 would deterministically replay the same 78k mutants), same
   corpus dir (4 findings reloaded, dedup intact). It will bleed
   again eventually; each relaunch buys ~3.5h.

7. **Load flake, finding-3/4 bucket (no action)** — the 70404
   relaunch's startup regression gate reported 1/57: audit-jul2026/03
   with interp=Trace([]) (empty), jit=the known-correct value. The
   gate fires all 57 checks concurrently on top of two live campaigns;
   the interp child lost the startup burst race. Re-checked 3/3 AGREE
   under normal load. More weight behind the morning re-run-
   uncontended policy for every interp-Timeout/empty-trace verdict.
8. **OPEN (root-caused read-only, fix after campaigns stop) — NODE-WALK
   over-fire: tail-loop forces `event.init` on passive re-polls** —
   corpus-fuzz/divergence_000004 (~20:55, the restarted campaign's
   first find). Original mutant queues three `queuefn` calls as
   statements before a fused tail recursion; interp fires the result
   once per drain cycle, jit once. Hand-reduced (p32–p45): queuefn is
   a red herring — ANY multi-cycle statement works, and the recursion
   never needs to recurse. Pinned at `findings/tailloop-overfire-
   jul2026/01`:

   ```graphix
   {seq(i64:0, i64:2);
    {let rec g = |n: i64| -> i64
       select n {i64:0 => cast<i64>(true)$, _ => g(n - i64:1)};
     g(i64:0)}}
   ```

   interp `1,1,1` (offsets 0,1,2), jit `1` once. THE JIT IS CORRECT —
   this is an interp over-fire. Mechanism: `GXLambda::update`'s
   `tail_loop` branch (lambda.rs:262) wraps EVERY body pass in
   `event.init = true` — including the first pass of a passive
   re-poll when an unrelated event (the seq tick) flows through the
   block. Init-gated Constants re-fire, the select re-emits its arm
   value, the callsite returns Some, the block re-emits. Requires
   `let rec` + self-tail-call (tail_loop set) + select with a
   const-bearing arm: non-rec (p42), rec-without-self-call (p43),
   and select-free bodies (p36/p37) all agree. The forcing exists so
   loop RE-ENTRIES emulate the old fresh-body-per-level node-walk —
   but the FIRST pass is an ordinary call/poll. Dropping it there
   cannot under-fire: the non-rec path never forces init on re-calls
   and agrees with the jit under genuinely re-firing args (p44), rec
   likewise (p45). Fix sketch: in the tail loop, honor the event's
   real init flag on the first pass; force init only on iterations
   entered via a `pending_tail_call` rebind. Node-walk-is-canonical
   note: canonical means we fix the node-walk to the intended
   firing semantics (an output fires when a feeding input fired) —
   same shape as the wrap and ref-write-echo rulings.

   Update (~02:15): second bleed-out, 40,547 programs on seed 70404
   (its one find was divergence_000004 = item 8). Relaunched as seed
   70405, startup gate 57/57 clean. Bleed rate ≈ one death per 3–5h
   under this corpus mix — babysitting is fine overnight; the refill
   fix lands with the morning batch.
9. **OPEN (root-caused read-only, fix after campaigns stop) — fused
   fold over a lifted accumulator with a closed callback never
   re-fires** — corpus-reactive/divergence_000000 (~23:47, the
   reactive campaign's first finding; probes p46–p55). Pinned at
   `findings/hof-lift-firing-jul2026/01`:

   ```graphix
   // schedule-v1: cap=64 events=512; in0=i64:1
   {let v1: Array<i64> = []; v1 <- in0 ~ array::push(v1, in0);
    let v3: i64 = array::fold(v1, i64:42, |a, b| i64:100); v3}
   ```

   interp `42, 100; 100`, jit `42` then SILENCE — the fused fold's
   result fires once at init and is permanently STALE. Deterministic
   3/3. Discriminating matrix: callback that consumes a param
   (`|a,b| a+b`) → agrees (p51); `array::map` with a const callback
   over the SAME lifted array → agrees (p53); non-lifted source →
   agrees (p52); LIVE init (`fold(v1, in0, ...)`) does NOT unwedge
   (p54) — so when the callback body is closed (consumes neither
   param, captures nothing), the fold result's disc drops BOTH the
   source-array and init firing channels, even though the value still
   depends on the array via the length/emptiness test. In the
   original finding the enclosing kernel still fired (len(v1)
   tracked) so the program limped along with a stale 42; in the
   reduced form the result never fires at all — a MissingFire, the
   exact class the trace oracle was built to catch (first reactive-
   campaign scalp). Fix direction: the fold emit /
   inherit_hof_firing must OR the source array's (and init's) disc
   into the result disc unconditionally — the length test consumes
   the array even when the body consumes nothing.

10. **HARNESS BUG (fix in the morning) — recorded reactive findings
    can't be re-checked and would gate vacuously** — found while
    triaging item 9: the corpus file itself re-checks AGREE
    (CompileErr in BOTH modes — "could not resolve module test")
    while its minimized text in a clean file diverges 3/3. Two
    defects: (a) `Corpus::record` writes `// mutant: {mutant}` with
    the mutant VERBATIM — a reactive mutant is multi-line (schedule
    header + program), so its program line lands UNCOMMENTED in the
    file, garbling any downstream leading-comment extraction
    (run_pool's println already escapes newlines; record must too).
    (b) `build.rs strip_header` (feeding REGRESSION_CORPUS) strips
    ALL leading `//` lines including a `// schedule-v1:` header, so
    an embedded reactive finding runs with ZERO injections — a
    quiesced-at-init program in both modes, vacuous AGREE. And
    `Schedule::parse` reads only line 1, so the schedule must lead
    the file for file-based check (the new pin is ordered that way).
    Fixes: escape newlines in record's mutant field; make
    strip_header/loaders preserve (or re-attach) the schedule line;
    consider Schedule::parse scanning past comment lines. Until then
    reactive pins in findings/ are regress no-ops (noted inside the
    pin).
11. **DUPLICATE of item 9 (no new action)** — corpus-reactive/
    divergence_000001 (~00:46): `array::fold(v0, i64:-100,
    |in0, v2| i64:1)` over a lifted accumulator — closed const
    callback again, different surface text so the minimized-text
    dedup didn't collapse it. Every jit value is exactly
    `(v1 - in0) * (-100)` — the fold stuck at init; the callback's
    param shadowing (in0/v2) is incidental. Verified: verbatim
    minimized diverges (p56), same program with a consuming callback
    (`|a, b| a + b`) agrees (p57). Second regression case for the
    item-9 fix.

   Update (~04:0x): third bleed-out — 75,611 programs on seed 70405,
   0 new findings (its 1 divergence dedup'd against the corpus).
   Relaunched as seed 70406, gate 57/57 clean.
12. **ACCEPTED CLASS (same as item 2, no action)** — corpus-fuzz/
    crash_000005 (~01:32): `array::group(seq(i64:MIN, i64:4), ...)`
    child HANG on the outer deadline — a ~2^63-element seq runaway,
    the async-producer flood Eric ruled "live with it" on 07-04. The
    mutator keeps finding this shape by perturbing seq bounds toward
    edge literals; if it keeps collecting corpus slots, a possible
    morning tweak is capping seq spans in the mutator's literal-edge
    table (harness, not compiler).
13. **ACCEPTED CLASS (items 2/12, no action)** — corpus-fuzz/
    crash_000006 (~02:0x): `array::group(seq(i64:-1, i64:MAX),
    |n, _| i64:10 == i64:-100)` — seq runaway again, this time with
    an always-false predicate so the group also never closes. Third
    instance tonight; the morning mutator seq-span cap (item 12)
    graduates from "possible" to "do it".
14. **ACCEPTED CLASS (items 2/12/13, no action)** — corpus-fuzz/
    crash_000007 (~02:3x): `array::group(seq(i64:0, i64:MAX), ...)`.
    Fourth seq-runaway. Note the dedup gap feeding this: crash dedup
    keys on RAW program text, and the mutator keeps minting fresh
    bound/predicate literals, so every variant lands a new corpus
    slot. The morning mutator cap should be paired with a coarser
    crash-dedup key (e.g. normalize integer literals) or a seq-shape
    exclusion in record_crash.
15. **DUPLICATE of item 9 (no new action)** — corpus-reactive/
    divergence_000002 (~03:1x): `array::fold(v0, in0, |v0, in0|
    i64:-100)` — the LIVE-init sub-case (probe p54's exact shape).
    jit result fires every epoch (len tracks) but with the fold
    stuck at in0's default: v1²·0 = 0 vs interp v1²·(-100). Third
    corpus entry for the item-9 mechanism; the reactive generator is
    now effectively farming this one bug — expect more dups until
    the morning fix. (If the corpus starts filling, stopping just
    the reactive campaign early is reasonable; generate/fuzz still
    explore other space.)
16. **OPEN (root-caused read-only, fix after campaigns stop) — #20
    Phase B typecheck regression: constrained-tvar arith accepts a
    known-incompatible operand; JIT leaks a pointer** — corpus-fuzz/
    divergence_000008 (~07:39, seed 70410). Pinned at
    `findings/arith-tc-unsound-jul2026/01`:

    ```graphix
    {let f = 'a: Number|x: 'a, y: 'a| -> 'a x + "hello"; f(i64:0, i64:2)}
    ```

    Should be a compile reject. Compiles under the post-Phase-B
    compiler: interp bottoms at runtime (checked add errors), jit
    emits i64 add on the ArcStr payload word and FIRES a heap
    pointer as I64 (value varies per run). Version-skew proof: the
    13:32 pre-Phase-B shell binary REJECTS this exact program
    ("[Number, duration, datetime] does not contain string"); the
    17:00 all-phases fuzz binary accepts. Mechanism: Phase B's arith
    typecheck0 returns early on the constrain path (tvar operand →
    add Number conjunct) without checking the KNOWN operand against
    number(); the deferred typecheck_tail at tc1 only runs inside
    swallowed per-site rechecks, and the def-time gate is typecheck0
    only — so the error surfaces nowhere. Boundary probes: known+
    known still rejects at tc0 (p58); string ARG to a Number param
    still rejects via cell-conjunct enforcement (p60). The hole is
    exactly (unbound constrained tvar) op (known incompatible) in a
    lambda body. Fix: tc0's constrain path must containment-check
    every KNOWN operand against Primitive(number()) immediately
    (binds nothing); audit Neg's negatable path for the same skip.
    HIGHEST severity of the night: a type-soundness hole with a
    memory-disclosure consequence in fused code.
17. **DUPLICATE of item 9 (no new action)** — corpus-reactive/
    divergence_000003 (~08:0x): fold over lifted accumulator, closed
    callback (|v3, v4| i64:7), sampled live init (in1 ~ v1); jit
    stuck at init → result 0 forever vs interp 7. Fourth corpus
    entry for the item-9 mechanism.
18. **OPEN (reduced, root cause needs instrumentation — fix after
    campaigns stop) — MissingFire: two monomorphizations + nested-
    select producer + arm consumption gates the kernel output
    forever** — corpus-generate/divergence_000001 (~09:16). Pinned
    at `findings/xkernel-gate-jul2026/01` (6-line minimal, p61–p79).
    jit trace EMPTY vs interp i64:1. The triangle (all legs
    verified): nested select producing v0, BOTH monomorphizations of
    one lambda called (the f64 one consuming v0, otherwise dead),
    final select arm consuming v0. Any leg removed → AGREE. Suspects:
    callee-result-flags (b10a5ee9) leaking not-fresh across the two
    callee instances, or #219 taint interplay — both from 07-04.
    Fifth real compiler bug of the soak.
19. **DUPLICATE of item 9 (no new action)** — corpus-reactive/
    divergence_000004 (~09:36): fold over lifted accumulator, closed
    callback (|v2, v1| i64:1), live init (in1); jit frozen at the
    init default. Fifth corpus entry for the item-9 mechanism — the
    reactive campaign has found nothing BUT this family since its
    first divergence; if slots keep filling before the fix lands,
    stop the reactive campaign first.
20. **DUPLICATE of item 16 (same fix; add as second fixture)** —
    corpus-fuzz/divergence_000009 (~09:5x): the struct flavor of the
    Phase B hole — `'a: Number` + `{x: f64:0., y: i64:42, z: f64:5.}
    + x` compiles. interp: the node-walk `+` BROADCASTS elementwise
    over the struct payload, yielding a nested array of
    Error("can't add error types")/F64 pairs as the traced result;
    jit: empty trace. The tc0 known-operand containment check fixes
    both flavors. Side observation for Eric: node-walk arith
    broadcasting over composite Values (rather than erroring to
    bottom) is itself surprising — unreachable from well-typed code
    once 16 lands, but worth a ruling on intended Value-level arith
    semantics.
21. **OPEN (root-caused read-only, fix after campaigns stop) — SIXTH
    real bug: recursive lambda with a UNION-typed param fuses the
    param as one arm's register ABI; leaks OR crashes** — corpus-fuzz/
    divergence_000010 family (~10:1x, seed 70412), probes p80-p88.
    Pinned at `findings/rec-union-abi-jul2026/01` (leak variant, no
    crash → regress-safe):

    ```graphix
    {let rec f = |n, acc| select n {i64:0 => acc, _ => f(i64:0, {bar: i64:42})}; f(i64:100, i64:0)}
    ```

    interp: acc = the struct; jit: struct pointer LEAKED as i64. NOT
    a Phase-B regression — the pre-Phase-B 13:32 shell also compiles
    it (legitimately typed). Root: acc's type is [i64, {struct}] (a
    scalar∪composite union); abi_kind of that correctly returns None
    (non-fusable), YET the recursive/cross-kernel kernel sig is
    derived from the INIT call's concrete arg (i64:0) and fuses acc
    as an i64 register. THREE manifestations, all probe-verified:
      - struct through i64 sig            -> pointer LEAK (p82/84/85)
      - i64:7 through {struct} sig (flip) -> MISALIGNED PTR DEREF panic
        (emit_helpers.rs:577, addr 0x7) (p87)
      - acc union [i64, f64]              -> cranelift worker panic
        "runtime did not respond" (audit-bug-2 shape) (p86)
    Discriminators: explicit `acc: [i64,{bar:i64}]` annotation ->
    AGREE (abi_kind sees union, node-walks); non-recursive union
    param -> AGREE (p83); pure-struct acc -> AGREE (p81). fix: the
    recursive/cross-kernel sig derivation must classify the param's
    DECLARED (fully-inferred) type via abi_kind and bail to node-walk
    on None — never take the register ABI from one call site's arg.
    Note the crash variants must NOT be promoted to findings/ (kill
    the in-process regress worker); only the leak pin is committed.

22. **DUPLICATE of item 8 (no new action)** — corpus-fuzz/
    divergence_000011 (~10:2x): `let rec g` + select +
    `cast<i64>(true)$` over `array::iter([0,1])` (a multi-cycle
    source); interp fires 3x, jit once. The `type F`/fn-in-struct/
    select-over-iter preamble is a red herring feeding a multi-cycle
    tick; the divergence is the item-8 tail-loop init over-fire.
    Second corpus entry for the item-8 fix.

## Resolution (2026-07-05, fix session)

All 7 distinct bugs FIXED, committed individually on unified-fusion-proto:

- item 16 (+20): `typ: #16 — arith tc0 rejects a known-incompatible
  operand at def time` — tc0 constrain path containment-checks known
  operands; both flavors now CompileErr in both modes.
- item 21: `fusion: #21 — recursive self-call args must match the
  formals' kernel ABI` — self_calls_abi_consistent de-fuse guard; the
  leak and both crash variants AGREE; well-typed recursion still fuses.
- item 9 (+11,15,17,19): `fusion: #9 — fold's firing channel is the
  acc carry (the chain), exactly` — result_is_firing on SlotFlags;
  full matrix (p46-p54, p89-p93, d15/d19) + hof-connect pin verified.
  First fix attempt (authoritative strip) traded the MissingFire for
  an ExtraFire on init-only fires — the node-walk's chain-propagation
  semantics (fires only through actual acc consumption) is the spec;
  the carry IS its kernel mirror.
- item 8 (+22): `interp: #8 — tail-loop honors real init on the first
  pass` — the JIT was the correct side (node-walk over-fire).
- item 5: `compiler: #5 — recompile clone_rebind resolves captures via
  remap+alias` — unconditional remap-resolved alias into the fresh rb
  scope. First attempt (alias without remap consultation) broke
  per-slot feeder resolution — the initiator's remap is the contract.
- item 1: `interp: #1 — Deref gets the structural clone_rebind
  override` — third sibling of Sample/ConnectDeref; needed #5's
  remap-aware aliasing too.
- item 18: root cause three layers deep — setup_bind's swallowed
  per-site typecheck0 ran against the DEF's FnType cells
  (lambda.rs init closure passed _typ to GXLambda, `resolved` only to
  builtins): site 1 bound def-'a := i64 permanently; site 2's
  swallowed recheck pushed i64 through the bound def cell into the
  caller's still-open select-result cell (swallowed ERRORS, permanent
  WRITES); fusion froze the f64-valued binding as Prim(I64); the
  runtime marshal treated the F64 payload as absent; #219 gated the
  output forever. Fix: the instance's signature is the site's
  `resolved` ftype (fallback to the def's on structural mismatch —
  late binding/arg subtyping). Two rejected variants recorded for the
  record: def-typ + non-binding recheck (kills monomorphization —
  fusion RELIES on the site binding the instance cells) and
  reset_tvars fresh cells (never-bound cells de-fused every
  cross-kernel call — caught by jit_lambda_call_probes).
  Diagnosed via GRAPHIX_DUMP_CLIF + a new permanent GRAPHIX_DBG_REGION
  dump (region input wiring: name/BindId/type/constraints/kind).

Gates at end: workspace 2146+ passed / 0 failed, FUSE audit zero
MISMATCH, regress 63/63 (first fully-green corpus), selfcheck 300+corpus
OK, 3000-program generate smoke: 0 divergences 0 crashes.

Still open (harness batch): items 6 (run_pool refill leak), 10
(record newline escaping + strip_header schedule preservation +
Schedule::parse comment-skip), 12-14 (crash dedup key normalization);
then the item-3/4 interp-Timeout uncontended re-runs.

## Harness batch resolution (2026-07-05, same session)

Committed as `fuzz: harness batch — pool refill leak, record escaping,
schedule preservation, crash dedup`. Items 6, 10, 12-14 all fixed; the
five damaged corpus-reactive records repaired in place (now genuinely
re-check with real injections — all AGREE post-#9).

Items 3/4 CLOSED as accepted-family, NOT flakes: both still diverge
uncontended on 32 idle cores (interp Timeout vs jit [0:8]). Mechanism:
the queuefn drain re-wakes the graph each cycle and the interp
re-enters the infinite tail loop with cached args — one one-shot
interrupt per cycle can't drain it, while the jit kernel aborts once
and never re-fires (const inputs stay STALE). This is the
infinite-loop mode-asymmetry Eric accepted on 07-04. OPEN QUESTION for
Eric: encode interp-Timeout-vs-jit-quiesced as a global oracle
relaxation in Trace::agrees_with (stops the corpus collecting this
shape), or keep the oracle strict and live with occasional re-findings?
Per the 2.3 calibration policy this needs explicit sign-off — default
remains zero relaxations.

## Items 3/4 follow-up (2026-07-05, Eric's push): FIXED, not accepted

Eric challenged the "not fixable / accepted family" assessment on the
interp-Timeout findings — correctly. Pulling the thread exposed a
FOUR-bug knot around tail loops, two per backend, all fixed
(commits `interp: tail loops — genuine-call gating and loop-local
formal state` + `fusion: tail loops carry firing — rebind the disc,
fold scrutinee STALE`):

- interp: tail interception looped on stale ctx.cached args on
  passive re-polls (the item-3/4 wedge — one interrupt per cycle,
  Timeout). Now gated on a genuine call (arg fired / init view);
  quiet self-tail-calls produce None without dispatching. Items 3/4
  now genuinely AGREE — the Timeout-vs-quiesced oracle-relaxation
  question is MOOT.
- interp: loop pat.bind rebinds leaked through ctx.cached into the
  next call's quiet args (g(in0, i64:0) accumulated across epochs).
  Snapshot/restore around the loop.
- jit: emit_tail_rebind_jump rebound payloads only — loop-carried
  discs stuck at entry (const seed's STALE) → fired once ever.
- jit: tail-position selects never folded scrutinee firing into the
  return (no merge point) → all-const value chains stayed quiet even
  with a fired loop bound. LowerCtx::tail_scrut_stale.

Pins: findings/tailloop-firing-jul2026/{01,02}. Neither generator
produced live-input-driven `let rec` (the reactive generator has no
recursion vocabulary) — a generator gap worth closing in the next
fuzzer iteration.
