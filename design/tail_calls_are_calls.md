# Tail calls are calls

Status: ruled 2026-09-26 (Eric). Steps 1 and 2 built 2026-09-26; step 3
G1–G3 built 2026-09-26, G4 in progress.
Pins: `findings/tail-calls-are-calls-sep2026/` (the sep25b over-fire),
`lang::functions::{tail_depth_catches_up_an_outer_write, tail_loop_deep}`
(the deep loop is JIT-only: the node-walk recurses past the stack
budget), and every `findings/` tail and frame pin, whose node-walk now
recurses.

## The rule

The node-walk has no frames. Every call, tail or not, dispatches an
activation: a retained instance per recursion depth, shrink = delete
(`recursive_activations.md` §2). The JIT's native tail loop — rebind
the formals and jump — is an optimization of the kernel, used only
where it answers what the activations answer; the fuzzer holds it to
that, with the node-walk as the reference.

## Why

A framed tail loop ran every iteration of a stateless tail recursion
through ONE activation: the formals rebound in a private overlay
(`Event::enter_frame`), `event.init` forced and the real one carried in
`ctx.dispatch_init`, replay caches cleared between passes
(`Update::reset_replay`), handler errors parked in `ctx.frame_outbox`,
a resumed recursion flagged across cycles (`resumes_mid_recursion`),
the result's tag rebuilt from the tail spine (`tail_scrut_fired`). The
premise was that a stateless body cannot tell one activation from many.
It can: an activation's select keeps its selection and its wake
catch-up bits, and one select standing for every depth answered
differently (sep25b: a fire bit an arm consumed only inside a frame
was delivered two cycles later).

What the machinery cost: 73 sites in 11 compiler files read frame
state, every node and every builtin implements `reset_replay` (115
impls, the stdlib and external packages included), and 18 finding
directories (44 pins) are frame or tail-loop bugs. What it bought
since stacker: memory and time. An activation is ~25 KB and ~50 µs
(fusion off, `sum(i - 1, acc + i)` at depth 100k: 49 MB / 0.2 s framed,
2.5 GB / 5.1 s as activations). Programs run with fusion on (images
made its startup free, UIs included), and a fused tail loop is a
native loop, so the cost falls only on tail loops that do not fuse.

## Who pays

Surveyed 2026-09-26 (`GXDBG_TAIL` pass counts, fusion on): the stdlib
has no `let rec`; the admin TUI's `pad`, the book's mandelbrot
`iterate` and `native_array_ops.gx` fuse; `cons_list.gx`'s
`filter_map_l` does not. Of the corpus's 56 tail-looping programs, 26
node-walked their loop with fusion on, for these reasons:

| class | shape | corpus |
|---|---|---|
| G1 position | the call is an argument of a node fusion does not descend into: a builtin with no fast call (`count(f(m))`, `uniq`, `once`, `array::group`), `~`, a literal with a non-fusing sibling | 7 |
| G2 local `let rec` | defined in a nested block: an operand, an array element, a HOF callback body ("function-valued let") | 8 |
| G3 argument | an argument that does not fuse fails the whole call instead of entering as an input: `f(10, { let s = 0; s <- 1; s })` | 2 |
| G4 body shape | a primitive-union result (`i64`/`f64`, `i64`/`bool`), a slice rest bind `[_, tail..]`, a nullable scrutinee bind, a function-valued `let` in the body, a tail and a non-tail self call together, a shadowed `let` with a cast | 10 |
| rule | a reference read, a `?` under a handler, a `<-` to an outer variable, a stateless builtin with no fast call (`is_err`, `opt::*`, `product`) | 2 |

G1–G4 are fusion gaps and are closed as part of this work (below). The
rule row is what stays: a recursive loop whose body reads a reference,
raises to a handler, writes an outer variable or calls a builtin with
no fast call runs as activations, one per level. That, a fusion-off
embedder, and the fuzzer's interp engine are the whole cost.

## The work

1. **The node-walk without frames.** `GXLambda::update` dispatches the
   body once; `run_tail_loop`, `PendingTailCall`, the call site's tail
   interception (`tail_arg_order`) and `Select::tail_dispatch_select`
   go. `ExecCtx::{frame_depth, dispatch_init, frame_outbox,
   tail_scrut_fired, pending_tail_call}`, `Event::{frames,
   enter_frame, exit_frame}` and `ExecCtx::in_frame` go; every branch on
   them keeps its depth-0 arm. `Update::reset_replay` and
   `Apply::reset_replay` go, in every node, builtin and package.
   `TrackedFires` loses its frame exclusion. `GXLambda::tail_loop`
   stays: it is the JIT's gate (`KernelSig::has_tail_loop`) and what
   `#[tail_recursive]` asserts.
2. **The JIT answers what the activations answer.** With the node-walk
   recursing, the corpus (508 programs) and the fixtures agree across
   engines with no JIT change beyond deleting its frame mirrors: the
   context word's quiet bit had no reader (the wake bit is now bit 1),
   and `FusedKernel::update`'s frame-depth reads went with
   `frame_depth`. Where a loop is ever found to answer differently, the
   body recurses natively instead (the path a stateful tail loop
   takes).
3. **Close G1–G4**, each with a probe that node-walks today and a
   fusion pin (`FuseExpect::Jit`, `#[native]`) that holds it.
   - G1 and G2, built: a node that does not fuse whole descends
     (`fusion::fuse_parts`, the `fuse` of every node with children
     that is not a container); a part that calls a function (a lambda
     call, a collection operation) is tried as a region of its own, any
     other part only descends — arithmetic over a leaf is not worth a
     kernel. A `let` bound to a lambda literal emits nothing in a
     kernel, which calls the lambda statically (a value read of it is
     an undefined local, refused), so a block defining a local
     recursion fuses whole, a collection callback included. The
     absorbed attribute keeps its target rule (`#[native]` on a
     function or a declaration is an error wherever it sits,
     `Attribute::check_target`). Pins: `lang::fusion::{loop_under_a_node_walked_builtin,
     loop_beside_node_walked_siblings, local_let_rec_in_nested_blocks,
     local_lambda_in_a_loop_body}`.
   - G3, built: a lambda call whose argument fails discovery (an
     effect, a stateful builtin) fuses with that argument as a feeder
     of its kernel (`fusion::try_fuse_feeding_args`): the node-walk
     runs it and the kernel reads its production as an input, the
     equivalence `f(e)` ≡ `{ let a = e; f(a) }` gives. A fed argument
     the kernel does not read (a skipped invariant fn formal) would
     drop its effect, so the call then stays whole in the node-walk.
     Pin: `lang::fusion::call_fed_by_node_walked_args`.
   - G4: the function-valued `let` is closed by G2's rule. Open: a
     slice rest bind, a nullable bind of a non-scalar payload, a tail
     and a non-tail self call together, a primitive-union result, and
     a cast from a varint wire type (`v32`/`z32`/`v64`/`z64` have no
     kernel representation; the corpus case was `cast<i64>(z64:1)`).

What users are told (the book's performance chapter): a recursive
function whose body fuses runs as a native loop; one that does not
costs an activation per level, and the four rule-row causes are the
reasons a body does not fuse.
