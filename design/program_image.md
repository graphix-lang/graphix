# Program image: cached compilation, in three steps

Status: proposal; nothing built. The experiments below name the pins
that will hold it.
Supersedes: the compiled-packages plan (build-time package-definition
images).

## Goal and budget

A 5 kloc or a 100 kloc Graphix application deploys as one executable
and starts quickly on a machine ten times slower than the development
box: a free-tier cloud instance with part of a hyperthread. The first
run after a rebuild may be slow; every run after it is fast. The
application's root program is fixed (the admin TUI, the netidx
browser), but the same mechanism serves the shell running a file.

The current fast-machine profile (`jit_startup.md`, admin workload):

| phase | ms |
|---|---:|
| registration (AST decode 10, definition gate 64, rest 16) | 90 |
| app request, fusion off (2,513 instances of 382 defs: 120) | 189 |
| app request, fusion on (fusion 190, Cranelift backend 120) | 390 |

That is 4.8 s on the target machine. A first frame near 500 ms there
leaves roughly 50 ms of fast-machine-equivalent work for everything
before it. Two consequences shape the design:

- Machine code is cached. Fusion alone is 1.9 s on the target.
- Startup is proportional to what the first frame reaches, not to what
  was written. Eager decode of every instance is already the whole
  budget at 5 kloc and twenty times over at 100 kloc.

## Everything is a cache written by the binary that reads it

Nothing is produced at build time. The executable that will consume a
compiled artifact is the only artifact with the complete builtin
registry, and it is the exact bytes the artifact must match, so the
executable produces the artifact on its first run and reads it back on
every later run. This removes build scripts, host/target registries,
cross-compilation, sidecars, and any way for a producer and consumer to
disagree.

**Key.** An entry's key is composed of:

- the executable's build id (the linker's `.note.gnu.build-id`, or a
  build-time constant), never a hash of the whole executable, which is
  too slow to compute at every startup on the target machine;
- each package's source checksum, baked into the package at AST-pack
  time (`graphix-ast-pack`), so a package's identity is known without
  rehashing;
- the checksum of every source the resolvers read while compiling the
  root, recorded in the entry like a depfile and re-verified on the
  next run.

**Store.** `$XDG_CACHE_HOME/graphix/<build-id>/…`. Entries are written
to a temporary file and renamed into place, so concurrent first runs
are safe. Build ids older than the current few are collected.

**Correctness.** An entry is written only after the work it caches
succeeded, and before any cycle runs, so a cache never masks a
compile error and never captures live state. A hit must be
indistinguishable from a cold compile: the cached path is the shipped
path, so the test suite and the fuzzer run it (see IDs below for why
that is possible), and a warm-versus-cold differential is the pin for
every cache here. A miss is the documented cold path, not a silent
fallback. `--warm` runs the cold path, writes every entry and exits,
for deployment pipelines that want the first run fast too.

Package validation stays where it is: a package's type errors surface
when its tests run and at its first use, exactly as today.

## Step 1: cache the definition gate

At registration every let-bound lambda in every package is checked
once as a definition: `Lambda::typecheck0` builds faux arguments,
compiles the body from source through `def.init`, typechecks it and
deletes it (`graphix-compiler/src/node/lambda.rs:1199`, `:1233`).
That is 339 bodies and about 64 ms of the 90 ms registration.

The gate produces three things: validation; the definition's scheme
facts, recorded as cell constraints by `constrain_known`
(`typ/fntyp.rs:596`) with closed inferences kept by
`unbind_open_tvars` (`lambda.rs:1262`); and the builtin check applies
retained for per-site `typecheck1` (`node/callsite.rs:120`). Only the
scheme facts are data the runtime needs afterward, and the builtin
checks are cheap and stay.

The entry is every definition's post-gate scheme, keyed within the
entry by a stable definition identity (the lambda literal's preorder
index in its packed module, or `def.src`; `ExprId`s are fresh per
decode). On a hit, registration parses and compiles the package top
level as today, and where the gate would run it decodes the stored
scheme, aliases its variables by name onto the fresh scheme's cells
with the mechanism definition creation already uses (`lambda.rs:976`),
and applies the constraints. Nested gates are skipped the same way.

The pin: register the stdlib cold and warm and compare every
definition's resolved scheme. The gate's other side effects look
inert (`TypeRef` cells are write-once, nested defs and `bind_to_lambda`
entries die with the deleted body); the comparison is what proves it.

Expected: registration from about 90 ms to about 35 ms.

## Step 2: cache kernels at the Cranelift boundary

| fusion phase | ms |
|---|---:|
| discovery, return types, inputs, callees | ~35 |
| CLIF construction | 45 |
| Cranelift bodies, wrappers, stubs | ~120 |

The cut is at the backend. Key: the CLIF function's text plus ISA
flags and the Cranelift version, under the build id. Value: the
compiled bytes and their relocations, installed into the same
`JITModule` through `define_function_bytes`. Relocations name helpers
by symbol, as they already do. The front half still runs because it
decides the region shape and produces the key. Keyed on CLIF content
the cache is per region, not per program: every program shares entries
for the same stdlib regions, and an emitter change invalidates exactly
the kernels it touches.

**Prerequisite: deterministic CLIF.** The emitter bakes process
addresses and per-process counters as immediates:

- fast-call function pointers, which move every run under PIE
  (`fusion/emit/call.rs:183`, `:188`);
- interned strings, values, types and `QopSite` pointers
  (`fusion/emit/body.rs:855` through `:910`);
- slot state-table `Arc` pointers (`fusion/emit/call.rs:553`);
- `AbstractId`, a per-registration counter (`fusion/emit/nodes.rs:989`).

Each becomes an imported symbol resolved by name at load (fast fns,
like helpers) or a load from a per-kernel constants block whose entries
carry a recipe the loader recomputes in the running process: this
interned string, this serialized type, this abstract type by name,
this qop site by key. About eight emit sites and one constants block.
The same table relocates kernels stored in the image (step 3).

Pins: emit the admin workload in two processes and compare CLIF byte
for byte; a warm run against a cold run under the fuzzer's `detcheck`.

Expected: about 120 ms off the app request. Steps 1 and 2 together
take the fast-machine startup from about 480 ms to about 300 ms.

## Step 3: cache the compiled program

What remains after steps 1 and 2 is instantiation and typechecking
(~190 ms), CLIF construction (~45 ms) and analysis (~17 ms): the work
that produces the keys, uncacheable by content. The program image
caches it by program.

### The image is the compiled program, not the package definitions

A package-definition image cannot remove this cost. The gate deletes
the bodies it compiles, and every call site builds its instance from
the source `Expr` through `GXLambda::new` (`lambda.rs:992`). Compiled
package bodies would be bodies no runtime path consumes, and deriving
instances from a compiled template is the deleted `clone_rebind`
machinery.

The image is the state of one compiler session after the root has
been compiled, every static call site instantiated, analysis run and
fusion emitted: the environment, the definitions, every instance's node
graph, the analysis facts and the kernels. One session, one shared
`Env`, one ID space. Its key is the root's depfile under the build id.

### Format: an indexed object graph with offsets

The codec is an object graph, not a tree. Object numbers come from
allocation identity at encode time; every shared object is written
once; decode registers an object before filling its recursive contents
so cycles resolve. The tables span the whole image: cell constraints
refer back to cells, abandoned cells forward to other `TVar`s, nodes
share specs, types share subtrees. These edges are decoded as edges,
never reconstructed by re-running aliasing, constraint merging or
typechecking.

Every object has an offset, and an instance's graph is locatable
without decoding its neighbours. Eager whole-image decode is the first
implementation; lazy decode is then a decoder change, not a format
change. Immutable data (`Expr` specs, types, kernels) and mutable data
(node state, staging, activation words) are laid out separately, so a
later zero-copy decoder can leave the immutable part in the mapped
file.

The syntax codec (`expr/serialize.rs`) deliberately does less: fresh
`TVar` cells, skipped `TypeRef` resolutions, fresh IDs. Those choices
suit syntax that will be compiled. The image codec never recovers
correctness by recompiling.

### IDs: dense renumbering and block relocation

Encode renumbers every identity domain densely into `[0, N)`. Decode
reserves one block of `N` per domain with a single fetch-add on that
domain's allocator and adds the base to every ID as it is decoded. No
floors: an allocator never moves backwards and never overlaps anything
already handed out, and the same image loads into several runtimes in
one process (the test suite, the GUI harness, the LSP), each with its
own block. This is one add per ID during a pass that touches every ID
anyway.

`atomic_id!` (`../netidx/netidx-core/src/utils.rs:173`) needs a
reserve-block API; `from_inner` does not reserve. Every persisted
domain is accounted for (`BindId`, `LambdaId`, `ExprId`, `TVarId`,
`AbstractId`, `TraitId`, `TypeRef` cells, nominal registries on the
Rust side), and nominal identities reconnect by name, not by
registration order.

Definition identity and activation identity differ. Loading preserves
a definition's IDs. A later call still allocates fresh locals and state
owners above the block, while captures keep the identities of the
bindings actually captured.

### Env: the lexical edit log

Only four `Env` fields are lexical: `binds`, `modules`, `typedefs`,
`traits` (`graphix-compiler/src/env.rs:330`). Everything else is
global and is written once, as the final tables.

The lexical maps are built by a sequence of inserts, and each
definition's `def_env` is a point in that sequence. The image stores
the insert log with a snapshot marker at every definition's point.
Entries between two markers are sorted, so decode builds each stretch
with `insert_many`; a snapshot is a persistent clone at the marker,
O(1) and sharing structure exactly as the original build did. Within a
module the snapshots are a prefix chain of one map, so the log is close
to minimal. `immutable_chunkmap`'s internals stay private.

### TVars: two levels of sharing

`TVar` is an `Arc<TVarInner>` holding wrapper metadata and an `Arc` of
a separately mutable `TCell` (`typ/tvar.rs:136`). A clone shares the
wrapper; distinct wrappers can share a cell. Both relationships are
object references in the image. Neither names, structural equality nor
`TVarId` can stand in: `alias_cells` redirects the cell without
updating the wrapper's id (`typ/tvar.rs:444`).

The image retains the wrapper's id and frozen flag and the cell's
bound type, constraints and `cycle_refused` state. The snapshot is
taken outside any definition check: `rigid_gates` is verified zero,
never discarded. Stored schemes are immutable; instantiation for a
later call still mints its own cells as `resolve_tvars` does today.

Round-trip tests mutate after decoding: bind through an alias, redirect
a shared wrapper, add a constraint visible through another alias, keep
equal-named independent variables independent, and cover forwarding
cells and recursive constraints. Comparing printed types would miss
every one of these.

### Nodes, builtins and lambdas

Every node kind has a codec (48 `Update`/`Apply` impls; a node tag
plus the concrete node's codec rebuilds the `Node` wrapper). State is
pristine: the image is taken before any cycle, so no timer has
started and no subscription has opened. Process-local fields have
explicit attachment rules.

Builtin occurrences store a symbol plus immutable prepared data and
reconnect to the linked Rust factory at decode; closures, vtable
addresses and handles are never image data. `BuiltIn::init` takes a
full context, scope and argument nodes
(`graphix-compiler/src/lib.rs:844`) and does not separate preparation
from runtime construction; that audit is part of the work, per builtin
(57 impls).

`LambdaDef::init` is a closure over source and environment
(`lambda.rs:992`). In the image it is data: the source body, the
lexical snapshot marker, argspec, scheme and flags, driven by ordinary
runtime code.

### Instances materialize on first dispatch

The runtime already creates instances at first dispatch: a new
recursion depth, a new collection slot, a dynamic call. Decoding an
instance's graph from the image when its site first dispatches is that
same path, not new semantics; a labeled default still delivers FIRED
at the fresh callee's first dispatch. Typing and analysis remain
whole-program, done on the cold run, and are not repeated.

Instances that do not exist in the image (a slot the array grew into,
a depth not reached on the cold run, a dynamic callee) compile from
source against the definition's lexical snapshot as they do today. The
compiler stays linked into the executable.

Restoring an instance from the image and creating a fresh one at
runtime need not share an ID policy. Unpacking a second copy of an
instance with the same local IDs does not create independent state;
the fresh-instance path keeps its binding rules.

### Kernels and analysis facts

Kernels are stored as the step 2 entries they already are, referenced
from the instance that owns them and attached when it materializes.
Reusable facts (effects, statelessness, recursion kinds, local binding
sets, blockers) are stored with their definitions and instances; facts
that depend on callbacks store the dependency summary. Cycles that
only runtime-created instances introduce are analysed then, as today.
Nothing rescans stored bodies to rediscover established facts.

### Shell and runtime API

`Shell::init` (`graphix-shell/src/lib.rs:210`) consults the cache
between registration and root compilation, and `GXConfig`
(`graphix-rt/src/gx.rs:260`) gains an alternative to `compile_root`
that installs a restored root without re-entering the compiler.
`setup_context` still runs before any restored node constructs a
resource, and after the ID blocks are reserved. The LSP registers once
per session and is untouched.

## Experiments, in order

1. Count first dispatches in the admin init cycle against its 2,513
   instances. If the first frame reaches most of them, per-instance
   decode cost governs the first frame and laziness governs growth; if
   it reaches a few hundred, laziness governs both.
2. Step 1, with its warm/cold scheme comparison. Measure registration.
3. The CLIF determinism pin, then step 2 with the `detcheck` warm/cold
   route. Measure the app request.
4. The image codec on a small representative set: a generic
   higher-order function, closures with the same source and different
   captures, independent mutable locals, an async producer,
   caller-owned error handling, nominal and trait identity across an
   import. The TVar mutation tests above.
5. Eager restore of the admin image. Record decoded bytes, allocations,
   time per phase (decode, ID relocation, env replay, builtin
   attachment, kernel attachment) against the cold compile. Then the
   lazy decoder, same measurements.
6. Differential: warm against cold reactive traces, fusion on and off,
   through sleep/wake, recursion, deletion and reactivation; the fuzzer
   gains a warm route.
7. Growth: unused bodies at 10x and 100x with the reached set constant;
   then vary reached instances, declaration count and activation count
   separately. This distinguishes removing work from postponing it.
