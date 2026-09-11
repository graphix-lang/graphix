# Program image: cached compilation, in three steps

Status: the registration image is built (2026-09-10); the kernel
cache and the program image are proposals. Pins:
`stdlib/graphix-tests/src/lang/image.rs`, `graphix-compiler/src/image/mod.rs`
and `shared_map.rs` unit tests, `graphix-shell/src/cache.rs`.
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

## Step 2: dropped

Kernels travel inside the program image as the bytes and relocations
the cold run produced, keyed within the image by the in-process kernel
key, so the program image needs no CLIF determinism and no second
cache. A content-keyed kernel cache would only speed up the cold run
after an edit and share kernels across programs; if that matters
later it is additive, and the determinism pin becomes its
prerequisite then. The original design follows for the record.

### Step 2 as designed: cache kernels at the Cranelift boundary

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

## Built: the registration image

The first slice of step 3, keyed and stored as the cache rule above
says. The image is the session after the package root modules compiled
and before any cycle: the environment (`image/env.rs`), every lambda
definition as data (`image/defs.rs`: body, lexical snapshot, scope,
scheme, analysis facts; `init` is rebuilt by `make_init`, a builtin's
check `Apply` on first use), the context's tables, the root nodes and
the root scope (`image/registration.rs`). Ids relocate through
`image_id!`; expressions keep their ids and origins; type variables
keep their two-level sharing; handlers of the dynamic scope, module
paths, origins and type-reference resolution cells are shared
objects. A definition's or a module's snapshot carries only the four
lexical fields, the only ones `restore_lexical_env` reads, which took
the stdlib test image from 7.5 MB to 1.2 MB.

Expressions and function types are objects too, keyed by address
(`image::object_len/encode/decode`): a node's spec and a definition's
body are value clones of subtrees of one tree, and a binding's type
shares its definition's `FnType`, so each is written once and
referenced afterwards. The session cannot pin them, so everything a
session encodes must be borrowed from the context and the root nodes
for the whole session. That took the full stdlib image from 2.3 MB to
1.6 MB and the node phase of the restore from 9 ms to 5 ms.

A typedef remembers when a seed walk filled every reachable
resolution cell (`TypeDef::seeded`, carried in the image), so
`seed_typedef_refs` at the end of every compile walks only the
typedefs added since; it cost each compile 3.6 ms in release before.

The package root compiles with fusion off: its ten fused constants
bought nothing at runtime and would have put kernels in the image.

Measured over the full stdlib in an optimized build (frame pointers,
no LTO), pinned to a performance core with the governor on
performance; this machine idles at 400 MHz on powersave and a 30 ms
process is otherwise mostly clock ramp:

| root init, `--check` of a one-line file | ms |
|---|---|
| cold (`--no-cache`, one core) | 32 |
| warm, before these changes | 22 |
| warm, typedef seed memo | 18 |
| warm, expression and function-type objects | 14 |

The warm split: environment 3.9, definitions 5.1, nodes 5.1, the
program's own compile 0.5. In the debug profile over the graphix-tests
package set: 36 ms cold, 7.8 ms warm, 0.77 MB.

Node kinds imaged so far are the ones package modules produce at top
level; the rest, `FusedKernel` included, come with the program image.
Not yet imaged: core hook sites (empty before any cycle), dynamic
modules' runtime environment, `DefOrigin::Runtime` definitions.

## Built: the program image, slice (a)

Every node kind a fusion-off program produces has a codec: the
registration kinds plus parentheses, string interpolation, connect
and connect-through-reference, casts, `any`, sampling, array and map
references and slices, struct update and field and tuple references,
abstract construction, place references and dereferences, catch,
`?`, seq guards, `or_never`, select with its pattern nodes, the call
site, and the collection intrinsics (`NodeTag::Collection` plus the
intrinsic, one decoder per `MapQ`/`FoldQ` instantiation). A call
site's callee travels three ways: unbound (a dynamic site before its
first cycle), an imaged instance of a lambda (`GXLambda`: argument
patterns, body, scheme, analysis facts, lexical snapshot), or a
builtin rebuilt at decode by the restored definition's factory over
the imaged argument references, with the types the cold run resolved.
Nothing is typechecked at decode: re-running static resolution
against restored inference state fails (the corpus differential found
it), so the image carries every resolved type it needs. A `?`'s
handler and a catch's own handler are the scope codec's shared
objects. Dynamic modules' runtime environment and
`DefOrigin::Runtime` definitions stay outside the image.

The runtime compiles a script at construction (`GXConfig::program`),
before any cycle, so the image can carry it; the image records the
program root's id, output flag and type, and `GXHandle::program`
hands the embedder what `load` used to. A program that fails to
compile is reported through the same call, so the shell's error text
is unchanged. The shell keeps two cache entries per build id, the
registration and the program (its key adds the program source), and
loads the most complete one it has; the runtime writes whichever was
missing. Fusion-on programs hold kernels and fail the write, so they
run cold until slice (d). A warm run compiles nothing, so
compile-time diagnostics (an uncaught `?`, unused bindings) print on
the cold run only.

Pins: `program_image_restores` restores a program of every new kind
and compares the value sequence of the first cycles, cold against
warm; a cold-versus-warm differential over the whole fuzz corpus and
the bench programs (490 programs, fusion off) ran clean before
landing, after finding a decode-time re-resolution failure, a
builtin's lost argument type and a dropped select fact.

Measured, fusion off. A one-page script in the optimized build,
pinned: the whole session restores in the 14 ms the registration
alone took, against 30 ms of cold registration plus 2 to 4 ms of
compile. The admin TUI (`milestone_image` in
graphix-package-netidx-admin, unpinned): optimized without LTO, cold
462 ms, warm 114 to 122 ms; debug, cold 4.9 s, warm 0.73 to 0.76 s;
image 9.1 MB either way. The image holds every instance the cold run
created and the restore decodes all of them; the census says the
first frame reaches about a third, which is what slice (b) is for.

## Step 3: cache the compiled program

Built in slices, each landing green: (a) every node kind that a
fusion-off program produces gets a codec, call sites carry their
statically bound instance, builtin occurrences rebuild through the
registered factory, and the shell images a program compiled with
fusion off (a fusion-on program fails the write and runs cold, as
any un-imaged kind does); (b) instances materialize on first
dispatch; (c) the program cache key and `--warm`, measured on the
admin TUI; (d) kernels as stored bytes; (e) the fuzzer's warm route
and the growth study.

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

### Env: maps packed with their sharing (built)

Only four `Env` fields are lexical: `binds`, `modules`, `typedefs`,
`traits` (`graphix-compiler/src/env.rs:330`). Everything else is
global and is written once, as the final tables.

A definition's `def_env` is a persistent snapshot sharing all but a
root path with its neighbours, so the image packs the maps with their
sharing instead of replaying their construction.
`immutable-chunkmap` 2.2 exposes the tree's structure (`Map::root`,
`NodeRef`, the unsafe `NodeHandle::create`, `Map::from_root`), and
`graphix-compiler/src/shared_map.rs` packs a map through it: each node
is written once and referenced afterwards, a definition before its
subtrees and numbered after them, so the decoder is recursive and a
reference names a node by the rank at which the decoder completed it.
A snapshot whose maps are unchanged costs one reference per field.
The node tables belong to the caller — an `EncodeTable` per image
write, a `DecodeTable` per runtime so an instance decoded later
resolves into nodes decoded earlier — installed for a call by a
session guard. `encoded_len` is an upper bound: the image writer
reserves it and patches the prefix after encoding, and no
length-wrapped derive holds a shared map.

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
