# Extracting netidx networking from the graphix core

## Goal and context (Eric's direction, 2026-07-22)

netidx networking becomes an ordinary add-on package. graphix-compiler
and graphix-rt end with ZERO dependency on the `netidx` (networking)
and `netidx-protocols` crates. The VALUE layer stays: netidx-value
(future `dynamic-value`, to be renamed and released independently
upstream) and netidx-core (Path, Pack, atomic_id — `netidx::path::Path`
IS `netidx_core::path::Path`, verified) remain graphix's data layer.

Why netidx was central: graphix was designed as the embedded language
of Atlas (netidx-browser). Atlas still wants to load graphix code over
netidx — that becomes a trait-object module loader provided by the net
package, not a hardwired resolver variant. The immediate trigger: the
fuzz-soak port-exhaustion ceiling (every runtime is a netidx node —
resolver + publisher + subscriber per fuzz child — and the jul22
attempt to make that lazy produced two novel races in an hour; the
architecture was talking). With the extraction, tests, fuzzing,
--check, and the LSP have no network AT ALL, and the net package gains
flexibility (user-accessible config hook, custom config locations).

## The cut line (verified by full-tree inventory, 2026-07-22)

Networking coupling lives in exactly these places:

- **compiler lib.rs**: `Event.{netidx, writes, rpc_calls}` (the three
  networking event maps, lib.rs:471-473) and 11 networking `Rt`
  methods (`subscribe/unsubscribe/publish/update/unpublish/
  publish_rpc/unpublish_rpc/call_rpc/list/list_table/stop_list`,
  lib.rs:1164-1268). Everything else in the trait
  (`spawn/spawn_var/watch/watch_var/set_var/ref_var/set_timer/...`)
  is (BindId, Value)-generic and stays.
- **compiler expr/resolver.rs**: `ModuleResolver::Netidx` variant +
  `resolve_from_netidx` + `parse_env`'s `netidx:` arm.
- **graphix-rt rt.rs**: ~15 networking fields on GXRt (publisher,
  subscriber, batch, subscribed, published, net_updates, net_writes,
  rpc_*, pending_unsubscribe, change_trackers, updates/writes/rpcs
  channels) + the 11 Rt impls.
- **graphix-rt gx.rs**: 3 networking select arms, the per-cycle batch
  commit, `Source::Netidx` module fetch, GRAPHIX_MODPATH parse,
  unsubscribe grace, rpc-client GC.
- **graphix-rt lib.rs**: UpdateBatch/WriteBatch types,
  `GXHandleInner.subscriber` + `GXHandle::subscriber()`.
- **shell**: netidx config/auth Params, get_pub_sub, InternalOnly,
  `netidx:` file prefix, ShellBuilder publisher/subscriber fields,
  lsp_backend/examples_compile InternalOnly.
- **stdlib**: sys/net.rs (the whole user surface — sole caller of the
  11 Rt methods); gui data_table (bypasses Rt via `gx.subscriber()`);
  core testing.rs (InternalOnly per TestCtx).
- Already clean: http (reqwest/hyper; its `netidx` dep is value-alias
  only), graphix-package, the `mod x dynamic` machinery (source is a
  graphix EXPRESSION — netidx-free; dynamic-over-netidx keeps working
  because the source expr can call net::subscribe).

The generic machinery the net package builds on ALREADY EXISTS and has
three working precedents: `sys::watch` (notify events →
`ctx.rt.watch`), `http::server` (long-lived serve loop → CustomBuiltinType
with reply channels), `db::subscribe` (sled watch_prefix pump → watch
channel — the closest template). Plus `EvalCachedAsync` (one-shot
async builtins: tcp/fs) and `LibState` (per-ExecCtx typed state bag,
seeded by the embedder before registration — the `ProgramArgs`
precedent).

## Phase 1 — ModuleResolver becomes a trait object

Replace the enum (resolver.rs:52-57) with:

```rust
pub trait ModuleLoader: Send + Sync {
    fn resolve(&self, path: &Path, ...) -> impl Future<...>;  // async
    // relative-include base / buffer-overrides hooks as needed by the
    // 5 existing match sites (resolver.rs:463-491, 713-741)
}
```

- `VFS` and `Files` loaders stay in-core (netidx-free).
- The `Netidx` loader moves to the net package (Atlas's requirement).
- `GXConfig.resolvers: Vec<ModuleResolver>` → `Vec<Arc<dyn
  ModuleLoader>>`.
- `Source::Netidx(Path)` is netidx-free by type (netidx-core Path);
  keep the variant, loaders interpret it. gx.rs's direct
  `subscribe_nondurable_one` fetch (gx.rs:804-823) routes through the
  loader list instead.
- `parse_env` (GRAPHIX_MODPATH): the `file:` arm stays core; scheme →
  loader-factory becomes a registry the embedder/packages populate
  (the shell registers the net package's `netidx:` factory).

Independently landable; tree green.

## Phase 2 — de-netidx the Event, the Rt trait, GXRt, and the loop

- Delete `Event.{netidx, writes, rpc_calls}`; delete the 11 Rt
  methods; delete GXRt's networking fields and the gx.rs arms/commit/
  grace/GC. `GXHandle::subscriber()` deleted.
- Compiler drops `netidx` + `netidx-protocols` from Cargo.toml after
  the mechanical alias rewrite (~40 value-layer `use netidx::{path,
  utils, publisher::Typ, subscriber::Value, protocol::value}` →
  `netidx_core::*` / `netidx_value::*`). Same for graphix-rt. The
  `krb5_iov` feature closure moves to the net package/shell.
- sys/net.rs is deleted in the same commit (Phase 3 recreates it in
  the new package) — the tree can be red between 2 and 3 per the
  standing refactor rule, or 2+3 land as one arc.

## Phase 3 — graphix-package-net (the new home)

New crate, `defpackage!`-standard. Proposed module path: top-level
`net` (breaking rename from `sys::net`; pre-release, ~27 book files +
examples/tests to update — DECISION 1 below).

**NetState in `ctx.libstate`** (created lazily on first use via
`get_or_else`):
- `OnceCell<Subscriber>` / `OnceCell<Publisher>` — built from the
  `NetConfig` libstate entry (see below); Publisher build is async →
  first-use materialization runs through the same spawn machinery the
  builtins already use. No side-runtime tricks: package code runs on
  the runtime and can spawn.
- Subscription pump: ONE `watch_var`-registered channel; the pump
  task translates netidx update batches → `(BindId, Value)` batches
  (SubId→BindId routing owned by NetState). Exactly db::subscribe's
  shape.
- Publish: `Val` handles + an UpdateBatch owned by NetState; the
  update builtin queues and pings a flusher task over a channel
  (try_send from sync update), which coalesces and commits. This
  replaces the end-of-cycle commit hook — near-identical batching in
  practice (DECISION 2).
- Write requests: publisher writes → pump → a `CustomBuiltinType`
  event (http::server's reply-channel pattern).
- RPC server: `RpcCall` (with reply) → CustomBuiltinType events over
  a watch channel. RPC client: spawn_var per call (+ the 60s client
  GC as a NetState timer task). list/list_table: EvalCachedAsync +
  ChangeTracker per Apply + the read-your-writes `publisher.flushed()
  .await` barrier (the one durable insight from the abandoned
  lazy-netidx branch — without it list races this process's own
  registrations).
- Unsubscribe grace (60s Dval hold): NetState timer task.

**NetConfig — the config hook** (Eric's flexibility win):
- A libstate entry seeded by the embedder BEFORE registration
  (ProgramArgs precedent): `Ready(Publisher, Subscriber)` |
  `Config { path/auth/bind }` | `Internal` (InternalOnly on demand —
  tests) | `Disabled` (net builtins return a catchable error).
- PLUS a language-level hook: `net::configure(...)` builtin to load a
  config from a non-standard location at runtime, before first use.

## Phase 4 — consumers

- **shell**: keeps its netidx Params (it IS the netidx-aware
  embedder); builds NetConfig and seeds libstate; `--no-netidx` →
  seeds nothing (package defaults to `Internal` on demand — or
  `Disabled`, DECISION 3). ShellBuilder loses publisher/subscriber
  fields. `netidx:` file prefix → the net package's loader factory.
- **testing.rs**: TestCtx loses InternalOnly entirely; net round-trip
  tests seed `NetConfig::Internal`. Fuzz children: zero network. The
  port ceiling dies structurally; soak returns to full PAR with no
  sysctl.
- **gui data_table**: the one Rt-bypassing consumer
  (`gx.subscriber()`, data_table/mod.rs:277). Mechanism: ONE new
  generic handle hook — `GXHandle::with_ctx(FnOnce(&mut ExecCtx))`
  (netidx-free, generally useful) — through which gui (now depending
  on package-net) reads NetState's subscriber. (DECISION 4.)
- **lsp**: `GXRt` with no netidx at all — nothing to construct.
- **book/examples**: path updates if `net` renames; the dynamic-module
  chapter unchanged in substance (source exprs still use
  net::subscribe).

## Phase 5 — gates

Full workspace suite (net round-trips + data_table + dynamic-module
tests are the sensitive ones), fuzz regress + selfcheck + detcheck,
examples_compile (InternalOnly-free), benches (the per-process compile
baseline should drop a few more ms — resolver spinup + if_addrs gone
from every fuzz child), then a fresh full-PAR soak campaign — the
ceiling should be gone with TIME_WAIT flat; the monitor's TIME_WAIT
line verifies. Book rebuild.

## Decisions for Eric

1. Module path: top-level `net` (rec) vs keeping `sys::net` (less
   churn, but net stays inside package-sys or needs cross-package
   module mounting).
2. Publish batching: package-side coalescing flusher (rec) vs adding a
   generic post-cycle hook to the core for exact end-of-cycle commit
   semantics.
3. `--no-netidx` semantics: `Internal`-on-demand (today's behavior,
   rec) vs `Disabled` (hard error from net builtins).
4. data_table access: `GXHandle::with_ctx` generic hook (rec) vs a
   dedicated net-package handle API.

## Sequencing / ops

Phases 1 → (2+3 as one arc) → 4 → 5; differential gates after each
landing. Soak stays down until Phase 5 (or bridge with the sysctl if
wanted). The jul22 lazy-netidx branch stays stashed
(`git stash list`: "lazy-netidx retrofit") — superseded; its
`flushed()` insight is folded into Phase 3.

## Decisions RESOLVED (Eric, 2026-07-22)

1. Module path: KEEP `sys::net` — net.rs and net.gx/gxi stay in
   graphix-package-sys (which keeps its netidx dep); no new crate.
   The extraction's goal is CORE cleanliness (compiler + rt), not
   sys-crate purity. The `netidx:` ModuleLoader factory also lives in
   package-sys.
2. Publish batching: package-side coalescing flusher.
3. Unseeded default: Internal-on-demand (InternalOnly materialized in
   NetState on first net use).
4. data_table: NO new core hook — netidx init is centralized in
   NetState in `ctx.libstate`; any library reads it at a ctx-bearing
   point. The data_table BUILTIN (which has &mut ExecCtx) clones the
   subscriber out of NetState at widget construction and threads it
   into the widget, replacing `gx.subscriber()`. NetState is defined
   in package-sys; gui depends on it.

## Status: LANDED (2026-07-22/23)

- Phase 1 dd0d71af: ModuleResolver trait; netidx loader in
  package-sys (loader.rs).
- Phase 3 cf919be6 (+ace02ca6): sys::net on package-owned NetState
  (netstate.rs) — pump with shared-Dval fan-out routing,
  CustomBuiltinType write/rpc events, package-side flusher, Internal-
  on-demand on a side thread. NetConfig/NetTimeouts in package-core.
- Phase 2 05617a51: the core cut. graphix-compiler and graphix-rt
  have NO netidx/netidx-protocols dependency (net -527 lines).
  GXRt::new() takes no args; GXHandle::with_ctx (generic ToGX
  closure) is the handle-side bridge to ctx.libstate — needed
  because the gui data_table thread has no ctx (amends decision 4:
  the data flows via libstate as ruled, with_ctx is just the
  accessor).
- TIME_WAIT flat at 1 after fuzz bursts; the port ceiling is
  structurally dead; soak restored to full PAR.

## Shell API tidy (2026-07-23, Eric's design)

The shell *library* is now netidx-agnostic too. ShellBuilder's
netidx-specific fields (`net_config`, `publish_timeout`,
`resolve_timeout`) are replaced by two generic mechanisms:

- `setup_context: Option<SetupContext<X>>` — a `FnOnce(&mut
  ExecCtx)` run in `Shell::init` right after context creation. The
  CLI (main.rs, the netidx-aware embedder) seeds `NetConfig` and
  `NetTimeouts` there; any package's embedder-seeded libstate entry
  uses the same hook.
- `resolver_factories` passthrough to `GXConfig` — the CLI registers
  the sys package's `netidx:` factory itself.

`ResolverFactory` now receives `&mut LibState` (parse_modpath runs
inside `GX::new` with the ctx in hand), and the netidx factory reads
its config from libstate at registration time (Eric's call): the raw
handles live in a standalone shared libstate entry `NetHandles`
(package-sys netstate.rs) that BOTH the module loader and
`NetState` materialize through, whichever touches netidx first —
one universe, no ordering constraint. This retires the "netidx:
MODPATH only under NetConfig::Ready" limitation: the factory
registers unconditionally and materializes from whatever NetConfig
is seeded (Internal when unseeded).

Deferred (small, non-blocking): the `net::configure(...)` language-
level config hook (only the publish timeout is currently meaningful
global state — `NetTimeouts.subscribe` was trimmed as dead, and
per-call `#timeout` args are the graphix-shaped alternative for
subscribe/rpc); the hollow `krb5_iov` feature stubs left in
compiler/rt for feature-unification compatibility.
