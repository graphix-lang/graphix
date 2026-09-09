# The core is network-free: netidx lives in `sys::net`

Status: built 2026-07
Pins: `stdlib/graphix-tests/src/lib_tests/net.rs` (`net_rpc0`, `net_subscribe_arm_rewake`, `net_publish_arm_rewake`), `graphix-shell/tests/examples_compile.rs`, `stdlib/graphix-package-gui/src/widgets/data_table/`

## The rule

`graphix-compiler` and `graphix-rt` depend only on netidx's VALUE layer
(`netidx-core` for `Path`/Pack/`atomic_id`, `netidx-value` for `Value`
and `Type`). The networking crates appear only in stdlib packages —
`graphix-package-sys` owns netidx end to end. A runtime that never
touches `sys::net` has no network at all: tests, fuzz children,
`--check` and the LSP construct nothing.

Netidx was central because graphix was designed as the embedded
language of Atlas (netidx-browser), and every runtime was a netidx node
— resolver, publisher and subscriber per process. That made the
fuzz-soak's port exhaustion a ceiling on the whole fleet, and an
attempt to make the node lazy inside the core produced two novel races
in an hour: the architecture was talking. Atlas still loads graphix
code over netidx; that is a module loader provided by the package, not
a hardwired resolver variant.

## The mechanisms

**Module loading is a trait object.** `ModuleResolver`
(`graphix-compiler/src/expr/resolver.rs`) is async `resolve`/
`for_source`/`fetch_source`; `VfsResolver` and `FilesResolver` live in
the core, `NetidxResolver` in `graphix-package-sys/src/loader.rs`.
`GRAPHIX_MODPATH` schemes map to loaders through a `ResolverFactory`
registry the embedder populates; a factory receives `&mut LibState`, so
a package's factory shares state with its builtins.

**`NetState` in `ctx.libstate`** (`graphix-package-sys/src/netstate.rs`)
is the whole `sys::net` surface over the generic conduits every package
already has (`Rt::watch_var`, `spawn_var`, `CustomBuiltinType` events):

- one subscription pump translating netidx update batches into
  `(BindId, Value)` batches, with shared-Dval fan-out routing — netidx
  SHARES Dvals by path, so several subscribers of one path ride one
  Dval;
- publisher writes and RPC-server calls as `CustomBuiltinType` events
  carrying reply channels (the `http::server` pattern);
- a package-side coalescing publish flusher: the update builtin queues
  and pings a task that coalesces and commits, in place of a core
  end-of-cycle commit hook — near-identical batching without a hook
  every embedder would have to run;
- a 60s Dval unsubscribe graveyard and an on-use-GC'd RPC client
  cache;
- `list`/`list_table` wait on `publisher.flushed()` before reading, so
  they see this process's own registrations.

**`NetHandles`** is a standalone shared libstate entry holding the raw
publisher/subscriber. BOTH the module loader and `NetState`
materialize through it, whichever touches netidx first — one universe
per context, no ordering constraint between "load a module over
netidx" and "subscribe". Materialization reads the seeded `NetConfig`
(defined in package-core: `Ready(publisher, subscriber)` / `Config` /
`Internal`); unseeded defaults to `Internal`, a process-internal netidx
built on demand on a dedicated side thread, which is what test
contexts get.

**The shell library is netidx-agnostic.** `ShellBuilder::setup_context`
is a `FnOnce(&mut ExecCtx)` run at init — the generic hook for seeding
any package's embedder-owned libstate entry — and `resolver_factories`
passes scheme registrations through to `GXConfig`. The CLI (`main.rs`)
is the netidx-aware embedder: it seeds `NetConfig`/`NetTimeouts` in the
hook and registers the `netidx:` factory. `GXHandle::with_ctx` (a boxed
closure delivered to the runtime) is the handle-side bridge to
`ctx.libstate` for code with no context in hand — the gui `data_table`
fetches the subscriber through it, replacing the old
`GXHandle::subscriber()`.

## Decisions

- **`sys::net` stays where it is** rather than becoming a top-level
  `net` package: the goal was CORE cleanliness, not sys-crate purity,
  and the rename would have churned ~27 book files for no
  architectural gain.
- **Package-side flusher, not a core post-cycle hook:** a hook is one
  more thing every embedder's loop must call; a flusher task needs
  nothing from the loop and batches equally well in practice.
- **Unseeded means `Internal` on demand**, not a hard error: the
  default a test or a script gets should be a working netidx, and a
  `Disabled` mode would make `sys::net` calls a catchable error nobody
  asked for.
- **No dedicated net-package handle API**: netidx init is centralized
  in `NetState`; any library reads it at a ctx-bearing point, and the
  generic `with_ctx` covers the one thread with no ctx.
- **Rejected: lazy netidx inside the core.** The runtime would still
  own a network node and its races; moving ownership to the package
  removes the node from every runtime that does not ask for it.

Measured after the cut: `TIME_WAIT` flat at 1 after fuzz bursts; the
port ceiling is structurally gone. Deferred: a language-level
`net::configure(..)` hook (only the publish timeout is meaningful
global state; per-call `#timeout` arguments are the graphix-shaped
alternative for subscribe/rpc).
