#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
//! A general purpose graphix runtime
//!
//! This module implements a generic graphix runtime suitable for most
//! applications, including applications that implement custom graphix
//! builtins. The graphix interperter is run in a background task, and
//! can be interacted with via a handle. All features of the standard
//! library are supported by this runtime.
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use derive_builder::Builder;
use enumflags2::BitFlags;
use graphix_compiler::{
    BindId, CFlag, Control, Event, ExecCtx, FusionStats, NoUserEvent, Scope, UserEvent,
    env::Env,
    expr::{ExprId, ModPath, ModuleResolver, Source},
    ide::Ide,
    typ::{FnType, Type},
};
use log::error;
use netidx::{
    protocol::valarray::ValArray,
    publisher::{Value, WriteRequest},
    subscriber::{self, SubId},
};
use netidx_core::atomic_id;
use netidx_value::FromValue;
use nohash::IntSet;
use poolshark::global::GPooled;
use serde_derive::{Deserialize, Serialize};
use smallvec::SmallVec;
use std::{fmt, future, sync::Arc, time::Duration};
use tokio::{
    sync::{
        mpsc::{self as tmpsc},
        oneshot,
    },
    task::{self, JoinHandle},
};

mod gx;
mod rt;
use gx::GX;
pub use rt::GXRt;

/// Trait to extend the event loop
///
/// The Graphix event loop has two steps,
/// - update event sources, polls external async event sources like
///   netidx, sockets, files, etc
/// - do cycle, collects all the events and delivers them to the dataflow
///   graph as a batch of "everything that happened"
///
/// As such to extend the event loop you must implement two things. A function
/// to poll your own external event sources, and a function to take the events
/// you got from those sources and represent them to the dataflow graph. You
/// represent them either by setting generic variables (bindid -> value map), or
/// by setting some custom structures that you define as part of your UserEvent
/// implementation.
///
/// Your Graphix builtins can access both your custom structure, to register new
/// event sources, etc, and your custom user event structure, to receive events
/// who's types do not fit nicely as `Value`. If your event payload does fit
/// nicely as a `Value`, then just use a variable.
pub trait GXExt: Default + fmt::Debug + Send + Sync + 'static {
    type UserEvent: UserEvent + Send + Sync + 'static;

    /// Update your custom event sources
    ///
    /// Your `update_sources` MUST be cancel safe.
    fn update_sources(&mut self) -> impl Future<Output = Result<()>> + Send;

    /// Collect events that happened and marshal them into the event structure
    ///
    /// for delivery to the dataflow graph. `do_cycle` will be called, and a
    /// batch of events delivered to the graph until `is_ready` returns false.
    /// It is possible that a call to `update_sources` will result in
    /// multiple calls to `do_cycle`, but it is not guaranteed that
    /// `update_sources` will not be called again before `is_ready`
    /// returns false.
    fn do_cycle(&mut self, event: &mut Event<Self::UserEvent>) -> Result<()>;

    /// Return true if there are events ready to deliver
    fn is_ready(&self) -> bool;

    /// Clear the state
    fn clear(&mut self);

    /// Create and return an empty custom event structure
    fn empty_event(&mut self) -> Self::UserEvent;
}

#[derive(Debug, Default)]
pub struct NoExt;

impl GXExt for NoExt {
    type UserEvent = NoUserEvent;

    async fn update_sources(&mut self) -> Result<()> {
        future::pending().await
    }

    fn do_cycle(&mut self, _event: &mut Event<Self::UserEvent>) -> Result<()> {
        Ok(())
    }

    fn is_ready(&self) -> bool {
        false
    }

    fn clear(&mut self) {}

    fn empty_event(&mut self) -> Self::UserEvent {
        NoUserEvent
    }
}

type UpdateBatch = GPooled<Vec<(SubId, subscriber::Event)>>;
type WriteBatch = GPooled<Vec<WriteRequest>>;

#[derive(Debug)]
pub struct CompExp<X: GXExt> {
    pub id: ExprId,
    pub typ: Type,
    pub output: bool,
    rt: GXHandle<X>,
}

impl<X: GXExt> Drop for CompExp<X> {
    fn drop(&mut self) {
        let _ = self.rt.0.tx.send(ToGX::Delete { id: self.id });
    }
}

#[derive(Debug)]
pub struct CompRes<X: GXExt> {
    pub exprs: SmallVec<[CompExp<X>; 1]>,
    pub env: Env,
}

/// Result of a typecheck-only compile pass. Carries the env as it
/// would be after the source was compiled, plus every IDE side-channel
/// ([`Ide`]) encountered during compilation. The `Ide` collections are
/// `GPooled` so the buffers return to the named pools after crossing the
/// LSP thread boundary, keeping the recompile-per-keystroke loop
/// allocation-free in steady state. `ide` is empty for non-LSP compiles.
#[derive(Debug)]
pub struct CheckResult {
    pub env: Env,
    pub ide: Ide,
}

pub struct Ref<X: GXExt> {
    pub id: ExprId,
    // the most recent value of the variable
    pub last: Option<Value>,
    pub bid: BindId,
    pub target_bid: Option<BindId>,
    pub typ: Type,
    rt: GXHandle<X>,
}

impl<X: GXExt> Drop for Ref<X> {
    fn drop(&mut self) {
        let _ = self.rt.0.tx.send(ToGX::Delete { id: self.id });
    }
}

impl<X: GXExt> Ref<X> {
    /// set the value of the ref `r <-`
    ///
    /// This will cause all nodes dependent on this id to update. This is the
    /// same thing as the `<-` operator in Graphix. This does the same thing as
    /// `GXHandle::set`
    pub fn set<T: Into<Value>>(&mut self, v: T) -> Result<()> {
        let v = v.into();
        self.last = Some(v.clone());
        self.rt.set(self.bid, v)
    }

    /// set the value pointed to by ref `*r <-`
    ///
    /// This will cause all nodes dependent on *id to update. This is the same
    /// as the `*r <-` operator in Graphix. This does the same thing as
    /// `GXHandle::set` using the target id.
    pub fn set_deref<T: Into<Value>>(&mut self, v: T) -> Result<()> {
        if let Some(id) = self.target_bid {
            self.rt.set(id, v)?
        }
        Ok(())
    }

    /// Process an update
    ///
    /// If the expr id refers to this ref, then set `last` to `v` and return a
    /// mutable reference to `last`, otherwise return None. This will also
    /// update `last` if the id matches.
    pub fn update(&mut self, id: ExprId, v: &Value) -> Option<&mut Value> {
        if self.id == id {
            self.last = Some(v.clone());
            self.last.as_mut()
        } else {
            None
        }
    }
}

pub struct TRef<X: GXExt, T: FromValue> {
    pub r: Ref<X>,
    pub t: Option<T>,
}

impl<X: GXExt, T: FromValue> TRef<X, T> {
    /// Create a new typed reference from `r`
    ///
    /// If conversion of `r` fails, return an error.
    pub fn new(mut r: Ref<X>) -> Result<Self> {
        let t = r.last.take().map(|v| v.cast_to()).transpose()?;
        Ok(TRef { r, t })
    }

    /// Process an update
    ///
    /// If the expr id refers to this tref, then convert the value into a `T`
    /// update `t` and return a mutable reference to the new `T`, otherwise
    /// return None. Return an Error if the conversion failed.
    pub fn update(&mut self, id: ExprId, v: &Value) -> Result<Option<&mut T>> {
        if self.r.id == id {
            let v = v.clone().cast_to()?;
            self.t = Some(v);
            Ok(self.t.as_mut())
        } else {
            Ok(None)
        }
    }
}

impl<X: GXExt, T: Into<Value> + FromValue + Clone> TRef<X, T> {
    /// set the value of the tref `r <-`
    ///
    /// This will cause all nodes dependent on this id to update. This is the
    /// same thing as the `<-` operator in Graphix. This does the same thing as
    /// `GXHandle::set`
    pub fn set(&mut self, t: T) -> Result<()> {
        self.t = Some(t.clone());
        self.r.set(t)
    }

    /// set the value pointed to by tref `*r <-`
    ///
    /// This will cause all nodes dependent on *id to update. This is the same
    /// as the `*r <-` operator in Graphix. This does the same thing as
    /// `GXHandle::set` using the target id.
    pub fn set_deref(&mut self, t: T) -> Result<()> {
        self.t = Some(t.clone());
        self.r.set_deref(t.into())
    }
}

atomic_id!(CallableId);

pub struct Callable<X: GXExt> {
    rt: GXHandle<X>,
    id: CallableId,
    env: Env,
    pub typ: FnType,
    pub expr: ExprId,
}

impl<X: GXExt> Drop for Callable<X> {
    fn drop(&mut self) {
        let _ = self.rt.0.tx.send(ToGX::DeleteCallable { id: self.id });
    }
}

impl<X: GXExt> Callable<X> {
    /// Get the id of this callable
    pub fn id(&self) -> CallableId {
        self.id
    }

    /// Call the lambda with args
    ///
    /// Argument types and arity will be checked and an error will be returned
    /// if they are wrong. If you call the function more than once before it
    /// returns there is no guarantee that the returns will arrive in the order
    /// of the calls. There is no guarantee that a function must return.
    pub async fn call(&self, args: ValArray) -> Result<()> {
        if self.typ.args.len() != args.len() {
            bail!("expected {} args", self.typ.args.len())
        }
        for (i, (a, v)) in self.typ.args.iter().zip(args.iter()).enumerate() {
            if !a.typ.is_a(&self.env, v) {
                bail!("type mismatch arg {i} expected {}", a.typ)
            }
        }
        self.call_unchecked(args).await
    }

    /// Call the lambda with args. Argument types and arity will NOT
    /// be checked. This can result in a runtime panic, invalid
    /// results, and probably other bad things.
    pub async fn call_unchecked(&self, args: ValArray) -> Result<()> {
        self.rt
            .0
            .tx
            .send(ToGX::Call { id: self.id, args })
            .map_err(|_| anyhow!("runtime is dead"))
    }

    /// Return Some(v) if this update is the return value of the callable
    pub fn update<'a>(&self, id: ExprId, v: &'a Value) -> Option<&'a Value> {
        if self.expr == id { Some(v) } else { None }
    }
}

enum DeferredCall {
    Call(ValArray, oneshot::Sender<Result<()>>),
    CallUnchecked(ValArray, oneshot::Sender<Result<()>>),
}

pub struct NamedCallable<X: GXExt> {
    fname: Ref<X>,
    current: Option<Callable<X>>,
    ids: IntSet<ExprId>,
    deferred: Vec<DeferredCall>,
    h: GXHandle<X>,
}

impl<X: GXExt> NamedCallable<X> {
    /// Update the named callable function
    ///
    /// This method does two things,
    /// - Handle late binding. When the name ref updates to an actual function
    ///   compile the real call site
    /// - Return Ok(Some(v)) when the called function returns
    pub async fn update<'a>(
        &mut self,
        id: ExprId,
        v: &'a Value,
    ) -> Result<Option<&'a Value>> {
        match self.fname.update(id, v) {
            Some(v) => {
                let callable = self.h.compile_callable(v.clone()).await?;
                self.ids.insert(callable.expr);
                for dc in self.deferred.drain(..) {
                    match dc {
                        DeferredCall::Call(args, reply) => {
                            let _ = reply.send(callable.call(args).await);
                        }
                        DeferredCall::CallUnchecked(args, reply) => {
                            let _ = reply.send(callable.call_unchecked(args).await);
                        }
                    }
                }
                self.current = Some(callable);
                Ok(None)
            }
            None if self.ids.contains(&id) => Ok(Some(v)),
            None => Ok(None),
        }
    }

    /// Call the lambda with args
    ///
    /// Argument types and arity will be checked and an error will be returned
    /// if they are wrong. If you call the function more than once before it
    /// returns there is no guarantee that the returns will arrive in the order
    /// of the calls. There is no guarantee that a function must return. In
    /// order to handle late binding you must keep calling `update` while
    /// waiting for this method.
    ///
    /// While a late bound function is unresolved calls will queue internally in
    /// the NamedCallsite and will happen when the function is resolved.
    pub async fn call(&mut self, args: ValArray) -> Result<()> {
        match &self.current {
            Some(c) => c.call(args).await,
            None => {
                let (tx, rx) = oneshot::channel();
                self.deferred.push(DeferredCall::Call(args, tx));
                rx.await?
            }
        }
    }

    /// call the function with the specified args
    ///
    /// Argument types and arity will NOT be checked by this method. If you call
    /// the function more than once before it returns there is no guarantee that
    /// the returns will arrive in the order of the calls. There is no guarantee
    /// that a function must return. In order to handle late binding you must
    /// keep calling `update` while waiting for this method.
    ///
    /// While a late bound function is unresolved calls will queue internally in
    /// the NamedCallsite and will happen when the function is resolved.
    pub async fn call_unchecked(&mut self, args: ValArray) -> Result<()> {
        match &self.current {
            Some(c) => c.call(args).await,
            None => {
                let (tx, rx) = oneshot::channel();
                self.deferred.push(DeferredCall::CallUnchecked(args, tx));
                rx.await?
            }
        }
    }
}

enum ToGX<X: GXExt> {
    GetEnv {
        res: oneshot::Sender<Env>,
    },
    Delete {
        id: ExprId,
    },
    Load {
        path: Source,
        rt: GXHandle<X>,
        res: oneshot::Sender<Result<CompRes<X>>>,
    },
    Check {
        path: Source,
        /// If provided, override the runtime's default resolver chain
        /// for this check only. Used by IDE tooling that needs
        /// project-scoped module resolution without rebuilding the
        /// runtime.
        resolvers: Option<Vec<ModuleResolver>>,
        /// If provided, compile the source under this scope rather
        /// than at the root. Used by IDE tooling editing a graphix
        /// package crate (`graphix-package-<x>`) so its `mod.gx` body
        /// registers under `<x>::` rather than at root, matching the
        /// way the runtime would load it via `mod <x>;` from another
        /// project. Any pre-existing registrations under that scope
        /// are scrubbed from the working env first so the package's
        /// own pre-loaded contents don't trip duplicate-module guards.
        initial_scope: Option<ArcStr>,
        res: oneshot::Sender<Result<CheckResult>>,
    },
    Compile {
        text: ArcStr,
        rt: GXHandle<X>,
        res: oneshot::Sender<Result<CompRes<X>>>,
    },
    CompileCallable {
        id: Value,
        rt: GXHandle<X>,
        res: oneshot::Sender<Result<Callable<X>>>,
    },
    CompileRef {
        id: BindId,
        rt: GXHandle<X>,
        res: oneshot::Sender<Result<Ref<X>>>,
    },
    Set {
        id: BindId,
        v: Value,
    },
    /// Set several variables ATOMICALLY — all delivered in the same
    /// cycle. Two separate `Set` messages can land in different input
    /// batches (and so different cycles) depending on scheduler
    /// timing, which makes "simultaneous" injections nondeterministic;
    /// one `SetMany` is processed in one batch by construction. See
    /// [`GXHandle::set_many`].
    SetMany {
        sets: SmallVec<[(BindId, Value); 4]>,
    },
    Call {
        id: CallableId,
        args: ValArray,
    },
    DeleteCallable {
        id: CallableId,
    },
    /// Introspection: check the compiled root node for `id` against a
    /// `NodeShape` spec. `None` if no node is registered for `id`;
    /// `Some(Ok)` on match; `Some(Err(reason))` on mismatch. Used by
    /// graph-shape tests.
    MatchShape {
        id: ExprId,
        spec: graphix_compiler::node_shape::NodeShape,
        res: oneshot::Sender<Option<std::result::Result<(), String>>>,
    },
    /// Introspection: render the compiled root node for `id` as an
    /// indented text tree (authoring aid for writing a `NodeShape`).
    /// `None` if no node is registered for `id`.
    DescribeShape {
        id: ExprId,
        res: oneshot::Sender<Option<String>>,
    },
    /// Introspection: snapshot the compiler-env + runtime-ref
    /// registry sizes for accounting / leak invariant tests.
    EnvStats {
        res: oneshot::Sender<EnvStats>,
    },
    /// Introspection: snapshot the compile-time fusion outcome
    /// counters accumulated on the `ExecCtx`. See
    /// [`graphix_compiler::FusionStats`].
    FusionStats {
        res: oneshot::Sender<FusionStats>,
    },
    CycleReady {
        res: oneshot::Sender<bool>,
    },
    /// Wait for the next value emitted by `id`, or `None` if the
    /// runtime goes idle (no cycle ready) before any value arrives.
    /// See [`GXHandle::wait_result_or_idle`].
    WaitResultOrIdle {
        id: ExprId,
        res: oneshot::Sender<Option<Value>>,
    },
    /// Start (or restart) runtime-side tracing. See
    /// [`GXHandle::trace_start`].
    TraceStart {
        max_events: usize,
        max_cycles: u64,
    },
    /// Wait for the runtime to go idle (or a trace cap to trip), then
    /// take the recorded segment. `None` = no trace is active. See
    /// [`GXHandle::trace_wait_idle`].
    TraceWaitIdle {
        res: oneshot::Sender<Option<TraceSegment>>,
    },
}

#[derive(Debug, Clone)]
pub enum GXEvent {
    Updated(ExprId, Value),
    Env(Env),
    /// A runtime diagnostic (see [`graphix_compiler::RtDiagnostic`]):
    /// a failure whose value-level outcome is BOTTOM by design (e.g. a
    /// call-depth-limit trip) — nothing arrives on the value channel,
    /// so embedders subscribe here to tell the user which expression
    /// produced nothing and why. `id` is the top-level expression
    /// whose update produced the diagnostic (`None` for the rare trip
    /// outside a node update, e.g. a callable invocation).
    Diagnostic(Option<ExprId>, graphix_compiler::RtDiagnostic),
}

/// One entry in a runtime-side trace (see [`GXHandle::trace_start`]).
/// `cycle` numbers are the runtime's internal cycle counter — they are
/// NOT deterministic across runs (control messages and startup traffic
/// shift them), so consumers must compare cycles RELATIVE to an anchor
/// (the `Compiled` marker, or an input ref's own `Updated`), never
/// absolutely.
#[derive(Debug, Clone)]
pub enum TraceEvent {
    /// A `compile`/`load` completed for the expression `id`; `cycle` is
    /// the cycle that will run next — the program's init cycle — so an
    /// `Updated` produced during init has this same cycle number. This
    /// marker is recorded runtime-side at the moment the nodes are
    /// registered, which is what makes it a sound epoch anchor: the
    /// `Compile` *response* races the compile cycle, the marker does not.
    Compiled { cycle: u64, id: ExprId },
    /// The node registered for `id` emitted `value` during `cycle`.
    Updated { cycle: u64, id: ExprId, value: Value },
}

/// The events recorded since tracing started (or since the previous
/// segment was taken), returned by [`GXHandle::trace_wait_idle`].
#[derive(Debug)]
pub struct TraceSegment {
    pub events: GPooled<Vec<TraceEvent>>,
    /// The runtime cycle at which this segment closed (idle reached or
    /// a cap tripped). Same non-determinism caveat as
    /// [`TraceEvent`] cycles — relative use only.
    pub end_cycle: u64,
    /// The trace hit its active-cycle budget (a runaway program). Once
    /// tripped the trace is permanently quiet — see
    /// [`GXHandle::trace_start`].
    pub capped_cycles: bool,
    /// The trace hit its total event budget. Permanently quiet, as above.
    pub capped_events: bool,
}

/// A snapshot of the compiler-env binding registry and the runtime
/// ref-var registry sizes. Used by accounting-invariant tests that
/// grow and shrink a reactive structure (e.g. an impure HOF array)
/// and assert these counts return to baseline — i.e. that every
/// binding/ref minted by per-slot `clone_rebind` is unbound on
/// teardown, catching a silent `env.by_id` / `by_ref` growth leak.
/// See [`GXHandle::env_stats`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EnvStats {
    /// number of bindings registered in the compiler env (`env.by_id`)
    pub by_id_len: usize,
    /// number of distinct BindIds with at least one live runtime ref
    pub ref_var_keys: usize,
    /// total runtime ref edges (sum of all ref counts across `by_ref`)
    pub ref_var_total: usize,
}

struct GXHandleInner<X: GXExt> {
    tx: tmpsc::UnboundedSender<ToGX<X>>,
    task: JoinHandle<()>,
    subscriber: netidx::subscriber::Subscriber,
    /// Shared (cloned from `ctx.control`) interrupt/abort control. See
    /// [`GXHandle::interrupt`] / [`GXHandle::abort`].
    control: triomphe::Arc<Control>,
}

impl<X: GXExt> Drop for GXHandleInner<X> {
    fn drop(&mut self) {
        // Signal abort first so a wedged `do_cycle` loop breaks and the
        // run loop returns before its next cycle; `task.abort()` alone
        // can't interrupt a wedged *sync* loop (no `.await` to fire at).
        self.control.abort();
        self.task.abort()
    }
}

/// A handle to a running GX instance.
///
/// Drop the handle to shutdown the associated background tasks.
pub struct GXHandle<X: GXExt>(Arc<GXHandleInner<X>>);

impl<X: GXExt> fmt::Debug for GXHandle<X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GXHandle")
    }
}

impl<X: GXExt> Clone for GXHandle<X> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<X: GXExt> GXHandle<X> {
    /// Get a clone of the netidx subscriber used by this runtime.
    pub fn subscriber(&self) -> netidx::subscriber::Subscriber {
        self.0.subscriber.clone()
    }

    /// Request that in-flight loops in the runtime abort to bottom this
    /// cycle — a runaway sync tail-loop or a `map`/`fold`/… over a huge
    /// array won't wedge the runtime thread. The runtime keeps running.
    pub fn interrupt(&self) {
        self.0.control.interrupt()
    }

    /// Shut the runtime down, breaking any wedged loop first. Unlike
    /// dropping the handle, this can be called *while commands are in
    /// flight* (it borrows `&self`, rather than consuming the last
    /// handle), so pending commands resolve to errors instead of
    /// deadlocking against a wedged runtime. Dropping the last handle
    /// also triggers this.
    pub fn abort(&self) {
        self.0.control.abort()
    }

    async fn exec<R, F: FnOnce(oneshot::Sender<R>) -> ToGX<X>>(&self, f: F) -> Result<R> {
        let (tx, rx) = oneshot::channel();
        self.0.tx.send(f(tx)).map_err(|_| anyhow!("runtime is dead"))?;
        Ok(rx.await.map_err(|_| anyhow!("runtime did not respond"))?)
    }

    /// Get a copy of the current graphix environment
    pub async fn get_env(&self) -> Result<Env> {
        self.exec(|res| ToGX::GetEnv { res }).await
    }

    /// Check that a graphix module compiles and type-checks.
    ///
    /// If path starts with `netidx:` the module is loaded from
    /// netidx; otherwise it is loaded from the filesystem (or read
    /// directly if `Source::Internal`). On success returns a
    /// `CheckResult` containing both an env snapshot (as it would be
    /// after the module was compiled) and the set of resolved name
    /// references the compiler observed — useful for IDE tooling
    /// (`textDocument/references`). The runtime's live environment
    /// is not altered — to keep the bindings live, use `compile` or
    /// `load`.
    ///
    /// # Error position info
    ///
    /// Compile and parse failures attach a structured context to the
    /// returned `anyhow::Error` carrying the originating `Origin` and
    /// `SourcePosition`. IDE tooling and other consumers should
    /// `downcast_ref` the error rather than scraping the chain's
    /// message strings:
    ///
    /// - [`graphix_compiler::expr::ErrorContext`] — wraps compile-time
    ///   failures (`bailat!`-style bails and `wrap!`-attached typecheck
    ///   errors). Carries the failing `Expr`, from which `pos` and
    ///   `ori` are read.
    /// - [`graphix_compiler::expr::ParserContext`] — wraps combine
    ///   parser failures with `Origin` + `SourcePosition` fields.
    ///
    /// `anyhow::Error::downcast_ref` walks the context chain via
    /// anyhow's vtable and returns the outermost match, which for the
    /// runtime's compile path is the right one.
    ///
    /// # IDE / LSP usage
    ///
    /// `CheckResult` carries IDE side-channels populated only when
    /// `env.lsp_mode` is set: `references`, `module_references`,
    /// `type_references`, `scope_map`, `sig_links`, and
    /// `module_internals`. The first four record where the compiler saw
    /// each name and where it resolved; `sig_links` ties `val foo` in a
    /// `.gxi` to its `let foo = …` impl in the paired `.gx`;
    /// `module_internals` carries each module's impl-side env so IDE
    /// queries inside a module body can chase impl bind metadata that
    /// isn't visible from the project's external view.
    ///
    /// To check editor buffers without saving, layer a
    /// [`ModuleResolver::BufferOverride`] into the resolver chain — its
    /// override map shadows the on-disk version per path while
    /// preserving `Source::File` origins, so reference matching and
    /// goto-def land on the same file paths as a disk check would.
    pub async fn check(
        &self,
        path: Source,
        initial_scope: Option<ArcStr>,
    ) -> Result<CheckResult> {
        Ok(self
            .exec(|tx| ToGX::Check { path, resolvers: None, initial_scope, res: tx })
            .await??)
    }

    /// Like `check` but overrides the runtime's resolver chain for
    /// this call only. Used by IDE tooling to compile a project
    /// against a project-scoped resolver chain (e.g. `Files(<root>)`)
    /// without having to rebuild the runtime.
    ///
    /// `initial_scope`, when set, scopes the entire compilation under
    /// the given module path (as if the source were the body of a
    /// `mod <scope> { ... }` block). Used by the LSP when editing a
    /// graphix package crate so its modules register under the
    /// package's namespace.
    pub async fn check_with_resolvers(
        &self,
        path: Source,
        resolvers: Vec<ModuleResolver>,
        initial_scope: Option<ArcStr>,
    ) -> Result<CheckResult> {
        Ok(self
            .exec(|tx| ToGX::Check {
                path,
                resolvers: Some(resolvers),
                initial_scope,
                res: tx,
            })
            .await??)
    }

    /// Compile and execute a graphix expression
    ///
    /// If it generates results, they will be sent to all the channels that are
    /// subscribed. When the `CompExp` objects contained in the `CompRes` are
    /// dropped their corresponding expressions will be deleted. Therefore, you
    /// can stop execution of the whole expression by dropping the returned
    /// `CompRes`.
    pub async fn compile(&self, text: ArcStr) -> Result<CompRes<X>> {
        Ok(self.exec(|tx| ToGX::Compile { text, res: tx, rt: self.clone() }).await??)
    }

    /// Load and execute a file or netidx value
    ///
    /// When the `CompExp` objects contained in the `CompRes` are
    /// dropped their corresponding expressions will be
    /// deleted. Therefore, you can stop execution of the whole file
    /// by dropping the returned `CompRes`.
    pub async fn load(&self, path: Source) -> Result<CompRes<X>> {
        Ok(self.exec(|tx| ToGX::Load { path, res: tx, rt: self.clone() }).await??)
    }

    /// Assert the compiled graph's shape: check the root node
    /// registered for `id` (e.g. a `CompExp`'s expr id) against a
    /// [`NodeShape`](graphix_compiler::node_shape::NodeShape) spec.
    /// The walk-and-compare runs in-task against the live post-fusion
    /// graph. `Ok(())` on match; an error (with the mismatch reason or
    /// "no node registered") otherwise.
    pub async fn match_shape(
        &self,
        id: ExprId,
        spec: graphix_compiler::node_shape::NodeShape,
    ) -> Result<()> {
        match self.exec(|res| ToGX::MatchShape { id, spec, res }).await? {
            None => bail!("no node registered for {id:?}"),
            Some(Ok(())) => Ok(()),
            Some(Err(reason)) => bail!("graph shape mismatch: {reason}"),
        }
    }

    /// Render the compiled graph for `id` as an indented text tree —
    /// an authoring aid for writing a `NodeShape` spec. Errors if no
    /// node is registered for `id`.
    pub async fn describe_shape(&self, id: ExprId) -> Result<String> {
        self.exec(|res| ToGX::DescribeShape { id, res })
            .await?
            .ok_or_else(|| anyhow!("no node registered for {id:?}"))
    }

    /// Snapshot the compiler-env binding registry (`env.by_id`) and
    /// the runtime ref-var registry sizes. Used by accounting-
    /// invariant tests: grow a reactive structure, shrink it back,
    /// and assert these counts return to baseline (no per-cycle
    /// binding/ref leak). See [`EnvStats`].
    pub async fn env_stats(&self) -> Result<EnvStats> {
        self.exec(|res| ToGX::EnvStats { res }).await
    }

    /// Snapshot the compile-time fusion outcome counters accumulated
    /// on the `ExecCtx` by every `compile()` this runtime has
    /// dispatched (the root module included). Stats are compile-time
    /// only — they don't change while a program runs — so fetch any
    /// time after the compile of interest. See
    /// [`graphix_compiler::FusionStats`].
    pub async fn fusion_stats(&self) -> Result<FusionStats> {
        self.exec(|res| ToGX::FusionStats { res }).await
    }

    /// Whether the runtime has pending work scheduled for the next
    /// cycle (an updated node, a queued var/custom/net update, a ready
    /// extension, …). For a purely-synchronous program, once this is
    /// `false` the runtime is quiescent and will never produce another
    /// value on its own — so a result that hasn't been emitted by then
    /// is *bottom*. The fuzz oracle uses this to detect a no-result
    /// (div-by-zero, filtered, …) program instantly instead of waiting
    /// out the whole timeout.
    pub async fn cycle_ready(&self) -> Result<bool> {
        self.exec(|res| ToGX::CycleReady { res }).await
    }

    /// Wait for the next value `id` emits, or `None` when the runtime
    /// goes idle before any value arrives. This is the clean quiescence-
    /// aware result wait for synchronous programs: it returns `Some(v)`
    /// the cycle `id` produces `v`, and `None` the moment the runtime has
    /// no pending work (so no future cycle can ever produce a result).
    ///
    /// Note the race a caller must handle: if `id` already emitted before
    /// this call was serviced (e.g. a synchronous program that produced
    /// its value during compile), the runtime is already idle and this
    /// returns `None` — the value is in the event stream, not here. A
    /// caller that needs that already-emitted value should drain the
    /// event subscription on `None`.
    pub async fn wait_result_or_idle(&self, id: ExprId) -> Result<Option<Value>> {
        self.exec(|res| ToGX::WaitResultOrIdle { id, res }).await
    }

    /// Start (or restart) runtime-side tracing. While a trace is
    /// active, every value emitted by a registered node is recorded as
    /// a [`TraceEvent::Updated`] and every `compile`/`load` records a
    /// [`TraceEvent::Compiled`] anchor. Segments are taken with
    /// [`trace_wait_idle`](Self::trace_wait_idle). Restarting discards
    /// any recorded events (and cancels a pending waiter). Tracing
    /// costs one branch per emitted value when off.
    ///
    /// Both budgets are declared here, up front, rather than per wait:
    /// recording must be a pure function of the traced program's own
    /// event stream for a trace to be comparable across two runs, and a
    /// per-wait budget would cut a runaway program's recording at a
    /// point that depends on when the wait message happened to arrive.
    /// `max_events` bounds the total events recorded across the whole
    /// trace; `max_cycles` bounds the ACTIVE cycles (cycles in which at
    /// least one traced node emitted — control-message cycles don't
    /// count) per segment. When either budget is exhausted the trace
    /// goes PERMANENTLY quiet (the segment reports `capped_*`), so
    /// later segments of a capped trace are deterministically empty.
    pub fn trace_start(&self, max_events: usize, max_cycles: u64) -> Result<()> {
        self.0
            .tx
            .send(ToGX::TraceStart { max_events, max_cycles })
            .map_err(|_| anyhow!("runtime is dead"))
    }

    /// Wait until the runtime goes idle (no pending work — quiescent)
    /// or a trace cap trips, then take everything recorded since the
    /// trace started (or since the previous segment was taken). Unlike
    /// [`wait_result_or_idle`](Self::wait_result_or_idle) there is no
    /// already-emitted race: a value produced before this call was
    /// serviced is already in the segment. A second concurrent call
    /// supersedes the first (which resolves as an error). Errors if no
    /// trace is active.
    pub async fn trace_wait_idle(&self) -> Result<TraceSegment> {
        match self.exec(|res| ToGX::TraceWaitIdle { res }).await? {
            Some(seg) => Ok(seg),
            None => bail!("no trace is active (call trace_start first)"),
        }
    }

    /// Compile a callable interface to a lambda id
    ///
    /// This is how you call a lambda directly from rust. When the returned
    /// `Callable` is dropped the associated callsite will be delete.
    pub async fn compile_callable(&self, id: Value) -> Result<Callable<X>> {
        Ok(self
            .exec(|tx| ToGX::CompileCallable { id, rt: self.clone(), res: tx })
            .await??)
    }

    /// Compile a callable interface to a late bound function by name
    ///
    /// This allows you to call a function by name. Because of late binding it
    /// has some additional complexity (though less than implementing it
    /// yourself). You must call `update` on `NamedCallable` when you recieve
    /// updates from the runtime in order to drive late binding. `update` will
    /// also return `Some` when one of your function calls returns.
    pub async fn compile_callable_by_name(
        &self,
        env: &Env,
        scope: &Scope,
        name: &ModPath,
    ) -> Result<NamedCallable<X>> {
        let r = self.compile_ref_by_name(env, scope, name).await?;
        match &r.typ {
            Type::Fn(_) => (),
            t => bail!(
                "{name} in scope {} has type {t}. expected a function",
                scope.lexical
            ),
        }
        Ok(NamedCallable {
            fname: r,
            current: None,
            ids: IntSet::default(),
            deferred: vec![],
            h: self.clone(),
        })
    }

    /// Compile a ref to a bind id
    ///
    /// This will NOT return an error if the id isn't in the environment.
    pub async fn compile_ref(&self, id: impl Into<BindId>) -> Result<Ref<X>> {
        Ok(self
            .exec(|tx| ToGX::CompileRef { id: id.into(), res: tx, rt: self.clone() })
            .await??)
    }

    /// Compile a ref to a name
    ///
    /// Return an error if the name does not exist in the environment
    pub async fn compile_ref_by_name(
        &self,
        env: &Env,
        scope: &Scope,
        name: &ModPath,
    ) -> Result<Ref<X>> {
        let id = env
            .lookup_bind(&scope.lexical, name)
            .ok_or_else(|| anyhow!("no such value {name} in scope {}", scope.lexical))?
            .1
            .id;
        self.compile_ref(id).await
    }

    /// Set the variable idenfified by `id` to `v`
    ///
    /// triggering updates of all dependent node trees. This does the same thing
    /// as`Ref::set` and `TRef::set`
    pub fn set<T: Into<Value>>(&self, id: BindId, v: T) -> Result<()> {
        let v = v.into();
        self.0.tx.send(ToGX::Set { id, v }).map_err(|_| anyhow!("runtime is dead"))
    }

    /// Set several variables ATOMICALLY: every update is delivered to
    /// the graph in the SAME cycle. Separate [`set`](Self::set) calls
    /// give no such guarantee — the messages can be batched into
    /// different cycles depending on scheduler timing — so any caller
    /// that needs simultaneity (e.g. the fuzzer's injection epochs)
    /// must use this.
    pub fn set_many(
        &self,
        sets: impl IntoIterator<Item = (BindId, Value)>,
    ) -> Result<()> {
        let sets: SmallVec<[(BindId, Value); 4]> = sets.into_iter().collect();
        self.0.tx.send(ToGX::SetMany { sets }).map_err(|_| anyhow!("runtime is dead"))
    }

    /// Call a callable by id with the given arguments
    ///
    /// This is a fire-and-forget call that does not wait for the result.
    /// Unlike `Callable::call`, no type or arity checking is performed.
    pub fn call(&self, id: CallableId, args: ValArray) -> Result<()> {
        self.0.tx.send(ToGX::Call { id, args }).map_err(|_| anyhow!("runtime is dead"))
    }
}

#[derive(Builder)]
#[builder(pattern = "owned")]
pub struct GXConfig<X: GXExt> {
    /// The subscribe timeout to use when resolving modules in
    /// netidx. Resolution will fail if the subscription does not
    /// succeed before this timeout elapses.
    #[builder(setter(strip_option), default)]
    resolve_timeout: Option<Duration>,
    /// The publish timeout to use when sending published batches. Default None.
    #[builder(setter(strip_option), default)]
    publish_timeout: Option<Duration>,
    /// The execution context with any builtins already registered
    ctx: ExecCtx<GXRt<X>, X::UserEvent>,
    /// The text of the root module
    #[builder(setter(strip_option), default)]
    root: Option<ArcStr>,
    /// The set of module resolvers to use when resolving loaded modules
    #[builder(default)]
    resolvers: Vec<ModuleResolver>,
    /// The channel that will receive events from the runtime
    sub: tmpsc::Sender<GPooled<Vec<GXEvent>>>,
    /// The set of compiler flags. Default empty.
    #[builder(default)]
    flags: BitFlags<CFlag>,
    /// If true, populate IDE side-channels (`ide_binds`,
    /// references, module references, scope map, type-ref sink) on
    /// every compile and check. Carries a per-compile cost; only
    /// the LSP backend should set it.
    #[builder(default)]
    lsp_mode: bool,
}

impl<X: GXExt> GXConfig<X> {
    /// Create a new config
    pub fn builder(
        ctx: ExecCtx<GXRt<X>, X::UserEvent>,
        sub: tmpsc::Sender<GPooled<Vec<GXEvent>>>,
    ) -> GXConfigBuilder<X> {
        GXConfigBuilder::default().ctx(ctx).sub(sub)
    }

    /// Start the graphix runtime with the specified config,
    ///
    /// return a handle capable of interacting with it. root is the text of the
    /// root module you wish to initially load. This will define the environment
    /// for the rest of the code compiled by this runtime. The runtime starts
    /// completely empty, with only the language, no core library, no standard
    /// library. To build a runtime with the full standard library and nothing
    /// else simply pass the output of `graphix_stdlib::register` to start.
    pub async fn start(self) -> Result<GXHandle<X>> {
        let subscriber = self.ctx.rt.subscriber.clone();
        // Clone the interrupt/abort control before `self` moves into the
        // spawned task, so the handle and the running `ExecCtx` share it.
        let control = self.ctx.control.clone();
        let (init_tx, init_rx) = oneshot::channel();
        let (tx, rx) = tmpsc::unbounded_channel();
        let task = task::spawn(async move {
            match GX::new(self).await {
                Ok(bs) => {
                    let _ = init_tx.send(Ok(()));
                    if let Err(e) = bs.run(rx).await {
                        error!("run loop exited with error {e:?}")
                    }
                }
                Err(e) => {
                    let _ = init_tx.send(Err(e));
                }
            };
        });
        init_rx.await??;
        Ok(GXHandle(Arc::new(GXHandleInner { tx, task, subscriber, control })))
    }
}
