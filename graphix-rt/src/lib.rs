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
    expr::{ExprId, ModPath, ResolverFactory, ResolverRef, Source},
    ide::Ide,
    typ::{FnType, Type},
};
use log::error;
use netidx_core::atomic_id;
use netidx_value::FromValue;
use netidx_value::{ValArray, Value};
use nohash::IntSet;
use poolshark::global::{GPooled, Pool};
use serde_derive::{Deserialize, Serialize};
use smallvec::SmallVec;
use std::{fmt, future, sync::Arc};
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

/// Result of a typecheck-only compile pass: the env as it would be after
/// the source compiled, plus the IDE side-channels ([`Ide`]) seen during
/// compilation (empty for non-LSP compiles).
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
    /// Run a closure with the runtime's ExecCtx; the bridge for
    /// handle-side consumers that need `ctx.libstate`.
    WithCtx {
        f: Box<dyn FnOnce(&mut ExecCtx<GXRt<X>, X::UserEvent>) + Send>,
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
        /// Override the runtime's resolver chain for this check only.
        resolvers: Option<Vec<ResolverRef>>,
        /// Compile the source under this module scope rather than at the
        /// root; pre-existing registrations under that scope are scrubbed
        /// from the working env first.
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
    /// Set several variables atomically, all delivered in the same cycle.
    /// See [`GXHandle::set_many`].
    SetMany {
        sets: GPooled<Vec<(BindId, Value)>>,
    },
    Call {
        id: CallableId,
        args: ValArray,
    },
    DeleteCallable {
        id: CallableId,
    },
    /// Check the compiled root node for `id` against a `NodeShape` spec.
    /// `None` if no node is registered for `id`.
    MatchShape {
        id: ExprId,
        spec: graphix_compiler::node_shape::NodeShape,
        res: oneshot::Sender<Option<std::result::Result<(), String>>>,
    },
    /// Render the compiled root node for `id` as an indented text tree.
    /// `None` if no node is registered for `id`.
    DescribeShape {
        id: ExprId,
        res: oneshot::Sender<Option<String>>,
    },
    /// Snapshot the compiler-env and runtime-ref registry sizes.
    EnvStats {
        res: oneshot::Sender<EnvStats>,
    },
    /// Snapshot the fusion outcome counters. See
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
    /// A runtime diagnostic (see [`graphix_compiler::RtDiagnostic`]) for a
    /// failure whose outcome is bottom, so nothing arrives on the value
    /// channel. `id` is the top-level expression whose update produced it
    /// (`None` outside a node update, e.g. a callable invocation).
    Diagnostic(Option<ExprId>, graphix_compiler::RtDiagnostic),
}

/// One entry in a runtime-side trace (see [`GXHandle::trace_start`]).
/// `cycle` numbers are not deterministic across runs; compare them
/// relative to an anchor (the `Compiled` marker, or an input ref's own
/// `Updated`), never absolutely.
#[derive(Debug, Clone)]
pub enum TraceEvent {
    /// A `compile`/`load` completed for the expression `id`; `cycle` is
    /// the program's init cycle, so an `Updated` produced during init has
    /// the same cycle number. Recorded when the nodes are registered.
    Compiled { cycle: u64, id: ExprId },
    /// The node registered for `id` emitted `value` during `cycle`.
    Updated { cycle: u64, id: ExprId, value: Value },
}

/// The events recorded since tracing started (or since the previous
/// segment was taken), returned by [`GXHandle::trace_wait_idle`].
#[derive(Debug)]
pub struct TraceSegment {
    pub events: GPooled<Vec<TraceEvent>>,
    /// The runtime cycle at which this segment closed; relative use only.
    pub end_cycle: u64,
    /// The trace hit its worked-cycle budget and is permanently quiet.
    pub capped_cycles: bool,
    /// The trace hit its total event budget. Permanently quiet, as above.
    pub capped_events: bool,
}

/// A snapshot of the compiler-env binding registry and the runtime
/// ref-var registry sizes, for tests that grow and shrink a reactive
/// structure and assert the counts return to baseline. See
/// [`GXHandle::env_stats`].
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
    /// Shared (cloned from `ctx.control`) interrupt/abort control. See
    /// [`GXHandle::interrupt`] / [`GXHandle::abort`].
    control: triomphe::Arc<Control>,
}

impl<X: GXExt> Drop for GXHandleInner<X> {
    fn drop(&mut self) {
        // Signal abort first so a wedged `do_cycle` loop breaks;
        // `task.abort()` alone cannot interrupt a sync loop.
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
    /// Run `f` with the runtime's `ExecCtx` on the runtime task and
    /// return its result; the accessor for handle-side consumers of
    /// `ctx.libstate`.
    pub async fn with_ctx<T, F>(&self, f: F) -> Result<T>
    where
        T: Send + 'static,
        F: FnOnce(&mut ExecCtx<GXRt<X>, X::UserEvent>) -> T + Send + 'static,
    {
        let (tx, rx) = oneshot::channel();
        self.0
            .tx
            .send(ToGX::WithCtx {
                f: Box::new(move |ctx| {
                    let _ = tx.send(f(ctx));
                }),
            })
            .map_err(|_| anyhow!("runtime is shut down"))?;
        Ok(rx.await?)
    }

    /// Abort in-flight loops in the runtime to bottom this cycle; the
    /// runtime keeps running. This is the only containment for a program
    /// that spins forever inside one cycle, which the language permits.
    /// Nothing arms it but a human or an embedder: the shell arms it on
    /// Ctrl-C, and an embedder that wants a watchdog builds one:
    ///
    /// ```ignore
    /// let gx = handle.clone();
    /// tokio::spawn(async move {
    ///     loop {
    ///         tokio::time::sleep(Duration::from_secs(5)).await;
    ///         if gx.get_env().now_or_never().is_none() {
    ///             log::error!("graphix program exceeded its budget; interrupting");
    ///             gx.interrupt();
    ///         }
    ///     }
    /// });
    /// ```
    ///
    /// The aborted cycle rides its last result and re-fires next cycle,
    /// so a wrongly-fired watchdog costs a cycle, not correctness.
    pub fn interrupt(&self) {
        self.0.control.interrupt()
    }

    /// True if the stack budget (`graphix_compiler::set_stack_budget`)
    /// aborted this runtime.
    pub fn budget_aborted(&self) -> bool {
        self.0.control.budget_aborted()
    }

    /// Shut the runtime down, breaking any wedged loop first. Borrows
    /// `&self`, so it can be called while commands are in flight; pending
    /// commands resolve to errors. Dropping the last handle also does this.
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

    /// Check that a graphix module compiles and type-checks without
    /// altering the runtime's live environment. A `netidx:` path loads
    /// from netidx; otherwise from the filesystem (or the text itself for
    /// `Source::Internal`).
    ///
    /// Compile and parse failures carry structured context on the
    /// `anyhow::Error`: `downcast_ref` to
    /// [`graphix_compiler::expr::ErrorContext`] (compile-time failures,
    /// carrying the failing `Expr`) or
    /// [`graphix_compiler::expr::ParserContext`] (`Origin` +
    /// `SourcePosition`) rather than scraping messages.
    ///
    /// The `CheckResult` IDE side-channels are populated only when
    /// `env.lsp_mode` is set. To check unsaved editor buffers, layer a
    /// buffer-override resolver into the resolver chain.
    pub async fn check(
        &self,
        path: Source,
        initial_scope: Option<ArcStr>,
    ) -> Result<CheckResult> {
        Ok(self
            .exec(|tx| ToGX::Check { path, resolvers: None, initial_scope, res: tx })
            .await??)
    }

    /// Like `check` but overrides the runtime's resolver chain for this
    /// call only. `initial_scope`, when set, compiles the source as the
    /// body of `mod <scope> { ... }`.
    pub async fn check_with_resolvers(
        &self,
        path: Source,
        resolvers: Vec<ResolverRef>,
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

    /// Check the root node registered for `id` against a
    /// [`NodeShape`](graphix_compiler::node_shape::NodeShape) spec, on the
    /// live post-fusion graph. Errors with the mismatch reason or
    /// "no node registered".
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

    /// Render the compiled graph for `id` as an indented text tree.
    /// Errors if no node is registered for `id`.
    pub async fn describe_shape(&self, id: ExprId) -> Result<String> {
        self.exec(|res| ToGX::DescribeShape { id, res })
            .await?
            .ok_or_else(|| anyhow!("no node registered for {id:?}"))
    }

    /// Snapshot the compiler-env binding registry and the runtime ref-var
    /// registry sizes. See [`EnvStats`].
    pub async fn env_stats(&self) -> Result<EnvStats> {
        self.exec(|res| ToGX::EnvStats { res }).await
    }

    /// Snapshot the fusion outcome counters accumulated by every
    /// `compile()` this runtime has dispatched. Compile-time only, so
    /// fetch any time after the compile of interest. See
    /// [`graphix_compiler::FusionStats`].
    pub async fn fusion_stats(&self) -> Result<FusionStats> {
        self.exec(|res| ToGX::FusionStats { res }).await
    }

    /// Whether the runtime has pending work for the next cycle. For a
    /// purely synchronous program, once this is `false` the runtime will
    /// never produce another value on its own.
    pub async fn cycle_ready(&self) -> Result<bool> {
        self.exec(|res| ToGX::CycleReady { res }).await
    }

    /// Wait for the next value `id` emits, or `None` when the runtime goes
    /// idle before any value arrives.
    ///
    /// If `id` already emitted before this call was serviced the runtime
    /// is already idle and this returns `None`; the value is in the event
    /// stream, so drain the event subscription on `None`.
    pub async fn wait_result_or_idle(&self, id: ExprId) -> Result<Option<Value>> {
        self.exec(|res| ToGX::WaitResultOrIdle { id, res }).await
    }

    /// Start (or restart) runtime-side tracing: every value a registered
    /// node emits is recorded as a [`TraceEvent::Updated`] and every
    /// `compile`/`load` records a [`TraceEvent::Compiled`] anchor.
    /// Segments are taken with [`trace_wait_idle`](Self::trace_wait_idle).
    /// Restarting discards recorded events and cancels a pending waiter.
    ///
    /// `max_events` bounds the total events recorded; `max_cycles` bounds
    /// the worked cycles per segment, so a wait resolves even for a
    /// program that never quiesces. Once either budget is exhausted the
    /// trace is permanently quiet (the segment reports `capped_*`).
    pub fn trace_start(&self, max_events: usize, max_cycles: u64) -> Result<()> {
        self.0
            .tx
            .send(ToGX::TraceStart { max_events, max_cycles })
            .map_err(|_| anyhow!("runtime is dead"))
    }

    /// Wait until the runtime goes idle or a trace cap trips, then take
    /// everything recorded since the previous segment. There is no
    /// already-emitted race. A second concurrent call supersedes the
    /// first (which resolves as an error). Errors if no trace is active.
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
            .lookup_bind(&scope.lexical, name)?
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

    /// Set several variables atomically: every update is delivered to
    /// the graph in the same cycle. Separate [`set`](Self::set) calls
    /// give no such guarantee.
    pub fn set_many(
        &self,
        sets: impl IntoIterator<Item = (BindId, Value)>,
    ) -> Result<()> {
        static SETS: std::sync::LazyLock<Pool<Vec<(BindId, Value)>>> =
            std::sync::LazyLock::new(|| Pool::new(16, 8192));
        let mut batch = SETS.take();
        batch.extend(sets);
        self.0
            .tx
            .send(ToGX::SetMany { sets: batch })
            .map_err(|_| anyhow!("runtime is dead"))
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
    /// The execution context with any builtins already registered
    ctx: ExecCtx<GXRt<X>, X::UserEvent>,
    /// The text of the root module
    #[builder(setter(strip_option), default)]
    root: Option<ArcStr>,
    /// The set of module resolvers to use when resolving loaded modules
    #[builder(default)]
    resolvers: Vec<ResolverRef>,
    /// GRAPHIX_MODPATH scheme -> resolver factories. `file:` is built in.
    #[builder(default)]
    resolver_factories: ahash::AHashMap<arcstr::ArcStr, ResolverFactory>,
    /// The channel that will receive events from the runtime
    sub: tmpsc::Sender<GPooled<Vec<GXEvent>>>,
    /// The set of compiler flags. Default empty.
    #[builder(default)]
    flags: BitFlags<CFlag>,
    /// Populate IDE side-channels on every compile and check. Carries a
    /// per-compile cost; only the LSP backend should set it.
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
        // The handle and the running `ExecCtx` share the control.
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
        Ok(GXHandle(Arc::new(GXHandleInner { tx, task, control })))
    }
}
