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
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use derive_builder::Builder;
use enumflags2::BitFlags;
use graphix_compiler::{
    BindId, CFlag, Control, Event, ExecState, FusionStats, LambdaId, NoUserEvent, Scope,
    UserEvent,
    env::Env,
    expr::{ExprId, ModPath, Origin, ResolverFactory, ResolverRef, Source},
    ide::Ide,
    image::ProgramRoot,
    node::lambda::LambdaDef,
    typ::{FnType, Type},
};
use log::error;
use netidx_core::atomic_id;
use netidx_value::FromValue;
use netidx_value::{ValArray, Value};
use poolshark::global::{GPooled, Pool};
use serde_derive::{Deserialize, Serialize};
use smallvec::{SmallVec, smallvec};
use std::{fmt, future, path::PathBuf, sync::Arc};
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
/// you got from those sources and represent them to the dataflow graph, in the
/// custom structures you define as part of your UserEvent implementation.
/// `do_cycle` runs after the cycle's variables were delivered, so it fills the
/// user event only; a payload that fits a `Value` goes through a variable the
/// runtime delivers (`Rt::watch_var`, `Rt::spawn_var`), which stores it and
/// wakes its readers.
///
/// Your Graphix builtins can access both your custom structure, to register new
/// event sources, etc, and your custom user event structure, to receive events
/// whose types do not fit nicely as `Value`.
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
        self.rt.set_deref(self.bid, v)
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

/// The bind id a widget value's struct holds in the field `name`: a
/// struct value is an array of `[name, value]` pairs.
#[doc(hidden)]
pub fn field_id(v: &Value, name: &str) -> Result<u64> {
    match v {
        Value::Array(fields) => fields
            .iter()
            .find_map(|f| match f {
                Value::Array(kv)
                    if kv.len() == 2
                        && matches!(&kv[0], Value::String(s) if s == name) =>
                {
                    kv[1].clone().cast_to::<u64>().ok()
                }
                _ => None,
            })
            .ok_or_else(|| anyhow!("no field {name}")),
        _ => bail!("expected a struct with the field {name}, got {v}"),
    }
}

/// A widget's reactive properties, each named once with its type: the
/// struct of `TRef`s, `compile` from a widget value whose fields are the
/// properties' bind ids (it may hold other fields), compiling every ref
/// at once, and `update`, true when one of them took the update.
///
/// ```ignore
/// props! { pub(crate) struct GaugeProps { label: Option<SpanV>, ratio: f64 } }
/// ```
#[macro_export]
macro_rules! props {
    ($vis:vis struct $name:ident { $($field:ident: $ty:ty),* $(,)? }) => {
        $vis struct $name<X: $crate::GXExt> {
            $(pub $field: $crate::TRef<X, $ty>,)*
        }

        impl<X: $crate::GXExt> $name<X> {
            pub async fn compile(
                gx: &$crate::GXHandle<X>,
                v: &netidx::publisher::Value,
            ) -> anyhow::Result<Self> {
                use anyhow::Context as _;
                let names = [$(stringify!($field)),*];
                let refs = futures::future::try_join_all(
                    names.into_iter().map(|name| gx.compile_field(v, name)),
                )
                .await?;
                let mut refs = refs.into_iter();
                Ok(Self {
                    $($field: $crate::TRef::new(refs.next().expect("one ref a field"))
                        .context(stringify!($field))?,)*
                })
            }

            #[allow(unused)]
            pub fn update(
                &mut self,
                id: graphix_compiler::expr::ExprId,
                v: &netidx::publisher::Value,
            ) -> anyhow::Result<bool> {
                use anyhow::Context as _;
                let mut changed = false;
                $(changed |= self.$field.update(id, v).context(stringify!($field))?.is_some();)*
                Ok(changed)
            }
        }
    };
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

    /// Set the value the reference names, as `*r <- t` does.
    pub fn set_deref(&mut self, t: T) -> Result<()> {
        self.t = Some(t.clone());
        self.r.set_deref(t)
    }
}

atomic_id!(CallableId);

pub struct Callable<X: GXExt> {
    rt: GXHandle<X>,
    id: CallableId,
    lambda: LambdaId,
    env: Env,
    pub typ: FnType,
    pub expr: ExprId,
    /// The id a `call_answered` call's answer arrives under.
    pub answer: ExprId,
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

    /// Whether this call site was compiled for the lambda `v`
    pub fn is_for(&self, v: &Value) -> bool {
        v.downcast_ref::<LambdaDef<GXRt<X>, X::UserEvent>>()
            .is_some_and(|l| l.id == self.lambda)
    }

    /// Call the lambda with args
    ///
    /// Argument types and arity will be checked and an error will be returned
    /// if they are wrong. If you call the function more than once before it
    /// returns there is no guarantee that the returns will arrive in the order
    /// of the calls. There is no guarantee that a function must return.
    pub async fn call(&self, args: ValArray) -> Result<()> {
        self.check_args(&args)?;
        self.send(args, false)
    }

    /// Call the lambda and have it answer: at the end of the cycle that
    /// writes `args`, an update under `self.answer` carries what the call
    /// site produced in that cycle, null when it produced nothing. A later
    /// output of the call site answers nothing.
    pub async fn call_answered(&self, args: ValArray) -> Result<()> {
        self.check_args(&args)?;
        self.send(args, true)
    }

    fn check_args(&self, args: &ValArray) -> Result<()> {
        if self.typ.args.len() != args.len() {
            bail!("expected {} args", self.typ.args.len())
        }
        for (i, (a, v)) in self.typ.args.iter().zip(args.iter()).enumerate() {
            if !a.typ.is_a(&self.env, v) {
                bail!("type mismatch arg {i} expected {}", a.typ)
            }
        }
        Ok(())
    }

    fn send(&self, args: ValArray, answered: bool) -> Result<()> {
        self.rt
            .0
            .tx
            .send(ToGX::Call { id: self.id, args, answered })
            .map_err(|_| self.rt.stopped("runtime is dead"))
    }

    /// Call the lambda with args. Argument types and arity will NOT
    /// be checked. This can result in a runtime panic, invalid
    /// results, and probably other bad things.
    pub async fn call_unchecked(&self, args: ValArray) -> Result<()> {
        self.send(args, false)
    }

    /// Return Some(v) if this update is the return value of the callable
    pub fn update<'a>(&self, id: ExprId, v: &'a Value) -> Option<&'a Value> {
        if self.expr == id { Some(v) } else { None }
    }
}

enum ToGX<X: GXExt> {
    GetEnv {
        res: oneshot::Sender<Env>,
    },
    /// Run a closure with the runtime's ExecState; the bridge for
    /// handle-side consumers that need `ctx.libstate`.
    WithCtx {
        f: Box<dyn FnOnce(&mut ExecState<GXRt<X>, X::UserEvent>) + Send>,
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
        /// Record every checked node's type (`Ide::expr_types`).
        expr_types: bool,
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
    SetDeref {
        cell: BindId,
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
        answered: bool,
    },
    DeleteCallable {
        id: CallableId,
    },
    /// Check the compiled root node for `id` against a `NodeShape` spec.
    /// `None` if no node is registered for `id`.
    MatchShape {
        id: ExprId,
        spec: graphix_compiler::node_shape::NodeShape,
        res: oneshot::Sender<Option<Result<()>>>,
    },
    /// Render the compiled root node for `id` as an indented text tree.
    /// `None` if no node is registered for `id`.
    DescribeShape {
        id: ExprId,
        res: oneshot::Sender<Option<String>>,
    },
    /// Snapshot the compiler-env and runtime-ref registry sizes.
    Program {
        res: oneshot::Sender<(Option<Result<ProgramRoot, String>>, Env)>,
    },
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
    /// Answer when the runtime next goes idle. See [`GXHandle::wait_idle`].
    WaitIdle {
        res: oneshot::Sender<()>,
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
    /// The last runtime cycle the segment covers; relative use only.
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
    /// number of variables with a delivery in the runtime store
    pub store_len: usize,
    /// number of lambda definitions the context holds (`lambda_defs`)
    pub lambda_defs_len: usize,
    /// number of seq lowerings the context keeps (`lowered_seqs`)
    pub lowered_seqs_len: usize,
    /// the registration was restored from an image (a bad image compiles
    /// cold instead)
    pub restored: bool,
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
    /// Run `f` with the runtime's `ExecState` on the runtime task and
    /// return its result; the accessor for handle-side consumers of
    /// `ctx.libstate`.
    pub async fn with_ctx<T, F>(&self, f: F) -> Result<T>
    where
        T: Send + 'static,
        F: FnOnce(&mut ExecState<GXRt<X>, X::UserEvent>) -> T + Send + 'static,
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
    // CR claude for eric: [doc-drift] The doc above (lines 654-655) and
    // design/atomic_recursion.md:84-85 say an interrupted cycle 're-fires next cycle,
    // so a wrongly-fired watchdog costs a cycle, not correctness', but nothing
    // re-schedules an interrupted root. GXLambda::update and FusedKernel ride their
    // resident, do_cycle clears `updated`, and the inputs' fires are spent, so the
    // derivation recomputes only when an input fires again. A one-shot trigger's result
    // is therefore lost: at the REPL, a Ctrl-C one second into a 2.7 s `sum(0, go ~
    // 400000000)` leaves `r` with no value for good, while the uninterrupted run prints
    // 80000000200000000 (probe: design/review-2026-10-05/repro/rt-06.py). Either re-run
    // the roots an interrupted cycle updated, with their trigger fires restored, or
    // correct both docs and the watchdog advice. (rt-06)
    pub fn interrupt(&self) {
        self.0.control.interrupt()
    }

    /// The runtime's interrupt, budget and parallel control.
    pub fn control(&self) -> &Control {
        &self.0.control
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
        self.0.tx.send(f(tx)).map_err(|_| self.stopped("runtime is dead"))?;
        Ok(rx.await.map_err(|_| self.stopped("runtime did not respond"))?)
    }

    /// Why the runtime stopped, when its stack budget stopped it, else
    /// `what`.
    pub fn stopped(&self, what: &'static str) -> anyhow::Error {
        if self.budget_aborted() {
            anyhow!(
                "the runtime exceeded its stack budget (GRAPHIX_STACK_BUDGET) and stopped"
            )
        } else {
            anyhow!(what)
        }
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
    /// the runtime is built with `lsp_mode`. To check unsaved editor buffers, layer a
    /// buffer-override resolver into the resolver chain.
    pub async fn check(
        &self,
        path: Source,
        initial_scope: Option<ArcStr>,
    ) -> Result<CheckResult> {
        Ok(self
            .exec(|tx| ToGX::Check {
                path,
                resolvers: None,
                initial_scope,
                expr_types: false,
                res: tx,
            })
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
                expr_types: false,
                res: tx,
            })
            .await??)
    }

    /// Like `check_with_resolvers`, and the result's `ide.expr_types`
    /// holds every checked node's type outside lambda bodies. A fused
    /// region is opaque to it, so check on a runtime with fusion off.
    pub async fn check_with_types(
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
                expr_types: true,
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
            Some(Err(e)) => Err(e.context("graph shape mismatch")),
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

    /// The program the runtime compiled or restored at construction
    /// (`GXConfig::program`), as `load` would have returned it; its
    /// compile error when it failed, `None` when none was configured.
    pub async fn program(&self) -> Result<Option<CompRes<X>>> {
        let (root, env) = self.exec(|res| ToGX::Program { res }).await?;
        match root {
            None => Ok(None),
            Some(Err(e)) => Err(anyhow!("{e}")),
            Some(Ok(r)) => Ok(Some(CompRes {
                exprs: smallvec![CompExp {
                    id: r.id,
                    typ: r.typ,
                    output: r.output,
                    rt: self.clone()
                }],
                env,
            })),
        }
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

    /// Wait until the runtime has no cycle ready, confirmed on a second
    /// pass. Every update of the cycles before it is already in the event
    /// stream. A pending timer, IO or spawned task reads as idle: what it
    /// brings later is not waited for.
    pub async fn wait_idle(&self) -> Result<()> {
        self.exec(|res| ToGX::WaitIdle { res }).await
    }

    /// Start (or restart) runtime-side tracing: every value a registered
    /// node emits is recorded as a [`TraceEvent::Updated`] and every
    /// `compile`/`load` records a [`TraceEvent::Compiled`] anchor.
    /// Segments are taken with [`trace_wait_idle`](Self::trace_wait_idle).
    /// Restarting discards recorded events and cancels a pending waiter.
    ///
    /// `max_events` bounds the events recorded per segment, compile anchors
    /// included; `max_cycles` bounds the worked cycles per segment, so a
    /// wait resolves even for a
    /// program that never quiesces. Once either budget is exhausted the
    /// trace is permanently quiet (the segment reports `capped_*`).
    pub fn trace_start(&self, max_events: usize, max_cycles: u64) -> Result<()> {
        self.0
            .tx
            .send(ToGX::TraceStart { max_events, max_cycles })
            .map_err(|_| self.stopped("runtime is dead"))
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

    /// Point `current` at the lambda `v`, keeping the call site it already
    /// has when `v` is the lambda it was compiled for. A reference to a
    /// struct field fires with every update of the struct, so a handler
    /// read out of a widget record arrives again and again unchanged.
    pub async fn update_callable(
        &self,
        current: &mut Option<Callable<X>>,
        v: Value,
    ) -> Result<()> {
        if current.as_ref().is_none_or(|c| !c.is_for(&v)) {
            *current = Some(self.compile_callable(v).await?);
        }
        Ok(())
    }

    /// Compile a ref to a bind id
    ///
    /// This will NOT return an error if the id isn't in the environment.
    pub async fn compile_ref(&self, id: impl Into<BindId>) -> Result<Ref<X>> {
        Ok(self
            .exec(|tx| ToGX::CompileRef { id: id.into(), res: tx, rt: self.clone() })
            .await??)
    }

    /// Compile a ref to the bind id a widget value's struct holds in its
    /// field `name`.
    pub async fn compile_field(&self, v: &Value, name: &str) -> Result<Ref<X>> {
        self.compile_ref(field_id(v, name)?)
            .await
            .with_context(|| format!("field {name}"))
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
        self.0.tx.send(ToGX::Set { id, v }).map_err(|_| self.stopped("runtime is dead"))
    }

    /// Write `v` through the reference `cell` (`*r <- v`): a place patches
    /// its root, a chained reference sets its binding, a chainless one sets
    /// the cell.
    pub fn set_deref<T: Into<Value>>(&self, cell: BindId, v: T) -> Result<()> {
        let v = v.into();
        self.0
            .tx
            .send(ToGX::SetDeref { cell, v })
            .map_err(|_| self.stopped("runtime is dead"))
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
            .map_err(|_| self.stopped("runtime is dead"))
    }

    /// Call a callable by id with the given arguments
    ///
    /// This is a fire-and-forget call that does not wait for the result.
    /// Unlike `Callable::call`, no type or arity checking is performed.
    pub fn call(&self, id: CallableId, args: ValArray) -> Result<()> {
        self.0
            .tx
            .send(ToGX::Call { id, args, answered: false })
            .map_err(|_| self.stopped("runtime is dead"))
    }
}

/// The module search after a program's own directory: the entries of
/// `GRAPHIX_MODPATH`, then the user's data directory.
pub fn search_path(
    factories: &ahash::AHashMap<ArcStr, ResolverFactory>,
    libstate: &mut graphix_compiler::LibState,
) -> Result<Vec<ResolverRef>> {
    let mut res = match std::env::var("GRAPHIX_MODPATH") {
        Ok(mp) => graphix_compiler::expr::parse_modpath(factories, libstate, &mp)
            .map_err(|e| e.context("GRAPHIX_MODPATH"))?,
        Err(_) => vec![],
    };
    if let Some(dd) = dirs::data_dir() {
        res.push(graphix_compiler::expr::FilesResolver::new(dd.join("graphix"), None));
    }
    Ok(res)
}

/// The directories [`search_path`] searches, in order, for its file
/// entries.
pub fn search_dirs() -> Vec<PathBuf> {
    let mp = std::env::var("GRAPHIX_MODPATH").unwrap_or_default();
    let mut res: Vec<PathBuf> = graphix_compiler::expr::modpath_dirs(&mp).collect();
    res.extend(dirs::data_dir().map(|dd| dd.join("graphix")));
    res
}

/// What a runtime does about its session image: restore the first of
/// `restore` that reads instead of compiling the root, and when none
/// does, send the image of the root it compiled, taken before any
/// cycle, to `save`, so a later start can restore it.
#[derive(Default)]
pub struct RegistrationImage {
    /// each image with what it is, for the warning when it does not read
    pub restore: SmallVec<[(ArcStr, Bytes); 2]>,
    pub save: Option<oneshot::Sender<Result<Bytes>>>,
}

impl RegistrationImage {
    pub fn load(image: Bytes) -> Self {
        Self { restore: smallvec![(arcstr::literal!("the image"), image)], save: None }
    }

    pub fn save(tx: oneshot::Sender<Result<Bytes>>) -> Self {
        Self { restore: SmallVec::new(), save: Some(tx) }
    }
}

/// The session image taken after a program compiled, and every source
/// its compile read (its file, each module's and interface's, once).
pub struct ProgramImage {
    pub image: Bytes,
    pub sources: Vec<triomphe::Arc<Origin>>,
}

#[derive(Builder)]
#[builder(pattern = "owned")]
pub struct GXConfig<X: GXExt> {
    /// See [`RegistrationImage`].
    #[builder(setter(strip_option), default)]
    registration: Option<RegistrationImage>,
    /// A program compiled right after the root, before any cycle, so
    /// the image can carry it; a restored image that holds a program
    /// leaves this one alone. [`GXHandle::program`] hands it back.
    #[builder(setter(strip_option), default)]
    program: Option<Source>,
    /// Receives the image taken after `program` compiled, taken before
    /// any cycle, when the registration was not restored with one.
    #[builder(setter(strip_option), default)]
    program_image: Option<oneshot::Sender<Result<ProgramImage>>>,
    /// Arm a trace (`max_events`, `max_cycles`, as [`GXHandle::trace_start`])
    /// before the program's init cycle, anchored at the program root.
    #[builder(setter(strip_option), default)]
    trace: Option<(usize, u64)>,
    /// The execution context with any builtins already registered
    ctx: ExecState<GXRt<X>, X::UserEvent>,
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
        ctx: ExecState<GXRt<X>, X::UserEvent>,
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
        // The handle and the running `ExecState` share the control.
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
        let st = std::time::Instant::now();
        init_rx.await??;
        log::info!("runtime start wait: {:?}", st.elapsed());
        Ok(GXHandle(Arc::new(GXHandleInner { tx, task, control })))
    }
}
