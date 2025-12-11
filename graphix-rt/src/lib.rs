//! A general purpose graphix runtime
//!
//! This module implements a generic graphix runtime suitable for most
//! applications, including applications that implement custom graphix
//! builtins. The graphix interperter is run in a background task, and
//! can be interacted with via a handle. All features of the standard
//! library are supported by this runtime.
use anyhow::{anyhow, bail, Result};
use arcstr::ArcStr;
use derive_builder::Builder;
use enumflags2::BitFlags;
use fxhash::FxHashSet;
use graphix_compiler::{
    env::Env,
    expr::{ExprId, ModPath, ModuleResolver, Source},
    typ::{FnType, Type},
    BindId, CFlag, Event, ExecCtx, NoUserEvent, Scope, UserEvent,
};
use log::error;
use netidx::{
    protocol::valarray::ValArray,
    publisher::{Value, WriteRequest},
    subscriber::{self, SubId},
};
use netidx_core::atomic_id;
use netidx_value::FromValue;
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
    pub env: Env<GXRt<X>, X::UserEvent>,
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
    env: Env<GXRt<X>, X::UserEvent>,
    pub typ: FnType,
    pub expr: ExprId,
}

impl<X: GXExt> Drop for Callable<X> {
    fn drop(&mut self) {
        let _ = self.rt.0.tx.send(ToGX::DeleteCallable { id: self.id });
    }
}

impl<X: GXExt> Callable<X> {
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
        if self.expr == id {
            Some(v)
        } else {
            None
        }
    }
}

enum DeferredCall {
    Call(ValArray, oneshot::Sender<Result<()>>),
    CallUnchecked(ValArray, oneshot::Sender<Result<()>>),
}

pub struct NamedCallable<X: GXExt> {
    fname: Ref<X>,
    current: Option<Callable<X>>,
    ids: FxHashSet<ExprId>,
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
        res: oneshot::Sender<Env<GXRt<X>, X::UserEvent>>,
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
        res: oneshot::Sender<Result<()>>,
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
    Call {
        id: CallableId,
        args: ValArray,
    },
    DeleteCallable {
        id: CallableId,
    },
}

#[derive(Debug, Clone)]
pub enum GXEvent<X: GXExt> {
    Updated(ExprId, Value),
    Env(Env<GXRt<X>, X::UserEvent>),
}

struct GXHandleInner<X: GXExt> {
    tx: tmpsc::UnboundedSender<ToGX<X>>,
    task: JoinHandle<()>,
}

impl<X: GXExt> Drop for GXHandleInner<X> {
    fn drop(&mut self) {
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
    async fn exec<R, F: FnOnce(oneshot::Sender<R>) -> ToGX<X>>(&self, f: F) -> Result<R> {
        let (tx, rx) = oneshot::channel();
        self.0.tx.send(f(tx)).map_err(|_| anyhow!("runtime is dead"))?;
        Ok(rx.await.map_err(|_| anyhow!("runtime did not respond"))?)
    }

    /// Get a copy of the current graphix environment
    pub async fn get_env(&self) -> Result<Env<GXRt<X>, X::UserEvent>> {
        self.exec(|res| ToGX::GetEnv { res }).await
    }

    /// Check that a graphix module compiles
    ///
    /// If path startes with `netidx:` then the module will be loaded
    /// from netidx, otherwise it will be loaded from the
    /// filesystem. If the file compiles successfully return Ok(())
    /// otherwise an error describing the problem. The environment
    /// will not be altered by checking an expression, so you will not
    /// be able to use any defined names later in the program. If you
    /// want to do that see `compile`.
    pub async fn check(&self, path: Source) -> Result<()> {
        Ok(self.exec(|tx| ToGX::Check { path, res: tx }).await??)
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
        env: &Env<GXRt<X>, X::UserEvent>,
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
            ids: FxHashSet::default(),
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
        env: &Env<GXRt<X>, X::UserEvent>,
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
    sub: tmpsc::Sender<GPooled<Vec<GXEvent<X>>>>,
    /// The set of compiler flags. Default empty.
    #[builder(default)]
    flags: BitFlags<CFlag>,
}

impl<X: GXExt> GXConfig<X> {
    /// Create a new config
    pub fn builder(
        ctx: ExecCtx<GXRt<X>, X::UserEvent>,
        sub: tmpsc::Sender<GPooled<Vec<GXEvent<X>>>>,
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
        Ok(GXHandle(Arc::new(GXHandleInner { tx, task })))
    }
}
