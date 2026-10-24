use anyhow::{Result, bail};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, Node, Refs, Rt, Scope, Tag, TagValue,
    UserEvent,
    effects::Effect,
    expr::ExprId,
    image::{self, ImageBuf},
    node::genn,
    rep,
    typ::{FnType, Type},
};
use netidx_core::pack::{Pack, PackError};
use netidx_value::Value;

use crate::seam_value;

fn fc_is_some(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Null => Some(Value::Bool(false)),
        _ => Some(Value::Bool(true)),
    }
}

crate::fast_builtin!(pub(crate) IsSome, IsSomeEv, "core_opt_is_some", fc_is_some);

fn fc_is_none(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Null => Some(Value::Bool(true)),
        _ => Some(Value::Bool(false)),
    }
}

crate::fast_builtin!(pub(crate) IsNone, IsNoneEv, "core_opt_is_none", fc_is_none);

/// `a`, or `b` when `a` is null: `or` and `or_default` alike.
fn fc_or(args: &[Value]) -> Option<Value> {
    match &args[..] {
        [Value::Null, b] => Some(b.clone()),
        [a, _] => Some(a.clone()),
        _ => None,
    }
}

crate::fast_builtin!(pub(crate) OrDefault, OrDefaultEv, "core_opt_or_default", fc_or);
crate::fast_builtin!(pub(crate) Or, OrEv, "core_opt_or", fc_or);

fn fc_and(args: &[Value]) -> Option<Value> {
    match &args[..] {
        [Value::Null, _] => Some(Value::Null),
        [_, b] => Some(b.clone()),
        _ => None,
    }
}

crate::fast_builtin!(pub(crate) And, AndEv, "core_opt_and", fc_and);

fn fc_xor(args: &[Value]) -> Option<Value> {
    let (a, b) = (&args[0], &args[1]);
    let a_some = !matches!(a, Value::Null);
    let b_some = !matches!(b, Value::Null);
    Some(if a_some && !b_some {
        a.clone()
    } else if !a_some && b_some {
        b.clone()
    } else {
        Value::Null
    })
}

crate::fast_builtin!(pub(crate) Xor, XorEv, "core_opt_xor", fc_xor);

fn fc_zip(args: &[Value]) -> Option<Value> {
    match &args[..] {
        [Value::Null, _] | [_, Value::Null] => Some(Value::Null),
        [a, b] => Some(rep::tuple([a.clone(), b.clone()])),
        _ => None,
    }
}

crate::fast_builtin!(pub(crate) Zip, ZipEv, "core_opt_zip", fc_zip);

fn fc_unzip(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Null => Some(rep::tuple([Value::Null, Value::Null])),
        p if rep::tuple_elems(p).is_some_and(|e| e.len() == 2) => Some(p.clone()),
        _ => None,
    }
}

crate::fast_builtin!(pub(crate) Unzip, UnzipEv, "core_opt_unzip", fc_unzip);

fn fc_ok_or(args: &[Value]) -> Option<Value> {
    match &args[..] {
        [Value::Null, e] => Some(Value::Error(e.clone().into())),
        [v, _] => Some(v.clone()),
        _ => None,
    }
}

crate::fast_builtin!(pub(crate) OkOr, OkOrEv, "core_opt_ok_or", fc_ok_or);

/// A production's value and tag, when it has a value.
fn produced(tv: &TagValue) -> Option<(Value, Tag)> {
    seam_value(tv).map(|tv| (tv.value_cloned(), tv.tag()))
}

/// Deliver a production to a binding the callback reads.
fn feed<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    id: BindId,
    prod: Option<(Value, Tag)>,
) {
    if let Some((v, tag)) = prod {
        ctx.deliver(id, TagValue::tagged(v, tag));
    }
}

/// What an `or_else` makes of the fallback's value.
pub(crate) trait Fallback: std::fmt::Debug + Send + Sync + 'static {
    const NAME: &str;

    fn wrap(v: Value) -> Value;
}

/// `a`, or when it is null the latest value of `f()`, which is always
/// driven. The output follows the inputs as the select spelling does: a
/// stale production surfaces as stale.
#[derive(Debug)]
// CR claude for claude: [perf/medium] `OptHof` and `OrElse` re-implement the select
// spelling in about 300 lines of feed/bind/answer bookkeeping. They are `Effect::Sync`
// with no fast call, so any lambda calling `opt::map`, `flat_map`, `filter`,
// `is_some_and`, `is_none_or`, `or_else` or `ok_or_else` cannot fuse ("builtin
// `opt::map` has no fast-call entry"), and an arm holding one sleeps. The select
// spelling, `|v: ['a, null], f: fn(x: 'a) -> 'b throws 'e| -> ['b, null] throws 'e
// select v { null as _ => null, x => f(x) }`, typechecks as a definition in opt.gx,
// gives the same trace and fuses. Fix: define the seven HOFs in opt.gx over select and
// delete `OptHof`, `OrElse`, `Unary` and the kinds. Do the same for core `filter_err`
// (`select r { error as e => e, _ => never() }`). probe:
// design/review-2026-10-12/repro/x-builtin-effects-07-opt-map-defuses.gx
// (x-builtin-effects-06)
// 2026-10-13 claude: map, flat_map, filter, is_some_and and is_none_or are now the
// select spelling in opt.gx and OptHof is gone. or_else and ok_or_else stay: their
// docs promise f() driven eagerly, which a select arm would not keep, so moving them is
// a semantics change to decide; filter_err's select spelling would bottom where it now
// holds its last error.
// 2026-10-13 claude: re-addressed: whether or_else and ok_or_else may give up their
// documented eager f() (driven while v is non-null, so a later null answers at once)
// for the select spelling, and filter_err its held error for bottom, is Eric's call.
// 2026-10-24 claude: Eric ruled (2026-10-24): define or_else and filter_err over
// select.
pub(crate) struct OrElse<W, R: Rt, E: UserEvent> {
    inner: Node<R, E>,
    fid: BindId,
    out: TagValue,
    wrap: std::marker::PhantomData<fn() -> W>,
}

impl<W: Fallback, R: Rt, E: UserEvent> BuiltIn<R, E> for OrElse<W, R, E> {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let inner = image::decode_node(ctx, buf)?;
        let fid = BindId::decode(buf)?;
        Ok(Box::new(Self {
            inner,
            fid,
            out: TagValue::phantom(),
            wrap: std::marker::PhantomData,
        }))
    }

    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = W::NAME;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        if from.len() != 2 {
            bail!("expected two arguments");
        }
        let typ = resolved.unwrap_or(typ);
        let ptyp = match &typ.args[1].typ {
            Type::Fn(ft) => ft.clone(),
            t => bail!("expected a function not {t}"),
        };
        let fid = BindId::new();
        let fnode = genn::reference(ctx, fid, Type::Fn(ptyp.clone()), top_id);
        let inner =
            genn::apply(fnode, scope.clone(), smallvec::smallvec![], &ptyp, top_id)?;
        Ok(Box::new(Self {
            inner,
            fid,
            out: TagValue::phantom(),
            wrap: std::marker::PhantomData,
        }))
    }
}

impl<W: Fallback, R: Rt, E: UserEvent> Apply<R, E> for OrElse<W, R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.inner.image_encode(buf)?;
        self.fid.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let f = produced(from[1].update(ctx));
        feed(ctx, self.fid, f);
        let a = from[0].update(ctx);
        let (a_bottom, a_tag) = (a.tag().is_bottom(), a.tag());
        let a = seam_value(a).map(|tv| tv.value_cloned());
        let f = self.inner.update(ctx);
        if a_bottom {
            return self.out.set_bottom(a_tag.triggers());
        }
        match a {
            Some(Value::Null) => match seam_value(f) {
                Some(fv) => {
                    let fired = a_tag.is_fired() || fv.is_fired();
                    let v = W::wrap(fv.value_cloned());
                    self.out.set(if fired {
                        TagValue::fired(v)
                    } else {
                        TagValue::stale(v)
                    })
                }
                None => self.out.set_bottom(a_tag.triggers() || f.tag().triggers()),
            },
            Some(v) => self.out.set(TagValue::tagged(v, a_tag)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.inner.typecheck0(ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.inner.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.rt.store_remove(&self.fid);
        self.inner.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.inner.sleep(ctx);
    }
}

#[derive(Debug)]
pub(crate) struct OrElseKind;

impl Fallback for OrElseKind {
    const NAME: &str = "core_opt_or_else";

    fn wrap(v: Value) -> Value {
        v
    }
}

#[derive(Debug)]
pub(crate) struct OkOrElseKind;

impl Fallback for OkOrElseKind {
    const NAME: &str = "core_opt_ok_or_else";

    fn wrap(v: Value) -> Value {
        Value::Error(v.into())
    }
}

pub(crate) type OptOrElse<R, E> = OrElse<OrElseKind, R, E>;
pub(crate) type OptOkOrElse<R, E> = OrElse<OkOrElseKind, R, E>;
