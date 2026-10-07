use anyhow::{Result, bail};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, Node, Refs, Rt, Scope, Tag, TagValue,
    UserEvent,
    effects::Effect,
    expr::ExprId,
    image::{self, ImageBuf},
    node::genn,
    typ::{FnType, Type},
};
use netidx_core::pack::{Pack, PackError};
use netidx_value::{ValArray, Value};

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

fn fc_contains(args: &[Value]) -> Option<Value> {
    Some(Value::Bool(match &args[..] {
        [Value::Null, _] => false,
        [v, x] => v == x,
        _ => return None,
    }))
}

crate::fast_builtin!(pub(crate) Contains, ContainsEv, "core_opt_contains", fc_contains);

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
        [a, b] => Some(Value::Array(ValArray::from_iter_exact(
            [a.clone(), b.clone()].into_iter(),
        ))),
        _ => None,
    }
}

crate::fast_builtin!(pub(crate) Zip, ZipEv, "core_opt_zip", fc_zip);

fn fc_unzip(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Null => Some(Value::Array(ValArray::from_iter_exact(
            [Value::Null, Value::Null].into_iter(),
        ))),
        Value::Array(pair) if pair.len() == 2 => Some(Value::Array(
            ValArray::from_iter_exact([pair[0].clone(), pair[1].clone()].into_iter()),
        )),
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

/// The input a unary option HOF last saw.
#[derive(Debug)]
enum Input {
    /// nothing yet, or bottom
    Unset,
    Null,
    Value(Value),
}

/// A unary option HOF: `f` over the option's value, and what a null
/// answers.
pub(crate) trait Unary: std::fmt::Debug + Send + Sync + 'static {
    const NAME: &str;

    fn on_null() -> Value;

    /// What the callback's answer `r` to the input `x` makes.
    fn answer(_x: &Value, r: Value) -> Value {
        r
    }
}

/// `f` fed the option's inner value; latest-wins: a new input while the
/// callback is in flight overwrites `x` (wrap with `queue` for ordered
/// delivery). The output follows the input the way the select spelling
/// does: a null answers `on_null` and silences the callback, and a stale
/// production surfaces as stale, at a first entry or a wake.
#[derive(Debug)]
pub(crate) struct OptHof<K, R: Rt, E: UserEvent> {
    inner: Node<R, E>,
    fid: BindId,
    x: BindId,
    input: Input,
    out: TagValue,
    kind: std::marker::PhantomData<fn() -> K>,
}

impl<K: Unary, R: Rt, E: UserEvent> BuiltIn<R, E> for OptHof<K, R, E> {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let inner = image::decode_node(ctx, buf)?;
        let fid = BindId::decode(buf)?;
        let x = BindId::decode(buf)?;
        Ok(Box::new(Self {
            inner,
            fid,
            x,
            input: Input::Unset,
            out: TagValue::phantom(),
            kind: std::marker::PhantomData,
        }))
    }

    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = K::NAME;

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
        if ptyp.args.is_empty() {
            bail!("expected unary callback");
        }
        let x_typ = ptyp.args[0].typ.clone();
        let (x, xn) = genn::bind(ctx, &scope.lexical, "x", x_typ, top_id);
        let fid = BindId::new();
        let fnode = genn::reference(ctx, fid, Type::Fn(ptyp.clone()), top_id);
        let inner =
            genn::apply(fnode, scope.clone(), smallvec::smallvec![xn], &ptyp, top_id);
        Ok(Box::new(Self {
            inner,
            fid,
            x,
            input: Input::Unset,
            out: TagValue::phantom(),
            kind: std::marker::PhantomData,
        }))
    }
}

impl<K: Unary, R: Rt, E: UserEvent> Apply<R, E> for OptHof<K, R, E> {
    /// The input seen exists only once a cycle has run.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if !matches!(self.input, Input::Unset) {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.inner.image_encode(buf)?;
        self.fid.encode(buf)?;
        self.x.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let f = produced(from[1].update(ctx));
        feed(ctx, self.fid, f);
        let input = from[0].update(ctx);
        let (bottom, trig) = (input.tag().is_bottom(), input.tag().triggers());
        let direct = match seam_value(input) {
            Some(tv) => match tv.value_cloned() {
                Value::Null => {
                    self.input = Input::Null;
                    Some(TagValue::tagged(K::on_null(), tv.tag()))
                }
                v => {
                    feed(ctx, self.x, Some((v.clone(), tv.tag())));
                    self.input = Input::Value(v);
                    None
                }
            },
            None => {
                self.input = Input::Unset;
                None
            }
        };
        let answer = self.inner.update(ctx);
        if bottom {
            return self.out.set_bottom(trig);
        }
        if let Some(tv) = direct {
            return self.out.set(tv);
        }
        match (&self.input, seam_value(answer)) {
            (Input::Value(x), Some(r)) => {
                self.out.set(TagValue::tagged(K::answer(x, r.value_cloned()), r.tag()))
            }
            // the callback has not answered this input yet
            (Input::Value(_), None) => self.out.set_bottom(answer.tag().triggers()),
            // while the input is null, the callback's own fires answer nothing
            (Input::Null | Input::Unset, _) => self.out.ride(),
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
        ctx.rt.store_remove(&self.x);
        ctx.env.unbind_variable(self.x);
        self.inner.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.input = Input::Unset;
        self.inner.sleep(ctx);
    }
}

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
        ctx.rt.store_insert(id, TagValue::fired(v.clone()));
        ctx.event.variables.insert(id, TagValue::tagged(v, tag));
    }
}

macro_rules! unary {
    ($kind:ident, $alias:ident, $name:literal, $on_null:expr) => {
        #[derive(Debug)]
        pub(crate) struct $kind;

        impl Unary for $kind {
            const NAME: &str = $name;

            fn on_null() -> Value {
                $on_null
            }
        }

        pub(crate) type $alias<R, E> = OptHof<$kind, R, E>;
    };
}

unary!(MapKind, OptMap, "core_opt_map", Value::Null);
unary!(FlatMapKind, OptFlatMap, "core_opt_flat_map", Value::Null);
unary!(IsSomeAndKind, OptIsSomeAnd, "core_opt_is_some_and", Value::Bool(false));
unary!(IsNoneOrKind, OptIsNoneOr, "core_opt_is_none_or", Value::Bool(true));

#[derive(Debug)]
pub(crate) struct FilterKind;

impl Unary for FilterKind {
    const NAME: &str = "core_opt_filter";

    fn on_null() -> Value {
        Value::Null
    }

    fn answer(x: &Value, r: Value) -> Value {
        match r {
            Value::Bool(true) => x.clone(),
            _ => Value::Null,
        }
    }
}

pub(crate) type OptFilter<R, E> = OptHof<FilterKind, R, E>;

/// What an `or_else` makes of the fallback's value.
pub(crate) trait Fallback: std::fmt::Debug + Send + Sync + 'static {
    const NAME: &str;

    fn wrap(v: Value) -> Value;
}

/// `a`, or when it is null the latest value of `f()`, which is always
/// driven. The output follows the inputs as the select spelling does: a
/// stale production surfaces as stale.
#[derive(Debug)]
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
            genn::apply(fnode, scope.clone(), smallvec::smallvec![], &ptyp, top_id);
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
