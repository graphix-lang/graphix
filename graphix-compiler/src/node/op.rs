use super::{CFlag, WakeBit, compiler::compile, coretraits, dense_gate};
use crate::{
    CompileCtx, ExecCtx, Node, NodeView, Refs, Rt, Scope, Tag, TagValue, Update,
    UserEvent, defetyp,
    env::Env,
    expr::{Expr, ExprId},
    fusion::{
        self,
        emit::{
            BodyCx, CompiledExpr, emit_arith_node, emit_bool_node,
            emit_checked_arith_node, emit_cmp_node, emit_neg_node, emit_not_node,
        },
    },
    image::{
        ImageBuf,
        nodes::{NodeTag, decode_node, put_tag},
    },
    node::error::{diagnostic_site, report_failure},
    stack::ensure_sufficient,
    typ::{ContainsFlags, Type},
    wrap,
};
use anyhow::{Result, bail};
use arcstr::{ArcStr, literal};
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Typ, ValArray, Value};
use triomphe::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

// CR claude for eric: [structure] symbol() repeats five of expr::BinOp::token()'s
// strings. Its only use is arith_rule's message (line 741), which rebuilds the checked
// token by appending "?". arith_op!'s $name is already the matching expr::BinOp variant
// (Add, CheckedAdd, ..), and arith_rule uses op for nothing but that message. Pass
// crate::expr::BinOp::$name, print its token(), and delete symbol(). (c-error-op-07)
impl BinOp {
    fn symbol(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Lte,
    Gte,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoolOp {
    And,
    Or,
}

/// The node of a binary operator: its operands, resident and wake bit,
/// their codec and plumbing. `$typ` is the result type at construction;
/// the operator supplies `update`, `typecheck0`, `typecheck1` and
/// `emit_clif`.
macro_rules! binary_node {
    ($name:ident, $typ:expr, { $($methods:tt)* }) => {
        #[derive(Debug)]
        pub struct $name<R: Rt, E: UserEvent> {
            pub(crate) spec: Expr,
            pub typ: Type,
            pub lhs: Node<R, E>,
            pub rhs: Node<R, E>,
            resident: TagValue,
            /// wake catch-up: set by `sleep()`, taken by the next update
            slept: WakeBit,
            fork: crate::cost::ForkSite,
        }

        impl<R: Rt, E: UserEvent> $name<R, E> {
            pub(crate) fn compile(
                ctx: &mut CompileCtx<R, E>,
                flags: BitFlags<CFlag>,
                spec: Expr,
                scope: &Scope,
                top_id: ExprId,
                lhs: &Expr,
                rhs: &Expr,
            ) -> Result<Node<R, E>> {
                let lhs = compile(ctx, flags, lhs.clone(), scope, top_id)?;
                let rhs = compile(ctx, flags, rhs.clone(), scope, top_id)?;
                Ok(Self::node(spec, $typ, lhs, rhs))
            }

            pub(crate) fn image_decode(
                ctx: &mut ExecCtx<'_, R, E>,
                buf: &mut &[u8],
            ) -> Result<Node<R, E>, PackError> {
                let spec = Expr::decode(buf)?;
                let typ = Type::decode(buf)?;
                let lhs = decode_node(ctx, buf)?;
                let rhs = decode_node(ctx, buf)?;
                Ok(Self::node(spec, typ, lhs, rhs))
            }

            fn node(spec: Expr, typ: Type, lhs: Node<R, E>, rhs: Node<R, E>) -> Node<R, E> {
                Node::new(Self {
                    spec,
                    typ,
                    lhs,
                    rhs,
                    resident: TagValue::phantom(),
                    slept: WakeBit::default(),
                    fork: Default::default(),
                })
            }
        }

        impl<R: Rt, E: UserEvent> Update<R, E> for $name<R, E> {
            fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
                put_tag(NodeTag::$name, buf);
                self.spec.encode(buf)?;
                self.typ.encode(buf)?;
                self.lhs.image_encode(buf)?;
                self.rhs.image_encode(buf)
            }

            fn spec(&self) -> &Expr {
                &self.spec
            }

            fn typ(&self) -> &Type {
                &self.typ
            }

            fn refs(&self, refs: &mut Refs) {
                self.lhs.refs(refs);
                self.rhs.refs(refs);
            }

            fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                self.lhs.delete(ctx);
                self.rhs.delete(ctx);
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                self.slept.set();
                self.lhs.sleep(ctx);
                self.rhs.sleep(ctx);
            }

            fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
                fusion::fuse_parts([&mut self.lhs, &mut self.rhs], ctx)
            }

            fn view(&self) -> NodeView<'_, R, E> {
                NodeView::$name(self)
            }

            $($methods)*
        }
    };
}

/// Update both operands of a `binary_node!` and gate on them: a bottom
/// operand bottoms and a quiet cycle rides the resident, both returning
/// from the caller; otherwise the operands, whether they triggered and
/// the result's tag.
macro_rules! gated_operands {
    ($self:ident, $ctx:ident) => {{
        let woke = $self.slept.take();
        let (lhs, rhs) = (&mut $self.lhs, &mut $self.rhs);
        let (l, r) = $crate::branch::join2(
            &mut $self.fork,
            $ctx,
            |c| lhs.update(c),
            |c| rhs.update(c),
        );
        let (lt, rt) = (l.tag(), r.tag());
        let trig = lt.triggers() || rt.triggers();
        dense_gate!($self.resident, trig, lt.is_bottom() || rt.is_bottom(), woke);
        let tag = if lt.is_fired() || rt.is_fired() { Tag::FIRED } else { Tag::STALE };
        (l, r, trig, tag)
    }};
}

/// The one type both operands of a binary operator have, or `None`: each
/// contains the other, probed both ways without binding (a failed binding
/// walk cannot be undone), then committed both ways. A ⊥ operand never
/// produces, so the other operand's type stands.
fn operand_type<'t>(env: &Env, lt: &'t Type, rt: &'t Type) -> Result<Option<&'t Type>> {
    let bottom = |t: &Type| t.with_deref(|t| matches!(t, Some(Type::Bottom)));
    if bottom(rt) {
        return Ok(Some(lt));
    }
    if bottom(lt) {
        return Ok(Some(rt));
    }
    let probe = ContainsFlags::RigidCheck.into();
    let commit = ContainsFlags::Commit | ContainsFlags::RigidCheck;
    let one = lt.contains_with_flags(probe, env, rt)?
        && rt.contains_with_flags(probe, env, lt)?
        && lt.contains_with_flags(commit, env, rt)?
        && rt.contains_with_flags(commit, env, lt)?;
    Ok(one.then_some(lt))
}

/// An operand whose type is known must be in `bound` now; an open cell,
/// alone or a member of a union, carries `bound` as a constraint for
/// later: binding it to `bound` would claim every type the bound admits
/// at once.
pub(super) fn constrain_operand(env: &Env, bound: &Type, t: &Type) -> Result<()> {
    ensure_sufficient(|| match t {
        Type::TVar(tv) => match tv.binding() {
            Some(b) => constrain_operand(env, bound, &b),
            None => tv.narrow_cell(env, bound.clone()),
        },
        Type::Set(ts) => ts.iter().try_for_each(|m| constrain_operand(env, bound, m)),
        t => bound.check_contains(env, t),
    })
}

/// The numeric primitives a value of `t` may be, through bound cells,
/// unions and typedefs. An open cell and `Any` contribute none.
fn numeric_members(env: &Env, t: &Type) -> Result<BitFlags<Typ>> {
    ensure_sufficient(|| {
        t.with_deref(|t| match t {
            Some(Type::Primitive(p)) => Ok(*p & Typ::number()),
            Some(Type::Set(ts)) => ts
                .iter()
                .try_fold(BitFlags::empty(), |acc, t| Ok(acc | numeric_members(env, t)?)),
            Some(t @ Type::Ref(_)) => match t.lookup_ref_with(env, false)? {
                Some(t) => numeric_members(env, &t),
                None => Ok(BitFlags::empty()),
            },
            _ => Ok(BitFlags::empty()),
        })
    })
}

/// Operands of one type that may be two numeric types would compute by
/// promotion: arithmetic refuses them.
fn refuse_mixed_numeric(env: &Env, t: &Type) -> Result<()> {
    if numeric_members(env, t)?.len() > 1 {
        crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
            bail!(
                "cannot compute with values of {t}: it holds more than one numeric type (cast to one)"
            )
        })
    } else {
        Ok(())
    }
}

macro_rules! compare_op {
    ($name:ident, $op:tt) => {
        binary_node!($name, Type::boolean(), {
            fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
                let (l, r, _, tag) = gated_operands!(self, ctx);
                let v = coretraits::with_hooks(ctx, || {
                    l.with_value(|lv| r.with_value(|rv| (lv $op rv).into()))
                });
                self.resident.set(TagValue::tagged(v, tag))
            }

            // CR claude for eric: [structure] typecheck0 and typecheck1 here are
            // repeated word for word in bool_op! (lines 342-360) and arith_op!
            // (795-817), and typecheck0_instance is repeated in bool_op!. Arith's
            // typecheck0_instance differs only by its settle tail. Move the three into
            // binary_node! next to the operand plumbing, with each operator giving
            // typecheck_own and an instance tail (nothing for compare and bool, the
            // settle for arith). Then correct binary_node!'s doc, which says the
            // operator supplies typecheck0 and typecheck1. (c-error-op-08)
            fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck0(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck0(ctx))?;
                self.typecheck_own(ctx)
            }

            fn typecheck0_instance(
                &mut self,
                ctx: &mut CompileCtx<R, E>,
                types: &mut super::lambda::InstanceTypes,
            ) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck0_instance(ctx, types))?;
                wrap!(self.rhs, self.rhs.typecheck0_instance(ctx, types))
            }

            fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck1(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck1(ctx))
            }

            fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
                emit_cmp_node(cx, CmpOp::$name, &self.lhs, &self.rhs)
            }
        });

        impl<R: Rt, E: UserEvent> $name<R, E> {
            // CR claude for eric: [bug] `<`, `>`, `<=` and `>=` accept references. A
            // reference's value is its bind id, and parallel compile mints ids in
            // thread order, so `r1 < r2` over two instances' `&v` prints true on some
            // runs and false on others, in both engines, with nothing fused. This
            // breaks the rule that a reference is not a number, and the rule in
            // design/parallel_compile.md that "anything that iterates by id must not
            // change output". Refuse the ordering operators when the operand type
            // `holds_ref`, as `TypeCast` does. A generic `|a, b| a < b` called with
            // references needs the same refusal at the call (a bound, as `Singleton`
            // is), and `array::sort` and map keys over references flip in the same way.
            // probe: design/review-2026-10-05/repro/c-error-op-04.gx (c-error-op-04)
            // 2026-10-06 claude: comparisons now carry the `Discernible` bound, which
            // a generic definition takes from its body and each call checks, but it
            // does not cover this: a lone reference type has no two members to mix
            // up, so `&i64 < &i64` is still Discernible. Refusing ordering over
            // references could ride the same machinery (a second bound, or
            // Discernible refusing references under the orderings only).
            fn typecheck_own(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
                let (lt, rt) = (self.lhs.typ(), self.rhs.typ());
                match wrap!(self, operand_type(&ctx.env, lt, rt))? {
                    Some(t) => {
                        t.require_discernible();
                        let judgment = crate::PendingSettle::Discernible {
                            typ: t.clone(),
                            what: crate::Discerned::Compared,
                            spec: Arc::new(self.spec.clone()),
                        };
                        super::defer_judgment(ctx, judgment);
                        Ok(())
                    }
                    None => wrap!(
                        self,
                        $crate::format_with_flags($crate::PrintFlag::DerefTVars, || {
                            bail!(
                                "cannot compare {lt} with {rt}: comparison is \
                                 fn('a, 'a) -> bool — both operands must be one type"
                            )
                        })
                    ),
                }
            }
        }
    };
}

compare_op!(Eq, ==);
compare_op!(Ne, !=);
compare_op!(Lt, <);
compare_op!(Gt, >);
compare_op!(Lte, <=);
compare_op!(Gte, >=);

macro_rules! bool_op {
    ($name:ident, $op:tt) => {
        binary_node!($name, Type::boolean(), {
            // Strict, not short-circuit: `false && ⊥ = ⊥`.
            fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
                let (l, r, _, tag) = gated_operands!(self, ctx);
                let v = l.with_value(|lv| {
                    r.with_value(|rv| match (lv, rv) {
                        (Value::Bool(b0), Value::Bool(b1)) => Some(Value::Bool(*b0 $op *b1)),
                        _ => None,
                    })
                });
                match v {
                    Some(v) => self.resident.set(TagValue::tagged(v, tag)),
                    None => self.resident.ride(),
                }
            }

            fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck0(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck0(ctx))?;
                self.typecheck_own(ctx)
            }

            fn typecheck0_instance(
                &mut self,
                ctx: &mut CompileCtx<R, E>,
                types: &mut super::lambda::InstanceTypes,
            ) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck0_instance(ctx, types))?;
                wrap!(self.rhs, self.rhs.typecheck0_instance(ctx, types))
            }

            fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck1(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck1(ctx))
            }

            fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
                emit_bool_node(cx, BoolOp::$name, &self.lhs, &self.rhs)
            }
        });

        impl<R: Rt, E: UserEvent> $name<R, E> {
            fn typecheck_own(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
                let bt = Type::boolean();
                wrap!(self.lhs, bt.check_contains(&ctx.env, self.lhs.typ()))?;
                wrap!(self.rhs, bt.check_contains(&ctx.env, self.rhs.typ()))
            }
        }
    };
}

bool_op!(And, &&);
bool_op!(Or, ||);

#[derive(Debug)]
pub struct Not<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub n: Node<R, E>,
    resident: TagValue,
    /// wake catch-up: set by `sleep()`, taken by the next update
    slept: WakeBit,
}

impl<R: Rt, E: UserEvent> Not<R, E> {
    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        n: &Expr,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, flags, n.clone(), scope, top_id)?;
        Ok(Self::node(spec, Type::boolean(), n))
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        Ok(Self::node(spec, typ, n))
    }

    fn node(spec: Expr, typ: Type, n: Node<R, E>) -> Node<R, E> {
        Node::new(Self {
            spec,
            typ,
            n,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
        })
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Not<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Not, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.n.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let tv = self.n.update(ctx);
        let tag = tv.tag();
        dense_gate!(self, tag.triggers(), tag.is_bottom());
        match tv.with_value(|v| match v {
            Value::Bool(b) => Some(!*b),
            _ => None,
        }) {
            Some(b) => self.resident.set(TagValue::tagged(Value::Bool(b), tag)),
            None => self.resident.ride(),
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.n.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        self.n.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.n], ctx)
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck0(ctx))?;
        wrap!(self.n, Type::boolean().check_contains(&ctx.env, self.n.typ()))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        wrap!(self.n, self.n.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck1(ctx))
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Not(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_not_node(cx, &self.n)
    }
}

#[derive(Debug)]
pub struct Neg<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub n: Node<R, E>,
    resident: TagValue,
    /// wake catch-up: set by `sleep()`, taken by the next update
    slept: WakeBit,
}

impl<R: Rt, E: UserEvent> Neg<R, E> {
    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        n: &Expr,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, flags, n.clone(), scope, top_id)?;
        Ok(Self::node(spec, Type::empty_tvar(), n))
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        Ok(Self::node(spec, typ, n))
    }

    fn node(spec: Expr, typ: Type, n: Node<R, E>) -> Node<R, E> {
        Node::new(Self {
            spec,
            typ,
            n,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
        })
    }

    fn negatable() -> Type {
        Type::Primitive(Typ::signed_integer() | Typ::float() | Typ::Decimal)
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Neg<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Neg, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.n.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        // Integers wrap, matching the JIT's `ineg`.
        let tv = self.n.update(ctx);
        let tag = tv.tag();
        dense_gate!(self, tag.triggers(), tag.is_bottom());
        let neg = tv.with_value(|v| match v {
            Value::I8(x) => Some(Value::I8(x.wrapping_neg())),
            Value::I16(x) => Some(Value::I16(x.wrapping_neg())),
            Value::I32(x) => Some(Value::I32(x.wrapping_neg())),
            Value::Z32(x) => Some(Value::Z32(x.wrapping_neg())),
            Value::I64(x) => Some(Value::I64(x.wrapping_neg())),
            Value::Z64(x) => Some(Value::Z64(x.wrapping_neg())),
            Value::F32(x) => Some(Value::F32(-*x)),
            Value::F64(x) => Some(Value::F64(-*x)),
            Value::Decimal(x) => Some(Value::Decimal(Arc::new(-**x))),
            _ => None,
        });
        match neg {
            Some(v) => self.resident.set(TagValue::tagged(v, tag)),
            None => self.resident.ride(),
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.n.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        self.n.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.n], ctx)
    }

    /// The operand is negatable once the check settles its cell.
    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck0(ctx))?;
        self.typecheck_own(ctx)
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        wrap!(self.n, self.n.typecheck0_instance(ctx, types))?;
        match wrap!(self, types.settle(self.spec.id, &self.typ))? {
            true => Ok(()),
            false => self.typecheck_own(ctx),
        }
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck1(ctx))
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Neg(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_neg_node(cx, &self.n)
    }
}

defetyp!(ARITH_ERR, ARITH_ERR_TAG, "ArithError", "Error<`{}(string)>");

/// A checked op's raw failure as the catchable `ArithError`; success
/// passes through.
fn wrap_arith_error(result: Value) -> Value {
    match result {
        Value::Error(e) => {
            let msg = match &*e {
                Value::String(s) => Value::String(s.clone()),
                e => Value::from(format_compact!("{e}")),
            };
            let tag = Value::String(ARITH_ERR_TAG.clone());
            Value::Error(Value::Array(ValArray::from_iter([tag, msg])).into())
        }
        v => v,
    }
}

/// `l op r` for a same-variant integer pair, in that variant (netidx's
/// operators fold `v32`/`z32`/`v64`/`z64` into their fixed-width twins).
/// Unchecked `+ - *` wrap, matching the JIT; any other overflow and a
/// zero divisor are an error value. `None` for every other shape.
// CR claude for eric: [doc-drift] The docs disagree with this function. CLAUDE.md says
// "unchecked wraps, integer div0 bottoms", but unchecked / and % also bottom on MIN /
// -1 and MIN % -1 (checked_div/checked_rem below; the JIT tests for this case in
// fusion/emit/nodes.rs). book/src/core/fundamental_types.md:28 and :56 say unchecked
// operators bottom on overflow, but + - * wrap (MAX + 1 gives MIN in both engines), and
// fused code logs nothing. The book's ArithError strings ("attempt to divide by zero"
// and "attempt to subtract with overflow" at fundamental_types.md:70 and :86,
// error.md:93) are produced nowhere: every integer failure is "arithmetic error" (line
// 669), which names neither cause. Fix the three docs, or decide that unchecked / and %
// wrap too. Either way, have line 669 say which failure happened. (c-error-op-06)
fn int_arith(op: BinOp, checked: bool, l: &Value, r: &Value) -> Option<Value> {
    macro_rules! int {
        ($va:ident, $a:expr, $b:expr) => {{
            let (a, b) = ($a, $b);
            let v = match (op, checked) {
                (BinOp::Add, false) => Some(a.wrapping_add(b)),
                (BinOp::Sub, false) => Some(a.wrapping_sub(b)),
                (BinOp::Mul, false) => Some(a.wrapping_mul(b)),
                (BinOp::Add, true) => a.checked_add(b),
                (BinOp::Sub, true) => a.checked_sub(b),
                (BinOp::Mul, true) => a.checked_mul(b),
                // CR claude for eric: [doc-drift] Unchecked / and % bottom on MIN / -1
                // here and in the JIT guard (fusion/emit/nodes.rs:262-276).
                // CLAUDE.md:518 says unchecked arithmetic wraps. The book says
                // unchecked + bottoms on overflow (core/fundamental_types.md:28 and
                // :56, functions/polymorphism.md:61 and :184,
                // core/reading_types.md:303), but + - * wrap. x % -1 is 0 for every x,
                // so MIN % -1 bottoming and MIN %? -1 returning ArithError refuse a
                // representable result. Settle the rule (e.g. + - * wrap, % by -1 is 0,
                // / bottoms on a zero divisor and on MIN / -1) and state it in
                // CLAUDE.md and the book. probe:
                // design/review-2026-10-05/repro/x-engine-collections-07.gx
                // (x-engine-collections-07)
                (BinOp::Div, _) => a.checked_div(b),
                (BinOp::Mod, _) => a.checked_rem(b),
            };
            Some(match v {
                Some(v) => Value::$va(v),
                None => Value::error(literal!("arithmetic error")),
            })
        }};
    }
    match (l, r) {
        (Value::I8(a), Value::I8(b)) => int!(I8, *a, *b),
        (Value::I16(a), Value::I16(b)) => int!(I16, *a, *b),
        (Value::I32(a), Value::I32(b)) => int!(I32, *a, *b),
        (Value::I64(a), Value::I64(b)) => int!(I64, *a, *b),
        (Value::U8(a), Value::U8(b)) => int!(U8, *a, *b),
        (Value::U16(a), Value::U16(b)) => int!(U16, *a, *b),
        (Value::U32(a), Value::U32(b)) => int!(U32, *a, *b),
        (Value::U64(a), Value::U64(b)) => int!(U64, *a, *b),
        (Value::V32(a), Value::V32(b)) => int!(V32, *a, *b),
        (Value::V64(a), Value::V64(b)) => int!(V64, *a, *b),
        (Value::Z32(a), Value::Z32(b)) => int!(Z32, *a, *b),
        (Value::Z64(a), Value::Z64(b)) => int!(Z64, *a, *b),
        _ => None,
    }
}

/// `l op r` as both engines compute it. A failure is an error value, a
/// checked op's the catchable `ArithError`.
pub(crate) fn arith(op: BinOp, checked: bool, l: Value, r: Value) -> Value {
    let v = match int_arith(op, checked, &l, &r) {
        Some(v) => v,
        None => match (op, checked) {
            (BinOp::Add, false) => l + r,
            (BinOp::Sub, false) => l - r,
            (BinOp::Mul, false) => l * r,
            (BinOp::Div, false) => l / r,
            (BinOp::Mod, false) => l % r,
            (BinOp::Add, true) => l.checked_add(r),
            (BinOp::Sub, true) => l.checked_sub(r),
            (BinOp::Mul, true) => l.checked_mul(r),
            (BinOp::Div, true) => l.checked_div(r),
            (BinOp::Mod, true) => l.checked_rem(r),
        },
    };
    if checked { wrap_arith_error(v) } else { v }
}

macro_rules! arith_emit_clif {
    (false, $base:ident) => {
        fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
            emit_arith_node(cx, BinOp::$base, &self.typ, &self.lhs, &self.rhs)
        }
    };
    (true, $base:ident) => {
        fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
            emit_checked_arith_node(cx, BinOp::$base, &self.lhs, &self.rhs)
        }
    };
}

/// `fn('a: Number, 'a) -> 'a`: both operands `lt` and `rt` and the
/// result `out` are one numeric type. Idempotent.
pub(crate) fn arith_rule(
    env: &Env,
    op: BinOp,
    checked: bool,
    lt: &Type,
    rt: &Type,
    out: &Type,
) -> Result<()> {
    // A declared `'a: Number` formal is rigid while its def gate is
    // open: `x + f64:0.` must reject, not bind 'a.
    let Some(t) = operand_type(env, lt, rt)? else {
        // CR claude for eric: [doc-drift] This refusal and arith_rule's doc comment
        // (line 724) give arithmetic as `fn('a: Number, 'a) -> 'a`, which is neither
        // the rule nor a parseable fn type. CLAUDE.md and the book
        // (core/reading_types.md:281) give `fn<'a: Number + Singleton>(x: 'a, y: 'a) ->
        // 'a`. `let x = 1; x + "s"` shows the user the wrong form. State the current
        // signature in both places. (t-parser-b-16)
        return crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
            bail!(
                "cannot compute {lt} {}{} {rt}: arithmetic is fn('a: Number, 'a) -> 'a — \
                 both operands must be one numeric type (cast one side explicitly)",
                op.symbol(),
                if checked { "?" } else { "" }
            )
        });
    };
    // The operands are one type now, so one bound covers both. A known
    // operand must be numeric at typecheck0; the def-time acceptance gate
    // for a lambda body runs only there.
    constrain_operand(env, &Type::Primitive(Typ::number()), t)?;
    refuse_mixed_numeric(env, t)?;
    t.narrow_singleton(env)?;
    match checked {
        true => out.check_contains(
            env,
            &Type::Set(Arc::from_iter([t.clone(), ARITH_ERR.clone()])),
        ),
        false => out.check_contains(env, t),
    }
}

/// Defer an operator's settle of the operand `n` to the check's settle.
fn defer_operand<R: Rt, E: UserEvent>(ctx: &mut CompileCtx<R, E>, n: &Node<R, E>) {
    if let Type::TVar(tv) = n.typ() {
        super::defer_settle(ctx, || crate::PendingSettle::Operand {
            tv: tv.clone(),
            spec: Arc::new(n.spec().clone()),
        })
    }
}

macro_rules! arith_op {
    ($name:ident, $checked:tt, $base:ident) => {
        binary_node!($name, Type::empty_tvar(), {
            arith_emit_clif!($checked, $base);

            fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
                let (l, r, trig, tag) = gated_operands!(self, ctx);
                let v = l.with_value(|lv| {
                    r.with_value(|rv| {
                        arith(BinOp::$base, $checked, lv.clone(), rv.clone())
                    })
                });
                match v {
                    Value::Error(e) if !$checked => {
                        if trig {
                            let site = diagnostic_site(&self.spec);
                            report_failure!(&format_compact!("arith error {site} {e}"));
                        }
                        self.resident.set_bottom(trig)
                    }
                    v => self.resident.set(TagValue::tagged(v, tag)),
                }
            }

            fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck0(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck0(ctx))?;
                self.typecheck_own(ctx)
            }

            fn typecheck0_instance(
                &mut self,
                ctx: &mut CompileCtx<R, E>,
                types: &mut super::lambda::InstanceTypes,
            ) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck0_instance(ctx, types))?;
                wrap!(self.rhs, self.rhs.typecheck0_instance(ctx, types))?;
                if !wrap!(self, types.settle(self.spec.id, &self.typ))? {
                    self.typecheck_own(ctx)?;
                }
                Ok(())
            }

            fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck1(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck1(ctx))
            }
        });

        impl<R: Rt, E: UserEvent> $name<R, E> {
            fn typecheck_own(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
                let (lt, rt) = (self.lhs.typ(), self.rhs.typ());
                wrap!(
                    self,
                    arith_rule(&ctx.env, BinOp::$base, $checked, lt, rt, &self.typ)
                )?;
                defer_operand(ctx, &self.lhs);
                defer_operand(ctx, &self.rhs);
                Ok(())
            }
        }
    };
}

arith_op!(Add, false, Add);
arith_op!(Sub, false, Sub);
arith_op!(Mul, false, Mul);
arith_op!(Div, false, Div);
arith_op!(Mod, false, Mod);
arith_op!(CheckedAdd, true, Add);
arith_op!(CheckedSub, true, Sub);
arith_op!(CheckedMul, true, Mul);
arith_op!(CheckedDiv, true, Div);
arith_op!(CheckedMod, true, Mod);

impl<R: Rt, E: UserEvent> Neg<R, E> {
    fn typecheck_own(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.n, constrain_operand(&ctx.env, &Self::negatable(), self.n.typ()))?;
        wrap!(self, self.typ.check_contains(&ctx.env, self.n.typ()))?;
        defer_operand(ctx, &self.n);
        super::defer_settle(ctx, || crate::PendingSettle::Contains {
            outer: Self::negatable(),
            inner: self.n.typ().clone(),
            spec: Arc::new(self.n.spec().clone()),
        });
        Ok(())
    }
}
