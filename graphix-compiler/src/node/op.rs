use super::{CFlag, WakeBit, compiler::compile, coretraits, dense_gate};
use crate::{
    CompileCtx, ExecCtx, Node, NodeView, Rt, Scope, TagValue, Update, UserEvent, defetyp,
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
/// their codec, plumbing and checks. `$typ` is the result type at
/// construction; the operator supplies `update`, `emit_clif` and
/// `typecheck_own`, which an instance runs again only for an operator that
/// `settles` and whose row its definition's check did not record.
macro_rules! binary_node {
    ($name:ident, $typ:expr, $(state: $state:ty,)? $(settles: $settles:literal,)? { $($methods:tt)* }) => {
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
            $(
                /// the operator's own
                state: $state,
            )?
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
                    $(state: <$state>::default(),)?
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
                if false $(|| $settles)? && !types.settle(self.spec.id, &self.typ) {
                    self.typecheck_own(ctx)?;
                }
                Ok(())
            }

            fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
                f(&self.lhs);
                f(&self.rhs)
            }

            fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
                f(&mut self.lhs);
                f(&mut self.rhs)
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
        $self.fork.decide_siblings($ctx, 2, || {
            $crate::analysis::independent([&*lhs, &*rhs].into_iter(), $ctx)
        });
        let (l, r) = $crate::branch::join2(
            &mut $self.fork,
            $ctx,
            |c| lhs.update(c),
            |c| rhs.update(c),
        );
        let (lt, rt) = (l.tag(), r.tag());
        let tag = lt.join(rt);
        dense_gate!($self.resident, tag, tag.is_bottom(), woke);
        (l, r, tag.triggers(), tag)
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
    ($name:ident, $op:tt, $ordered:literal) => {
        binary_node!($name, Type::boolean(), state: Equality, {
            fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
                if !$ordered && matches!(self.state, Equality::Undecided) {
                    self.state = Equality::of(&ctx.env, self.lhs.typ(), self.rhs.typ());
                }
                let (l, r, _, tag) = gated_operands!(self, ctx);
                let v = match &self.state {
                    Equality::Targets(t) => {
                        let lv = l.with_value(|v| ctx.ref_targets(t, v));
                        let rv = r.with_value(|v| ctx.ref_targets(t, v));
                        coretraits::with_hooks(ctx, || (lv $op rv).into())
                    }
                    Equality::Undecided | Equality::Values => coretraits::with_hooks(ctx, || {
                        l.with_value(|lv| r.with_value(|rv| (lv $op rv).into()))
                    }),
                };
                self.resident.set(TagValue::tagged(v, tag))
            }

            fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
                emit_cmp_node(cx, CmpOp::$name, &self.lhs, &self.rhs)
            }
        });

        impl<R: Rt, E: UserEvent> $name<R, E> {
            fn typecheck_own(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
                let (lt, rt) = (self.lhs.typ(), self.rhs.typ());
                match wrap!(self, operand_type(&ctx.env, lt, rt))? {
                    Some(t) => {
                        let bound = match $ordered {
                            true => Type::Ordered,
                            false => Type::Discernible,
                        };
                        t.require_compared(&bound);
                        let judgment = crate::PendingSettle::Discernible {
                            typ: t.clone(),
                            what: crate::Discerned::Compared { ordered: $ordered },
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

compare_op!(Eq, ==, false);
compare_op!(Ne, !=, false);
compare_op!(Lt, <, true);
compare_op!(Gt, >, true);
compare_op!(Lte, <=, true);
compare_op!(Gte, >=, true);

/// How `==` and `!=` compare, decided at the first update from the
/// operand type: by value, or with each reference replaced by what it
/// names (a reference's value is its own cell).
#[derive(Debug, Default)]
enum Equality {
    #[default]
    Undecided,
    Values,
    Targets(Type),
}

impl Equality {
    fn of(env: &Env, lhs: &Type, rhs: &Type) -> Self {
        match [lhs, rhs].into_iter().find(|t| t.compares_refs(env)) {
            Some(t) => Self::Targets(t.clone()),
            None => Self::Values,
        }
    }
}

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
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.n)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.n)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Not, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.n.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let tv = self.n.update(ctx);
        let tag = tv.tag();
        dense_gate!(self, tag, tag.is_bottom());
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
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.n)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.n)
    }

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
        dense_gate!(self, tag, tag.is_bottom());
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
        match types.settle(self.spec.id, &self.typ) {
            true => Ok(()),
            false => self.typecheck_own(ctx),
        }
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
/// Unchecked `+ - *` wrap, matching the JIT; `x % -1` is 0; any other
/// overflow and a zero divisor are an error value naming the failure.
/// `None` for every other shape.
// XCR claude for claude: [doc-drift] The docs disagree with this function. CLAUDE.md says
// "unchecked wraps, integer div0 bottoms", but unchecked / and % also bottom on MIN /
// -1 and MIN % -1 (checked_div/checked_rem below; the JIT tests for this case in
// fusion/emit/nodes.rs). book/src/core/fundamental_types.md:28 and :56 say unchecked
// operators bottom on overflow, but + - * wrap (MAX + 1 gives MIN in both engines), and
// fused code logs nothing. The book's ArithError strings ("attempt to divide by zero"
// and "attempt to subtract with overflow" at fundamental_types.md:70 and :86,
// error.md:93) are produced nowhere: every integer failure is "arithmetic error" (line
// 669), which names neither cause. Fix the three docs, or decide that unchecked / and %
// wrap too. Either way, have line 669 say which failure happened. (c-error-op-06)
// 2026-10-09 claude: Eric ruled 10-09 (with x-engine-collections-07): + - * wrap; /
// bottoms on a zero divisor and on MIN / -1; x % -1 is 0, MIN included, checked and
// unchecked, and % bottoms only on a zero divisor; every failure names its cause (attempt
// to add with overflow, .. divide by zero, .. divide with overflow, .. calculate the
// remainder with a divisor of zero). Both engines share op::arith; the JIT's inline guard
// divides by 1 for a -1 divisor of %. The book (fundamental_types, polymorphism,
// reading_types) and CLAUDE.md say so. Pins:
// lang::errors::checked_failures_name_their_cause, checked_div0, rem_by_neg_one_is_zero
// (its jit modes fail with the JIT guard undone).
const ADD_OVERFLOW: ArcStr = literal!("attempt to add with overflow");
const SUB_OVERFLOW: ArcStr = literal!("attempt to subtract with overflow");
const MUL_OVERFLOW: ArcStr = literal!("attempt to multiply with overflow");
const DIV_ZERO: ArcStr = literal!("attempt to divide by zero");
const DIV_OVERFLOW: ArcStr = literal!("attempt to divide with overflow");
const REM_ZERO: ArcStr =
    literal!("attempt to calculate the remainder with a divisor of zero");

fn int_arith(op: BinOp, checked: bool, l: &Value, r: &Value) -> Option<Value> {
    macro_rules! int {
        ($va:ident, $a:expr, $b:expr) => {{
            let (a, b) = ($a, $b);
            let v = match (op, checked) {
                (BinOp::Add, false) => Ok(a.wrapping_add(b)),
                (BinOp::Sub, false) => Ok(a.wrapping_sub(b)),
                (BinOp::Mul, false) => Ok(a.wrapping_mul(b)),
                (BinOp::Add, true) => a.checked_add(b).ok_or(ADD_OVERFLOW),
                (BinOp::Sub, true) => a.checked_sub(b).ok_or(SUB_OVERFLOW),
                (BinOp::Mul, true) => a.checked_mul(b).ok_or(MUL_OVERFLOW),
                // XCR claude for claude: [doc-drift] Unchecked / and % bottom on MIN / -1
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
                // 2026-10-09 claude: fixed with c-error-op-06's ruling above: MIN % -1
                // and MIN %? -1 are 0 in both engines. Pin:
                // lang::errors::rem_by_neg_one_is_zero.
                (BinOp::Div, _) if b == 0 => Err(DIV_ZERO),
                (BinOp::Div, _) => a.checked_div(b).ok_or(DIV_OVERFLOW),
                (BinOp::Mod, _) if b == 0 => Err(REM_ZERO),
                (BinOp::Mod, _) => Ok(a.wrapping_rem(b)),
            };
            Some(match v {
                Ok(v) => Value::$va(v),
                Err(msg) => Value::error(msg),
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

/// `fn<'a: Number + Singleton>(x: 'a, y: 'a) -> 'a`: both operands
/// `lt` and `rt` and the result `out` are one numeric type. Idempotent.
pub(crate) fn arith_rule(
    env: &Env,
    op: crate::expr::BinOp,
    checked: bool,
    lt: &Type,
    rt: &Type,
    out: &Type,
) -> Result<()> {
    // A declared `'a: Number` formal is rigid while its def gate is
    // open: `x + f64:0.` must reject, not bind 'a.
    let Some(t) = operand_type(env, lt, rt)? else {
        return crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
            bail!(
                "cannot compute {lt} {} {rt}: arithmetic is \
                 fn<'a: Number + Singleton>(x: 'a, y: 'a) -> 'a — both operands must \
                 be one numeric type (cast one side explicitly)",
                op.token()
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
        binary_node!($name, Type::empty_tvar(), settles: true, {
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
                        self.resident.set_bottom_as(tag)
                    }
                    v => self.resident.set(TagValue::tagged(v, tag)),
                }
            }

        });

        impl<R: Rt, E: UserEvent> $name<R, E> {
            fn typecheck_own(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
                let (lt, rt) = (self.lhs.typ(), self.rhs.typ());
                wrap!(
                    self,
                    arith_rule(
                        &ctx.env,
                        crate::expr::BinOp::$name,
                        $checked,
                        lt,
                        rt,
                        &self.typ
                    )
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
