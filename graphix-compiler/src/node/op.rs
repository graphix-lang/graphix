use super::{CFlag, Cached, compiler::compile};
use crate::{
    Event, ExecCtx, Node, NodeView, Refs, Rt, Scope, TagValue, Update, UserEvent,
    defetyp,
    expr::{Expr, ExprId},
    fusion::emit::{BodyCx, CompiledExpr, emit_neg_node, emit_not_node},
    typ::Type,
    wrap,
};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_value::{Typ, ValArray, Value};
use std::{
    fmt,
    ops::{Add as _, Div as _, Mul as _, Rem as _, Sub as _},
};
use triomphe::Arc;

// ─── Scalar operator taxonomy ────────────────────────────────────
//
// The arithmetic / comparison / boolean operators, named once and
// shared by both evaluators: the node-walk in this module constructs
// them and the JIT (`fusion::emit`) consumes them when emitting CLIF.
// They live with the node-walk — the canonical evaluator that defines
// these operators' semantics — and the JIT imports them from here.

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

macro_rules! compare_op {
    ($name:ident, $op:tt) => {
        #[derive(Debug)]
        pub struct $name<R: Rt, E: UserEvent> {
            pub(crate) spec: Expr,
            pub typ: Type,
            pub lhs: Cached<R, E>,
            pub rhs: Cached<R, E>,
        }

        impl<R: Rt, E: UserEvent> $name<R, E> {
            /// Build the comparison node from already-compiled children.
            /// Used by AOT-generated code.
            #[allow(dead_code)]
            pub fn new(lhs: Node<R, E>, rhs: Node<R, E>, spec: Expr) -> Node<R, E> {
                let lhs = Cached::new(lhs);
                let rhs = Cached::new(rhs);
                let typ = Type::Primitive(Typ::Bool.into());
                Box::new(Self { spec, typ, lhs, rhs })
            }

            pub(crate) fn compile(
                ctx: &mut ExecCtx<R, E>,
                flags: BitFlags<CFlag>,
                spec: Expr,
                scope: &Scope,
                top_id: ExprId,
                lhs: &Expr,
                rhs: &Expr
            ) -> Result<Node<R, E>> {
                let lhs = Cached::new(compile(ctx, flags, lhs.clone(), scope, top_id)?);
                let rhs = Cached::new(compile(ctx, flags, rhs.clone(), scope, top_id)?);
                let typ = Type::Primitive(Typ::Bool.into());
                Ok(Box::new(Self { spec, typ, lhs, rhs }))
            }
        }

        impl<R: Rt, E: UserEvent> Update<R, E> for $name<R, E> {
            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                event: &mut Event<E>,
            ) -> Option<TagValue> {
                // Two-channel propagation, the CLIF `propagate_flags`
                // twin: ANY production (fired or stale) recomputes and
                // emits; the result fires iff a consumed production
                // fired (AND-reduced STALE); consumed-cache taint ORs
                // in and short-circuits the op.
                let l = self.lhs.update(ctx, event);
                let r = self.rhs.update(ctx, event);
                if l.is_some() || r.is_some() {
                    if self.lhs.tag.is_tainted() || self.rhs.tag.is_tainted() {
                        return Some(TagValue::tainted(Value::Null));
                    }
                    let fired = l.is_some_and(|t| t.is_fired())
                        || r.is_some_and(|t| t.is_fired());
                    let tag = if fired { $crate::Tag::FIRED } else { $crate::Tag::STALE };
                    return self.lhs.cached.as_ref().and_then(|lhs| {
                        self.rhs
                            .cached
                            .as_ref()
                            .map(|rhs| TagValue::tagged((lhs $op rhs).into(), tag))
                    })
                }
                None
            }

            fn spec(&self) -> &Expr {
                &self.spec
            }

            fn typ(&self) -> &Type {
                &self.typ
            }

            fn refs(&self, refs: &mut Refs) {
                self.lhs.node.refs(refs);
                self.rhs.node.refs(refs);
            }

            fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.node.delete(ctx);
                self.rhs.node.delete(ctx)
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.sleep(ctx);
                self.rhs.sleep(ctx)
            }

            fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.reset_replay(ctx);
                self.rhs.reset_replay(ctx)
            }

            fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs.node, self.lhs.node.typecheck0(ctx))?;
                wrap!(self.rhs.node, self.rhs.node.typecheck0(ctx))?;
                // `fn('a, 'a) -> bool` (Eric's ruling, 2026-07-12):
                // both operands are ONE type. The old asymmetric
                // `lhs ⊇ rhs` admitted direction-dependent cross-type
                // compares whose runtime meaning is the netidx
                // Typ-DISCRIMINANT order — numeric nonsense for
                // `i64:1 < f64:2.` (and the instance-elaboration
                // acceptance witnesses rode exactly that looseness).
                // PROBE both directions first (empty flags — no
                // binding: a failed binding walk has no backtracking,
                // so committing the losing direction first would
                // pollute cells), then COMMIT the widening direction.
                let lt = self.lhs.node.typ().clone();
                let rt = self.rhs.node.typ().clone();
                let e = BitFlags::empty();
                if lt.contains_with_flags(e, &ctx.env, &rt)? {
                    wrap!(self, lt.check_contains(&ctx.env, &rt))?;
                } else if rt.contains_with_flags(e, &ctx.env, &lt)? {
                    wrap!(self, rt.check_contains(&ctx.env, &lt))?;
                } else {
                    wrap!(
                        self,
                        $crate::format_with_flags(
                            $crate::PrintFlag::DerefTVars,
                            || -> Result<()> {
                                bail!(
                                    "cannot compare {lt} with {rt}: comparison \
                                     is fn('a, 'a) -> bool — both operands must \
                                     be one type (cast one side explicitly)"
                                )
                            }
                        )
                    )?;
                }
                wrap!(self, self.typ.check_contains(&ctx.env, &Type::boolean()))
            }

            fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs.node, self.lhs.node.typecheck1(ctx))?;
                wrap!(self.rhs.node, self.rhs.node.typecheck1(ctx))
            }

            fn view(&self) -> $crate::NodeView<'_, R, E> {
                $crate::NodeView::$name(self)
            }

            fn emit_clif(
                &self,
                cx: &mut $crate::fusion::emit::BodyCx,
            ) -> Result<$crate::fusion::emit::CompiledExpr> {
                $crate::fusion::emit::emit_cmp_node(
                    cx,
                    $crate::node::op::CmpOp::$name,
                    &self.lhs.node,
                    &self.rhs.node,
                )
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
        #[derive(Debug)]
        pub struct $name<R: Rt, E: UserEvent> {
            pub(crate) spec: Expr,
            pub typ: Type,
            pub lhs: Cached<R, E>,
            pub rhs: Cached<R, E>,
        }

        impl<R: Rt, E: UserEvent> $name<R, E> {
            #[allow(dead_code)]
            pub fn new(lhs: Node<R, E>, rhs: Node<R, E>, spec: Expr) -> Node<R, E> {
                let lhs = Cached::new(lhs);
                let rhs = Cached::new(rhs);
                let typ = Type::Primitive(Typ::Bool.into());
                Box::new(Self { spec, typ, lhs, rhs })
            }

            pub(crate) fn compile(
                ctx: &mut ExecCtx<R, E>,
                flags: BitFlags<CFlag>,
                spec: Expr,
                scope: &Scope,
                top_id: ExprId,
                lhs: &Expr,
                rhs: &Expr
            ) -> Result<Node<R, E>> {
                let lhs = Cached::new(compile(ctx, flags, lhs.clone(), scope, top_id)?);
                let rhs = Cached::new(compile(ctx, flags, rhs.clone(), scope, top_id)?);
                let typ = Type::Primitive(Typ::Bool.into());
                Ok(Box::new(Self { spec, typ, lhs, rhs }))
            }
        }

        impl<R: Rt, E: UserEvent> Update<R, E> for $name<R, E> {
            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                event: &mut Event<E>,
            ) -> Option<TagValue> {
                let l = self.lhs.update(ctx, event);
                let r = self.rhs.update(ctx, event);
                if l.is_some() || r.is_some() {
                    if self.lhs.tag.is_tainted() || self.rhs.tag.is_tainted() {
                        return Some(TagValue::tainted(Value::Null));
                    }
                    let fired = l.is_some_and(|t| t.is_fired())
                        || r.is_some_and(|t| t.is_fired());
                    let tag = if fired { $crate::Tag::FIRED } else { $crate::Tag::STALE };
                    // STRICT — like every other binary op, `&&`/`||` need
                    // BOTH operands. A bottom (non-firing) operand makes
                    // the result bottom: `false && ⊥ = ⊥`, `true || ⊥ =
                    // ⊥`. NOT short-circuit: in a dataflow language a
                    // value must reflect all its inputs, so a downstream
                    // consumer never commits to a decision before every
                    // input is known.
                    return match (self.lhs.cached.as_ref(), self.rhs.cached.as_ref()) {
                        (Some(Value::Bool(b0)), Some(Value::Bool(b1))) => {
                            Some(TagValue::tagged(Value::Bool(*b0 $op *b1), tag))
                        }
                        (_, _) => None
                    }
                }
                None
            }

            fn spec(&self) -> &Expr {
                &self.spec
            }

            fn typ(&self) -> &Type {
                &self.typ
            }

            fn refs(&self, refs: &mut Refs) {
                self.lhs.node.refs(refs);
                self.rhs.node.refs(refs);
            }

            fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.node.delete(ctx);
                self.rhs.node.delete(ctx)
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.sleep(ctx);
                self.rhs.sleep(ctx)
            }

            fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.reset_replay(ctx);
                self.rhs.reset_replay(ctx)
            }

            fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs.node, self.lhs.node.typecheck0(ctx))?;
                wrap!(self.rhs.node, self.rhs.node.typecheck0(ctx))?;
                let bt = Type::Primitive(Typ::Bool.into());
                wrap!(self.lhs.node, bt.check_contains(&ctx.env, self.lhs.node.typ()))?;
                wrap!(self.rhs.node, bt.check_contains(&ctx.env, self.rhs.node.typ()))?;
                wrap!(self, self.typ.check_contains(&ctx.env, &Type::boolean()))
            }

            fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs.node, self.lhs.node.typecheck1(ctx))?;
                wrap!(self.rhs.node, self.rhs.node.typecheck1(ctx))
            }

            fn view(&self) -> $crate::NodeView<'_, R, E> {
                $crate::NodeView::$name(self)
            }

            fn emit_clif(
                &self,
                cx: &mut $crate::fusion::emit::BodyCx,
            ) -> Result<$crate::fusion::emit::CompiledExpr> {
                $crate::fusion::emit::emit_bool_node(
                    cx,
                    $crate::node::op::BoolOp::$name,
                    &self.lhs.node,
                    &self.rhs.node,
                )
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
}

impl<R: Rt, E: UserEvent> Not<R, E> {
    #[allow(dead_code)]
    pub fn new(n: Node<R, E>, spec: Expr) -> Node<R, E> {
        let typ = Type::Primitive(Typ::Bool.into());
        Box::new(Self { spec, typ, n })
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        n: &Expr,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, flags, n.clone(), scope, top_id)?;
        let typ = Type::Primitive(Typ::Bool.into());
        Ok(Box::new(Self { spec, typ, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Not<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
        self.n.update(ctx, event).and_then(|tv| {
            if tv.is_tainted() {
                return Some(TagValue::tainted(Value::Null));
            }
            let (v, tag) = tv.into_parts();
            match v {
                Value::Bool(b) => Some(TagValue::tagged(Value::Bool(!b), tag)),
                _ => None,
            }
        })
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

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck0(ctx))?;
        let bt = Type::Primitive(Typ::Bool.into());
        wrap!(self.n, bt.check_contains(&ctx.env, self.n.typ()))?;
        wrap!(self, self.typ.check_contains(&ctx.env, &Type::boolean()))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
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
}

impl<R: Rt, E: UserEvent> Neg<R, E> {
    #[allow(dead_code)]
    pub fn new(n: Node<R, E>, spec: Expr) -> Node<R, E> {
        Box::new(Self { spec, typ: Type::empty_tvar(), n })
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        n: &Expr,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, flags, n.clone(), scope, top_id)?;
        Ok(Box::new(Self { spec, typ: Type::empty_tvar(), n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Neg<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
        // Unchecked negation: integers wrap (two's-complement, matching the
        // JIT's `ineg`); floats/decimal negate directly. `Value` equality
        // collapses Z32/I32 and Z64/I64, so producing the operand's own
        // variant agrees with the JIT's I32/I64-discriminated result.
        self.n.update(ctx, event).and_then(|tv| {
            if tv.is_tainted() {
                return Some(TagValue::tainted(Value::Null));
            }
            let (v, tag) = tv.into_parts();
            let neg = match v {
                Value::I8(x) => Some(Value::I8(x.wrapping_neg())),
                Value::I16(x) => Some(Value::I16(x.wrapping_neg())),
                Value::I32(x) => Some(Value::I32(x.wrapping_neg())),
                Value::Z32(x) => Some(Value::Z32(x.wrapping_neg())),
                Value::I64(x) => Some(Value::I64(x.wrapping_neg())),
                Value::Z64(x) => Some(Value::Z64(x.wrapping_neg())),
                Value::F32(x) => Some(Value::F32(-x)),
                Value::F64(x) => Some(Value::F64(-x)),
                Value::Decimal(x) => Some(Value::Decimal(triomphe::Arc::new(-*x))),
                _ => None,
            };
            neg.map(|v| TagValue::tagged(v, tag))
        })
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

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck0(ctx))?;
        // The operand must be a signed-negatable number, so `-x` on an
        // unsigned type is a compile-time error rather than a silent
        // runtime underflow. Output type = operand type.
        let negatable =
            Type::Primitive(Typ::signed_integer() | Typ::float() | Typ::Decimal);
        if self.n.typ().with_deref(|t| t.is_some()) {
            wrap!(self.n, negatable.check_contains(&ctx.env, self.n.typ()))?;
        } else if let Type::TVar(tv) = self.n.typ() {
            // constrain, don't bind — the check re-runs at typecheck1
            // once the cell settles (design/tvar_constraints.md phase B)
            tv.add_cell_constraint(negatable);
        }
        wrap!(self, self.typ.check_contains(&ctx.env, self.n.typ()))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck1(ctx))?;
        if let Type::TVar(tv) = self.n.typ() {
            wrap!(self.n, tv.settle(&ctx.env))?;
        }
        let negatable =
            Type::Primitive(Typ::signed_integer() | Typ::float() | Typ::Decimal);
        wrap!(self.n, negatable.check_contains(&ctx.env, self.n.typ()))
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Neg(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_neg_node(cx, &self.n)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Op {
    Add,
    CheckedAdd,
    Sub,
    CheckedSub,
    Mul,
    CheckedMul,
    Div,
    CheckedDiv,
    Mod,
    CheckedMod,
}

impl Op {
    fn base_op(self) -> Op {
        match self {
            Op::CheckedAdd => Op::Add,
            Op::CheckedSub => Op::Sub,
            Op::CheckedMul => Op::Mul,
            Op::CheckedDiv => Op::Div,
            Op::CheckedMod => Op::Mod,
            other => other,
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::Add => write!(f, "+"),
            Op::CheckedAdd => write!(f, "+?"),
            Op::Sub => write!(f, "-"),
            Op::CheckedSub => write!(f, "-?"),
            Op::Mul => write!(f, "*"),
            Op::CheckedMul => write!(f, "*?"),
            Op::Div => write!(f, "/"),
            Op::CheckedDiv => write!(f, "/?"),
            Op::Mod => write!(f, "%"),
            Op::CheckedMod => write!(f, "%?"),
        }
    }
}

defetyp!(ARITH_ERR, ARITH_ERR_TAG, "ArithError", "Error<`{}(string)>");

/// Wrap a checked-arith result: a raw `Value::Error` from netidx's
/// `checked_*` ops (overflow / underflow / div-by-zero) becomes the
/// catchable `ArithError` error VALUE the checked operators produce
/// (`[T, Error<`ArithError(string)>]`); success passes through. The
/// single semantic core shared by the node-walk update (canonical) and
/// the JIT's `graphix_value_checked_*` helpers — the two can't drift.
pub(crate) fn wrap_arith_error(result: Value) -> Value {
    match result {
        Value::Error(e) => {
            let tag = Value::String(ARITH_ERR_TAG.clone());
            let err = Value::from(format_compact!("{e}"));
            let var = Value::Array(ValArray::from_iter([tag, err]));
            Value::Error(Arc::new(var))
        }
        v => v,
    }
}

/// Unchecked integer `+`/`-`/`*` WRAP on overflow — the documented
/// semantics, matching the JIT's `iadd`/`isub`/`imul` and [`Neg`]'s
/// two's-complement `wrapping_neg`. netidx's `Value` operators return an
/// overflow Error instead, which the unchecked path converts to bottom —
/// and a bottomed tail-call argument stalls its loop FOREVER (soak
/// finding 2026-07-04; Eric's ruling: the node-walk was wrong here).
/// Same-variant integer pairs only; every other shape (floats, mixed
/// coercions, datetime/duration, div/mod which keep bottom-on-div0)
/// falls through to the netidx operator.
fn wrapping_int_arith(op: BinOp, l: &Value, r: &Value) -> Option<Value> {
    macro_rules! w {
        ($va:ident, $a:expr, $b:expr) => {
            match op {
                BinOp::Add => Some(Value::$va($a.wrapping_add($b))),
                BinOp::Sub => Some(Value::$va($a.wrapping_sub($b))),
                BinOp::Mul => Some(Value::$va($a.wrapping_mul($b))),
                BinOp::Div | BinOp::Mod => None,
            }
        };
    }
    match (l, r) {
        (Value::I8(a), Value::I8(b)) => w!(I8, *a, *b),
        (Value::I16(a), Value::I16(b)) => w!(I16, *a, *b),
        (Value::I32(a), Value::I32(b)) => w!(I32, *a, *b),
        (Value::I64(a), Value::I64(b)) => w!(I64, *a, *b),
        (Value::U8(a), Value::U8(b)) => w!(U8, *a, *b),
        (Value::U16(a), Value::U16(b)) => w!(U16, *a, *b),
        (Value::U32(a), Value::U32(b)) => w!(U32, *a, *b),
        (Value::U64(a), Value::U64(b)) => w!(U64, *a, *b),
        (Value::V32(a), Value::V32(b)) => w!(V32, *a, *b),
        (Value::V64(a), Value::V64(b)) => w!(V64, *a, *b),
        (Value::Z32(a), Value::Z32(b)) => w!(Z32, *a, *b),
        (Value::Z64(a), Value::Z64(b)) => w!(Z64, *a, *b),
        _ => None,
    }
}

/// Generate the `Update::emit_clif` override for an [`arith_op!`] type.
/// `$base` is the unchecked [`BinOp`] (`Add` for both `+`
/// and `+?`). Unchecked ops emit through the shared arith relay;
/// checked ops route to the checked relay (Value-shape result — the
/// success value or the `ArithError` error value).
macro_rules! arith_emit_clif {
    (false, $base:ident) => {
        fn emit_clif(
            &self,
            cx: &mut $crate::fusion::emit::BodyCx,
        ) -> Result<$crate::fusion::emit::CompiledExpr> {
            $crate::fusion::emit::emit_arith_node(
                cx,
                $crate::node::op::BinOp::$base,
                &self.lhs.node,
                &self.rhs.node,
            )
        }
    };
    (true, $base:ident) => {
        fn emit_clif(
            &self,
            cx: &mut $crate::fusion::emit::BodyCx,
        ) -> Result<$crate::fusion::emit::CompiledExpr> {
            $crate::fusion::emit::emit_checked_arith_node(
                cx,
                $crate::node::op::BinOp::$base,
                &self.lhs.node,
                &self.rhs.node,
            )
        }
    };
}

macro_rules! arith_op {
    ($name:ident, $opn:expr, $checked:tt, $method:ident, $base:ident) => {
        #[derive(Debug)]
        pub struct $name<R: Rt, E: UserEvent> {
            pub(crate) spec: Expr,
            pub typ: Type,
            pub lhs: Cached<R, E>,
            pub rhs: Cached<R, E>,
        }

        impl<R: Rt, E: UserEvent> $name<R, E> {
            /// Build the arithmetic op from already-compiled children,
            /// with the resolved `typ` supplied by the caller. AOT
            /// codegen uses this to skip the interpreter's late type
            /// unification — the type is already known after
            /// typecheck.
            #[allow(dead_code)]
            pub fn new(
                lhs: Node<R, E>,
                rhs: Node<R, E>,
                typ: Type,
                spec: Expr,
            ) -> Node<R, E> {
                let lhs = Cached::new(lhs);
                let rhs = Cached::new(rhs);
                Box::new(Self { spec, typ, lhs, rhs })
            }

            pub(crate) fn compile(
                ctx: &mut ExecCtx<R, E>,
                flags: BitFlags<CFlag>,
                spec: Expr,
                scope: &Scope,
                top_id: ExprId,
                lhs: &Expr,
                rhs: &Expr,
            ) -> Result<Node<R, E>> {
                let lhs = Cached::new(compile(ctx, flags, lhs.clone(), scope, top_id)?);
                let rhs = Cached::new(compile(ctx, flags, rhs.clone(), scope, top_id)?);
                let typ = Type::empty_tvar();
                Ok(Box::new(Self { spec, typ, lhs, rhs }))
            }

            /// The `ut` result-typing table. Runs at typecheck0 when both
            /// operand types are already known, otherwise deferred to
            /// typecheck1 after the operand cells settle
            /// (design/tvar_constraints.md phase B). Idempotent.
            fn typecheck_tail(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                let lhs = self.lhs.node.typ();
                let rhs = self.rhs.node.typ();
                // init types that aren't known by now to Number
                let typ = Type::Primitive(Typ::number());
                wrap!(self.lhs.node, typ.contains(&ctx.env, lhs))?;
                wrap!(self.rhs.node, typ.contains(&ctx.env, rhs))?;
                // Duration and DateTime can be involved in some arith operations however
                let typ = Type::Primitive(Typ::number() | Typ::Duration | Typ::DateTime);
                wrap!(self.lhs.node, typ.check_contains(&ctx.env, lhs))?;
                wrap!(self.rhs.node, typ.check_contains(&ctx.env, rhs))?;
                let base = $opn.base_op();
                let ut = match (
                    lhs.with_deref(|t| t.cloned()),
                    rhs.with_deref(|t| t.cloned()),
                ) {
                    (None, _) | (_, None) => bail!("type must be known"),
                    (
                        Some(lhs @ Type::Primitive(p0)),
                        Some(rhs @ Type::Primitive(p1)),
                    ) => {
                        if p0.contains(Typ::DateTime) {
                            if p1 == Typ::Duration && (base == Op::Add || base == Op::Sub)
                            {
                                Type::Primitive(Typ::DateTime.into())
                            } else {
                                bail!("can't perform {lhs} {} {rhs}", $opn)
                            }
                        } else if p1.contains(Typ::DateTime) {
                            if p0 == Typ::Duration && base == Op::Add {
                                Type::Primitive(Typ::DateTime.into())
                            } else {
                                bail!("can't perform {lhs} {} {rhs}", $opn)
                            }
                        } else if p0.contains(Typ::Duration) {
                            if p1 == Typ::Duration && (base == Op::Add || base == Op::Sub)
                            {
                                Type::Primitive(Typ::Duration.into())
                            } else if (Typ::integer() | Typ::F32 | Typ::F64).contains(p1)
                                && (base == Op::Mul || base == Op::Div)
                            {
                                Type::Primitive(Typ::Duration.into())
                            } else {
                                bail!("can't perform {lhs} {} {rhs}", $opn)
                            }
                        } else if p1.contains(Typ::Duration) {
                            if (Typ::integer() | Typ::F32 | Typ::F64).contains(p0)
                                && base == Op::Mul
                            {
                                Type::Primitive(Typ::Duration.into())
                            } else {
                                bail!("can't perform {lhs} {} {rhs}", $opn)
                            }
                        } else {
                            wrap!(self, lhs.union(&ctx.env, &rhs))?
                        }
                    }
                    (Some(_), Some(_)) => wrap!(self, lhs.union(&ctx.env, rhs))?,
                };
                let ut = if $checked {
                    Type::Set(Arc::from_iter([ut, ARITH_ERR.clone()]))
                } else {
                    ut
                };
                wrap!(self, self.typ.check_contains(&ctx.env, &ut))?;
                Ok(())
            }
        }

        impl<R: Rt, E: UserEvent> Update<R, E> for $name<R, E> {
            arith_emit_clif!($checked, $base);

            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                event: &mut Event<E>,
            ) -> Option<TagValue> {
                let l = self.lhs.update(ctx, event);
                let r = self.rhs.update(ctx, event);
                let produced = l.is_some() || r.is_some();
                if self.lhs.tag.is_tainted() || self.rhs.tag.is_tainted() {
                    // never attempt the op on a taint placeholder — pass
                    // the taint toward its force point (and don't log a
                    // synthetic error off it)
                    return if produced {
                        Some(TagValue::tainted(Value::Null))
                    } else {
                        None
                    };
                }
                let lhs = self.lhs.cached.as_ref()?;
                let rhs = self.rhs.cached.as_ref()?;
                if produced {
                    let fired = l.is_some_and(|t| t.is_fired())
                        || r.is_some_and(|t| t.is_fired());
                    let tag = if fired { $crate::Tag::FIRED } else { $crate::Tag::STALE };
                    if !$checked {
                        if let Some(v) = wrapping_int_arith(BinOp::$base, lhs, rhs) {
                            return Some(TagValue::tagged(v, tag));
                        }
                    }
                    let result = lhs.clone().$method(rhs.clone());
                    if $checked {
                        Some(TagValue::tagged(wrap_arith_error(result), tag))
                    } else {
                        match result {
                            Value::Error(e) => {
                                if ctx.frame_depth > 0 {
                                    // In a sequential frame this is a
                                    // GENUINE bottom — the kernel's taint
                                    // channel, and like the kernel it is
                                    // SILENT (the log is a reactive-mode
                                    // debugging aid, not value semantics).
                                    return Some(TagValue::tainted(Value::Null));
                                }
                                log::error!(
                                    "arith error in {} at {} {e}",
                                    self.spec.ori,
                                    self.spec.pos
                                );
                                eprintln!(
                                    "arith error in {} at {} {e}",
                                    self.spec.ori, self.spec.pos
                                );
                                None
                            }
                            v => Some(TagValue::tagged(v, tag)),
                        }
                    }
                } else {
                    None
                }
            }

            fn spec(&self) -> &Expr {
                &self.spec
            }

            fn typ(&self) -> &Type {
                &self.typ
            }

            fn refs(&self, refs: &mut Refs) {
                self.lhs.node.refs(refs);
                self.rhs.node.refs(refs);
            }

            fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.node.delete(ctx);
                self.rhs.node.delete(ctx);
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.sleep(ctx);
                self.rhs.sleep(ctx);
            }

            fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.reset_replay(ctx);
                self.rhs.reset_replay(ctx);
            }

            fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs.node, self.lhs.node.typecheck0(ctx))?;
                wrap!(self.rhs.node, self.rhs.node.typecheck0(ctx))?;
                {
                    // The operand pre-bind: an UNANNOTATED formal
                    // paired with a known operand infers MONOMORPHIC
                    // (first concrete fact wins — `|x| x + i64:1` is
                    // fn(i64) -> i64; annotate `'a: Number` to get
                    // promotion polymorphism). Retiring this for
                    // numerics was tried under ruling (a) and reverted:
                    // an impl that stays generic breaks the interface /
                    // fn-subsumption matchers, which compare signatures
                    // structurally (`val add: fn(x: i64) -> i64` vs
                    // `fn(x: '_a: [Real, i64])` — dynamic_module0,
                    // first_class_lambdas). ANNOTATED formals are
                    // rigid, skip this bind, and get the promotion
                    // obligation below instead.
                    let lhs = self.lhs.node.typ();
                    let rhs = self.rhs.node.typ();
                    match (lhs.with_deref(|t| t.cloned()), rhs.with_deref(|t| t.cloned()))
                    {
                        (None, None) | (Some(_), Some(_)) => (),
                        (Some(t), None) => {
                            let _ = rhs.contains(&ctx.env, &t);
                        }
                        (None, Some(t)) => {
                            let _ = lhs.contains(&ctx.env, &t);
                        }
                    }
                }
                let lk = self.lhs.node.typ().with_deref(|t| t.is_some());
                let rk = self.rhs.node.typ().with_deref(|t| t.is_some());
                if lk && rk {
                    return self.typecheck_tail(ctx);
                }
                // A KNOWN operand paired with an unbound one still has to
                // be arith-compatible — check it NOW (pure containment,
                // binds nothing). typecheck_tail's full `ut` narrowing is
                // deferred to typecheck1, but the def-time acceptance gate
                // for a lambda body is typecheck0 only (typecheck1 on the
                // body runs only in swallowed per-site rechecks), so a
                // known-incompatible operand like `x + "hello"` must be
                // rejected here or it never is — the JIT would otherwise
                // emit an i64 add on the string's payload word and leak a
                // pointer (#16, soak jul04).
                let arith =
                    Type::Primitive(Typ::number() | Typ::Duration | Typ::DateTime);
                if lk {
                    wrap!(
                        self.lhs.node,
                        arith.check_contains(&ctx.env, self.lhs.node.typ())
                    )?;
                }
                if rk {
                    wrap!(
                        self.rhs.node,
                        arith.check_contains(&ctx.env, self.rhs.node.typ())
                    )?;
                }
                // Constrain, don't bind (design/tvar_constraints.md
                // phase B): an unbound operand cell records "arithmetic
                // happened here" as a cell conjunct; the `ut` table runs
                // at typecheck1 once the cells settle.
                //
                // PROMOTION OBLIGATION (Eric's ruling (a), 2026-07-12):
                // when the OTHER operand is a concrete numeric, the
                // generic cell records the ABSORBER set instead of bare
                // Number — the site's instantiation must be a type the
                // runtime promotion keeps (`i64 + f64:0.` is F64 at
                // runtime, so a formal used that way can't instantiate
                // at i64; `x + i64:1` still admits every float and
                // i64). The conjunct rides the ordinary constraint
                // machinery: carried to site copies by `reset_tvars`,
                // validated by `cell_constraints_ok` at the arg bind —
                // where the rejection propagates, unlike the instance
                // body's swallowed union check. Closes the class where
                // the JIT froze a scalar slot the promoted value never
                // honors (fuzz/pending-ruling/promo_lie_dual_mono.gx).
                // Only the current def gate's OWN declared tvars are
                // eligible (`ExecCtx::promo_eligible`): a param's
                // quantified 'a reached through `f(y) + 1` must keep
                // the bare Number conjunct — constraining it re-created
                // the 5634fbdc poisoned-conjunct class
                // (first_class_lambdas).
                let promo = |ctx: &ExecCtx<R, E>,
                             tv: &$crate::typ::TVar,
                             other: &Type|
                 -> Option<Type> {
                    if !ctx.promo_eligible.contains(&tv.cell_addr()) {
                        return None;
                    }
                    other.with_deref(|t| match t {
                        Some(Type::Primitive(p)) if p.len() == 1 => {
                            crate::typ::numeric_absorbers(p.iter().next().unwrap())
                                .map(Type::Primitive)
                        }
                        _ => None,
                    })
                };
                let num = Type::Primitive(Typ::number());
                if !lk && let Type::TVar(tv) = self.lhs.node.typ() {
                    match promo(ctx, tv, self.rhs.node.typ()) {
                        Some(c) => tv.add_cell_constraint(c),
                        None => tv.add_cell_constraint(num.clone()),
                    }
                }
                if !rk && let Type::TVar(tv) = self.rhs.node.typ() {
                    match promo(ctx, tv, self.lhs.node.typ()) {
                        Some(c) => tv.add_cell_constraint(c),
                        None => tv.add_cell_constraint(num.clone()),
                    }
                }
                // Result: same-cell operands make arith type-preserving,
                // so the result IS the operand cell (`|a| a + a` stays
                // polymorphic). Distinct cells get a fresh
                // number-constrained result cell that typecheck1's `ut`
                // narrows.
                let same_cell = match (self.lhs.node.typ(), self.rhs.node.typ()) {
                    (Type::TVar(a), Type::TVar(b)) => a.same_cell(b),
                    _ => false,
                };
                let rt = if same_cell {
                    self.lhs.node.typ().clone()
                } else {
                    let rt = Type::empty_tvar();
                    if let Type::TVar(tv) = &rt {
                        tv.add_cell_constraint(num);
                    }
                    rt
                };
                let ut = if $checked {
                    Type::Set(Arc::from_iter([rt, ARITH_ERR.clone()]))
                } else {
                    rt
                };
                wrap!(self, self.typ.check_contains(&ctx.env, &ut))
            }

            fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs.node, self.lhs.node.typecheck1(ctx))?;
                wrap!(self.rhs.node, self.rhs.node.typecheck1(ctx))?;
                // Settle still-unbound operand cells to their
                // conjunction, then run the deferred `ut`.
                if let Type::TVar(tv) = self.lhs.node.typ() {
                    wrap!(self.lhs.node, tv.settle(&ctx.env))?;
                }
                if let Type::TVar(tv) = self.rhs.node.typ() {
                    wrap!(self.rhs.node, tv.settle(&ctx.env))?;
                }
                self.typecheck_tail(ctx)
            }

            fn view(&self) -> $crate::NodeView<'_, R, E> {
                $crate::NodeView::$name(self)
            }
        }
    };
}

// Unchecked ops use the operator trait methods (`add` = wrapping for ints,
// `div`/`rem` error on divide-by-zero). Checked ops use the `checked_*`
// inherent methods, which return `Value::Error` on integer overflow /
// underflow / divide-by-zero — the `arith_op!` body then wraps that error as
// the `ArithError` union. Using the bare `+`/`-`/`*` operators for the checked
// variants (the previous behavior) silently wrapped on overflow, so `+?`/`-?`/
// `*?` never produced an error.
arith_op!(Add, Op::Add, false, add, Add);
arith_op!(Sub, Op::Sub, false, sub, Sub);
arith_op!(Mul, Op::Mul, false, mul, Mul);
arith_op!(Div, Op::Div, false, div, Div);
arith_op!(Mod, Op::Mod, false, rem, Mod);

arith_op!(CheckedAdd, Op::CheckedAdd, true, checked_add, Add);
arith_op!(CheckedSub, Op::CheckedSub, true, checked_sub, Sub);
arith_op!(CheckedMul, Op::CheckedMul, true, checked_mul, Mul);
arith_op!(CheckedDiv, Op::CheckedDiv, true, checked_div, Div);
arith_op!(CheckedMod, Op::CheckedMod, true, checked_rem, Mod);
