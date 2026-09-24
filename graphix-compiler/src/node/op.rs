use super::{CFlag, WakeBit, compiler::compile, coretraits, dense_gate};
use crate::{
    Event, ExecCtx, Node, NodeView, Refs, Rt, Scope, Tag, TagValue, Update, UserEvent,
    defetyp,
    env::Env,
    expr::{Expr, ExprId},
    fusion::emit::{
        BodyCx, CompiledExpr, emit_arith_node, emit_bool_node, emit_checked_arith_node,
        emit_cmp_node, emit_neg_node, emit_not_node,
    },
    image::{
        ImageBuf,
        nodes::{NodeTag, decode_node, put_tag, tag_len},
    },
    node::error::{diagnostic_site, report_failure},
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
        }

        impl<R: Rt, E: UserEvent> $name<R, E> {
            pub(crate) fn compile(
                ctx: &mut ExecCtx<R, E>,
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
                ctx: &mut ExecCtx<R, E>,
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
                })
            }
        }

        impl<R: Rt, E: UserEvent> Update<R, E> for $name<R, E> {
            fn image_len(&self) -> usize {
                tag_len()
                    + self.spec.encoded_len()
                    + self.typ.encoded_len()
                    + self.lhs.image_len()
                    + self.rhs.image_len()
            }

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

            fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.delete(ctx);
                self.rhs.delete(ctx);
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.slept.set();
                self.lhs.sleep(ctx);
                self.rhs.sleep(ctx);
            }

            fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.reset_replay(ctx);
                self.rhs.reset_replay(ctx);
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
    ($self:ident, $ctx:ident, $event:ident) => {{
        let woke = $self.slept.take();
        let l = $self.lhs.update($ctx, $event);
        let r = $self.rhs.update($ctx, $event);
        let (lt, rt) = (l.tag(), r.tag());
        let trig = lt.triggers() || rt.triggers();
        dense_gate!($self.resident, $ctx, trig, lt.is_bottom() || rt.is_bottom(), woke);
        let tag = if lt.is_fired() || rt.is_fired() { Tag::FIRED } else { Tag::STALE };
        (l, r, trig, tag)
    }};
}

/// Both operands of a binary operator are one type: probe each
/// direction without binding (a failed binding walk cannot be undone),
/// then commit the one that holds. The committed type, or `None` when
/// neither operand's type contains the other's.
fn unify_operands(env: &Env, lt: &Type, rt: &Type) -> Result<Option<Type>> {
    let probe = ContainsFlags::RigidCheck.into();
    let commit =
        ContainsFlags::AliasTVars | ContainsFlags::InitTVars | ContainsFlags::RigidCheck;
    let (wide, narrow) = if lt.contains_with_flags(probe, env, rt)? {
        (lt, rt)
    } else if rt.contains_with_flags(probe, env, lt)? {
        (rt, lt)
    } else {
        return Ok(None);
    };
    if !wide.contains_with_flags(commit, env, narrow)? {
        wide.check_contains(env, narrow)?;
    }
    Ok(Some(wide.clone()))
}

/// An operand whose type is known must be in `bound` now; an open cell
/// carries `bound` as a constraint for later.
fn constrain_operand(env: &Env, bound: &Type, t: &Type) -> Result<()> {
    if t.with_deref(|t| t.is_some()) {
        bound.check_contains(env, t)
    } else {
        if let Type::TVar(tv) = t {
            tv.add_cell_constraint(bound.clone());
        }
        Ok(())
    }
}

macro_rules! compare_op {
    ($name:ident, $op:tt) => {
        binary_node!($name, Type::boolean(), {
            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                event: &mut Event<E>,
            ) -> &TagValue {
                let (l, r, _, tag) = gated_operands!(self, ctx, event);
                // XCR claude for eric: undecided by the docs, so not changed: `[i64, f64]`
                // operands of two variants order by `Value` (variant first) while arith
                // promotes. Recommend refusing `<`..`>=` over a union of 2+ numeric types
                // at typecheck: a numeric `<` would disagree with `==`, map keys and sort.
                let v = coretraits::with_hooks(ctx, event, || {
                    l.with_value(|lv| r.with_value(|rv| (lv $op rv).into()))
                });
                self.resident.set(TagValue::tagged(v, tag))
            }

            fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck0(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck0(ctx))?;
                let (lt, rt) = (self.lhs.typ(), self.rhs.typ());
                if wrap!(self, unify_operands(&ctx.env, lt, rt))?.is_none() {
                    return wrap!(
                        self,
                        $crate::format_with_flags($crate::PrintFlag::DerefTVars, || {
                            bail!(
                                "cannot compare {lt} with {rt}: comparison is \
                                 fn('a, 'a) -> bool — both operands must be one type \
                                 (cast one side explicitly)"
                            )
                        })
                    );
                }
                Ok(())
            }

            fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck1(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck1(ctx))
            }

            fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
                emit_cmp_node(cx, CmpOp::$name, &self.lhs, &self.rhs)
            }
        });
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
            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                event: &mut Event<E>,
            ) -> &TagValue {
                let (l, r, _, tag) = gated_operands!(self, ctx, event);
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

            fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck0(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck0(ctx))?;
                let bt = Type::boolean();
                wrap!(self.lhs, bt.check_contains(&ctx.env, self.lhs.typ()))?;
                wrap!(self.rhs, bt.check_contains(&ctx.env, self.rhs.typ()))
            }

            fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck1(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck1(ctx))
            }

            fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
                emit_bool_node(cx, BoolOp::$name, &self.lhs, &self.rhs)
            }
        });
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
        ctx: &mut ExecCtx<R, E>,
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
        ctx: &mut ExecCtx<R, E>,
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
    fn image_len(&self) -> usize {
        tag_len() + self.spec.encoded_len() + self.typ.encoded_len() + self.n.image_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Not, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.n.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.n.update(ctx, event);
        let tag = tv.tag();
        dense_gate!(self, ctx, tag.triggers(), tag.is_bottom());
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

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.slept.set();
        self.n.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck0(ctx))?;
        wrap!(self.n, Type::boolean().check_contains(&ctx.env, self.n.typ()))
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
    resident: TagValue,
    /// wake catch-up: set by `sleep()`, taken by the next update
    slept: WakeBit,
}

impl<R: Rt, E: UserEvent> Neg<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
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
        ctx: &mut ExecCtx<R, E>,
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
    fn image_len(&self) -> usize {
        tag_len() + self.spec.encoded_len() + self.typ.encoded_len() + self.n.image_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Neg, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.n.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // Integers wrap, matching the JIT's `ineg`.
        let tv = self.n.update(ctx, event);
        let tag = tv.tag();
        dense_gate!(self, ctx, tag.triggers(), tag.is_bottom());
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

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.slept.set();
        self.n.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck0(ctx))?;
        wrap!(self.n, constrain_operand(&ctx.env, &Self::negatable(), self.n.typ()))?;
        wrap!(self, self.typ.check_contains(&ctx.env, self.n.typ()))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck1(ctx))?;
        if let Type::TVar(tv) = self.n.typ() {
            wrap!(self.n, tv.settle(&ctx.env))?;
        }
        wrap!(self.n, Self::negatable().check_contains(&ctx.env, self.n.typ()))
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

macro_rules! arith_op {
    ($name:ident, $checked:tt, $base:ident) => {
        impl<R: Rt, E: UserEvent> $name<R, E> {
            /// `fn('a: Number, 'a) -> 'a`: both operands and the result
            /// are one numeric type. Idempotent; runs at typecheck0 and
            /// again at typecheck1 after the operand cells settle.
            fn typecheck_tail(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                let num = Type::number();
                let (lt, rt) = (self.lhs.typ(), self.rhs.typ());
                // A known operand must be numeric at typecheck0; the
                // def-time acceptance gate for a lambda body runs only there.
                wrap!(self, constrain_operand(&ctx.env, &num, lt))?;
                wrap!(self, constrain_operand(&ctx.env, &num, rt))?;
                // A declared `'a: Number` formal is rigid while its def
                // gate is open: `x + f64:0.` must reject, not bind 'a.
                let Some(out) = wrap!(self, unify_operands(&ctx.env, lt, rt))? else {
                    return wrap!(
                        self,
                        $crate::format_with_flags($crate::PrintFlag::DerefTVars, || {
                            bail!(
                                "cannot compute {lt} {}{} {rt}: arithmetic is \
                                 fn('a: Number, 'a) -> 'a — both operands must be \
                                 one numeric type (cast one side explicitly)",
                                BinOp::$base.symbol(),
                                if $checked { "?" } else { "" }
                            )
                        })
                    );
                };
                let ut = if $checked {
                    Type::Set(Arc::from_iter([out, ARITH_ERR.clone()]))
                } else {
                    out
                };
                wrap!(self, self.typ.check_contains(&ctx.env, &ut))
            }
        }

        binary_node!($name, Type::empty_tvar(), {
            arith_emit_clif!($checked, $base);

            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                event: &mut Event<E>,
            ) -> &TagValue {
                let (l, r, trig, tag) = gated_operands!(self, ctx, event);
                let v = l.with_value(|lv| {
                    r.with_value(|rv| arith(BinOp::$base, $checked, lv.clone(), rv.clone()))
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

            fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck0(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck0(ctx))?;
                self.typecheck_tail(ctx)
            }

            fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck1(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck1(ctx))?;
                if let Type::TVar(tv) = self.lhs.typ() {
                    wrap!(self.lhs, tv.settle(&ctx.env))?;
                }
                if let Type::TVar(tv) = self.rhs.typ() {
                    wrap!(self.rhs, tv.settle(&ctx.env))?;
                }
                self.typecheck_tail(ctx)
            }
        });
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
