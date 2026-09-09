use super::{CFlag, compiler::compile, coretraits, dense_gate};
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
            pub lhs: Node<R, E>,
            pub rhs: Node<R, E>,
            resident: TagValue,
            /// wake catch-up: set by `sleep()`, taken by the next update
            slept: bool,
        }

        impl<R: Rt, E: UserEvent> $name<R, E> {
            /// Build the comparison node from already-compiled children.
            /// Used by AOT-generated code.
            #[allow(dead_code)]
            pub fn new(lhs: Node<R, E>, rhs: Node<R, E>, spec: Expr) -> Node<R, E> {
                let typ = Type::Primitive(Typ::Bool.into());
                Node::new(Self { spec, typ, lhs, rhs, resident: TagValue::phantom(), slept: false })
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
                let lhs = compile(ctx, flags, lhs.clone(), scope, top_id)?;
                let rhs = compile(ctx, flags, rhs.clone(), scope, top_id)?;
                let typ = Type::Primitive(Typ::Bool.into());
                Ok(Node::new(Self { spec, typ, lhs, rhs, resident: TagValue::phantom(), slept: false }))
            }
        }

        impl<R: Rt, E: UserEvent> Update<R, E> for $name<R, E> {
            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                event: &mut Event<E>,
            ) -> &TagValue {
                let woke = std::mem::take(&mut self.slept);
                let (lhs, rhs, resident) =
                    (&mut self.lhs, &mut self.rhs, &mut self.resident);
                coretraits::with_value_hooks(ctx, event, |ctx, event| {
                    let l = lhs.update(ctx, event);
                    let r = rhs.update(ctx, event);
                    let (lt, rt) = (l.tag(), r.tag());
                    let trig = lt.triggers() || rt.triggers();
                    dense_gate!(resident, ctx, trig, lt.is_bottom() || rt.is_bottom(), woke);
                    let fired = lt.is_fired() || rt.is_fired();
                    let tag = if fired { $crate::Tag::FIRED } else { $crate::Tag::STALE };
                    let v = l.with_value(|lv| r.with_value(|rv| (lv $op rv).into()));
                    resident.set(TagValue::tagged(v, tag))
                })
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
                self.rhs.delete(ctx)
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.slept = true;
                self.lhs.sleep(ctx);
                self.rhs.sleep(ctx)
            }

            fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.reset_replay(ctx);
                self.rhs.reset_replay(ctx)
            }

            fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck0(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck0(ctx))?;
                // Both operands share one type. Probe both directions
                // without binding (a failed binding walk cannot be undone),
                // then commit the widening direction.
                let lt = self.lhs.typ().clone();
                let rt = self.rhs.typ().clone();
                use $crate::typ::ContainsFlags as CF;
                let rc = CF::RigidCheck.into();
                let commit = CF::AliasTVars | CF::InitTVars | CF::RigidCheck;
                if lt.contains_with_flags(rc, &ctx.env, &rt)? {
                    if !lt.contains_with_flags(commit, &ctx.env, &rt)? {
                        wrap!(self, lt.check_contains(&ctx.env, &rt))?;
                    }
                } else if rt.contains_with_flags(rc, &ctx.env, &lt)? {
                    if !rt.contains_with_flags(commit, &ctx.env, &lt)? {
                        wrap!(self, rt.check_contains(&ctx.env, &lt))?;
                    }
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
                wrap!(self.lhs, self.lhs.typecheck1(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck1(ctx))
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
                    &self.lhs,
                    &self.rhs,
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
            pub lhs: Node<R, E>,
            pub rhs: Node<R, E>,
            resident: TagValue,
            /// wake catch-up: set by `sleep()`, taken by the next update
            slept: bool,
        }

        impl<R: Rt, E: UserEvent> $name<R, E> {
            #[allow(dead_code)]
            pub fn new(lhs: Node<R, E>, rhs: Node<R, E>, spec: Expr) -> Node<R, E> {
                let typ = Type::Primitive(Typ::Bool.into());
                Node::new(Self { spec, typ, lhs, rhs, resident: TagValue::phantom(), slept: false })
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
                let lhs = compile(ctx, flags, lhs.clone(), scope, top_id)?;
                let rhs = compile(ctx, flags, rhs.clone(), scope, top_id)?;
                let typ = Type::Primitive(Typ::Bool.into());
                Ok(Node::new(Self { spec, typ, lhs, rhs, resident: TagValue::phantom(), slept: false }))
            }
        }

        impl<R: Rt, E: UserEvent> Update<R, E> for $name<R, E> {
            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                event: &mut Event<E>,
            ) -> &TagValue {
                // Strict, not short-circuit: `false && ⊥ = ⊥`.
                let woke = std::mem::take(&mut self.slept);
                let l = self.lhs.update(ctx, event);
                let r = self.rhs.update(ctx, event);
                let (lt, rt) = (l.tag(), r.tag());
                let trig = lt.triggers() || rt.triggers();
                dense_gate!(self.resident, ctx, trig, lt.is_bottom() || rt.is_bottom(), woke);
                let fired = lt.is_fired() || rt.is_fired();
                let tag = if fired { $crate::Tag::FIRED } else { $crate::Tag::STALE };
                let v = l.with_value(|lv| {
                    r.with_value(|rv| match (lv, rv) {
                        (Value::Bool(b0), Value::Bool(b1)) => {
                            Some(Value::Bool(*b0 $op *b1))
                        }
                        _ => None,
                    })
                });
                match v {
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
                self.lhs.refs(refs);
                self.rhs.refs(refs);
            }

            fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.delete(ctx);
                self.rhs.delete(ctx)
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.slept = true;
                self.lhs.sleep(ctx);
                self.rhs.sleep(ctx)
            }

            fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.reset_replay(ctx);
                self.rhs.reset_replay(ctx)
            }

            fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck0(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck0(ctx))?;
                let bt = Type::Primitive(Typ::Bool.into());
                wrap!(self.lhs, bt.check_contains(&ctx.env, self.lhs.typ()))?;
                wrap!(self.rhs, bt.check_contains(&ctx.env, self.rhs.typ()))?;
                wrap!(self, self.typ.check_contains(&ctx.env, &Type::boolean()))
            }

            fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck1(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck1(ctx))
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
                    &self.lhs,
                    &self.rhs,
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
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Not<R, E> {
    #[allow(dead_code)]
    pub fn new(n: Node<R, E>, spec: Expr) -> Node<R, E> {
        let typ = Type::Primitive(Typ::Bool.into());
        Node::new(Self { spec, typ, n, resident: TagValue::phantom() })
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
        Ok(Node::new(Self { spec, typ, n, resident: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Not<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.n.update(ctx, event);
        let tag = tv.tag();
        if tag.is_bottom() {
            return self.resident.set(TagValue::tagged(Value::Null, tag));
        }
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
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Neg<R, E> {
    #[allow(dead_code)]
    pub fn new(n: Node<R, E>, spec: Expr) -> Node<R, E> {
        Node::new(Self {
            spec,
            typ: Type::empty_tvar(),
            n,
            resident: TagValue::phantom(),
        })
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
        Ok(Node::new(Self {
            spec,
            typ: Type::empty_tvar(),
            n,
            resident: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Neg<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // Integers wrap, matching the JIT's `ineg`.
        let tv = self.n.update(ctx, event);
        let tag = tv.tag();
        if tag.is_bottom() {
            return self.resident.set(TagValue::tagged(Value::Null, tag));
        }
        let neg = tv.with_value(|v| match v {
            Value::I8(x) => Some(Value::I8(x.wrapping_neg())),
            Value::I16(x) => Some(Value::I16(x.wrapping_neg())),
            Value::I32(x) => Some(Value::I32(x.wrapping_neg())),
            Value::Z32(x) => Some(Value::Z32(x.wrapping_neg())),
            Value::I64(x) => Some(Value::I64(x.wrapping_neg())),
            Value::Z64(x) => Some(Value::Z64(x.wrapping_neg())),
            Value::F32(x) => Some(Value::F32(-*x)),
            Value::F64(x) => Some(Value::F64(-*x)),
            Value::Decimal(x) => Some(Value::Decimal(triomphe::Arc::new(-**x))),
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
        self.n.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck0(ctx))?;
        let negatable =
            Type::Primitive(Typ::signed_integer() | Typ::float() | Typ::Decimal);
        if self.n.typ().with_deref(|t| t.is_some()) {
            wrap!(self.n, negatable.check_contains(&ctx.env, self.n.typ()))?;
        } else if let Type::TVar(tv) = self.n.typ() {
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

/// Convert a raw `Value::Error` from netidx's `checked_*` ops into the
/// catchable `ArithError` value; success passes through. Shared by the
/// node-walk and the JIT's `graphix_value_checked_*` helpers.
pub(crate) fn wrap_arith_error(result: Value) -> Value {
    match result {
        Value::Error(e) => {
            let tag = Value::String(ARITH_ERR_TAG.clone());
            let err = Value::from(format_compact!("{e}"));
            let var = Value::Array(ValArray::from_iter([tag, err]));
            Value::Error(var.into())
        }
        v => v,
    }
}

/// Unchecked integer `+`/`-`/`*` wrap on overflow, matching the JIT.
/// Same-variant integer pairs only; every other shape returns `None`
/// and falls through to the netidx operator.
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
                &self.typ,
                &self.lhs,
                &self.rhs,
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
                &self.lhs,
                &self.rhs,
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
            pub lhs: Node<R, E>,
            pub rhs: Node<R, E>,
            resident: TagValue,
            /// wake catch-up: set by `sleep()`, taken by the next update
            slept: bool,
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
                Node::new(Self {
                    spec,
                    typ,
                    lhs,
                    rhs,
                    resident: TagValue::phantom(),
                    slept: false,
                })
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
                let lhs = compile(ctx, flags, lhs.clone(), scope, top_id)?;
                let rhs = compile(ctx, flags, rhs.clone(), scope, top_id)?;
                let typ = Type::empty_tvar();
                Ok(Node::new(Self {
                    spec,
                    typ,
                    lhs,
                    rhs,
                    resident: TagValue::phantom(),
                    slept: false,
                }))
            }

            /// `fn('a: Number, 'a) -> 'a`: both operands and the result
            /// are one numeric type. Idempotent; runs at typecheck0 and
            /// again at typecheck1 after the operand cells settle.
            fn typecheck_tail(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                let num = Type::Primitive(Typ::number());
                let lt = self.lhs.typ().clone();
                let rt = self.rhs.typ().clone();
                // A known operand must be numeric at typecheck0; the
                // def-time acceptance gate for a lambda body runs only there.
                for (known, t) in [
                    (lt.with_deref(|t| t.is_some()), &lt),
                    (rt.with_deref(|t| t.is_some()), &rt),
                ] {
                    if known {
                        wrap!(self, num.check_contains(&ctx.env, t))?;
                    } else if let Type::TVar(tv) = t {
                        tv.add_cell_constraint(num.clone());
                    }
                }
                // A declared `'a: Number` formal is rigid while its def
                // gate is open: `x + f64:0.` must reject, not bind 'a.
                use $crate::typ::ContainsFlags as CF;
                let rc = CF::RigidCheck.into();
                let commit = CF::AliasTVars | CF::InitTVars | CF::RigidCheck;
                let out = if lt.contains_with_flags(rc, &ctx.env, &rt)? {
                    if !lt.contains_with_flags(commit, &ctx.env, &rt)? {
                        wrap!(self, lt.check_contains(&ctx.env, &rt))?;
                    }
                    lt
                } else if rt.contains_with_flags(rc, &ctx.env, &lt)? {
                    if !rt.contains_with_flags(commit, &ctx.env, &lt)? {
                        wrap!(self, rt.check_contains(&ctx.env, &lt))?;
                    }
                    rt
                } else {
                    wrap!(
                        self,
                        $crate::format_with_flags(
                            $crate::PrintFlag::DerefTVars,
                            || -> Result<Type> {
                                bail!(
                                    "cannot compute {lt} {} {rt}: arithmetic \
                                     is fn('a: Number, 'a) -> 'a — both \
                                     operands must be one numeric type (cast \
                                     one side explicitly)",
                                    $opn
                                )
                            }
                        )
                    )?
                };
                let ut = if $checked {
                    Type::Set(Arc::from_iter([out, ARITH_ERR.clone()]))
                } else {
                    out
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
            ) -> &TagValue {
                let woke = std::mem::take(&mut self.slept);
                let l = self.lhs.update(ctx, event);
                let r = self.rhs.update(ctx, event);
                let (lt, rt) = (l.tag(), r.tag());
                let trig = lt.triggers() || rt.triggers();
                dense_gate!(
                    self.resident,
                    ctx,
                    trig,
                    lt.is_bottom() || rt.is_bottom(),
                    woke
                );
                let fired = lt.is_fired() || rt.is_fired();
                let tag = if fired { $crate::Tag::FIRED } else { $crate::Tag::STALE };
                if !$checked {
                    let v = l.with_value(|lv| {
                        r.with_value(|rv| wrapping_int_arith(BinOp::$base, lv, rv))
                    });
                    if let Some(v) = v {
                        return self.resident.set(TagValue::tagged(v, tag));
                    }
                }
                let result =
                    l.with_value(|lv| r.with_value(|rv| lv.clone().$method(rv.clone())));
                if $checked {
                    self.resident.set(TagValue::tagged(wrap_arith_error(result), tag))
                } else {
                    match result {
                        Value::Error(e) => {
                            // only a fresh failure logs
                            if trig {
                                log::error!(
                                    "arith error in {} at {} {e}",
                                    self.spec.ori,
                                    self.spec.pos
                                );
                                eprintln!(
                                    "arith error in {} at {} {e}",
                                    self.spec.ori, self.spec.pos
                                );
                            }
                            let btag = if trig {
                                $crate::Tag::FRESH_BOTTOM
                            } else {
                                $crate::Tag::TAINT
                            };
                            self.resident.set(TagValue::tagged(Value::Null, btag))
                        }
                        v => self.resident.set(TagValue::tagged(v, tag)),
                    }
                }
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
                self.slept = true;
                self.lhs.sleep(ctx);
                self.rhs.sleep(ctx);
            }

            fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.lhs.reset_replay(ctx);
                self.rhs.reset_replay(ctx);
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

            fn view(&self) -> $crate::NodeView<'_, R, E> {
                $crate::NodeView::$name(self)
            }
        }
    };
}

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
