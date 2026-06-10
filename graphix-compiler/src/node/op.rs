use super::{compiler::compile, CFlag, Cached};
use crate::{
    defetyp,
    expr::{Expr, ExprId},
    typ::Type,
    wrap, Event, ExecCtx, Node, Refs, Rt, Scope, Update, UserEvent,
};
use anyhow::{bail, Result};
use arcstr::ArcStr;
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_value::{Typ, ValArray, Value};
use std::fmt;
use std::ops::{Add as _, Div as _, Mul as _, Rem as _, Sub as _};
use triomphe::Arc;

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
            ) -> Option<Value> {
                let lhs_up = self.lhs.update(ctx, event);
                let rhs_up = self.rhs.update(ctx, event);
                if lhs_up || rhs_up {
                    return self.lhs.cached.as_ref().and_then(|lhs| {
                        self.rhs.cached.as_ref().map(|rhs| (lhs $op rhs).into())
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
                self.lhs.node.sleep(ctx);
                self.rhs.node.sleep(ctx)
            }

            fn typecheck_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs.node, self.lhs.node.typecheck(ctx))?;
                wrap!(self.rhs.node, self.rhs.node.typecheck(ctx))?;
                wrap!(
                    self,
                    self.lhs.node.typ().check_contains(&ctx.env, &self.rhs.node.typ())
                )?;
                wrap!(self, self.typ.check_contains(&ctx.env, &Type::boolean()))
            }

            fn view(&self) -> $crate::NodeView<'_, R, E> {
                $crate::NodeView::$name(self)
            }

            fn clone_rebind(
                &self,
                ctx: &mut ExecCtx<R, E>,
                scope: &Scope,
            ) -> Node<R, E> {
                Box::new(Self {
                    spec: self.spec.clone(),
                    typ: self.typ.clone(),
                    lhs: Cached::new(self.lhs.node.clone_rebind(ctx, scope)),
                    rhs: Cached::new(self.rhs.node.clone_rebind(ctx, scope)),
                })
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
            ) -> Option<Value> {
                let lhs_up = self.lhs.update(ctx, event);
                let rhs_up = self.rhs.update(ctx, event);
                if lhs_up || rhs_up {
                    // STRICT — like every other binary op, `&&`/`||` need
                    // BOTH operands. A bottom (non-firing) operand makes
                    // the result bottom: `false && ⊥ = ⊥`, `true || ⊥ =
                    // ⊥`. NOT short-circuit: in a dataflow language a
                    // value must reflect all its inputs, so a downstream
                    // consumer never commits to a decision before every
                    // input is known.
                    return match (self.lhs.cached.as_ref(), self.rhs.cached.as_ref()) {
                        (Some(Value::Bool(b0)), Some(Value::Bool(b1))) => Some(Value::Bool(*b0 $op *b1)),
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

            fn typecheck_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs.node, self.lhs.node.typecheck(ctx))?;
                wrap!(self.rhs.node, self.rhs.node.typecheck(ctx))?;
                let bt = Type::Primitive(Typ::Bool.into());
                wrap!(self.lhs.node, bt.check_contains(&ctx.env, self.lhs.node.typ()))?;
                wrap!(self.rhs.node, bt.check_contains(&ctx.env, self.rhs.node.typ()))?;
                wrap!(self, self.typ.check_contains(&ctx.env, &Type::boolean()))
            }

            fn view(&self) -> $crate::NodeView<'_, R, E> {
                $crate::NodeView::$name(self)
            }

            fn clone_rebind(
                &self,
                ctx: &mut ExecCtx<R, E>,
                scope: &Scope,
            ) -> Node<R, E> {
                Box::new(Self {
                    spec: self.spec.clone(),
                    typ: self.typ.clone(),
                    lhs: Cached::new(self.lhs.node.clone_rebind(ctx, scope)),
                    rhs: Cached::new(self.rhs.node.clone_rebind(ctx, scope)),
                })
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
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        self.n.update(ctx, event).and_then(|v| match v {
            Value::Bool(b) => Some(Value::Bool(!b)),
            _ => None,
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

    fn typecheck_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck(ctx))?;
        let bt = Type::Primitive(Typ::Bool.into());
        wrap!(self.n, bt.check_contains(&ctx.env, self.n.typ()))?;
        wrap!(self, self.typ.check_contains(&ctx.env, &Type::boolean()))
    }

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::Not(self)
    }

    fn clone_rebind(
        &self,
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
    ) -> Node<R, E> {
        Box::new(Self {
            spec: self.spec.clone(),
            typ: self.typ.clone(),
            n: self.n.clone_rebind(ctx, scope),
        })
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

macro_rules! arith_op {
    ($name:ident, $opn:expr, $checked:literal, $method:ident) => {
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
                rhs: &Expr
            ) -> Result<Node<R, E>> {
                let lhs = Cached::new(compile(ctx, flags, lhs.clone(), scope, top_id)?);
                let rhs = Cached::new(compile(ctx, flags, rhs.clone(), scope, top_id)?);
                let typ = Type::empty_tvar();
                Ok(Box::new(Self { spec, typ, lhs, rhs }))
            }
        }

        impl<R: Rt, E: UserEvent> Update<R, E> for $name<R, E> {
            fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
                let lhs_up = self.lhs.update(ctx, event);
                let rhs_up = self.rhs.update(ctx, event);
                let lhs = self.lhs.cached.as_ref()?;
                let rhs = self.rhs.cached.as_ref()?;
                if lhs_up || rhs_up {
                    let result = lhs.clone().$method(rhs.clone());
                    match result {
                        Value::Error(e) if $checked => {
                            let tag = Value::String(ARITH_ERR_TAG.clone());
                            let err = Value::from(format_compact!("{e}"));
                            let var = Value::Array(ValArray::from_iter([tag, err]));
                            Some(Value::Error(Arc::new(var)))
                        }
                        Value::Error(e) => {
                            log::error!("arith error in {} at {} {e}", self.spec.ori, self.spec.pos);
                            eprintln!("arith error in {} at {} {e}", self.spec.ori, self.spec.pos);
                            None
                        }
                        v => Some(v)
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

            fn typecheck_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
                wrap!(self.lhs.node, self.lhs.node.typecheck(ctx))?;
                wrap!(self.rhs.node, self.rhs.node.typecheck(ctx))?;
                let lhs = self.lhs.node.typ();
                let rhs = self.rhs.node.typ();
                match (lhs.with_deref(|t| t.cloned()), rhs.with_deref(|t| t.cloned())) {
                    (None, None) | (Some(_), Some(_)) => (),
                    (Some(t), None) => { let _ = rhs.contains(&ctx.env, &t); }
                    (None, Some(t)) => { let _ = lhs.contains(&ctx.env, &t); },
                }
                // init types that aren't known by now to Number
                let typ = Type::Primitive(Typ::number());
                wrap!(self.lhs.node, typ.contains(&ctx.env, lhs))?;
                wrap!(self.rhs.node, typ.contains(&ctx.env, rhs))?;
                // Duration and DateTime can be involved in some arith operations however
                let typ = Type::Primitive(Typ::number() | Typ::Duration | Typ::DateTime);
                wrap!(self.lhs.node, typ.check_contains(&ctx.env, lhs))?;
                wrap!(self.rhs.node, typ.check_contains(&ctx.env, rhs))?;
                let base = $opn.base_op();
                let ut = match (lhs.with_deref(|t| t.cloned()), rhs.with_deref(|t| t.cloned())) {
                    (None, _) | (_, None) => bail!("type must be known"),
                    (Some(lhs@ Type::Primitive(p0)), Some(rhs@ Type::Primitive(p1))) => {
                        if p0.contains(Typ::DateTime) {
                            if p1 == Typ::Duration && (base == Op::Add || base == Op::Sub) {
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
                            if p1 == Typ::Duration && (base == Op::Add || base == Op::Sub) {
                                Type::Primitive(Typ::Duration.into())
                            } else if (Typ::integer() | Typ::F32 | Typ::F64).contains(p1) && (base == Op::Mul || base == Op::Div) {
                                Type::Primitive(Typ::Duration.into())
                            } else {
                                bail!("can't perform {lhs} {} {rhs}", $opn)
                            }
                        } else if p1.contains(Typ::Duration) {
                            if (Typ::integer() | Typ::F32 | Typ::F64).contains(p0) && base == Op::Mul {
                                Type::Primitive(Typ::Duration.into())
                            } else {
                                bail!("can't perform {lhs} {} {rhs}", $opn)
                            }
                        } else {
                            wrap!(self, lhs.union(&ctx.env, &rhs))?
                        }
                    }
                    (Some(_), Some(_)) => wrap!(self, lhs.union(&ctx.env, rhs))?
                };
                let ut = if $checked {
                    Type::Set(Arc::from_iter([ut, ARITH_ERR.clone()]))
                } else {
                    ut
                };
                wrap!(self, self.typ.check_contains(&ctx.env, &ut))?;
                Ok(())
            }

            fn view(&self) -> $crate::NodeView<'_, R, E> {
                $crate::NodeView::$name(self)
            }

            fn clone_rebind(
                &self,
                ctx: &mut ExecCtx<R, E>,
                scope: &Scope,
            ) -> Node<R, E> {
                Box::new(Self {
                    spec: self.spec.clone(),
                    typ: self.typ.clone(),
                    lhs: Cached::new(self.lhs.node.clone_rebind(ctx, scope)),
                    rhs: Cached::new(self.rhs.node.clone_rebind(ctx, scope)),
                })
            }
        }
    }
}

// Unchecked ops use the operator trait methods (`add` = wrapping for ints,
// `div`/`rem` error on divide-by-zero). Checked ops use the `checked_*`
// inherent methods, which return `Value::Error` on integer overflow /
// underflow / divide-by-zero — the `arith_op!` body then wraps that error as
// the `ArithError` union. Using the bare `+`/`-`/`*` operators for the checked
// variants (the previous behavior) silently wrapped on overflow, so `+?`/`-?`/
// `*?` never produced an error.
arith_op!(Add, Op::Add, false, add);
arith_op!(Sub, Op::Sub, false, sub);
arith_op!(Mul, Op::Mul, false, mul);
arith_op!(Div, Op::Div, false, div);
arith_op!(Mod, Op::Mod, false, rem);

arith_op!(CheckedAdd, Op::CheckedAdd, true, checked_add);
arith_op!(CheckedSub, Op::CheckedSub, true, checked_sub);
arith_op!(CheckedMul, Op::CheckedMul, true, checked_mul);
arith_op!(CheckedDiv, Op::CheckedDiv, true, checked_div);
arith_op!(CheckedMod, Op::CheckedMod, true, checked_rem);
