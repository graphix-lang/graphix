use super::{CFlag, WakeBit, compiler::compile, coretraits, dense_gate};
use crate::{
    CompileCtx, ExecCtx, Node, NodeView, Rt, Scope, TagValue, Update, defetyp,
    env::Env,
    expr::{Expr, ExprId, Told},
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
    typ::{Bound, TVar, Type, bounds},
    wrap,
};
use anyhow::Result;
use arcstr::{ArcStr, literal};
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Typ, Value};
use triomphe::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// An arithmetic operator, checked or not as its node says.
pub enum ArithOp {
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
        pub struct $name<R: Rt> {
            pub(crate) spec: Arc<Expr>,
            pub typ: Type,
            pub lhs: Node<R>,
            pub rhs: Node<R>,
            resident: TagValue,
            /// wake catch-up: set by `sleep()`, taken by the next update
            // CR claude for claude: [doc-drift/low] The doc above, "wake catch-up: set
            // by `sleep()`, taken by the next update", names the catch-up that
            // bind-driven bodies deleted. WakeBit's own doc says what the bit does now:
            // the first update after a sleep recomputes from the present world. Delete
            // this doc and its twins: Unary's `slept` in this file, and the `slept`
            // fields in array.rs, map.rs, data.rs and lambda.rs. Found by reading; not
            // run. (c-error-op-05)
            slept: WakeBit,
            pub(crate) fork: crate::cost::ForkSite,
            $(
                /// the operator's own
                state: $state,
            )?
        }

        impl<R: Rt> $name<R> {
            pub(crate) fn compile(
                ctx: &mut CompileCtx<R>,
                flags: BitFlags<CFlag>,
                spec: Arc<Expr>,
                scope: &Scope,
                top_id: ExprId,
                lhs: &Expr,
                rhs: &Expr,
            ) -> Result<Node<R>> {
                let lhs = compile(ctx, flags, lhs.clone(), scope, top_id)?;
                let rhs = compile(ctx, flags, rhs.clone(), scope, top_id)?;
                let typ = ctx.instance_node_type(spec.id).unwrap_or_else(|| $typ);
                Ok(Self::node(spec, typ, lhs, rhs, Default::default()))
            }

            pub(crate) fn image_decode(
                ctx: &mut ExecCtx<'_, R>,
                buf: &mut &[u8],
            ) -> Result<Node<R>, PackError> {
                let spec = Arc::new(Expr::decode(buf)?);
                let typ = Type::decode(buf)?;
                let lhs = decode_node(ctx, buf)?;
                let rhs = decode_node(ctx, buf)?;
                let fork = crate::cost::ForkSite::image_decode(buf)?;
                Ok(Self::node(spec, typ, lhs, rhs, fork))
            }

            fn node(
                spec: Arc<Expr>,
                typ: Type,
                lhs: Node<R>,
                rhs: Node<R>,
                fork: crate::cost::ForkSite,
            ) -> Node<R> {
                Node::new(Self {
                    spec,
                    typ,
                    lhs,
                    rhs,
                    resident: TagValue::phantom(),
                    slept: WakeBit::default(),
                    fork,
                    $(state: <$state>::default(),)?
                })
            }
        }

        impl<R: Rt> Update<R> for $name<R> {
            fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
                put_tag(NodeTag::$name, buf);
                self.spec.encode(buf)?;
                self.typ.encode(buf)?;
                self.lhs.image_encode(buf)?;
                self.rhs.image_encode(buf)?;
                self.fork.image_encode(buf)
            }

            fn spec(&self) -> &Expr {
                &self.spec
            }

            fn typ(&self) -> &Type {
                &self.typ
            }

            fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck0(ctx))?;
                wrap!(self.rhs, self.rhs.typecheck0(ctx))?;
                self.typecheck_own(ctx)
            }

            fn typecheck0_instance(
                &mut self,
                ctx: &mut CompileCtx<R>,
                types: &mut super::lambda::InstanceTypes,
            ) -> Result<()> {
                wrap!(self.lhs, self.lhs.typecheck0_instance(ctx, types))?;
                wrap!(self.rhs, self.rhs.typecheck0_instance(ctx, types))?;
                if false $(|| $settles)? && !types.settle(ctx, &self.spec, &self.typ)? {
                    self.typecheck_own(ctx)?;
                }
                Ok(())
            }

            fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
                f(&self.lhs);
                f(&self.rhs)
            }

            fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
                f(&mut self.lhs);
                f(&mut self.rhs)
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
                self.slept.set();
                self.lhs.sleep(ctx);
                self.rhs.sleep(ctx);
            }

            fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
                fusion::fuse_parts([&mut self.lhs, &mut self.rhs], ctx)
            }

            fn view(&self) -> NodeView<'_, R> {
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
        $self.fork.decide_siblings($ctx, 2, [&*lhs, &*rhs].into_iter());
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

/// Under a check by bounds, the one type both operands of an operator
/// have: a cell each flows into and that flows into each; `singleton`
/// where it must be a single type, as arithmetic's is.
fn same_by_bounds(env: &Env, lt: &Type, rt: &Type, holds: Bound) -> Result<Type> {
    let v = TVar::default();
    super::solving(|s| {
        s.declare(&v, Type::Bound(holds));
        s.track(&v)
    });
    let v = Type::TVar(v);
    // an operand that is never there is of every type: it ties nothing
    let never = |t: &Type| t.with_deref(|t| matches!(t, Some(Type::Bottom)));
    for t in [lt, rt].into_iter().filter(|t| !never(t)) {
        v.check_contains(env, t)?;
        t.check_contains(env, &v)?
    }
    Ok(v)
}

/// A comparison's operands under a check by bounds: what both may be
/// holds the predicate `holds`, and one's type holds the other's
/// (`Solving::compare`); neither decides the other's.
fn compared_by_bounds(env: &Env, lt: &Type, rt: &Type, holds: Bound) -> Result<()> {
    let v = TVar::default();
    super::solving(|s| {
        s.declare(&v, Type::Bound(holds));
        s.track(&v);
        s.compare(lt.clone(), rt.clone(), holds == Bound::Ordered)
    });
    let v = Type::TVar(v);
    [lt, rt].into_iter().try_for_each(|t| v.check_contains(env, t))
}

macro_rules! compare_op {
    ($name:ident, $op:tt, $ordered:literal) => {
        binary_node!($name, Type::boolean(), state: Equality, {
            fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
                if matches!(self.state, Equality::Undecided) {
                    self.state = Equality::of(&ctx.env, self.lhs.typ(), self.rhs.typ(), $ordered);
                }
                let (l, r, _, tag) = gated_operands!(self, ctx);
                if $ordered && matches!(self.state, Equality::Targets(_)) {
                    // CR claude for claude: [bug/medium] A standing ordered compare
                    // over references hidden under Any logs "references have no order,
                    // including under Any" at every cycle its statement is visited, not
                    // once. The bottom it leaves makes dense_gate! recompute it at each
                    // visit, and coretraits::ordered logs unconditionally, whereas the
                    // arith ops log only when `trig` is set. The line also names no
                    // site, unlike "arith error <site> ...". Fix: keep the `trig` that
                    // gated_operands! returns, test with crate::reference::orderable,
                    // and report once, with diagnostic_site(&self.spec), when trig is
                    // set. probe:
                    // design/review-2026-11-16/repro/c-error-op-ordered-ref-log-flood.gx
                    // (c-error-op-01)
                    let valid = coretraits::with_hooks(ctx, || {
                        l.with_value(coretraits::ordered) && r.with_value(coretraits::ordered)
                    });
                    if !valid {
                        return self.resident.set_bottom_as(tag);
                    }
                }
                let v = match &self.state {
                    // CR claude for eric: [bug/medium] A comparison over a held copy of
                    // a moving reference keeps its answer after the reference moves:
                    // with `held <- once(&a[i])` and `i` moved from 0 to 2, `held ==
                    // &a[0]` still says true while `*held` reads a[2] and the same
                    // comparison over `tick ~ held` says false. The targets are
                    // resolved only when an operand fires, and a copy does not fire
                    // when its cell's path moves; `*held` follows the move because
                    // Deref reads the cell with interest. Either the comparison reads
                    // the cells of its operands with interest too, or the rule
                    // 'compares by where it points now' is narrowed to 'when an operand
                    // fires' in design/place_references.md. probe:
                    // design/review-2026-11-16/repro/x-refs-held-copy-stale-equality.gx
                    // (x-refs-04)
                    Equality::Targets(t) if !$ordered => {
                        let lv = l.with_value(|v| ctx.ref_targets(t, v));
                        let rv = r.with_value(|v| ctx.ref_targets(t, v));
                        coretraits::with_hooks(ctx, || (lv $op rv).into())
                    }
                    Equality::Values { hooks: false } => {
                        l.with_value(|lv| r.with_value(|rv| (lv $op rv).into()))
                    }
                    _ => coretraits::with_hooks(ctx, || {
                        l.with_value(|lv| r.with_value(|rv| (lv $op rv).into()))
                    }),
                };
                self.resident.set(TagValue::tagged(v, tag))
            }

            fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
                emit_cmp_node(cx, CmpOp::$name, &self.lhs, &self.rhs)
            }
        });

        impl<R: Rt> $name<R> {
            fn typecheck_own(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
                let (lt, rt) = (self.lhs.typ(), self.rhs.typ());
                let holds = match $ordered {
                    true => Bound::Ordered,
                    false => Bound::Discernible,
                };
                wrap!(self, compared_by_bounds(&ctx.env, lt, rt, holds))?;
                Ok(())
            }
        }
    };
}

// CR claude for eric: [structure/medium] `x == null` over `x: [i64, null]` and a
// variant union compared with one of its tags are refused ("both operands must be one
// type") though the right operand is a member of the left one's type, so the commonest
// comparison in ordinary code needs `opt::is_none` or a select, and the message names
// neither. The arithmetic refusal over a nullable or a Result advises "cast one side
// explicitly" where the fix is `x$ + 1`. Decide whether `==` and `!=` admit an operand
// whose type the other's holds (both are values of the wider type), and either way name
// `$`, `opt::is_none` or a select in both messages when one operand's type is the
// other's plus null or an error. probe:
// design/review-2026-11-16/repro/a-language-check-refusals.sh (cases 6 to 8)
// (a-language-09)
compare_op!(Eq, ==, false);
compare_op!(Ne, !=, false);
compare_op!(Lt, <, true);
compare_op!(Gt, >, true);
compare_op!(Lte, <=, true);
compare_op!(Gte, >=, true);

/// How a comparison compares, decided at the first update from the
/// operand type: by value, through the core-trait hooks only where an
/// operand can hold an abstract value, or (`==` and `!=`) with each
/// reference replaced by what it names (a reference's value is its own
/// cell).
#[derive(Debug, Default)]
enum Equality {
    #[default]
    Undecided,
    Values {
        hooks: bool,
    },
    Targets(Type),
}

impl Equality {
    fn of(env: &Env, lhs: &Type, rhs: &Type, ordered: bool) -> Self {
        let refs = if ordered {
            [lhs, rhs].into_iter().find(|t| t.orders_refs(env))
        } else {
            compared_refs(env, lhs, rhs)
        };
        match refs {
            Some(t) => Self::Targets(t.clone()),
            None => Self::Values {
                hooks: [lhs, rhs]
                    .into_iter()
                    .any(|t| t.has_unbound() || t.holds_abstract(env)),
            },
        }
    }
}

/// The operand type whose references an `==` or `!=` over `lhs` and
/// `rhs` replaces by what they name, if either holds one.
pub(crate) fn compared_refs<'a>(
    env: &Env,
    lhs: &'a Type,
    rhs: &'a Type,
) -> Option<&'a Type> {
    [lhs, rhs].into_iter().find(|t| t.compares_refs(env))
}

macro_rules! bool_op {
    ($name:ident, $op:tt) => {
        binary_node!($name, Type::boolean(), {
            // Strict, not short-circuit: `false && ⊥ = ⊥`.
            fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
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

        impl<R: Rt> $name<R> {
            fn typecheck_own(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
                let bt = Type::boolean();
                wrap!(self.lhs, bt.check_contains(&ctx.env, self.lhs.typ()))?;
                wrap!(self.rhs, bt.check_contains(&ctx.env, self.rhs.typ()))
            }
        }
    };
}

bool_op!(And, &&);
bool_op!(Or, ||);

/// A prefix operator: `!` over a bool or `-` over a signed number.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug)]
pub struct Unary<R: Rt> {
    pub op: UnaryOp,
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub n: Node<R>,
    resident: TagValue,
    /// wake catch-up: set by `sleep()`, taken by the next update
    slept: WakeBit,
}

impl<R: Rt> Unary<R> {
    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        n: &Expr,
        op: UnaryOp,
    ) -> Result<Node<R>> {
        let n = compile(ctx, flags, n.clone(), scope, top_id)?;
        let typ = match op {
            UnaryOp::Not => Type::boolean(),
            UnaryOp::Neg => Type::empty_tvar(),
        };
        Ok(Self::node(op, spec, typ, n))
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
        op: UnaryOp,
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        Ok(Self::node(op, spec, typ, n))
    }

    fn node(op: UnaryOp, spec: Arc<Expr>, typ: Type, n: Node<R>) -> Node<R> {
        Node::new(Self {
            op,
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

    /// The operand is a bool, or negatable once the check settles its cell.
    fn typecheck_own(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        match self.op {
            UnaryOp::Not => {
                wrap!(self.n, Type::boolean().check_contains(&ctx.env, self.n.typ()))
            }
            UnaryOp::Neg => {
                wrap!(self.n, Self::negatable().check_contains(&ctx.env, self.n.typ()))?;
                wrap!(self, self.typ.check_contains(&ctx.env, self.n.typ()))?;
                Ok(())
            }
        }
    }
}

impl<R: Rt> Update<R> for Unary<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.n)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.n)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(
            match self.op {
                UnaryOp::Not => NodeTag::Not,
                UnaryOp::Neg => NodeTag::Neg,
            },
            buf,
        );
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.n.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let tv = self.n.update(ctx);
        let tag = tv.tag();
        dense_gate!(self, tag, tag.is_bottom());
        // integers wrap, matching the JIT's `ineg`
        let v = tv.with_value(|v| match (self.op, v) {
            (UnaryOp::Not, Value::Bool(b)) => Some(Value::Bool(!*b)),
            (UnaryOp::Not, _) => None,
            (UnaryOp::Neg, v) => neg(v),
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

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        self.n.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts([&mut self.n], ctx)
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        wrap!(self.n, self.n.typecheck0(ctx))?;
        self.typecheck_own(ctx)
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        wrap!(self.n, self.n.typecheck0_instance(ctx, types))?;
        match self.op == UnaryOp::Not || types.settle(ctx, &self.spec, &self.typ)? {
            true => Ok(()),
            false => self.typecheck_own(ctx),
        }
    }

    fn view(&self) -> NodeView<'_, R> {
        match self.op {
            UnaryOp::Not => NodeView::Not(self),
            UnaryOp::Neg => NodeView::Neg(self),
        }
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        match self.op {
            UnaryOp::Not => emit_not_node(cx, &self.n),
            UnaryOp::Neg => emit_neg_node(cx, &self.n, &self.typ),
        }
    }
}

pub(crate) fn neg(v: &Value) -> Option<Value> {
    match v {
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
            Value::Error(crate::rep::make_variant(ARITH_ERR_TAG.clone(), [msg]).into())
        }
        v => v,
    }
}

const ADD_OVERFLOW: ArcStr = literal!("attempt to add with overflow");
const SUB_OVERFLOW: ArcStr = literal!("attempt to subtract with overflow");
const MUL_OVERFLOW: ArcStr = literal!("attempt to multiply with overflow");
const DIV_ZERO: ArcStr = literal!("attempt to divide by zero");
const DIV_OVERFLOW: ArcStr = literal!("attempt to divide with overflow");
const REM_ZERO: ArcStr =
    literal!("attempt to calculate the remainder with a divisor of zero");

/// `l op r` for a same-variant integer pair, in that variant (netidx's
/// operators fold `v32`/`z32`/`v64`/`z64` into their fixed-width twins).
/// Unchecked `+ - *` wrap, matching the JIT; `x % -1` is 0; any other
/// overflow and a zero divisor are an error value naming the failure.
/// `None` for every other shape.
fn int_arith(op: ArithOp, checked: bool, l: &Value, r: &Value) -> Option<Value> {
    macro_rules! int {
        ($va:ident, $a:expr, $b:expr) => {{
            let (a, b) = ($a, $b);
            let v = match (op, checked) {
                (ArithOp::Add, false) => Ok(a.wrapping_add(b)),
                (ArithOp::Sub, false) => Ok(a.wrapping_sub(b)),
                (ArithOp::Mul, false) => Ok(a.wrapping_mul(b)),
                (ArithOp::Add, true) => a.checked_add(b).ok_or(ADD_OVERFLOW),
                (ArithOp::Sub, true) => a.checked_sub(b).ok_or(SUB_OVERFLOW),
                (ArithOp::Mul, true) => a.checked_mul(b).ok_or(MUL_OVERFLOW),
                (ArithOp::Div, _) if b == 0 => Err(DIV_ZERO),
                (ArithOp::Div, _) => a.checked_div(b).ok_or(DIV_OVERFLOW),
                (ArithOp::Mod, _) if b == 0 => Err(REM_ZERO),
                (ArithOp::Mod, _) => Ok(a.wrapping_rem(b)),
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

/// A checked `l op r` of two decimals, its failure naming its cause as
/// an integer's does. `None` for every other shape and unchecked op.
fn decimal_checked(op: ArithOp, checked: bool, l: &Value, r: &Value) -> Option<Value> {
    let (Value::Decimal(a), Value::Decimal(b)) = (l, r) else { return None };
    if !checked {
        return None;
    }
    let (a, b) = (**a, **b);
    let v = match op {
        ArithOp::Add => a.checked_add(b).ok_or(ADD_OVERFLOW),
        ArithOp::Sub => a.checked_sub(b).ok_or(SUB_OVERFLOW),
        ArithOp::Mul => a.checked_mul(b).ok_or(MUL_OVERFLOW),
        ArithOp::Div if b.is_zero() => Err(DIV_ZERO),
        ArithOp::Div => a.checked_div(b).ok_or(DIV_OVERFLOW),
        ArithOp::Mod if b.is_zero() => Err(REM_ZERO),
        ArithOp::Mod => a.checked_rem(b).ok_or(DIV_OVERFLOW),
    };
    Some(match v {
        Ok(v) => Value::Decimal(triomphe::Arc::new(v)),
        Err(msg) => Value::error(msg),
    })
}

/// `l op r` as both engines compute it. A failure is an error value, a
/// checked op's the catchable `ArithError`.
pub(crate) fn arith(op: ArithOp, checked: bool, l: Value, r: Value) -> Value {
    let v = match int_arith(op, checked, &l, &r)
        .or_else(|| decimal_checked(op, checked, &l, &r))
    {
        Some(v) => v,
        None => match (op, checked) {
            (ArithOp::Add, false) => l + r,
            (ArithOp::Sub, false) => l - r,
            (ArithOp::Mul, false) => l * r,
            (ArithOp::Div, false) => l / r,
            (ArithOp::Mod, false) => l % r,
            (ArithOp::Add, true) => l.checked_add(r),
            (ArithOp::Sub, true) => l.checked_sub(r),
            (ArithOp::Mul, true) => l.checked_mul(r),
            (ArithOp::Div, true) => l.checked_div(r),
            (ArithOp::Mod, true) => l.checked_rem(r),
        },
    };
    if checked { wrap_arith_error(v) } else { v }
}

macro_rules! arith_emit_clif {
    (false, $base:ident) => {
        fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
            emit_arith_node(
                cx,
                ArithOp::$base,
                &self.spec,
                &self.typ,
                &self.lhs,
                &self.rhs,
            )
        }
    };
    (true, $base:ident) => {
        fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
            emit_checked_arith_node(cx, ArithOp::$base, &self.lhs, &self.rhs)
        }
    };
}

/// `fn<'a: Number + Singleton>(x: 'a, y: 'a) -> 'a`: both operands
/// `lt` and `rt` and the result `out` are one numeric type. Idempotent.
pub(crate) fn arith_rule(
    env: &Env,
    op: crate::expr::BinOp,
    lt: &Type,
    rt: &Type,
    out: &Type,
) -> Result<()> {
    let checked = op.is_checked();
    // CR claude for claude: [readability/medium] "(cast one side explicitly)"
    // never works as written: cast returns a Result, so `cast<i64>(x) + 1` over
    // a nullable x and `cast<f64>(x) + y` for i64 + f64 are refused again with
    // the same advice. The common case, a nullable or a Result operand (`x + 1`
    // over `[i64, null]`, `a[1] + 1`), needs `x$`, which the message never
    // names, and `s + 1` over a string misses the interpolation hint that
    // string + string gets. Choose the advice from the operands: `x$`/`x?` on
    // an operand Strip::of would take, `cast<T>(x)$` for two numeric
    // primitives, interpolation for a string, nothing otherwise. probe:
    // design/review-2026-11-16/repro/x-diagnostics-arith-cast-advice.sh
    // (x-diagnostics-02)
    // 2026-11-18 claude: On bounds-spike the old operand rule and its '(cast
    // one side explicitly)' text are deleted with the old check; the refusal is
    // now 'cannot compute with +: arithmetic is fn<'a: Number + Singleton>...'
    // over the solver's leaf, which gives no advice at all. The fix proposed
    // (advice chosen from the operands) applies to that message.
    let told = |e| {
        let told = format_compact!(
            "cannot compute with {}: arithmetic is fn<'a: Number + Singleton>(x: \
             'a, y: 'a) -> 'a — both operands must be one numeric type",
            op.token()
        );
        Told::over(e, &told)
    };
    // a declared variable computed with itself, or with a type nothing has
    // decided, is told every bound it lacks
    let needs = [Type::Primitive(Typ::number()), Type::Bound(Bound::Singleton)];
    let cell = |t: &Type| match t {
        Type::TVar(tv) => tv.open_cell(),
        _ => None,
    };
    if let (Some(l), Some(r)) = (cell(lt), cell(rt)) {
        for tv in [l, r] {
            if let Some(lacks) = tv.undeclared_needs(env, &needs)? {
                return Err(told(anyhow::anyhow!("{lacks}")));
            }
        }
    }
    let t = same_by_bounds(env, lt, rt, Bound::Singleton).map_err(told)?;
    // the operands are one type here: one that holds no number is named
    Type::Primitive(Typ::number()).check_contains(env, &t).map_err(|e| {
        let one = match bounds::standing(&t) {
            Type::Set(_) | Type::TVar(_) | Type::Bottom | Type::Any => return told(e),
            Type::Primitive(p) if p.len() != 1 => return told(e),
            one => one,
        };
        let join = match &one {
            Type::Primitive(p) if *p == Typ::String => {
                " (to join strings, interpolate: \"[a][b]\")"
            }
            _ => "",
        };
        let told = crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
            format_compact!(
                "cannot compute {one} {} {one}: arithmetic takes numbers{join}",
                op.token()
            )
        });
        Told::over(e, &told)
    })?;
    match checked {
        true => out.check_contains(env, &bounds::union_of([t, ARITH_ERR.clone()])),
        false => out.check_contains(env, &t),
    }
}

macro_rules! arith_op {
    ($name:ident, $checked:tt, $base:ident) => {
        binary_node!($name, Type::empty_tvar(), settles: true, {
            arith_emit_clif!($checked, $base);

            fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
                let (l, r, trig, tag) = gated_operands!(self, ctx);
                let v = l.with_value(|lv| {
                    r.with_value(|rv| {
                        arith(ArithOp::$base, $checked, lv.clone(), rv.clone())
                    })
                });
                match v {
                    Value::Error(e) if !$checked => {
                        if trig {
                            let site = diagnostic_site(&self.spec);
                            report_failure!(&format_compact!(
                                "arith error {site} {}",
                                crate::typ::Naked(&e)
                            ));
                        }
                        self.resident.set_bottom_as(tag)
                    }
                    v => self.resident.set(TagValue::tagged(v, tag)),
                }
            }

        });

        impl<R: Rt> $name<R> {
            fn typecheck_own(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
                let (lt, rt) = (self.lhs.typ(), self.rhs.typ());
                wrap!(
                    self,
                    arith_rule(
                        &ctx.env,
                        crate::expr::BinOp::$name,
                        lt,
                        rt,
                        &self.typ
                    )
                )?;
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
