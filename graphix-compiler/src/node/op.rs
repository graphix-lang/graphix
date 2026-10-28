use super::{CFlag, WakeBit, compiler::compile, coretraits, dense_gate};
use crate::{
    CompileCtx, ExecCtx, Node, NodeView, Rt, Scope, TagValue, Update, defetyp,
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
    typ::{Bound, ContainsFlags, Type, tvar::require_bounds},
    wrap,
};
use anyhow::{Context, Result, bail};
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
                if false $(|| $settles)? && !types.settle(self.spec.id, &self.typ) {
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
            fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
                if matches!(self.state, Equality::Undecided) {
                    self.state = Equality::of(&ctx.env, self.lhs.typ(), self.rhs.typ(), $ordered);
                }
                let (l, r, _, tag) = gated_operands!(self, ctx);
                if $ordered && matches!(self.state, Equality::Targets(_)) {
                    let valid = coretraits::with_hooks(ctx, || {
                        l.with_value(coretraits::ordered) && r.with_value(coretraits::ordered)
                    });
                    if !valid {
                        return self.resident.set_bottom_as(tag);
                    }
                }
                let v = match &self.state {
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
                match wrap!(self, operand_type(&ctx.env, lt, rt))? {
                    Some(t) => {
                        let bound = match $ordered {
                            true => Type::Bound(Bound::Ordered),
                            false => Type::Bound(Bound::Discernible),
                        };
                        require_bounds(std::slice::from_ref(&bound), &t);
                        let judgment = crate::PendingSettle::Discernible {
                            typ: t.clone(),
                            what: crate::Discerned::Compared { ordered: $ordered },
                            spec: self.spec.clone(),
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
    // XCR claude for claude: [bug/medium] References held under Any compare by cell, not
    // by what they name: Targets is chosen only when an operand TYPE compares_refs, and
    // Any holds no reference statically. `let a: Any = &x; let b: Any = &x; (a == b, &x
    // == &x)` is (false, true) in both engines, against the CLAUDE.md rule that ==
    // compares references by what they name. Either give map_refs an Any arm that walks
    // the value and rewrites every reference abstract it finds (chosen here when an
    // operand type holds Any, never fused), or rule that Any compares references by
    // cell and document and pin it. The orderings under Any order references by cell
    // number, too. probe: design/review-2026-10-26/repro/c-error-op-any-ref-eq.gx
    // (c-error-op-02)
    // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q34, References
    // under Any): yes (the recommendation: walk for `==`, refuse the order.)
    // 2026-10-28 codex: Implemented q34: equality descends through Any and replaces
    // references by their named places; statically ref-free subtrees retain the typed
    // fast path. Ordering refuses a reference reached under Any, including singleton
    // sort/min/max/dedup, map construction, queries, edits, casts, collection results,
    // native map helpers and reference paths. An actual matching Eq/Ord implementation
    // remains authoritative; reference walks match its type arguments, so an impl for
    // another instantiation cannot hide a reference. Any comparisons remain unfused as
    // the ruling explicitly recommended. Pins lang::byref::any_reference_* plus
    // scalar/Ord controls cover all four modes; the final focused byref::any_ run
    // passed 48 tests. Equality probes failed before the implementation; ordering and
    // cast pins failed all four modes with the implementation removed; compiler
    // map_helpers_refuse_reference_keys and map_paths_refuse_reference_keys both failed
    // with it removed. The additional instantiated-impl equality and ordering probes
    // failed eight cases before the exact lookup correction and passed afterward.
    // Rules, book, design and language skill updated. Full-suite and fuzzer results are
    // recorded in the completion report.
    // 2026-10-28 codex: Gate reference-order walks on resolved argument/key types. Map
    // nodes, collection builders, by-reference map paths and ordered builtins retain the
    // decision from typecheck1 (and image it); native helpers receive constant gates.
    // The fast builtin path is used only when its argument type cannot conceal
    // references. Cast nodes and reader targets retain a conservative target-level gate;
    // the generic Type cast API also checks the static map-key type once before
    // iterating entries, so an unrelated Any in a map value never causes a key walk.
    // Existing netidx-facing signatures are unchanged. Pins:
    // ordered_builtin_checks_only_unknown_types and typed_map_cast_skips_reference_walk
    // both failed with their guards removed, then passed restored. Extended
    // map_helpers_refuse_reference_keys covers both native gate values;
    // key_type_check_does_not_change_place_identity pins that gate metadata is not
    // reference identity. The complete graphix-types, graphix-compiler and
    // graphix-package-core unit suites pass (336, 54 and 6 tests), and all 48
    // lang::byref::any_ integration tests pass. No socket tests are involved in these
    // focused runs.
    // 2026-10-28 codex: Follow-up validation found an abstract target can hide a map
    // representation behind a custom Ord. The outer type's comparison walk is not its
    // cast's representation walk. Pretyped casts now keep the key guard for any abstract
    // target (and Any); each actual map still checks only keys whose static type can
    // conceal references. The new any_reference_cast_abstract_map_is_refused pin failed
    // in all four modes with the earlier target gate. This does not change the unchecked
    // path for concrete structural targets such as Map<string, i64>.
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
                wrap!(
                    self.n,
                    constrain_operand(&ctx.env, &Self::negatable(), self.n.typ())
                )?;
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
        match self.op == UnaryOp::Not || types.settle(self.spec.id, &self.typ) {
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
            emit_arith_node(cx, ArithOp::$base, &self.typ, &self.lhs, &self.rhs)
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
    // A declared `'a: Number` formal is rigid while its def gate is
    // open: `x + f64:0.` must reject, not bind 'a.
    let checked = op.is_checked();
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
    constrain_operand(env, &Type::Primitive(Typ::number()), t).with_context(|| {
        crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
            let join = match t.with_deref(
                |t| matches!(t, Some(Type::Primitive(p)) if *p == Typ::String),
            ) {
                true => " (to join strings, interpolate: \"[a][b]\")",
                false => "",
            };
            format_compact!(
                "cannot compute {lt} {} {rt}: arithmetic takes numbers{join}",
                op.token()
            )
        })
    })?;
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
fn defer_operand<R: Rt>(ctx: &mut CompileCtx<R>, n: &Node<R>) {
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
