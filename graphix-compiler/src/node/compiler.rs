use super::{
    Any, Block, Connect, ConnectDeref, Constant, Never, Sample, StringInterpolate,
    TypeCast,
    array::{Array, ArrayRef, ArraySlice, ListLit},
    bind::{Bind, ByRef, Deref, Ref},
    callsite::CallSite,
    data::{Construct, Struct, StructRef, StructWith, Tuple, TupleRef, Variant},
    error::{Qop, SeqAbortEvent, SeqGuard},
    lambda::Lambda,
    module::Module,
    op::{Add, And, Div, Eq, Gt, Gte, Lt, Lte, Mod, Mul, Ne, Neg, Not, Or, Sub},
    select::Select,
};
use crate::{
    CFlag, DefAssertion, DefAssertionKind, ExecCtx, Node, NodeView, Rt, Scope, UserEvent,
    bailat,
    expr::{
        ApplyExpr, Expr, ExprId, ExprKind, ModuleKind, Name, SelectExpr, StructExpr,
        StructWithExpr, print::PrettyDisplay,
    },
    ide::{ModuleRefSite, ScopeMapEntry},
    node::{
        ExplicitParens, Nop,
        error::OrNever,
        map::{Map, MapRef},
        op::{CheckedAdd, CheckedDiv, CheckedMod, CheckedMul, CheckedSub},
    },
    stack::ensure_sufficient,
    typ::Type,
};
use anyhow::{Context, Result};
use enumflags2::BitFlags;
use smallvec::SmallVec;

/// Every per-kind `compile` recurses back through here or through
/// [`compile_module`], so these two are where graph construction
/// descends the program tree, and where it takes stack headroom for
/// however deeply the program nests.
pub(crate) fn compile<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    scope: &Scope,
    top_id: ExprId,
) -> Result<Node<R, E>> {
    ensure_sufficient(|| compile_inner(ctx, flags, spec, scope, top_id))
}

/// The lambda a definition-asserting attribute annotates: the node's
/// own, or its `let`'s value, seen through parentheses.
fn annotated_lambda<R: Rt, E: UserEvent>(node: &Node<R, E>) -> Option<crate::LambdaId> {
    let mut view = node.view();
    loop {
        view = match view {
            NodeView::Bind(b) => b.node.view(),
            NodeView::ExplicitParens(p) => p.n.view(),
            NodeView::Lambda(l) => return l.lambda_id::<R, E>(),
            _ => return None,
        }
    }
}

fn compile_inner<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    scope: &Scope,
    top_id: ExprId,
) -> Result<Node<R, E>> {
    if ctx.env.lsp_mode {
        ctx.env.push_scope_map_entry(ScopeMapEntry {
            pos: spec.pos,
            end: spec.end.0,
            ori: spec.ori.clone(),
            scope: scope.clone(),
        });
    }
    // Definition-asserting attribute names are compiler-reserved; any other
    // attribute must be registered or it is an error.
    let mut def_asserts: SmallVec<[DefAssertionKind; 2]> = SmallVec::new();
    if let Some(dec) = &spec.dec {
        for attr in dec.attrs.iter() {
            match DefAssertionKind::from_name(&attr.name) {
                Some(k) => def_asserts.push(k),
                None => {
                    if ctx.lookup_attribute(&attr.name).is_none() {
                        bailat!(spec, "unknown attribute #[{}]", attr.name);
                    }
                    // Every registry attribute must be dispatched or absorbed
                    // by the fusion walk (`compile_stmt` reconciles).
                    let mut census = ctx.attr_census.lock();
                    if !census.iter().any(|e| e.id == spec.id) {
                        census.push(spec.clone());
                    }
                }
            }
        }
    }
    if !def_asserts.is_empty() {
        let node = compile_kind(ctx, flags, &spec, scope, top_id)?;
        let Some(id) = annotated_lambda(&node) else {
            bailat!(spec, "#[{}] annotates a function definition", def_asserts[0].name());
        };
        let mut pending = ctx.def_assertions.lock();
        for kind in def_asserts.drain(..) {
            if !pending.iter().any(|a| a.id == id && a.kind == kind) {
                pending.push(DefAssertion { id, kind, spec: spec.clone() });
            }
        }
        return Ok(node);
    }
    compile_kind(ctx, flags, &spec, scope, top_id)
}

/// Compile a `mod` declaration, from statement position (a block or
/// module body, [`crate::compile_stmt`]) or a dynamic module in value
/// position. `predeclared`: the enclosing block registered the module's
/// path already, so it is not a duplicate.
pub(crate) fn compile_module<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    scope: &Scope,
    top_id: ExprId,
    name: &Name,
    value: &ModuleKind,
    predeclared: bool,
) -> Result<Node<R, E>> {
    ensure_sufficient(|| {
        compile_module_inner(ctx, flags, spec, scope, top_id, name, value, predeclared)
    })
}

fn compile_module_inner<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    scope: &Scope,
    top_id: ExprId,
    name: &Name,
    value: &ModuleKind,
    predeclared: bool,
) -> Result<Node<R, E>> {
    let enclosing = scope;
    let scope = scope.append(name);
    if !predeclared && ctx.env.modules.contains(&scope.lexical) {
        bailat!(spec, "duplicate module definition {}", scope.lexical)
    }
    // the module's own file, where its body's errors are
    let body_ori = match value {
        ModuleKind::Resolved { exprs, .. } => exprs.first().map(|e| e.ori.clone()),
        _ => None,
    };
    if ctx.env.lsp_mode {
        ctx.env.push_module_reference(ModuleRefSite {
            pos: name.pos_or(spec.pos),
            ori: spec.ori.clone(),
            name: crate::expr::ModPath::from([name.as_str()]),
            canonical: scope.lexical.clone(),
            def_ori: body_ori.clone(),
            segments: None,
        });
    }
    match value {
        ModuleKind::Unresolved { .. } => {
            bailat!(spec, "external modules are not allowed in this context")
        }
        ModuleKind::Resolved { exprs, sig: None, from_interface: _ } => {
            ctx.env.modules.insert_cow(scope.lexical.clone());
            let res = Block::compile(ctx, flags, spec, &scope, top_id, true, exprs);
            match body_ori {
                Some(ori) => res.with_context(|| ori),
                None => res,
            }
        }
        ModuleKind::Resolved { exprs, sig: Some(sig), from_interface: _ } => {
            Module::compile_static(
                ctx,
                flags,
                spec.clone(),
                &scope,
                sig.clone(),
                exprs.clone(),
                top_id,
            )
        }
        ModuleKind::Dynamic { sandbox, sig, source } => Module::compile_dynamic(
            ctx,
            flags,
            spec.clone(),
            enclosing,
            &scope,
            sandbox.clone(),
            sig.clone(),
            source.clone(),
            top_id,
        ),
    }
}

/// The refusal of a declaration where a value is expected.
fn not_an_expression<R: Rt, E: UserEvent>(spec: &Expr, what: &str) -> Result<Node<R, E>> {
    bailat!(
        spec,
        "{what} is not an expression — it may only appear as a statement in a \
         block or module body, not where a value is expected"
    )
}

fn compile_kind<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: &Expr,
    scope: &Scope,
    top_id: ExprId,
) -> Result<Node<R, E>> {
    macro_rules! binop {
        ($op:ident, $lhs:expr, $rhs:expr) => {
            $op::compile(ctx, flags, spec.clone(), scope, top_id, $lhs, $rhs)
        };
    }
    match &spec.kind {
        ExprKind::NoOp => Ok(Nop::new(Type::Bottom)),
        ExprKind::ExplicitParens(s) => ExplicitParens::compile(
            ctx,
            flags,
            spec.clone(),
            (**s).clone(),
            scope,
            top_id,
        ),
        ExprKind::Constant(v) => Constant::compile(spec.clone(), v),
        ExprKind::Do { exprs } => {
            let scope = scope.append_block("do", spec.id.inner());
            Block::compile(ctx, flags, spec.clone(), &scope, top_id, false, exprs)
        }
        ExprKind::Array { args } => {
            Array::compile(ctx, flags, spec.clone(), scope, top_id, args)
        }
        ExprKind::List { args } => {
            ListLit::compile(ctx, flags, spec.clone(), scope, top_id, args)
        }
        ExprKind::ArrayRef { source, i } => {
            ArrayRef::compile(ctx, flags, spec.clone(), scope, top_id, source, i)
        }
        ExprKind::ArraySlice { source, start, end } => ArraySlice::compile(
            ctx,
            flags,
            spec.clone(),
            scope,
            top_id,
            source,
            start,
            end,
        ),
        ExprKind::StringInterpolate { args } => {
            StringInterpolate::compile(ctx, flags, spec.clone(), scope, top_id, args)
        }
        ExprKind::Tuple { args } => {
            Tuple::compile(ctx, flags, spec.clone(), scope, top_id, args)
        }
        ExprKind::Construct { name, arg } => {
            Construct::compile(ctx, flags, spec.clone(), scope, top_id, name, arg)
        }
        ExprKind::Variant { tag, args } => {
            Variant::compile(ctx, flags, spec.clone(), scope, top_id, tag, args)
        }
        ExprKind::Struct(StructExpr { args }) => {
            Struct::compile(ctx, flags, spec.clone(), scope, top_id, args)
        }
        // Declarations (`use`, static `mod`, `type`, `trait`, `impl`) carry
        // no value and are compiled in statement position only; a dynamic
        // module produces a real `[error, null]` value, so it is an expression.
        ExprKind::Module { name, value } => match value {
            ModuleKind::Dynamic { .. } => compile_module(
                ctx,
                flags,
                spec.clone(),
                scope,
                top_id,
                name,
                value,
                false,
            ),
            _ => not_an_expression(spec, "a module definition"),
        },
        ExprKind::Use { .. } => not_an_expression(spec, "a use declaration"),
        ExprKind::Connect { name, value, deref: true } => {
            ConnectDeref::compile(ctx, flags, spec.clone(), scope, top_id, name, value)
        }
        ExprKind::Connect { name, value, deref: false } => {
            Connect::compile(ctx, flags, spec.clone(), scope, top_id, name, value)
        }
        ExprKind::Lambda(l) => {
            Lambda::compile(ctx, flags, spec.clone(), scope, l, top_id)
        }
        ExprKind::Any { args } => {
            Any::compile(ctx, flags, spec.clone(), scope, top_id, args)
        }
        ExprKind::Apply(ApplyExpr { args, function: f }) => {
            CallSite::compile(ctx, flags, spec.clone(), scope, top_id, args, f)
        }
        ExprKind::Bind(b) => Bind::compile(ctx, flags, spec.clone(), scope, top_id, b),
        ExprKind::Qop(e) | ExprKind::Rethrow(e) => {
            Qop::compile(ctx, flags, spec.clone(), scope, top_id, e)
        }
        ExprKind::SeqGuard(e) => {
            SeqGuard::compile(ctx, flags, spec.clone(), scope, top_id, e)
        }
        ExprKind::SeqAbort(e) => {
            SeqAbortEvent::compile(ctx, flags, spec.clone(), scope, top_id, e)
        }
        ExprKind::OrNever(e) => {
            OrNever::compile(ctx, flags, spec.clone(), scope, top_id, e)
        }
        ExprKind::Catch(_) => {
            bailat!(
                spec,
                "catch is only valid in statement position (a direct child of \
                 a block or module body)"
            )
        }
        ExprKind::ByRef(e) => ByRef::compile(ctx, flags, spec.clone(), scope, top_id, e),
        ExprKind::Deref(e) => Deref::compile(ctx, flags, spec.clone(), scope, top_id, e),
        ExprKind::Neg(e) => Neg::compile(ctx, flags, spec.clone(), scope, top_id, e),
        ExprKind::Ref { name } => Ref::compile(ctx, spec.clone(), scope, top_id, name),
        ExprKind::TupleRef { source, field } => {
            TupleRef::compile(ctx, flags, spec.clone(), scope, top_id, source, field)
        }
        ExprKind::StructRef { source, field } => {
            StructRef::compile(ctx, flags, spec.clone(), scope, top_id, source, field)
        }
        ExprKind::StructWith(StructWithExpr { source, replace }) => {
            StructWith::compile(ctx, flags, spec.clone(), scope, top_id, source, replace)
        }
        ExprKind::Seq { .. } => {
            let lowered = crate::expr::seq::desugar(spec, &ctx.env, &scope.lexical)?;
            // XCR claude for eric: only the CLI's `--expand` sets ExpandSeq (the
            // server never does), so stdout is its terminal. A sink would be a new
            // ExecCtx field or CheckResult channel for one debug print; recommend
            // it with the warning sink (env.rs `warn`), when both have a consumer.
            if flags.contains(CFlag::ExpandSeq) {
                println!("// seq at {}\n{}\n", spec.pos, lowered.to_string_pretty(80));
            }
            compile(ctx, flags, lowered, scope, top_id)
        }
        ExprKind::Until(_) => {
            bailat!(spec, "`until` is only legal in a seq block")
        }
        ExprKind::TryWith(_) => {
            bailat!(spec, "`try … with` is only legal as a seq statement")
        }
        ExprKind::Select(SelectExpr { arg, arms }) => {
            Select::compile(ctx, flags, spec.clone(), scope, top_id, arg, arms)
        }
        ExprKind::TypeCast { expr, typ } => {
            TypeCast::compile(ctx, flags, spec.clone(), scope, top_id, expr, typ)
        }
        ExprKind::Never { typ, args } => {
            Never::compile(ctx, flags, spec.clone(), scope, top_id, typ, args)
        }
        ExprKind::TypeDef(_) => not_an_expression(spec, "a type definition"),
        ExprKind::Trait(_) => not_an_expression(spec, "a trait definition"),
        ExprKind::Impl(_) => not_an_expression(spec, "an impl"),
        ExprKind::Map { args } => {
            Map::compile(ctx, flags, spec.clone(), scope, top_id, args)
        }
        ExprKind::MapRef { source, key } => {
            MapRef::compile(ctx, flags, spec.clone(), scope, top_id, source, key)
        }
        ExprKind::Not { expr } => {
            Not::compile(ctx, flags, spec.clone(), scope, top_id, expr)
        }
        ExprKind::Eq { lhs, rhs } => binop!(Eq, lhs, rhs),
        ExprKind::Ne { lhs, rhs } => binop!(Ne, lhs, rhs),
        ExprKind::Lt { lhs, rhs } => binop!(Lt, lhs, rhs),
        ExprKind::Gt { lhs, rhs } => binop!(Gt, lhs, rhs),
        ExprKind::Lte { lhs, rhs } => binop!(Lte, lhs, rhs),
        ExprKind::Gte { lhs, rhs } => binop!(Gte, lhs, rhs),
        ExprKind::And { lhs, rhs } => binop!(And, lhs, rhs),
        ExprKind::Or { lhs, rhs } => binop!(Or, lhs, rhs),
        ExprKind::Add { lhs, rhs } => binop!(Add, lhs, rhs),
        ExprKind::CheckedAdd { lhs, rhs } => binop!(CheckedAdd, lhs, rhs),
        ExprKind::Sub { lhs, rhs } => binop!(Sub, lhs, rhs),
        ExprKind::CheckedSub { lhs, rhs } => binop!(CheckedSub, lhs, rhs),
        ExprKind::Mul { lhs, rhs } => binop!(Mul, lhs, rhs),
        ExprKind::CheckedMul { lhs, rhs } => binop!(CheckedMul, lhs, rhs),
        ExprKind::Div { lhs, rhs } => binop!(Div, lhs, rhs),
        ExprKind::CheckedDiv { lhs, rhs } => binop!(CheckedDiv, lhs, rhs),
        ExprKind::Mod { lhs, rhs } => binop!(Mod, lhs, rhs),
        ExprKind::CheckedMod { lhs, rhs } => binop!(CheckedMod, lhs, rhs),
        ExprKind::Sample { lhs, rhs } => {
            Sample::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs, false)
        }
        ExprKind::StrictSample { lhs, rhs } => {
            Sample::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs, true)
        }
    }
}
