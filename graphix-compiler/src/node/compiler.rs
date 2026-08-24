use super::{
    Any, Block, Connect, ConnectDeref, Constant, Sample, StringInterpolate, TypeCast,
    TypeDef,
    array::{Array, ArrayRef, ArraySlice},
    bind::{Bind, ByRef, Deref, Ref},
    callsite::CallSite,
    compile_use,
    data::{Construct, Struct, StructRef, StructWith, Tuple, TupleRef, Variant},
    error::Qop,
    lambda::Lambda,
    module::Module,
    op::{Add, And, Div, Eq, Gt, Gte, Lt, Lte, Mod, Mul, Ne, Neg, Not, Or, Sub},
    select::Select,
    traits::{Impl, Trait},
};
use crate::{
    CFlag, ExecCtx, Node, Rt, Scope, UserEvent,
    expr::{
        self, ApplyExpr, Expr, ExprId, ExprKind, ModuleKind, SelectExpr, StructExpr,
        StructWithExpr,
    },
    node::{
        ExplicitParens, Nop,
        error::OrNever,
        map::{Map, MapRef},
        op::{CheckedAdd, CheckedDiv, CheckedMod, CheckedMul, CheckedSub},
    },
    typ::Type,
};
use anyhow::{Context, Result, bail};
use enumflags2::BitFlags;

/// Every per-kind `compile` recurses back through here, so this is the
/// one place graph construction descends the program tree — and the one
/// place it needs stack headroom for however deeply the program nests.
pub(crate) fn compile<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    scope: &Scope,
    top_id: ExprId,
) -> Result<Node<R, E>> {
    crate::stack::ensure_sufficient(|| compile_inner(ctx, flags, spec, scope, top_id))
}

fn compile_inner<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    scope: &Scope,
    top_id: ExprId,
) -> Result<Node<R, E>> {
    if ctx.env.lsp_mode {
        ctx.env.push_scope_map_entry(crate::ide::ScopeMapEntry {
            pos: spec.pos,
            ori: spec.ori.clone(),
            scope: scope.clone(),
        });
    }
    // Attribute handling — one pass here covers the whole tree (every Expr
    // re-enters `compile` exactly once; per-kind `compile`s recurse through
    // here). DEFINITION-ASSERTING names (`#[tail_recursive]`/`#[sync]`/
    // `#[async]`) are compiler-reserved (the `CollectionIntrinsic` precedent)
    // and are stamped onto `ctx.def_assertions` below, once the node exists;
    // everything else must be a registered attribute (dispatched later by
    // the fusion walk) or it is an unknown-attribute error.
    let mut def_asserts: smallvec::SmallVec<[crate::DefAssertionKind; 2]> =
        smallvec::SmallVec::new();
    if let Some(dec) = &spec.dec {
        for attr in dec.attrs.iter() {
            match crate::DefAssertionKind::from_name(&attr.name) {
                Some(k) => def_asserts.push(k),
                None => {
                    if ctx.lookup_attribute(&attr.name).is_none() {
                        crate::bailat!(spec, "unknown attribute #[{}]", attr.name);
                    }
                    // Honesty census: this registry attribute must be
                    // dispatched or absorbed by the fusion walk
                    // (`compile_stmt` reconciles; see `attr_census`).
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
        // The assertion's target: the definition the decorated statement
        // binds (`let f = |..| ..`, `let rec f = ..`) or a bare lambda
        // expression. Anything else can't carry a definition assertion.
        let lid = match node.view() {
            crate::NodeView::Bind(b) => match b.node.view() {
                crate::NodeView::Lambda(l) => l.lambda_id::<R, E>(),
                _ => None,
            },
            crate::NodeView::Lambda(l) => l.lambda_id::<R, E>(),
            _ => None,
        };
        let Some(id) = lid else {
            crate::bailat!(
                spec,
                "#[{}] annotates a function definition",
                match def_asserts[0] {
                    crate::DefAssertionKind::Sync => "sync",
                    crate::DefAssertionKind::Async => "async",
                    crate::DefAssertionKind::TailRecursive => "tail_recursive",
                }
            );
        };
        let mut pending = ctx.def_assertions.lock();
        for kind in def_asserts.drain(..) {
            if !pending.iter().any(|a| a.id == id && a.kind == kind) {
                pending.push(crate::DefAssertion { id, kind, spec: spec.clone() });
            }
        }
        return Ok(node);
    }
    compile_kind(ctx, flags, &spec, scope, top_id)
}

fn compile_kind<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: &Expr,
    scope: &Scope,
    top_id: ExprId,
) -> Result<Node<R, E>> {
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
        ExprKind::Module { name, value } => {
            let enclosing = scope;
            let scope = scope.append(&name);
            if !ctx.predeclared_mods.remove(&scope.lexical)
                && ctx.env.modules.contains(&scope.lexical)
            {
                bail!("duplicate module definition {}", scope.lexical)
            }
            if ctx.env.lsp_mode {
                let def_ori = match value {
                    ModuleKind::Resolved { exprs, .. } => {
                        exprs.first().map(|e| e.ori.clone())
                    }
                    _ => None,
                };
                ctx.env.push_module_reference(crate::ide::ModuleRefSite {
                    pos: spec.pos,
                    ori: spec.ori.clone(),
                    name: crate::expr::ModPath::from([name.as_str()]),
                    canonical: scope.lexical.clone(),
                    def_ori,
                });
            }
            match value {
                ModuleKind::Unresolved { .. } => {
                    bail!("external modules are not allowed in this context")
                }
                ModuleKind::Resolved { exprs, sig: None, from_interface: _ } => {
                    ctx.env.modules.insert_cow(scope.lexical.clone());
                    let res = Block::compile(
                        ctx,
                        flags,
                        spec.clone(),
                        &scope,
                        top_id,
                        true,
                        exprs,
                    )
                    .with_context(|| spec.ori.clone())?;
                    Ok(res)
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
        ExprKind::Use { reexport, names } => {
            compile_use(ctx, flags, spec.clone(), scope, *reexport, names)
        }
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
        ExprKind::Qop(e) => Qop::compile(ctx, flags, spec.clone(), scope, top_id, e),
        ExprKind::OrNever(e) => {
            OrNever::compile(ctx, flags, spec.clone(), scope, top_id, e)
        }
        ExprKind::Catch(_) => {
            bail!(
                "catch is only valid in statement position (a direct child of \
                 a block or module body) at {}",
                spec.pos
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
        ExprKind::Select(SelectExpr { arg, arms }) => {
            Select::compile(ctx, flags, spec.clone(), scope, top_id, arg, arms)
        }
        ExprKind::TypeCast { expr, typ } => {
            TypeCast::compile(ctx, flags, spec.clone(), scope, top_id, expr, typ)
        }
        ExprKind::TypeDef(expr::TypeDefExpr { name, params, body }) => {
            TypeDef::compile(ctx, spec.clone(), scope, name, params, body)
        }
        ExprKind::Trait(t) => Trait::compile(ctx, flags, spec.clone(), scope, t, top_id),
        ExprKind::Impl(i) => Impl::compile(ctx, flags, spec.clone(), scope, i, top_id),
        ExprKind::Map { args } => {
            Map::compile(ctx, flags, spec.clone(), scope, top_id, args)
        }
        ExprKind::MapRef { source, key } => {
            MapRef::compile(ctx, flags, spec.clone(), scope, top_id, source, key)
        }
        ExprKind::Not { expr } => {
            Not::compile(ctx, flags, spec.clone(), scope, top_id, expr)
        }
        ExprKind::Eq { lhs, rhs } => {
            Eq::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Ne { lhs, rhs } => {
            Ne::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Lt { lhs, rhs } => {
            Lt::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Gt { lhs, rhs } => {
            Gt::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Lte { lhs, rhs } => {
            Lte::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Gte { lhs, rhs } => {
            Gte::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::And { lhs, rhs } => {
            And::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Or { lhs, rhs } => {
            Or::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Add { lhs, rhs } => {
            Add::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::CheckedAdd { lhs, rhs } => {
            CheckedAdd::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Sub { lhs, rhs } => {
            Sub::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::CheckedSub { lhs, rhs } => {
            CheckedSub::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Mul { lhs, rhs } => {
            Mul::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::CheckedMul { lhs, rhs } => {
            CheckedMul::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Div { lhs, rhs } => {
            Div::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::CheckedDiv { lhs, rhs } => {
            CheckedDiv::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Mod { lhs, rhs } => {
            Mod::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::CheckedMod { lhs, rhs } => {
            CheckedMod::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Sample { lhs, rhs } => {
            Sample::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
    }
}
