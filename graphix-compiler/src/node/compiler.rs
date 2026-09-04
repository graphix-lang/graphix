use super::{
    Any, Block, Connect, ConnectDeref, Constant, Never, Sample, StringInterpolate,
    TypeCast,
    array::{Array, ArrayRef, ArraySlice, ListLit},
    bind::{Bind, ByRef, Deref, Ref},
    callsite::CallSite,
    data::{Construct, Struct, StructRef, StructWith, Tuple, TupleRef, Variant},
    error::Qop,
    lambda::Lambda,
    module::Module,
    op::{Add, And, Div, Eq, Gt, Gte, Lt, Lte, Mod, Mul, Ne, Neg, Not, Or, Sub},
    select::Select,
};
use crate::{
    CFlag, ExecCtx, Node, Rt, Scope, UserEvent,
    expr::{
        ApplyExpr, Expr, ExprId, ExprKind, ModuleKind, SelectExpr, StructExpr,
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

/// Compile a `mod` declaration. Only reachable from
/// [`super::compile_block_children`] in statement position — the general
/// `compile_kind` arm errors, since a module is not a value.
pub(crate) fn compile_module<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    scope: &Scope,
    top_id: ExprId,
    name: &arcstr::ArcStr,
    value: &ModuleKind,
) -> Result<Node<R, E>> {
    let enclosing = scope;
    let scope = scope.append(name);
    if !ctx.predeclared_mods.remove(&scope.lexical)
        && ctx.env.modules.contains(&scope.lexical)
    {
        bail!("duplicate module definition {}", scope.lexical)
    }
    if ctx.env.lsp_mode {
        let def_ori = match value {
            ModuleKind::Resolved { exprs, .. } => exprs.first().map(|e| e.ori.clone()),
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
            let res =
                Block::compile(ctx, flags, spec.clone(), &scope, top_id, true, exprs)
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
        // `use` and STATIC `mod` are DECLARATIONS, not expressions — they
        // carry no value. They are compiled directly by
        // `compile_block_children` / `compile_stmt` in statement position
        // (a non-final `do`-block item, a module-body item, a top-level
        // statement); reaching them HERE means they appear where a value
        // is expected (a `let` RHS, a call arg, a block's value slot),
        // which used to yield a `Bottom`-typed `Nop` that unified with
        // any downstream type and defeated soundness (aug27a aieka:
        // `let tag = use array::*` narrowed to `Array<i64>` while holding
        // an error struct). A DYNAMIC module is different — it produces a
        // real `[error, null]` load-status value (`let status = mod foo
        // dynamic {..}`), so it IS a legal expression.
        ExprKind::Module { name, value } => match value {
            ModuleKind::Dynamic { .. } => {
                compile_module(ctx, flags, spec.clone(), scope, top_id, name, value)
            }
            _ => bail!(
                "a module definition is not an expression — it may only \
                 appear as a statement in a block or module body, not \
                 where a value is expected"
            ),
        },
        ExprKind::Use { .. } => {
            bail!(
                "a use declaration is not an expression — it may only \
                 appear as a statement in a block or module body, not \
                 where a value is expected"
            )
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
        ExprKind::Seq { .. } => {
            let lowered = crate::expr::seq::desugar(spec)?;
            compile(ctx, flags, lowered, scope, top_id)
        }
        ExprKind::Until(_) => {
            crate::bailat!(spec, "`until` is only legal in a seq block")
        }
        ExprKind::SeqDo { .. } => {
            crate::bailat!(spec, "`do` is only legal in a seq block")
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
        // `type`/`trait`/`impl` are declarations like `use`/static `mod`
        // above — ⊥-typed, no value channel. aug31e ryouko: `let inner =
        // type M = ..` gave `inner` type ⊥ and a connect routed arrays
        // through it at runtime.
        ExprKind::TypeDef(_) => {
            bail!(
                "a type definition is not an expression — it may only \
                 appear as a statement in a block or module body, not \
                 where a value is expected"
            )
        }
        ExprKind::Trait(_) => {
            bail!(
                "a trait definition is not an expression — it may only \
                 appear as a statement in a block or module body, not \
                 where a value is expected"
            )
        }
        ExprKind::Impl(_) => {
            bail!(
                "an impl is not an expression — it may only appear as a \
                 statement in a block or module body, not where a value \
                 is expected"
            )
        }
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
            Sample::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs, false)
        }
        ExprKind::StrictSample { lhs, rhs } => {
            Sample::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs, true)
        }
    }
}
