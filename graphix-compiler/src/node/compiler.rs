use super::{
    Any, Block, Connect, ConnectDeref, Constant, Sample, StringInterpolate, TypeCast,
    TypeDef, Use,
    array::{Array, ArrayRef, ArraySlice},
    bind::{Bind, ByRef, Deref, Ref},
    callsite::CallSite,
    data::{Struct, StructRef, StructWith, Tuple, TupleRef, Variant},
    error::{Qop, TryCatch},
    lambda::Lambda,
    module::Module,
    op::{Add, And, Div, Eq, Gt, Gte, Lt, Lte, Mod, Mul, Ne, Neg, Not, Or, Sub},
    select::Select,
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
use compact_str::format_compact;
use enumflags2::BitFlags;

pub(crate) fn compile<R: Rt, E: UserEvent>(
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
    // Reject unknown attributes. Every Expr re-enters `compile` exactly once
    // (per-kind `compile`s recurse through here), so this single check covers
    // the whole tree. The per-attribute semantic check (e.g. `#[native]`) runs
    // post-fusion; this only validates that the attribute name is registered.
    if let Some(dec) = &spec.dec {
        for attr in dec.attrs.iter() {
            if ctx.lookup_attribute(&attr.name).is_none() {
                crate::bailat!(spec, "unknown attribute #[{}]", attr.name);
            }
        }
    }
    match &spec.kind {
        // sync-subset: a `sync { … }` block desugars into the existing
        // vocabulary (assignment = shadowing, for = fold over the
        // assigned set, arm-assigns hoist to tuple yields) and the
        // desugared expression compiles normally — one specification,
        // both evaluators (design/sync_subset.md).
        ExprKind::SyncBlock { exprs } => {
            let mut desugared =
                crate::expr::sync_desugar::desugar_sync_block(&spec, exprs)?;
            // The desugared root REPLACES the sync block, so the block's
            // decorations (`#[native]` above all) must ride along or the
            // attribute silently asserts nothing.
            desugared.dec = spec.dec.clone();
            return compile(ctx, flags, desugared, scope, top_id);
        }
        // Outside a sync block these forms have no meaning; inside one
        // the desugar consumed them. Anything left is a source error —
        // the backstop for assignment-as-value and loops outside sync.
        ExprKind::ForFold { iter, init, acc_pattern, elem_pattern, body } => {
            crate::node::forloop::For::compile(
                ctx,
                flags,
                spec.clone(),
                scope,
                top_id,
                iter,
                init,
                acc_pattern,
                elem_pattern,
                body,
            )
        }
        ExprKind::For { .. } => {
            crate::bailat!(spec, "`for` is only legal inside a sync block")
        }
        ExprKind::Assign { .. } => {
            crate::bailat!(
                spec,
                "assignment is a sync-block statement (a block statement or a                  select-arm body targeting a `let mut` of the enclosing sync                  block)"
            )
        }
        ExprKind::NoOp => Ok(Nop::new(Type::Bottom)),
        ExprKind::ExplicitParens(s) => {
            ExplicitParens::compile(ctx, flags, (**s).clone(), scope, top_id)
        }
        ExprKind::Constant(v) => Constant::compile(spec.clone(), v),
        ExprKind::Do { exprs } => {
            let scope = scope.append(&format_compact!("do{}", spec.id.inner()));
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
        ExprKind::Variant { tag, args } => {
            Variant::compile(ctx, flags, spec.clone(), scope, top_id, tag, args)
        }
        ExprKind::Struct(StructExpr { args }) => {
            Struct::compile(ctx, flags, spec.clone(), scope, top_id, args)
        }
        ExprKind::Module { name, value } => {
            let scope = scope.append(&name);
            if ctx.env.modules.contains(&scope.lexical) {
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
                    ctx.env.modules.insert_cow(scope.lexical.clone());
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
                    &scope,
                    sandbox.clone(),
                    sig.clone(),
                    source.clone(),
                    top_id,
                ),
            }
        }
        ExprKind::Use { name } => Use::compile(ctx, spec.clone(), scope, name),
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
        ExprKind::TryCatch(tc) => {
            TryCatch::new(ctx, flags, spec.clone(), scope, top_id, tc)
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
        ExprKind::TypeDef(expr::TypeDefExpr { name, params, typ }) => {
            TypeDef::compile(ctx, spec.clone(), scope, name, params, typ)
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
            Sample::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs)
        }
    }
}
