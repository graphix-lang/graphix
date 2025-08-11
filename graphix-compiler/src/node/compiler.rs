use super::{
    array::{Array, ArrayRef, ArraySlice},
    callsite::CallSite,
    data::{Struct, StructRef, StructWith, Tuple, TupleRef, Variant},
    dynamic::DynamicModule,
    lambda::Lambda,
    op::{Add, And, Div, Eq, Gt, Gte, Lt, Lte, Mod, Mul, Ne, Not, Or, Sub},
    select::Select,
    Any, Bind, Block, ByRef, Connect, ConnectDeref, Constant, Deref, Qop, Ref, Sample,
    StringInterpolate, TypeCast, TypeDef, Use,
};
use crate::{
    expr::{self, Expr, ExprId, ExprKind, ModPath, ModuleKind},
    ExecCtx, Node, Rt, UserEvent,
};
use anyhow::{bail, Context, Result};
use compact_str::format_compact;

pub(crate) fn compile<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    spec: Expr,
    scope: &ModPath,
    top_id: ExprId,
) -> Result<Node<R, E>> {
    match &spec.kind {
        ExprKind::Constant(v) => Constant::compile(spec.clone(), v),
        ExprKind::Do { exprs } => {
            let scope = ModPath(scope.append(&format_compact!("do{}", spec.id.inner())));
            Block::compile(ctx, spec.clone(), &scope, top_id, false, exprs)
        }
        ExprKind::Array { args } => {
            Array::compile(ctx, spec.clone(), scope, top_id, args)
        }
        ExprKind::ArrayRef { source, i } => {
            ArrayRef::compile(ctx, spec.clone(), scope, top_id, source, i)
        }
        ExprKind::ArraySlice { source, start, end } => {
            ArraySlice::compile(ctx, spec.clone(), scope, top_id, source, start, end)
        }
        ExprKind::StringInterpolate { args } => {
            StringInterpolate::compile(ctx, spec.clone(), scope, top_id, args)
        }
        ExprKind::Tuple { args } => {
            Tuple::compile(ctx, spec.clone(), scope, top_id, args)
        }
        ExprKind::Variant { tag, args } => {
            Variant::compile(ctx, spec.clone(), scope, top_id, tag, args)
        }
        ExprKind::Struct { args } => {
            Struct::compile(ctx, spec.clone(), scope, top_id, args)
        }
        ExprKind::Module { name, export: _, value } => {
            let scope = ModPath(scope.append(&name));
            match value {
                ModuleKind::Unresolved => {
                    bail!("external modules are not allowed in this context")
                }
                ModuleKind::Resolved(exprs) => {
                    let res =
                        Block::compile(ctx, spec.clone(), &scope, top_id, true, exprs)
                            .with_context(|| spec.ori.clone())?;
                    ctx.env.modules.insert_cow(scope.clone());
                    Ok(res)
                }
                ModuleKind::Inline(exprs) => {
                    let res =
                        Block::compile(ctx, spec.clone(), &scope, top_id, true, exprs)
                            .with_context(|| spec.ori.clone())?;
                    ctx.env.modules.insert_cow(scope.clone());
                    Ok(res)
                }
                ModuleKind::Dynamic { sandbox, sig, source } => DynamicModule::compile(
                    ctx,
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
            ConnectDeref::compile(ctx, spec.clone(), scope, top_id, name, value)
        }
        ExprKind::Connect { name, value, deref: false } => {
            Connect::compile(ctx, spec.clone(), scope, top_id, name, value)
        }
        ExprKind::Lambda(l) => Lambda::compile(ctx, spec.clone(), scope, l, top_id),
        ExprKind::Any { args } => Any::compile(ctx, spec.clone(), scope, top_id, args),
        ExprKind::Apply { args, function: f } => {
            CallSite::compile(ctx, spec.clone(), scope, top_id, args, f)
        }
        ExprKind::Bind(b) => Bind::compile(ctx, spec.clone(), scope, top_id, b),
        ExprKind::Qop(e) => Qop::compile(ctx, spec.clone(), scope, top_id, e),
        ExprKind::ByRef(e) => ByRef::compile(ctx, spec.clone(), scope, top_id, e),
        ExprKind::Deref(e) => Deref::compile(ctx, spec.clone(), scope, top_id, e),
        ExprKind::Ref { name } => Ref::compile(ctx, spec.clone(), scope, top_id, name),
        ExprKind::TupleRef { source, field } => {
            TupleRef::compile(ctx, spec.clone(), scope, top_id, source, field)
        }
        ExprKind::StructRef { source, field } => {
            StructRef::compile(ctx, spec.clone(), scope, top_id, source, field)
        }
        ExprKind::StructWith { source, replace } => {
            StructWith::compile(ctx, spec.clone(), scope, top_id, source, replace)
        }
        ExprKind::Select { arg, arms } => {
            Select::compile(ctx, spec.clone(), scope, top_id, arg, arms)
        }
        ExprKind::TypeCast { expr, typ } => {
            TypeCast::compile(ctx, spec.clone(), scope, top_id, expr, typ)
        }
        ExprKind::TypeDef(expr::TypeDef { name, params, typ }) => {
            TypeDef::compile(ctx, spec.clone(), scope, name, params, typ)
        }
        ExprKind::Not { expr } => Not::compile(ctx, spec.clone(), scope, top_id, expr),
        ExprKind::Eq { lhs, rhs } => {
            Eq::compile(ctx, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Ne { lhs, rhs } => {
            Ne::compile(ctx, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Lt { lhs, rhs } => {
            Lt::compile(ctx, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Gt { lhs, rhs } => {
            Gt::compile(ctx, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Lte { lhs, rhs } => {
            Lte::compile(ctx, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Gte { lhs, rhs } => {
            Gte::compile(ctx, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::And { lhs, rhs } => {
            And::compile(ctx, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Or { lhs, rhs } => {
            Or::compile(ctx, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Add { lhs, rhs } => {
            Add::compile(ctx, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Sub { lhs, rhs } => {
            Sub::compile(ctx, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Mul { lhs, rhs } => {
            Mul::compile(ctx, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Div { lhs, rhs } => {
            Div::compile(ctx, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Mod { lhs, rhs } => {
            Mod::compile(ctx, spec.clone(), scope, top_id, lhs, rhs)
        }
        ExprKind::Sample { lhs, rhs } => {
            Sample::compile(ctx, spec.clone(), scope, top_id, lhs, rhs)
        }
    }
}
