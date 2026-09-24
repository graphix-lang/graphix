use super::{
    Constant, NOP,
    bind::Ref,
    callsite::{Arg, ArgKey, ArgMap, CallSite},
};
use crate::{
    BindId, ExecCtx, Node, Rt, Scope, UserEvent,
    expr::{ApplyExpr, ExprId, ExprKind, ModPath, Origin},
    typ::{FnType, Type},
};
use anyhow::Result;
use combine::stream::position::SourcePosition;
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_value::Value;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::sync::LazyLock;
use triomphe::Arc;

/// The origin of every synthetic binding: default positions mark the
/// bind as synthetic for IDE tooling.
static SYNTHETIC: LazyLock<Arc<Origin>> = LazyLock::new(|| Arc::new(Origin::default()));

/// bind a variable and return a node referencing it
pub fn bind<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    scope: &ModPath,
    name: &str,
    typ: Type,
    top_id: ExprId,
) -> (BindId, Node<R, E>) {
    let id = ctx
        .env
        .bind_variable(
            scope,
            name,
            typ.clone(),
            SourcePosition::default(),
            SYNTHETIC.clone(),
        )
        .id;
    (id, reference(ctx, id, typ, top_id))
}

/// generate a reference to a bind id
pub fn reference<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    id: BindId,
    typ: Type,
    top_id: ExprId,
) -> Node<R, E> {
    Ref::new(ctx, id, typ, top_id, NOP.clone())
}

/// A constant `v` of the Graphix type `typ`.
pub fn constant<R: Rt, E: UserEvent>(v: Value, typ: Type) -> Node<R, E> {
    Constant::new(v, typ, (**NOP).clone())
}

/// generate and return an apply node for the given lambda
pub fn apply<R: Rt, E: UserEvent>(
    fnode: Node<R, E>,
    scope: Scope,
    args: SmallVec<[Node<R, E>; 2]>,
    typ: &FnType,
    top_id: ExprId,
) -> Node<R, E> {
    let ftype = typ.reset_tvars();
    ftype.alias_tvars(&mut LPooled::take());
    apply_inner(fnode, scope, args, typ, Some(ftype.clone()), ftype.rtype, top_id)
}

pub(crate) fn apply_prototype<R: Rt, E: UserEvent>(
    fnode: Node<R, E>,
    scope: Scope,
    args: SmallVec<[Node<R, E>; 2]>,
    typ: &FnType,
    top_id: ExprId,
) -> Node<R, E> {
    apply_inner(fnode, scope, args, typ, None, Type::empty_tvar(), top_id)
}

fn apply_inner<R: Rt, E: UserEvent>(
    fnode: Node<R, E>,
    scope: Scope,
    args: SmallVec<[Node<R, E>; 2]>,
    typ: &FnType,
    ftype: Option<FnType>,
    rtype: Type,
    top_id: ExprId,
) -> Node<R, E> {
    assert_eq!(args.len(), typ.args.len(), "a generated call supplies every formal");
    let mut spec = ExprKind::Apply(ApplyExpr {
        args: Arc::from_iter(
            args.iter()
                .zip(typ.args.iter())
                .map(|(node, farg)| (farg.label().cloned(), node.spec().clone())),
        ),
        function: Arc::new(fnode.spec().clone()),
    })
    .to_expr_nopos();
    spec.ori = fnode.spec().ori.clone();
    let args: ArgMap<R, E> = ArgKey::of_formals(&typ.args)
        .zip(args)
        .map(|(key, node)| (key, Arg::new(BindId::new(), Some(node), false)))
        .collect();
    Node::new(CallSite::unbound(
        Arc::new(spec),
        ftype,
        rtype,
        fnode,
        args,
        scope,
        BitFlags::empty(),
        top_id,
    ))
}

/// A static call to the function binding `bind` over argument bindings
/// synthesized for it, each written by whoever drives the call.
#[derive(Debug)]
pub(crate) struct SynthCall<R: Rt, E: UserEvent> {
    pub(crate) site: Node<R, E>,
    pub(crate) args: SmallVec<[BindId; 2]>,
}

impl<R: Rt, E: UserEvent> SynthCall<R, E> {
    /// Bind one argument per type in `arg_types`, named `{prefix}_{k}`
    /// in `scope`, and typecheck the call through both passes.
    pub(crate) fn build(
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
        prefix: &str,
        bind: BindId,
        ftype: &Arc<FnType>,
        arg_types: impl IntoIterator<Item = Type>,
        top_id: ExprId,
    ) -> Result<Self> {
        let mut args: SmallVec<[BindId; 2]> = SmallVec::new();
        let mut nodes: SmallVec<[Node<R, E>; 2]> = SmallVec::new();
        for (k, typ) in arg_types.into_iter().enumerate() {
            let name = format_compact!("{prefix}_{k}");
            let (id, n) = self::bind(ctx, &scope.lexical, &name, typ, top_id);
            args.push(id);
            nodes.push(n);
        }
        let fnode = reference(ctx, bind, Type::Fn(ftype.clone()), top_id);
        let site = apply(fnode, scope.clone(), nodes, ftype, top_id);
        let mut call = Self { site, args };
        match call.site.typecheck0(ctx).and_then(|()| call.site.typecheck1(ctx)) {
            Ok(()) => Ok(call),
            Err(e) => {
                call.delete(ctx);
                Err(e)
            }
        }
    }

    /// Delete the site and its argument bindings.
    pub(crate) fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.site.delete(ctx);
        for id in self.args.drain(..) {
            ctx.env.unbind_variable(id);
            ctx.rt.store_remove(&id);
        }
    }
}
