use super::{
    Constant, NOP, Nop,
    bind::Ref,
    callsite::{Arg, ArgKey, CallSite, Callee},
};
use crate::{
    BindId, ExecCtx, Node, Rt, Scope, UserEvent,
    expr::{ApplyExpr, ExprId, ExprKind, ModPath, Origin},
    typ::{FnType, Type},
};
use ahash::AHashMap;
use combine::stream::position::SourcePosition;
use enumflags2::BitFlags;
use netidx::publisher::{Typ, Value};
use parking_lot::Mutex;
use poolshark::local::LPooled;
use std::sync::atomic::AtomicBool;
use triomphe::Arc;

/// generate a no op with the specific type
pub fn nop<R: Rt, E: UserEvent>(typ: Type) -> Node<R, E> {
    Nop::new(typ)
}

/// bind a variable and return a node referencing it
pub fn bind<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    scope: &ModPath,
    name: &str,
    typ: Type,
    top_id: ExprId,
) -> (BindId, Node<R, E>) {
    // Generated bindings have no source position; pass defaults so IDE
    // tooling can detect synthetic binds.
    let id = ctx
        .env
        .bind_variable(
            scope,
            name,
            typ.clone(),
            SourcePosition::default(),
            Arc::new(Origin::default()),
        )
        .id;
    ctx.rt.ref_var(id, top_id);
    (id, Box::new(Ref { spec: NOP.clone(), typ, id, top_id }))
}

/// generate a reference to a bind id
pub fn reference<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    id: BindId,
    typ: Type,
    top_id: ExprId,
) -> Node<R, E> {
    ctx.rt.ref_var(id, top_id);
    Box::new(Ref { spec: NOP.clone(), typ, id, top_id })
}

pub fn constant<R: Rt, E: UserEvent>(v: Value) -> Node<R, E> {
    Box::new(Constant {
        spec: NOP.clone(),
        typ: Type::Primitive(Typ::get(&v).into()),
        value: v,
    })
}

/// generate and return an apply node for the given lambda
pub fn apply<R: Rt, E: UserEvent>(
    fnode: Node<R, E>,
    scope: Scope,
    args: Vec<Node<R, E>>,
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
    args: Vec<Node<R, E>>,
    typ: &FnType,
    top_id: ExprId,
) -> Node<R, E> {
    apply_inner(fnode, scope, args, typ, None, Type::empty_tvar(), top_id)
}

fn apply_inner<R: Rt, E: UserEvent>(
    fnode: Node<R, E>,
    scope: Scope,
    args: Vec<Node<R, E>>,
    typ: &FnType,
    ftype: Option<FnType>,
    rtype: Type,
    top_id: ExprId,
) -> Node<R, E> {
    let spec = ExprKind::Apply(ApplyExpr {
        args: Arc::from_iter(
            args.iter()
                .zip(typ.args.iter())
                .map(|(node, farg)| (farg.label().cloned(), node.spec().clone())),
        ),
        function: Arc::new(fnode.spec().clone()),
    })
    .to_expr_nopos();
    let mut positional = 0;
    let args: AHashMap<ArgKey, Arg<R, E>> = args
        .into_iter()
        .zip(typ.args.iter())
        .map(|(node, farg)| {
            let key = match farg.label() {
                Some(name) => ArgKey::Named(name.clone()),
                None => {
                    let key = ArgKey::Positional(positional);
                    positional += 1;
                    key
                }
            };
            (key, Arg::new(BindId::new(), Some(node), false))
        })
        .collect();
    Box::new(CallSite {
        spec: Arc::new(spec),
        rtype,
        ftype,
        args,
        arg_refs: Vec::new(),
        scope,
        flags: BitFlags::empty(),
        fnode,
        callee: Callee::DynamicUnbound,
        static_target: None,
        recursive_edge: AtomicBool::new(false),
        top_id,
        // Synthetic call sites are never recursion sites.
        is_self_tail_call: AtomicBool::new(false),
        tail_arg_order: Mutex::new(None),
        callee_lambda_id: Mutex::new(None),
    })
}
