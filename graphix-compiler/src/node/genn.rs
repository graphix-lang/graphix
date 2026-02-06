use super::{bind::Ref, callsite::CallSite, Constant, Nop, NOP};
use crate::{
    expr::{ExprId, ModPath, Origin},
    typ::{FnType, Type},
    BindId, ExecCtx, Node, Rt, Scope, UserEvent,
};
use combine::stream::position::SourcePosition;
use enumflags2::BitFlags;
use netidx::publisher::{Typ, Value};
use poolshark::local::LPooled;
use std::collections::HashMap;
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
    // Generated binds don't have source position info
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
    Box::new(CallSite {
        spec: NOP.clone(),
        rtype: ftype.rtype.clone(),
        ftype: Some(ftype),
        named_args: HashMap::default(),
        args,
        scope,
        flags: BitFlags::empty(),
        fnode,
        function: None,
        top_id,
    })
}
