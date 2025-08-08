use crate::{
    env::Env,
    expr::{Expr, ExprId, Sandbox, SigItem},
    BindId, ExecCtx, Node, Rt, UserEvent,
};
use anyhow::Result;
use fxhash::FxHashMap;
use triomphe::Arc;

struct DynamicModule<R: Rt, E: UserEvent> {
    spec: Expr,
    source: Node<R, E>,
    env: Env<R, E>,
    sandbox: Sandbox,
    sig: Arc<[SigItem]>,
    proxy: FxHashMap<BindId, BindId>,
    nodes: Box<[Node<R, E>]>,
    status: BindId,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> DynamicModule<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        sandbox: Sandbox,
        sig: Arc<[SigItem]>,
        source: Arc<Expr>,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        todo!()
    }
}
