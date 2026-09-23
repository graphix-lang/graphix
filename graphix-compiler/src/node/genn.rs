use super::{
    Constant, NOP, Nop, WakeBit,
    bind::Ref,
    callsite::{Arg, ArgKey, CallSite, Callee},
};
use crate::{
    BindId, ExecCtx, Node, Rt, Scope, TagValue, UserEvent,
    expr::{ApplyExpr, ExprId, ExprKind, ModPath, Origin},
    typ::{FnType, Type},
};
use combine::stream::position::SourcePosition;
use enumflags2::BitFlags;
use netidx_value::{Typ, Value};
use parking_lot::Mutex;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::sync::atomic::AtomicBool;
use triomphe::Arc;

// CR claude for eric: [dead] No caller in the workspace (graphix, stdlib, rt);
// it only forwards to `Nop::new`. Delete.
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
    // default positions mark the bind as synthetic for IDE tooling
    // CR claude for eric: [perf] A fresh `Arc<Origin>` per synthetic bind, and
    // collections call this once per slot; share one static default origin
    // (a `LazyLock` like `NOP`).
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
    // CR claude for eric: [structure] The rest of this fn is `reference(ctx, id,
    // typ, top_id)` pasted; call it. Both also build `Ref` by struct literal
    // rather than through `Ref::new` (bind.rs), so a new `Ref` field must be
    // added in three places.
    ctx.rt.ref_var(id, top_id);
    (
        id,
        Node::new(Ref {
            spec: NOP.clone(),
            typ,
            id,
            top_id,
            resident: TagValue::phantom(),
            instantiated: false,
        }),
    )
}

/// generate a reference to a bind id
pub fn reference<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    id: BindId,
    typ: Type,
    top_id: ExprId,
) -> Node<R, E> {
    ctx.rt.ref_var(id, top_id);
    Node::new(Ref {
        spec: NOP.clone(),
        typ,
        id,
        top_id,
        resident: TagValue::phantom(),
        instantiated: false,
    })
}

// CR claude for eric: [risk] The type is the value's runtime `Typ`, not its
// Graphix type: a lambda (graphix-rt gx.rs:901 passes one) is typed
// `Primitive(Abstract)` and an array `Primitive(Array)`. It works only because
// `apply` supplies `ftype`; the same node under `apply_prototype` fails
// `deref_typ!("fn", ..)`. Take the type from the caller.
pub fn constant<R: Rt, E: UserEvent>(v: Value) -> Node<R, E> {
    Node::new(Constant {
        spec: NOP.clone(),
        typ: Type::Primitive(Typ::get(&v).into()),
        value: v,
        resident: TagValue::phantom(),
    })
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
    // CR claude for eric: [risk] `args` is zipped with `typ.args` twice, so an
    // arity mismatch from a (stdlib) caller silently drops arguments and builds a
    // call site missing them; assert the lengths agree.
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
    let mut positional = 0;
    let args: crate::node::callsite::ArgMap<R, E> = args
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
    // CR claude for eric: [structure] A third struct-literal `CallSite` (with
    // callsite.rs:374 and :1876): every new CallSite field must be added here by
    // hand. Give CallSite one constructor for an unbound site and call it.
    Node::new(CallSite {
        slept: WakeBit::default(),
        spec: Arc::new(spec),
        rtype,
        ftype,
        args,
        lowered: None,
        arg_refs: Vec::new(),
        scope,
        flags: BitFlags::empty(),
        fnode,
        callee: Callee::DynamicUnbound,
        callee_is_builtin: false,
        static_target: None,
        recursive_edge: AtomicBool::new(false),
        top_id,
        is_self_tail_call: AtomicBool::new(false),
        tail_arg_order: Mutex::new(None),
        callee_lambda_id: Mutex::new(None),
        resident: TagValue::phantom(),
    })
}
