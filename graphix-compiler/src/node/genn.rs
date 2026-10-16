use super::{
    Constant, NOP,
    bind::Ref,
    callsite::{Arg, ArgKey, ArgMap, ArgStage, CallNode, CallSite},
};
use crate::SourcePosition;
use crate::{
    BindId, CompileCtx, ExecCtx, Node, Refs, Rt, Scope, TagValue, UserEvent,
    expr::{ApplyExpr, Expr, ExprId, ExprKind, ModPath, Origin},
    image::ImageBuf,
    typ::{FnType, Type},
};
use anyhow::{Result, bail};
use bytes::BufMut;
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint};
use netidx_value::Value;
use smallvec::SmallVec;
use std::sync::LazyLock;
use triomphe::Arc;

/// The origin of every synthetic binding: default positions mark the
/// bind as synthetic for IDE tooling.
pub(crate) static SYNTHETIC: LazyLock<Arc<Origin>> =
    LazyLock::new(|| Arc::new(Origin::default()));

/// bind a variable and return a node referencing it
pub fn bind<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
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
    let spec = name_spec(name, SourcePosition::default(), SYNTHETIC.clone());
    (id, Ref::new(ctx, id, typ, top_id, spec))
}

/// `name` as a reference written at `pos` in `ori`: what a generated
/// node reports as its source.
pub(crate) fn name_spec(name: &str, pos: SourcePosition, ori: Arc<Origin>) -> Expr {
    let mut spec = ExprKind::Ref { name: ModPath::from([name]) }.to_expr(pos);
    spec.ori = ori;
    spec
}

/// generate a reference to a bind id
pub fn reference<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    id: BindId,
    typ: Type,
    top_id: ExprId,
) -> Node<R, E> {
    Ref::new(ctx, id, typ, top_id, NOP.clone())
}

/// A constant `v` of the Graphix type `typ`.
pub fn constant<R: Rt, E: UserEvent>(v: Value, typ: Type) -> Node<R, E> {
    Constant::new(v, typ, NOP.clone())
}

/// generate and return an apply node for the given lambda
pub fn apply<R: Rt, E: UserEvent>(
    fnode: Node<R, E>,
    scope: Scope,
    args: SmallVec<[Node<R, E>; 2]>,
    typ: &FnType,
    top_id: ExprId,
) -> Result<Node<R, E>> {
    let ftype = typ.instantiate(&nohash::IntSet::default());
    apply_inner(fnode, scope, args, typ, Some(ftype.clone()), ftype.rtype, top_id)
}

/// A call of the function value `f` over `args`: bound at its first
/// update, or ahead of it by [`crate::branch::fork_instances`].
pub fn apply_value<R: Rt, E: UserEvent>(
    f: Value,
    scope: Scope,
    args: SmallVec<[Node<R, E>; 2]>,
    typ: &Arc<FnType>,
    top_id: ExprId,
) -> Result<Node<R, E>> {
    apply_constant(f, NOP.clone(), scope, args, typ, top_id)
}

/// [`apply_value`], the function reporting `spec` as its source.
pub(crate) fn apply_constant<R: Rt, E: UserEvent>(
    f: Value,
    spec: Arc<Expr>,
    scope: Scope,
    args: SmallVec<[Node<R, E>; 2]>,
    typ: &Arc<FnType>,
    top_id: ExprId,
) -> Result<Node<R, E>> {
    let fnode = Constant::new(f, Type::Fn(typ.clone()), spec);
    apply(fnode, scope, args, typ, top_id)
}

pub(crate) fn apply_prototype<R: Rt, E: UserEvent>(
    fnode: Node<R, E>,
    scope: Scope,
    args: SmallVec<[Node<R, E>; 2]>,
    typ: &FnType,
    top_id: ExprId,
) -> Result<Node<R, E>> {
    apply_inner(fnode, scope, args, typ, None, Type::empty_tvar(), top_id)
}

fn check_arity(n: usize, typ: &FnType) -> Result<()> {
    if n != typ.args.len() {
        let s = if n == 1 { "" } else { "s" };
        bail!(
            "this function is called with {n} argument{s}, but its type {typ} takes {}",
            typ.args.len()
        )
    }
    Ok(())
}

fn apply_inner<R: Rt, E: UserEvent>(
    fnode: Node<R, E>,
    scope: Scope,
    args: SmallVec<[Node<R, E>; 2]>,
    typ: &FnType,
    ftype: Option<FnType>,
    rtype: Type,
    top_id: ExprId,
) -> Result<Node<R, E>> {
    check_arity(args.len(), typ)?;
    let mut spec = ExprKind::Apply(ApplyExpr {
        args: Arc::from_iter(
            args.iter()
                .zip(typ.args.iter())
                .map(|(node, farg)| (farg.label().cloned(), node.spec().clone())),
        ),
        function: Arc::new(fnode.spec().clone()),
    })
    .to_expr(fnode.spec().pos);
    spec.ori = fnode.spec().ori.clone();
    let args: ArgMap<R, E> = ArgKey::of_formals(&typ.args)
        .zip(args)
        .map(|(key, node)| {
            let spec = Arc::new(node.spec().clone());
            (key, Arg::new(BindId::new(), Some(node), ArgStage::Given, spec))
        })
        .collect();
    Ok(Node::new(CallNode::Call(CallSite::unbound(
        Arc::new(spec),
        ftype,
        rtype,
        fnode,
        args,
        scope,
        BitFlags::empty(),
        top_id,
    ))))
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
        ctx: &mut CompileCtx<R, E>,
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
        let site = match apply(fnode, scope.clone(), nodes, ftype, top_id) {
            Ok(site) => site,
            Err(e) => {
                for id in args {
                    ctx.env.unbind_variable(id);
                    ctx.discard_stored(id);
                }
                return Err(e);
            }
        };
        let mut call = Self { site, args };
        match call.site.typecheck0(ctx).and_then(|()| call.site.typecheck1(ctx)) {
            Ok(()) => Ok(call),
            Err(e) => {
                let Self { site, args } = call;
                ctx.discard(site);
                for id in args {
                    ctx.env.unbind_variable(id);
                    ctx.discard_stored(id);
                }
                Err(e)
            }
        }
    }

    /// Delete the site and its argument bindings.
    pub(crate) fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.site.delete(ctx);
        for id in self.args.drain(..) {
            ctx.env.unbind_variable(id);
            ctx.rt.store_release(&id);
        }
    }
}

/// A call of a Graphix function from Rust, applied to arguments bound
/// privately. The owner feeds them (with the tag its own policy gives
/// them) and updates the call; delete drops every binding it owns.
#[derive(Debug)]
pub struct Callback<R: Rt, E: UserEvent> {
    call: Node<R, E>,
    function: Function,
    args: SmallVec<[BindId; 2]>,
}

/// What a [`Callback`] calls.
#[derive(Debug, Clone, Copy)]
enum Function {
    /// The function a binding of its own holds.
    Own(BindId),
    /// The function a binding its owner holds, and releases, holds.
    Shared(BindId),
    /// A function value, a constant of the call.
    Value,
}

impl<R: Rt, E: UserEvent> Callback<R, E> {
    /// A call of a function of type `typ`, its arguments named `names`,
    /// through a binding of its own.
    pub fn new(
        ctx: &mut CompileCtx<R, E>,
        scope: &Scope,
        typ: &Arc<FnType>,
        names: &[&str],
        top_id: ExprId,
    ) -> Result<Self> {
        Self::through_binding(
            ctx,
            scope,
            typ,
            names,
            Function::Own(BindId::new()),
            top_id,
        )
    }

    /// [`Self::new`] through the binding `fid`, which its owner holds and
    /// releases.
    pub fn through(
        ctx: &mut CompileCtx<R, E>,
        scope: &Scope,
        typ: &Arc<FnType>,
        names: &[&str],
        fid: BindId,
        top_id: ExprId,
    ) -> Result<Self> {
        Self::through_binding(ctx, scope, typ, names, Function::Shared(fid), top_id)
    }

    /// [`Self::new`] of the function value `f`.
    pub fn of_value(
        ctx: &mut CompileCtx<R, E>,
        scope: &Scope,
        typ: &Arc<FnType>,
        names: &[&str],
        f: Value,
        top_id: ExprId,
    ) -> Result<Self> {
        let (args, nodes) = Self::bind_args(ctx, scope, typ, names, top_id)?;
        let call = apply_value(f, scope.clone(), nodes, typ, top_id)?;
        Ok(Self { call, function: Function::Value, args })
    }

    fn through_binding(
        ctx: &mut CompileCtx<R, E>,
        scope: &Scope,
        typ: &Arc<FnType>,
        names: &[&str],
        function: Function,
        top_id: ExprId,
    ) -> Result<Self> {
        let (args, nodes) = Self::bind_args(ctx, scope, typ, names, top_id)?;
        let (Function::Own(fid) | Function::Shared(fid)) = function else {
            unreachable!("a binding")
        };
        let fnode = reference(ctx, fid, Type::Fn(typ.clone()), top_id);
        let call = apply(fnode, scope.clone(), nodes, typ, top_id)?;
        Ok(Self { call, function, args })
    }

    fn bind_args(
        ctx: &mut CompileCtx<R, E>,
        scope: &Scope,
        typ: &Arc<FnType>,
        names: &[&str],
        top_id: ExprId,
    ) -> Result<(SmallVec<[BindId; 2]>, SmallVec<[Node<R, E>; 2]>)> {
        check_arity(names.len(), typ)?;
        let mut args = SmallVec::new();
        let mut nodes = SmallVec::new();
        for (name, a) in names.iter().zip(typ.args.iter()) {
            let (id, node) = bind(ctx, &scope.lexical, name, a.typ.clone(), top_id);
            args.push(id);
            nodes.push(node);
        }
        Ok((args, nodes))
    }

    /// Feed the function the call's binding holds; a call of a value has
    /// none.
    pub fn feed_fn(&self, ctx: &mut ExecCtx<'_, R, E>, f: TagValue) {
        if let Function::Own(fid) | Function::Shared(fid) = self.function {
            ctx.deliver(fid, f)
        }
    }

    pub fn feed_arg(&self, ctx: &mut ExecCtx<'_, R, E>, i: usize, v: TagValue) {
        ctx.deliver(self.args[i], v)
    }

    pub fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        self.call.update(ctx)
    }

    pub fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.call.typecheck0(ctx)
    }

    pub fn refs(&self, refs: &mut Refs) {
        self.call.refs(refs)
    }

    pub fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.call.sleep(ctx)
    }

    /// The bindings the arguments are fed through, for an owner that
    /// writes one for a later cycle.
    pub fn args(&self) -> &[BindId] {
        &self.args
    }

    /// The call, for an owner that walks it (a fork of instances).
    pub fn call_mut(&mut self) -> &mut Node<R, E> {
        &mut self.call
    }

    /// Drop the call and every binding it owns, writes queued to them
    /// included.
    pub fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.call.delete(ctx);
        if let Function::Own(fid) = self.function {
            ctx.rt.store_release(&fid);
        }
        for id in &self.args {
            ctx.rt.store_release(id);
            ctx.env.unbind_variable(*id);
        }
    }

    pub fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.call.image_encode(buf)?;
        match self.function {
            Function::Own(fid) => {
                buf.put_u8(0);
                fid.encode(buf)?
            }
            Function::Shared(fid) => {
                buf.put_u8(1);
                fid.encode(buf)?
            }
            Function::Value => buf.put_u8(2),
        }
        encode_varint(self.args.len() as u64, buf);
        self.args.iter().try_for_each(|id| id.encode(buf))
    }

    pub fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let call = crate::image::decode_node(ctx, buf)?;
        let function = match u8::decode(buf)? {
            0 => Function::Own(BindId::decode(buf)?),
            1 => Function::Shared(BindId::decode(buf)?),
            2 => Function::Value,
            _ => return Err(PackError::UnknownTag),
        };
        let n = decode_varint(buf)? as usize;
        let args = (0..n).map(|_| BindId::decode(buf)).collect::<Result<_, _>>()?;
        Ok(Self { call, function, args })
    }
}
