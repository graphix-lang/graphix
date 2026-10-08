use super::{
    Any, Block, Connect, ConnectDeref, Constant, Never, Sample, StringInterpolate,
    TypeCast,
    array::{Array, ArrayRef, ArraySlice, ListLit},
    bind::{Bind, ByRef, Deref, Ref},
    callsite::CallSite,
    data::{Construct, Struct, StructRef, StructWith, Tuple, TupleRef, Variant},
    error::{Qop, SeqAbort, SeqGuard},
    fork_control::{ForkControl, ForkKind},
    lambda::Lambda,
    module::Module,
    op::{Add, And, Div, Eq, Gt, Gte, Lt, Lte, Mod, Mul, Ne, Neg, Not, Or, Sub},
    select::Select,
    seq_machine::{SeqCapture, SeqMachine},
};
use crate::{
    CFlag, CompileCtx, DefAssertion, DefAssertionKind, Node, NodeView, Rt, Scope,
    UserEvent, bailat,
    expr::{
        ApplyExpr, Arg, ArgKind, At, Attr, Decorations, Expr, ExprId, ExprKind,
        LambdaBody, LambdaExpr, ModPath, ModuleKind, Name, SelectExpr, StructExpr,
        StructWithExpr, StructurePattern, print::PrettyDisplay,
    },
    ide::{ModuleRefSite, ScopeMapEntry},
    node::{
        ExplicitParens, Nop,
        error::OrNever,
        map::{Map, MapRef},
        op::{CheckedAdd, CheckedDiv, CheckedMod, CheckedMul, CheckedSub},
    },
    stack::ensure_sufficient,
    typ::{TVar, Type},
};
use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_value::{Typ, Value};
use smallvec::SmallVec;
use triomphe::Arc;

/// Every per-kind `compile` recurses back through here or through
/// [`compile_module`], so these two are where graph construction
/// descends the program tree, and where it takes stack headroom for
/// however deeply the program nests.
pub(crate) fn compile<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    scope: &Scope,
    top_id: ExprId,
) -> Result<Node<R, E>> {
    ensure_sufficient(|| compile_inner(ctx, flags, spec, scope, top_id, false))
}

/// [`compile`] a statement: a `let` compiles here, and nowhere a value
/// is expected.
pub(crate) fn compile_statement_expr<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    scope: &Scope,
    top_id: ExprId,
) -> Result<Node<R, E>> {
    ensure_sufficient(|| compile_inner(ctx, flags, spec, scope, top_id, true))
}

/// The lambda a definition-asserting attribute annotates: the node's
/// own, or its `let`'s value, seen through parentheses.
fn annotated_lambda<R: Rt, E: UserEvent>(node: &Node<R, E>) -> Option<crate::LambdaId> {
    let mut view = node.view();
    loop {
        view = match view {
            NodeView::Bind(b) => b.node.view(),
            NodeView::ExplicitParens(p) => p.n.view(),
            NodeView::Lambda(l) => return l.lambda_id::<R, E>(),
            _ => return None,
        }
    }
}

/// The fork control `attr` asks for, if it is `#[parallel]`,
/// `#[parallel(grain)]` or `#[serial]`.
fn fork_kind(spec: &Expr, attr: &crate::expr::Attr) -> Result<Option<ForkKind>> {
    let grain = match &*attr.args {
        [] => None,
        [g] if attr.name == "parallel" => match &g.kind {
            ExprKind::Constant(Value::I64(n)) if *n > 0 && *n <= u32::MAX as i64 => {
                Some(*n as u32)
            }
            _ => bailat!(spec, "#[parallel(grain)] takes a positive integer literal"),
        },
        _ if ForkKind::of(&attr.name, None).is_some() => {
            bailat!(spec, "#[{}] takes no arguments here", attr.name)
        }
        _ => None,
    };
    Ok(ForkKind::of(&attr.name, grain))
}

/// `spec`'s decorations split in two: the fork-control attributes, and
/// everything else.
fn split_fork(dec: &Decorations) -> (SmallVec<[Attr; 2]>, Decorations) {
    let (fork, kept): (SmallVec<[_; 2]>, SmallVec<[_; 2]>) =
        dec.attrs.iter().cloned().partition(|a| ForkKind::of(&a.name, None).is_some());
    (
        fork,
        Decorations { comments: dec.comments.clone(), attrs: kept.into_iter().collect() },
    )
}

/// `e` with `attrs` added to its decorations.
fn decorated(e: &Expr, attrs: impl IntoIterator<Item = Attr>) -> Expr {
    let mut e = e.clone();
    let mut dec = e.dec.as_deref().cloned().unwrap_or_else(|| Decorations {
        comments: Arc::from_iter([]),
        attrs: Arc::from_iter([]),
    });
    dec.attrs = dec.attrs.iter().cloned().chain(attrs).collect();
    e.dec = Some(Arc::new(dec));
    e
}

/// A lambda literal, under any parens, with the fork-control attributes
/// `fork` moved onto its body: they apply to every instance's body. The
/// parens' own attributes stay on the lambda.
fn fork_on_lambda(e: &Expr, fork: &[Attr]) -> Option<Expr> {
    let mut lambda = e;
    let mut outer: SmallVec<[Attr; 2]> = SmallVec::new();
    loop {
        if let Some(dec) = &lambda.dec {
            outer.extend(dec.attrs.iter().cloned());
        }
        match &lambda.kind {
            ExprKind::ExplicitParens(inner) => lambda = inner,
            _ => break,
        }
    }
    let ExprKind::Lambda(l) = &lambda.kind else { return None };
    let LambdaBody::Expr(body) = &l.body else { return None };
    let mut l = (**l).clone();
    l.body = LambdaBody::Expr(decorated(body, fork.iter().cloned()));
    let mut value = lambda.clone();
    value.kind = ExprKind::Lambda(Arc::new(l));
    value.dec = None;
    Some(decorated(&value, outer))
}

/// `spec`, carrying a fork-control attribute, with the attribute moved
/// where it governs: onto a `let`'s value, or onto the body of the
/// lambda a `let` defines or `spec` is; `None` for anything else.
fn fork_on_body(spec: &Expr) -> Option<Expr> {
    let dec = spec.dec.as_ref()?;
    let (fork, kept) = split_fork(dec);
    let mut spec = match &spec.kind {
        ExprKind::Bind(b) => {
            let value = fork_on_lambda(&b.value, &fork)
                .unwrap_or_else(|| decorated(&b.value, fork.iter().cloned()));
            let mut bind = (**b).clone();
            bind.value = value;
            let mut spec = spec.clone();
            spec.kind = ExprKind::Bind(Arc::new(bind));
            spec.dec = None;
            spec
        }
        ExprKind::Lambda(_) => {
            let mut bare = spec.clone();
            bare.dec = None;
            fork_on_lambda(&bare, &fork)?
        }
        _ => return None,
    };
    let attrs: SmallVec<[Attr; 2]> = kept
        .attrs
        .iter()
        .cloned()
        .chain(spec.dec.iter().flat_map(|d| d.attrs.iter().cloned()))
        .collect();
    spec.dec = Some(Arc::new(Decorations {
        comments: kept.comments.clone(),
        attrs: attrs.into_iter().collect(),
    }));
    Some(spec)
}

fn compile_inner<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    scope: &Scope,
    top_id: ExprId,
    statement: bool,
) -> Result<Node<R, E>> {
    if ctx.env.ide.is_lsp() {
        ctx.env.push_scope_map_entry(ScopeMapEntry {
            pos: spec.pos,
            end: spec.end.0,
            ori: spec.ori.clone(),
            scope: scope.lexical.clone(),
        });
    }
    // Definition-asserting and fork-control attribute names are
    // compiler-reserved; any other attribute must be registered or it is
    // an error.
    let mut def_asserts: SmallVec<[DefAssertionKind; 2]> = SmallVec::new();
    let mut fork: Option<ForkKind> = None;
    if let Some(dec) = &spec.dec {
        for attr in dec.attrs.iter() {
            if let Some(k) = fork_kind(&spec, attr)? {
                if fork.replace(k).is_some() {
                    bailat!(spec, "at most one of #[parallel] and #[serial]");
                }
                continue;
            }
            match DefAssertionKind::from_name(&attr.name) {
                Some(k) => def_asserts.push(k),
                None => {
                    if ctx.lookup_attribute(&attr.name).is_none() {
                        bailat!(spec, "unknown attribute #[{}]", attr.name);
                    }
                    // Every registry attribute must be dispatched or absorbed
                    // by the fusion walk (`compile_stmt` reconciles).
                    let mut census = ctx.attr_census.lock();
                    if !census.iter().any(|e| e.id == spec.id) {
                        census.push(spec.clone());
                    }
                }
            }
        }
    }
    if let Some(kind) = fork {
        // on a definition it applies to the body of every instance
        if let Some(spec) = fork_on_body(&spec) {
            return compile_inner(ctx, flags, spec, scope, top_id, statement);
        }
        // the child keeps its id and every other attribute; the wrapper is
        // an expression of its own carrying the fork alone
        let dec = spec.dec.as_ref().expect("a fork attribute decorates");
        let (fork, kept) = split_fork(dec);
        let mut child = spec.clone();
        child.dec = Some(Arc::new(kept));
        let mut wrapper = spec.clone();
        wrapper.id = ExprId::new();
        wrapper.dec = Some(Arc::new(Decorations {
            comments: Arc::from_iter([]),
            attrs: fork.into_iter().collect(),
        }));
        let node = compile_inner(ctx, flags, child, scope, top_id, statement)?;
        return Ok(ForkControl::new(wrapper, kind, node));
    }
    if !def_asserts.is_empty() {
        let node = compile_kind(ctx, flags, &spec, scope, top_id, statement)?;
        let Some(id) = annotated_lambda(&node) else {
            bailat!(spec, "#[{}] annotates a function definition", def_asserts[0].name());
        };
        // a check never analyzes, so nothing would retire the assertion
        if flags.contains(CFlag::CheckOnly) {
            return Ok(node);
        }
        let mut pending = ctx.def_assertions.lock();
        for kind in def_asserts.drain(..) {
            if !pending.iter().any(|a| a.id == id && a.kind == kind) {
                pending.push(DefAssertion { id, kind, spec: spec.clone() });
            }
        }
        return Ok(node);
    }
    compile_kind(ctx, flags, &spec, scope, top_id, statement)
}

/// Compile a `mod` declaration, from statement position (a block or
/// module body, [`crate::compile_stmt`]) or a dynamic module in value
/// position. `predeclared`: the enclosing block registered the module's
/// path already, so it is not a duplicate.
pub(crate) fn compile_module<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    scope: &Scope,
    top_id: ExprId,
    name: &Name,
    value: &ModuleKind,
    predeclared: bool,
) -> Result<Node<R, E>> {
    ensure_sufficient(|| {
        compile_module_inner(ctx, flags, spec, scope, top_id, name, value, predeclared)
    })
}

fn compile_module_inner<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    scope: &Scope,
    top_id: ExprId,
    name: &Name,
    value: &ModuleKind,
    predeclared: bool,
) -> Result<Node<R, E>> {
    let enclosing = scope;
    let scope = scope.append(name);
    if !predeclared && ctx.env.modules.contains(&scope.lexical) {
        bailat!(spec, "duplicate module definition {}", scope.lexical)
    }
    // the module's own file, where its body's errors are
    let body_ori = value.implementation_origin().cloned();
    if ctx.env.ide.is_lsp() {
        ctx.env.push_module_reference(ModuleRefSite {
            pos: name.pos_or(spec.pos),
            ori: spec.ori.clone(),
            name: crate::expr::ModPath::from([name.as_str()]),
            canonical: scope.lexical.clone(),
            def_ori: body_ori.clone(),
            segments: None,
        });
    }
    match value {
        ModuleKind::Unresolved { .. } => {
            bailat!(spec, "external modules are not allowed in this context")
        }
        ModuleKind::Resolved { exprs, sig: None, from_interface: _ } => {
            ctx.env.modules.insert_cow(scope.lexical.clone());
            let res = Block::compile(ctx, flags, spec, &scope, top_id, true, exprs);
            match body_ori {
                Some(ori) => res.with_context(|| ori),
                None => res,
            }
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

/// The refusal of a declaration where a value is expected.
fn not_an_expression<R: Rt, E: UserEvent>(spec: &Expr, what: &str) -> Result<Node<R, E>> {
    bailat!(
        spec,
        "{what} is not an expression — it may only appear as a statement in a \
         block or module body, not where a value is expected"
    )
}

/// The type of a literal value: its primitive, or for `error:<v>` the
/// error of its payload's type. A literal no type names (`abstract:..`)
/// is refused.
fn constant_type(v: &Value) -> Result<Type> {
    ensure_sufficient(|| match v {
        Value::Error(p) => Ok(Type::Error(Arc::new(constant_type(p)?))),
        Value::Abstract(_) => {
            bail!("an abstract value literal has no type: build it with its constructor")
        }
        Value::Array(_) | Value::Map(_) => {
            bail!("a collection value literal has no type: write it as an expression")
        }
        v => Ok(Type::Primitive(Typ::get(v).into())),
    })
}

fn compile_kind<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: &Expr,
    scope: &Scope,
    top_id: ExprId,
    statement: bool,
) -> Result<Node<R, E>> {
    macro_rules! binop {
        ($op:ident, $lhs:expr, $rhs:expr) => {
            $op::compile(ctx, flags, spec.clone(), scope, top_id, $lhs, $rhs)
        };
    }
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
        ExprKind::Constant(v) => {
            let typ = constant_type(v).at(spec)?;
            Ok(Constant::new(v.clone(), typ, spec.clone()))
        }
        ExprKind::Block { exprs } => {
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
        // Declarations (`let`, `use`, static `mod`, `type`, `trait`, `impl`)
        // are compiled in statement position only; a dynamic module
        // produces a real `[error, null]` value, so it is an expression.
        ExprKind::Module { name, value } => match value {
            ModuleKind::Dynamic { .. } => compile_module(
                ctx,
                flags,
                spec.clone(),
                scope,
                top_id,
                name,
                value,
                false,
            ),
            _ => not_an_expression(spec, "a module definition"),
        },
        ExprKind::Use { .. } => not_an_expression(spec, "a use declaration"),
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
        ExprKind::Bind(b) if statement => {
            Bind::compile(ctx, flags, spec.clone(), scope, top_id, b)
        }
        ExprKind::Bind(_) => not_an_expression(spec, "a let binding"),
        ExprKind::Qop(e) | ExprKind::Rethrow(e) => {
            Qop::compile(ctx, flags, spec.clone(), scope, top_id, e)
        }
        ExprKind::SeqGuard(e) => {
            SeqGuard::compile(ctx, flags, spec.clone(), scope, top_id, e)
        }
        ExprKind::SeqAbort(e) => {
            SeqAbort::compile(ctx, flags, spec.clone(), scope, top_id, e)
        }
        ExprKind::SeqMachine(m) => {
            SeqMachine::compile(ctx, flags, spec.clone(), scope, top_id, m)
        }
        ExprKind::SeqCapture(c) => {
            SeqCapture::compile(ctx, flags, spec.clone(), scope, top_id, c)
        }
        ExprKind::OrNever(e) => {
            OrNever::compile(ctx, flags, spec.clone(), scope, top_id, e)
        }
        ExprKind::Catch(_) => {
            bailat!(
                spec,
                "catch is only valid in statement position (a direct child of \
                 a block or module body)"
            )
        }
        ExprKind::ByRef(m, e) => {
            ByRef::compile(ctx, flags, spec.clone(), scope, top_id, *m, e)
        }
        ExprKind::Deref(e) => Deref::compile(ctx, flags, spec.clone(), scope, top_id, e),
        ExprKind::Neg(e) => Neg::compile(ctx, flags, spec.clone(), scope, top_id, e),
        ExprKind::Ref { name } => match eta_dispatcher(ctx, scope, &spec, name)? {
            Some(eta) => compile(ctx, flags, eta, scope, top_id),
            None => Ref::compile(ctx, spec.clone(), scope, top_id, name),
        },
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
            let key = (spec.id, scope.lexical.clone());
            let lowered = match ctx.lowered_seqs.get(&key) {
                Some(lowered) => lowered.clone(),
                None => {
                    let lowered =
                        crate::expr::seq::desugar(spec, &ctx.env, &scope.lexical)?;
                    if flags.contains(CFlag::ExpandSeq) {
                        println!(
                            "// seq at {}\n{}\n",
                            spec.pos,
                            lowered.to_string_pretty(80)
                        );
                    }
                    ctx.lowered_seqs.insert(key, lowered.clone());
                    lowered
                }
            };
            compile(ctx, flags, lowered, scope, top_id)
        }
        ExprKind::Until(_) => {
            bailat!(spec, "`until` is only legal in a seq block")
        }
        ExprKind::TryWith(_) => {
            bailat!(spec, "`try … with` is only legal as a seq statement")
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
        ExprKind::TypeDef(_) => not_an_expression(spec, "a type definition"),
        ExprKind::Trait(_) => not_an_expression(spec, "a trait definition"),
        ExprKind::Impl(_) => not_an_expression(spec, "an impl"),
        ExprKind::Map { args } => {
            Map::compile(ctx, flags, spec.clone(), scope, top_id, args)
        }
        ExprKind::MapRef { source, key } => {
            MapRef::compile(ctx, flags, spec.clone(), scope, top_id, source, key)
        }
        ExprKind::Not { expr } => {
            Not::compile(ctx, flags, spec.clone(), scope, top_id, expr)
        }
        ExprKind::Eq { lhs, rhs } => binop!(Eq, lhs, rhs),
        ExprKind::Ne { lhs, rhs } => binop!(Ne, lhs, rhs),
        ExprKind::Lt { lhs, rhs } => binop!(Lt, lhs, rhs),
        ExprKind::Gt { lhs, rhs } => binop!(Gt, lhs, rhs),
        ExprKind::Lte { lhs, rhs } => binop!(Lte, lhs, rhs),
        ExprKind::Gte { lhs, rhs } => binop!(Gte, lhs, rhs),
        ExprKind::And { lhs, rhs } => binop!(And, lhs, rhs),
        ExprKind::Or { lhs, rhs } => binop!(Or, lhs, rhs),
        ExprKind::Add { lhs, rhs } => binop!(Add, lhs, rhs),
        ExprKind::CheckedAdd { lhs, rhs } => binop!(CheckedAdd, lhs, rhs),
        ExprKind::Sub { lhs, rhs } => binop!(Sub, lhs, rhs),
        ExprKind::CheckedSub { lhs, rhs } => binop!(CheckedSub, lhs, rhs),
        ExprKind::Mul { lhs, rhs } => binop!(Mul, lhs, rhs),
        ExprKind::CheckedMul { lhs, rhs } => binop!(CheckedMul, lhs, rhs),
        ExprKind::Div { lhs, rhs } => binop!(Div, lhs, rhs),
        ExprKind::CheckedDiv { lhs, rhs } => binop!(CheckedDiv, lhs, rhs),
        ExprKind::Mod { lhs, rhs } => binop!(Mod, lhs, rhs),
        ExprKind::CheckedMod { lhs, rhs } => binop!(CheckedMod, lhs, rhs),
        ExprKind::Sample { lhs, rhs } => {
            Sample::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs, false)
        }
        ExprKind::StrictSample { lhs, rhs } => {
            Sample::compile(ctx, flags, spec.clone(), scope, top_id, lhs, rhs, true)
        }
    }
}

/// A trait method named as a value (`let d = Desc::desc`) is its
/// eta-expansion at the method's signature, `|x| Desc::desc(x)`: a
/// dispatcher binding holds no
/// value, the call resolves by its receiver. A call's own function stays
/// a reference. `None` for any other name; a variadic method has no
/// expansion (a lambda cannot pass its rest on) and is refused.
fn eta_dispatcher<R: Rt, E: UserEvent>(
    ctx: &CompileCtx<R, E>,
    scope: &Scope,
    spec: &Expr,
    name: &ModPath,
) -> Result<Option<Expr>> {
    let Some((_, bind)) = ctx.env.lookup_bind(&scope.lexical, name).ok().flatten() else {
        return Ok(None);
    };
    let Some(tm) = ctx.env.trait_methods.get(&bind.id) else { return Ok(None) };
    let Some(m) =
        ctx.env.trait_defs.get(&tm.trait_id).and_then(|d| d.methods.get(tm.index))
    else {
        return Ok(None);
    };
    let ft = &m.typ;
    if ft.vargs.is_some() {
        bailat!(
            spec,
            "{name} is variadic: a trait method with a rest argument can be called, \
             not used as a value"
        )
    }
    // the signature over variables of its own, as an annotation writes
    // it; `self` would name an impl's receiver
    let mut tvs: AHashMap<ArcStr, TVar> = AHashMap::new();
    ft.collect_tvars(&mut tvs);
    let renamed: AHashMap<ArcStr, Type> = tvs
        .keys()
        .map(|n| (n.clone(), Type::TVar(TVar::empty_generic(arcstr::format!("eta_{n}")))))
        .collect();
    let constraints: SmallVec<[(TVar, Type); 2]> = ft
        .constraint_view()
        .iter()
        .filter_map(|(tv, bound)| match renamed.get(&tv.name) {
            Some(Type::TVar(r)) => Some((r.clone(), bound.replace_tvars(&renamed))),
            _ => None,
        })
        .collect();
    let ft = ft.replace_tvars(&renamed);
    let at = |kind| Expr::new(kind, spec.pos);
    let mut args: SmallVec<[Arg; 4]> = SmallVec::new();
    let mut call: SmallVec<[(Option<ArcStr>, Expr); 4]> = SmallVec::new();
    for (i, a) in ft.args.iter().enumerate() {
        let (kind, label, var) = match a.label() {
            Some(l) => (ArgKind::Labeled, Some(l.clone()), l.clone()),
            None => (ArgKind::Positional, None, arcstr::format!("eta{i}")),
        };
        args.push(Arg {
            kind,
            pattern: StructurePattern::Bind(Name::from(var.clone())),
            constraint: Some(a.typ.clone()),
            pos: Default::default(),
        });
        call.push((label, at(ExprKind::Ref { name: ModPath::from([var]) })));
    }
    let body = at(ExprKind::Apply(ApplyExpr {
        args: Arc::from_iter(call),
        function: Arc::new(at(ExprKind::Ref { name: name.clone() })),
    }));
    Ok(Some(at(ExprKind::Lambda(Arc::new(LambdaExpr {
        args: Arc::from_iter(args),
        vargs: None,
        rtype: Some(ft.rtype.clone()),
        constraints: Arc::from_iter(constraints),
        throws: ft.explicit_throws.then(|| ft.throws.clone()),
        body: LambdaBody::Expr(body),
    })))))
}
