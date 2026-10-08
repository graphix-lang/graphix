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
        ApplyExpr, Arg, ArgKind, Decorations, Expr, ExprId, ExprKind, LambdaBody,
        LambdaExpr, ModPath, ModuleKind, Name, SelectExpr, StructExpr, StructWithExpr,
        StructurePattern, print::PrettyDisplay,
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
use anyhow::{Context, Result};
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

/// `spec`, a `let` carrying a fork-control attribute, with the attribute
/// moved onto its value, or onto the body of the lambda it defines;
/// `None` if it is no `let`.
fn fork_on_body(spec: &Expr) -> Option<Expr> {
    let ExprKind::Bind(b) = &spec.kind else { return None };
    let dec = spec.dec.as_ref()?;
    let (moved, kept): (SmallVec<[_; 2]>, SmallVec<[_; 2]>) =
        dec.attrs.iter().cloned().partition(|a| ForkKind::of(&a.name, None).is_some());
    let decorate = |e: &Expr| {
        let mut e = e.clone();
        let mut dec = e.dec.as_deref().cloned().unwrap_or_else(|| Decorations {
            comments: Arc::from_iter([]),
            attrs: Arc::from_iter([]),
        });
        dec.attrs = dec.attrs.iter().cloned().chain(moved.iter().cloned()).collect();
        e.dec = Some(Arc::new(dec));
        e
    };
    let mut lambda = &b.value;
    while let ExprKind::ExplicitParens(inner) = &lambda.kind {
        lambda = inner;
    }
    let value = match &lambda.kind {
        ExprKind::Lambda(l) => match &l.body {
            LambdaBody::Expr(body) => {
                let mut l = (**l).clone();
                l.body = LambdaBody::Expr(decorate(body));
                let mut value = lambda.clone();
                value.kind = ExprKind::Lambda(Arc::new(l));
                value
            }
            LambdaBody::Builtin(_) => decorate(&b.value),
        },
        _ => decorate(&b.value),
    };
    let mut bind = (**b).clone();
    bind.value = value;
    let mut spec = spec.clone();
    spec.kind = ExprKind::Bind(Arc::new(bind));
    spec.dec = Some(Arc::new(Decorations {
        comments: dec.comments.clone(),
        attrs: kept.into_iter().collect(),
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
    // CR claude for claude: [bug] A fork attribute stops the other attributes on its
    // expression from being checked. This branch returns before `def_asserts` are
    // registered, so `let rec f = #[serial] #[tail_recursive] |n: i64, acc: i64| ..`
    // compiles even with a non-tail self-call. A fix here also needs annotated_lambda
    // to see through ForkControl. On a decorated `let`, fork_on_body (:116-127) strips
    // the parens around the lambda and their decorations with them, so `#[serial] let f
    // = #[tail_recursive] (|x| x + 1)` runs and `#[serial] let f = #[bogus] (|x| x +
    // 1)` passes even --check. probe: design/review-2026-10-05/repro/c-data-map-03.gx
    // (prints 4; deleting #[serial] from either line makes that line be refused).
    // (c-data-map-03)
    if let Some(kind) = fork {
        // on a definition it applies to the body of every instance
        if let Some(spec) = fork_on_body(&spec) {
            return compile_inner(ctx, flags, spec, scope, top_id, statement);
        }
        let node = compile_kind(ctx, flags, &spec, scope, top_id, statement)?;
        // CR claude for claude: [bug] The wrapper gets the decorated spec itself, so it
        // shares the child's id and carries the child's other attributes. #[native] is
        // then dispatched on the ForkControl, which never emits, and is refused ("fork
        // control runs its child under flags of its own") even though the child fused.
        // `#[parallel] let f = |xs: Array<i64>| -> Array<i64> #[native] array::map(xs,
        // |x| x * 2)` and `|x: i64| -> i64 #[serial] #[native] (x * 2 + 1)` are
        // refused, while `#[parallel] (#[native] array::map(..))` passes. The shared id
        // also makes DefTable::record drop both nodes' rows (lambda.rs:161), so every
        // instance checks that node itself instead of substituting. Give the wrapper a
        // spec of its own that carries only the fork attribute. probe:
        // design/review-2026-10-05/repro/c-data-map-06.gx (c-data-map-06)
        return Ok(ForkControl::new(spec, kind, node));
    }
    if !def_asserts.is_empty() {
        let node = compile_kind(ctx, flags, &spec, scope, top_id, statement)?;
        let Some(id) = annotated_lambda(&node) else {
            bailat!(spec, "#[{}] annotates a function definition", def_asserts[0].name());
        };
        // CR claude for claude: [bug] This records the assertion under CFlag::CheckOnly
        // too. A check returns before analysis::analyze (lib.rs:1958), and
        // check_def_assertions is the only thing that removes an entry, so a checked
        // assertion is never retired. The language server checks every edit with
        // CheckOnly on one runtime, so each check leaves one entry per
        // #[sync]/#[async]/#[tail_recursive] definition, and its spec pins that check's
        // AST and its Arc<Origin>, the whole source text. A 500 KB file with one tiny
        // #[sync] function grows the server by about 500 KB per edit, without bound.
        // Skip recording under CheckOnly and keep the "annotates a function definition"
        // error. probe: design/review-2026-10-05/repro/c-lib-03.py (c-lib-03)
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
        // CR claude for claude: [bug] Every Constant is typed
        // Type::Primitive(Typ::get(v)), but the literal parser (netidx's parse_value,
        // graphix-types/src/expr/parser/mod.rs:660) also yields `error:<v>` and
        // `abstract:<base64>` values. So `error:"boom"`, exactly how the shell prints
        // an Error<string>, is typed bare `error`, which no Error<T> contains except
        // Error<Any>. `let f = |e: Error<string>| e.0; f(error:"boom")` is refused
        // ("Error<string> does not contain error"), and `.0` on the literal gives
        // "expected tuple not error". Type an error constant as Error<type of its
        // payload>, or refuse the non-primitive forms in the parser and point at
        // error(..). probe: design/review-2026-10-05/repro/c-data-map-09.gx
        // (c-data-map-09)
        ExprKind::Constant(v) => Ok(Constant::new(
            v.clone(),
            Type::Primitive(Typ::get(v).into()),
            spec.clone(),
        )),
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
        ExprKind::Ref { name } => match eta_dispatcher(ctx, scope, &spec, name) {
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
/// a reference. `None` for any other name, and for a variadic method.
fn eta_dispatcher<R: Rt, E: UserEvent>(
    ctx: &CompileCtx<R, E>,
    scope: &Scope,
    spec: &Expr,
    name: &ModPath,
) -> Option<Expr> {
    let (_, bind) = ctx.env.lookup_bind(&scope.lexical, name).ok()??;
    let tm = ctx.env.trait_methods.get(&bind.id)?;
    let def = ctx.env.trait_defs.get(&tm.trait_id)?;
    let ft = &def.methods.get(tm.index)?.typ;
    if ft.vargs.is_some() {
        return None;
    }
    // the signature over variables of its own, as an annotation writes
    // it; `self` would name an impl's receiver
    let mut tvs: AHashMap<ArcStr, TVar> = AHashMap::new();
    ft.collect_tvars(&mut tvs);
    let renamed: AHashMap<ArcStr, Type> = tvs
        .keys()
        .map(|n| (n.clone(), Type::TVar(TVar::empty_named(arcstr::format!("eta_{n}")))))
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
    Some(at(ExprKind::Lambda(Arc::new(LambdaExpr {
        args: Arc::from_iter(args),
        vargs: None,
        rtype: Some(ft.rtype.clone()),
        constraints: Arc::from_iter(constraints),
        throws: ft.explicit_throws.then(|| ft.throws.clone()),
        body: LambdaBody::Expr(body),
    }))))
}
