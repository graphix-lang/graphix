use super::{
    super::typexp::flatten_bounds,
    attribute, decl_items, doc_comment, expr,
    input::{Input, R},
    keyword, leading_comments, name, statements, trail,
    typexp::{angle_list, tvar_opt_bound, typ, typath},
    typname,
};
use crate::{
    expr::{
        Attr, Comments, Doc, Eol, Expr, ExprKind, ImplExpr, ImplHead, Name,
        StructurePattern, TraitExpr, TraitMethod,
    },
    typ::{FnArgKind, TVar, Type},
};
use poolshark::local::LPooled;
use triomphe::Arc;

/// One trait item: `val name: fn(self, ..) -> T` with an optional
/// `= default` body. The signature must be a function type with a
/// positional `self` parameter. The `//` lines above it are kept; in an
/// interface (`sig`) the `///` doc lines below those are its doc, and
/// its `#[..]` requirements follow.
fn trait_method(i: &mut Input, sig: bool) -> R<TraitMethod> {
    let mut comments = leading_comments(i, sig)?;
    let doc = match sig {
        true => doc_comment(i)?,
        false => Doc(None),
    };
    i.spaces();
    let mut attrs: LPooled<Vec<Attr>> = LPooled::take();
    i.many(
        |i| {
            let a = attribute(i)?;
            i.spaces();
            Ok(a)
        },
        |a| attrs.push(a),
    )?;
    i.attempt(|i| {
        i.string("val")?;
        i.spaces1()
    })?;
    let name = name(i)?;
    i.sptoken(':')?;
    let typ = typ(i)?;
    let default = i.optional(|i| {
        i.attempt(|i| i.sptoken('='))?;
        expr(i)
    })?;
    let end = i.pos();
    let Type::Fn(ft) = &typ else {
        return Err(i.refuse(end, "a trait method must have a function type"));
    };
    let self_index = ft.args.iter().position(
        |a| matches!(&a.kind, FnArgKind::Positional { name: Some(n) } if &**n == "self"),
    );
    if self_index.is_none() && !ft.rtype.mentions_self() {
        return Err(i.refuse(
            end,
            "a trait method takes a `self` parameter (`fn(self, ..)`) or returns `self`",
        ));
    }
    Ok(TraitMethod {
        comments: Comments::of(comments.drain(..)),
        eol: Eol::default(),
        doc,
        attrs: Arc::from_iter(attrs.drain(..)),
        name,
        typ: ft.clone(),
        self_index,
        default,
    })
}

/// A trait declaration; `sig` in an interface, where methods carry docs.
pub(super) fn trait_decl(i: &mut Input, sig: bool) -> R<TraitExpr> {
    i.attempt(|i| {
        i.string("trait")?;
        i.spaces1()
    })?;
    let at = i.pos();
    let name = Name::written(typname(i)?, at);
    i.spaces();
    i.token('{')?;
    i.spaces();
    let (mut methods, below) =
        decl_items(i, &|i| trait_method(i, sig), &|i| i.token('}').map(drop), sig)?;
    i.sptoken('}')?;
    let end = i.pos();
    let dup = |(n, m): (usize, &TraitMethod)| {
        methods[..n].iter().any(|p| p.name.name == m.name.name)
    };
    if methods.iter().enumerate().any(dup) {
        return Err(i.refuse(end, "duplicate trait method"));
    }
    Ok(TraitExpr { name, methods: Arc::from_iter(methods.drain(..)), below })
}

/// `impl<'a: C, ..> Trait for Target`.
fn impl_head(i: &mut Input) -> R<ImplHead> {
    keyword(i, "impl")?;
    i.spaces();
    let params = i.optional(|i| angle_list(i, &tvar_opt_bound))?;
    i.spaces();
    let at_params = i.pos();
    let (trait_name, trait_at) = typath(i)?;
    i.spaces1()?;
    i.string("for")?;
    i.spaces1()?;
    let target = typ(i)?;
    let mut params = params.unwrap_or_else(LPooled::take);
    let tvs: Arc<[TVar]> = Arc::from_iter(params.iter().map(|(tv, _)| tv.clone()));
    if tvs.iter().enumerate().any(|(n, tv)| tvs[..n].iter().any(|p| p.name == tv.name)) {
        return Err(i.refuse(at_params, "duplicate impl type variable"));
    }
    let mut constraints =
        flatten_bounds(params.drain(..).filter_map(|(tv, b)| b.map(|b| (tv, b))));
    Ok(ImplHead {
        trait_name,
        trait_at,
        params: tvs,
        constraints: Arc::from_iter(constraints.drain(..)),
        target,
    })
}

/// An interface's `impl` item: the head alone, since the module's body
/// supplies the implementation.
pub(super) fn impl_sig(i: &mut Input) -> R<ImplHead> {
    let head = impl_head(i)?;
    let body = i.optional(|i| {
        i.attempt(|i| {
            i.spaces();
            let at = i.pos();
            i.look_ahead(|i| i.token('{'))?;
            Ok(at)
        })
    })?;
    match body {
        None => Ok(head),
        Some(at) => Err(i.refuse(at, "an interface declares an impl without a body")),
    }
}

/// `impl<'a: C, ..> Trait for Target { let m = ..; .. }` — the body is
/// optional, implementing a trait whose methods all have defaults.
pub(super) fn impl_decl(i: &mut Input) -> R<ImplExpr> {
    let head = impl_head(i)?;
    let methods = i.optional(|i| {
        i.attempt(|i| {
            i.spaces();
            i.look_ahead(|i| i.token('{'))
        })?;
        i.token('{')?;
        i.spaces();
        let ms = statements(i, &|i| i.token('}').map(drop))?;
        i.sptoken('}')?;
        Ok(ms)
    })?;
    let end = i.pos();
    let mut ms: LPooled<Vec<Expr>> = methods.unwrap_or_else(LPooled::take);
    let mut alone = Comments::default();
    // a trailing `;` leaves an empty statement holding the lines below it
    if let Some(empty) = ms.pop_if(|e| matches!(e.kind, ExprKind::NoOp)) {
        let below = empty.dec.as_ref().map(|d| &d.below[..]).unwrap_or(&[]);
        match ms.last_mut() {
            Some(m) => trail(m, None, below),
            None => alone = Comments::of(below.iter().cloned()),
        }
    }
    let simple = |m: &Expr| matches!(&m.kind, ExprKind::Bind(b) if matches!(b.pattern, StructurePattern::Bind(_)));
    if !ms.iter().all(simple) {
        return Err(i.refuse(end, "an impl body holds only `let name = ..` methods"));
    }
    Ok(ImplExpr { head, methods: Arc::from_iter(ms.drain(..)), below: alone })
}
