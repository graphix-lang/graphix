use crate::{
    expr::{
        Attr, Comment, Decorations, Doc, Expr, ExprKind, ModuleKind, SeqTrigger, Sig,
        SigKind, StrForm, StructurePattern, TraitExpr, TypeDefBody, TypeDefExpr,
        UseComment, UseItem,
    },
    stack::ensure_sufficient,
};
use arcstr::ArcStr;
use netidx_value::Value;
#[cfg(test)]
use poolshark::local::LPooled;

/// Something a parse keeps beside what expression equality sees, or that
/// prints on lines of its own: what the formatter must hand back, and what
/// cannot stand where a tree prints on one line.
#[derive(Debug, PartialEq)]
pub(crate) enum Ornament<'a> {
    UseComments(&'a [UseComment]),
    Decorations(&'a Decorations),
    Comments(&'a [Comment]),
    Eol(Option<&'a ArcStr>),
    Doc(&'a Doc),
    Attrs(&'a [Attr]),
    Delimiters(StrForm),
}

impl Ornament<'_> {
    /// A `//` or `///` line.
    fn is_comment(&self) -> bool {
        match self {
            Self::UseComments(c) => !c.is_empty(),
            Self::Decorations(d) => {
                !d.comments.is_empty() || d.eol.is_some() || !d.below.is_empty()
            }
            Self::Comments(c) => !c.is_empty(),
            Self::Eol(c) => c.is_some(),
            Self::Doc(d) => d.0.is_some(),
            Self::Attrs(_) | Self::Delimiters(_) => false,
        }
    }

    /// A `#[..]` attribute.
    fn is_attr(&self) -> bool {
        match self {
            Self::Decorations(d) => !d.attrs.is_empty(),
            Self::Attrs(a) => !a.is_empty(),
            _ => false,
        }
    }
}

fn holds<'a>(e: &'a Expr, is: impl Fn(&Ornament<'a>) -> bool) -> bool {
    let mut found = false;
    Ornaments::new(|_, o| found |= is(&o)).expr(e);
    found
}

/// Whether a `//` comment or a `///` doc stands anywhere in `e`, an
/// attribute's arguments and an interface's items included.
pub(crate) fn holds_comment(e: &Expr) -> bool {
    holds(e, Ornament::is_comment)
}

/// Whether a `#[..]` attribute stands anywhere in `e`, a trait method's
/// and an interface's items included.
pub(crate) fn holds_attr(e: &Expr) -> bool {
    holds(e, Ornament::is_attr)
}

/// The delimiters of every string literal in `e`, an expression's or a
/// pattern's, in source order: what printing as written must keep.
#[cfg(test)]
pub(crate) fn str_forms(e: &Expr) -> LPooled<Vec<StrForm>> {
    let mut forms: LPooled<Vec<StrForm>> = LPooled::take();
    Ornaments::new(|_, o| {
        if let Ornament::Delimiters(form) = o {
            forms.push(form)
        }
    })
    .expr(e);
    forms
}

/// The ornaments of a tree in source order, each handed to `f` beside the
/// preorder ordinal of the node it stands on (an expression or an
/// interface item), so one that moves to a neighbour is a difference.
pub(crate) struct Ornaments<F> {
    node: usize,
    f: F,
}

impl<'a, F: FnMut(usize, Ornament<'a>)> Ornaments<F> {
    pub(crate) fn new(f: F) -> Self {
        Self { node: 0, f }
    }

    fn push(&mut self, o: Ornament<'a>) {
        (self.f)(self.node, o)
    }

    /// A pattern's string literals' delimiters, in order.
    fn pattern(&mut self, p: &'a StructurePattern) {
        ensure_sufficient(|| {
            if let StructurePattern::Literal(Value::String(_), form) = p {
                self.push(Ornament::Delimiters(form.0))
            }
            p.for_each_sub(&mut |c| self.pattern(c))
        })
    }

    pub(crate) fn expr(&mut self, e: &'a Expr) {
        ensure_sufficient(|| {
            self.node += 1;
            if let Some(d) = &e.dec {
                self.push(Ornament::Decorations(d));
                for a in d.attrs.iter() {
                    a.args.iter().for_each(|a| self.expr(a))
                }
            }
            match &e.kind {
                ExprKind::Constant(Value::String(_))
                | ExprKind::StringInterpolate { .. } => {
                    self.push(Ornament::Delimiters(e.str_form))
                }
                ExprKind::Bind(b) => self.pattern(&b.pattern),
                ExprKind::Lambda(l) => {
                    for a in l.args.iter() {
                        self.pattern(&a.pattern);
                        self.push(Ornament::Comments(a.comments.above.lines()));
                        self.push(Ornament::Eol(a.comments.eol.text()));
                    }
                    self.push(Ornament::Comments(l.vargs_comments.above.lines()));
                    self.push(Ornament::Eol(l.vargs_comments.eol.text()));
                    self.push(Ornament::Comments(l.below.lines()));
                }
                ExprKind::Select(sel) => sel
                    .arms
                    .iter()
                    .for_each(|(p, _)| self.pattern(&p.structure_predicate)),
                ExprKind::Trait(t) => self.methods(t, |_, _| ()),
                ExprKind::Impl(im) => self.push(Ornament::Comments(im.below.lines())),
                ExprKind::TypeDef(td) => self.typedef(td),
                ExprKind::Use { names, .. } => self.uses(names),
                ExprKind::Module { value, .. } => match &**value {
                    ModuleKind::Dynamic { sig, .. }
                    | ModuleKind::Resolved { sig: Some(sig), .. } => self.sig(sig),
                    ModuleKind::Resolved { sig: None, .. }
                    | ModuleKind::Unresolved { .. } => (),
                },
                ExprKind::Seq(sq) if let Some(SeqTrigger::Bind(b)) = &sq.trigger => {
                    self.pattern(&b.pattern)
                }
                _ => (),
            }
            e.for_each_child(&mut |c| self.expr(c))
        })
    }

    /// A trait's ornaments; `default` walks a method's default body, which
    /// an expression's walk reaches as a child.
    fn methods(
        &mut self,
        t: &'a TraitExpr,
        mut default: impl FnMut(&mut Self, &'a Expr),
    ) {
        for m in t.methods.iter() {
            self.push(Ornament::Comments(m.comments.lines()));
            self.push(Ornament::Doc(&m.doc));
            self.push(Ornament::Attrs(&m.attrs));
            self.push(Ornament::Eol(m.eol.text()));
            if let Some(d) = &m.default {
                default(self, d)
            }
        }
        self.push(Ornament::Comments(t.below.lines()))
    }

    /// The comments on a type definition's fields and members, each item's
    /// in canonical order, so one that moved to another item shows.
    fn typedef(&mut self, td: &'a TypeDefExpr) {
        let (TypeDefBody::Alias(t) | TypeDefBody::Abstract(Some(t))) = &td.body else {
            return;
        };
        if td.notes.is_empty() {
            return;
        }
        td.notes.each_in(t, &mut |n| {
            let (above, eol, below) = match n {
                Some(n) => {
                    (n.comments.above.lines(), n.comments.eol.text(), n.below.lines())
                }
                None => (&[][..], None, &[][..]),
            };
            self.push(Ornament::Comments(above));
            self.push(Ornament::Eol(eol));
            self.push(Ornament::Comments(below));
        })
    }

    fn uses(&mut self, names: &'a [UseItem]) {
        for n in names {
            self.push(Ornament::UseComments(n.comments.lines()));
        }
    }

    pub(crate) fn sig(&mut self, sig: &'a Sig) {
        for si in sig.items.iter() {
            self.node += 1;
            self.push(Ornament::Comments(si.comments.lines()));
            self.push(Ornament::Doc(&si.doc));
            self.push(Ornament::Eol(si.eol.text()));
            match &si.kind {
                SigKind::Trait(t) => self.methods(t, |w, d| w.expr(d)),
                SigKind::TypeDef(td) => self.typedef(td),
                SigKind::Use { names, .. } => self.uses(names),
                SigKind::Impl(_) | SigKind::Bind(_) | SigKind::Module(_) => (),
            }
        }
        self.push(Ornament::Comments(sig.below.lines()))
    }
}
