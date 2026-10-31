//! Whether a value a program writes may read a binding, by name.

use crate::{
    expr::{
        ApplyExpr, Expr, ExprKind, LambdaBody, LambdaExpr, SeqTrigger, StructurePattern,
    },
    typ::Mutability,
};
use ahash::{AHashMap, AHashSet};
use compact_str::CompactString;
use netidx_core::path::Path;
use poolshark::local::LPooled;
use smallvec::SmallVec;

/// What a name nothing tracks may be given: every binding.
const ANY: &str = "#any";
/// What a dereference reads: whatever a reference was taken of, and
/// whatever was written through one.
const REFERENT: &str = "#referent";

type Names = SmallVec<[CompactString; 8]>;

/// Whether `value`, written in the program whose statements are `stmts`,
/// may read the binding named `target`: through each name it reads, what
/// that name is bound or written from, a function's body where it reads
/// the function, a parameter's arguments, a dereference's referents. Names
/// are not resolved, so a binding of the name anywhere counts, and a
/// parameter of a function handed on as a value may be given anything.
pub(crate) fn may_read<'a>(
    stmts: impl IntoIterator<Item = &'a Expr>,
    value: &Expr,
    target: &str,
) -> bool {
    let mut flows = Flows::default();
    for e in stmts {
        flows.expr(e, &mut Names::new());
    }
    let mut start = Names::new();
    flows.expr(value, &mut start);
    flows.close();
    flows.reaches(&start, target)
}

#[derive(Default)]
struct Flows {
    /// The names each name may be given.
    from: LPooled<AHashMap<CompactString, Names>>,
    /// The parameters of the lambdas a `let` binds, by its name.
    params: LPooled<AHashMap<CompactString, Names>>,
    /// Each function called by name, with what its arguments read.
    calls: LPooled<Vec<(CompactString, Names)>>,
    /// The names read other than as the function of a call.
    escaped: LPooled<AHashSet<CompactString>>,
    /// The names a `&mut` was taken of.
    writable: Names,
    /// What is written through references.
    through_refs: Names,
}

fn last(name: &str) -> CompactString {
    Path::basename(name).unwrap_or(name).into()
}

fn params(l: &LambdaExpr) -> Names {
    let mut names = Names::new();
    for a in l.args.iter() {
        a.pattern.with_names(&mut |n| names.push(n.as_str().into()));
    }
    names
}

impl Flows {
    fn give(&mut self, to: &str, what: &[CompactString]) {
        if !what.is_empty() {
            self.from.entry(to.into()).or_default().extend(what.iter().cloned())
        }
    }

    fn pattern(&mut self, p: &StructurePattern, what: &[CompactString]) {
        p.with_names(&mut |n| self.give(n, what))
    }

    /// A lambda's defaults and body, what reading the lambda reads.
    fn lambda(&mut self, l: &LambdaExpr, reads: &mut Names) {
        for a in l.args.iter() {
            if let Some(d) = a.kind.default() {
                let mut r = Names::new();
                self.expr(d, &mut r);
                self.pattern(&a.pattern, &r);
            }
        }
        if let LambdaBody::Expr(b) = &l.body {
            self.expr(b, reads)
        }
    }

    fn apply(&mut self, a: &ApplyExpr, reads: &mut Names) {
        let mut args = Names::new();
        for (_, e) in a.args.iter() {
            self.expr(e, &mut args);
        }
        match &a.function.unparen().kind {
            ExprKind::Ref { name } => {
                let name = last(name);
                self.calls.push((name.clone(), args.clone()));
                reads.push(name)
            }
            ExprKind::Lambda(l) => {
                for p in params(l) {
                    self.give(&p, &args)
                }
                self.lambda(l, reads)
            }
            _ => self.expr(&a.function, reads),
        }
        reads.extend(args)
    }

    fn expr(&mut self, e: &Expr, reads: &mut Names) {
        crate::stack::ensure_sufficient(|| match &e.kind {
            ExprKind::Ref { name } => {
                let name = last(name);
                self.escaped.insert(name.clone());
                reads.push(name)
            }
            ExprKind::Bind(b) => {
                let mut r = Names::new();
                match &b.value.unparen().kind {
                    ExprKind::Lambda(l) => {
                        let ps = params(l);
                        b.pattern.with_names(&mut |n| {
                            self.params
                                .entry(n.as_str().into())
                                .or_default()
                                .extend(ps.iter().cloned())
                        });
                        self.lambda(l, &mut r)
                    }
                    _ => self.expr(&b.value, &mut r),
                }
                self.pattern(&b.pattern, &r);
                reads.extend(r)
            }
            ExprKind::Connect { name, value, deref, .. } => {
                let mut r = Names::new();
                self.expr(value, &mut r);
                match deref {
                    true => self.through_refs.extend(r.iter().cloned()),
                    false => self.give(&last(name), &r),
                }
                reads.extend(r)
            }
            ExprKind::ByRef(m, inner) => {
                let mut r = Names::new();
                self.expr(inner, &mut r);
                self.give(REFERENT, &r);
                if *m == Mutability::Mut {
                    self.writable.extend(r.iter().cloned())
                }
                reads.extend(r)
            }
            ExprKind::Deref(inner) => {
                self.expr(inner, reads);
                reads.push(REFERENT.into())
            }
            ExprKind::Lambda(l) => {
                for p in params(l) {
                    self.give(&p, &[ANY.into()])
                }
                self.lambda(l, reads)
            }
            ExprKind::Apply(a) => self.apply(a, reads),
            ExprKind::Select(s) => {
                let mut r = Names::new();
                self.expr(&s.arg, &mut r);
                for (p, body) in s.arms.iter() {
                    self.pattern(&p.structure_predicate, &r);
                    if let Some(g) = &p.guard {
                        self.expr(g, reads)
                    }
                    self.expr(body, reads)
                }
                reads.extend(r)
            }
            ExprKind::Catch(c) => {
                self.give(&c.bind.name, &[ANY.into()]);
                e.for_each_child(&mut |c| self.expr(c, reads))
            }
            ExprKind::TryWith(t) => {
                self.give(&t.bind.name, &[ANY.into()]);
                e.for_each_child(&mut |c| self.expr(c, reads))
            }
            ExprKind::Seq(s) => {
                if let Some(SeqTrigger::Bind(b)) = &s.trigger {
                    let mut r = Names::new();
                    self.expr(&b.value, &mut r);
                    self.pattern(&b.pattern, &r)
                }
                e.for_each_child(&mut |c| self.expr(c, reads))
            }
            ExprKind::Use { names, .. } => {
                for item in names.iter() {
                    if let Some(rename) = &item.rename {
                        self.give(&rename.name, &[last(&item.path)])
                    }
                }
            }
            _ => e.for_each_child(&mut |c| self.expr(c, reads)),
        })
    }

    /// The arguments of each call to their function's parameters, and
    /// anything to the parameters of one handed on as a value.
    fn close(&mut self) {
        let calls = std::mem::take(&mut *self.calls);
        for (f, args) in calls.iter() {
            if let Some(ps) = self.params.get(f).cloned() {
                for p in ps.iter() {
                    self.give(p, args)
                }
            }
        }
        let escaped: Names = self
            .params
            .keys()
            .filter(|f| self.escaped.contains(*f))
            .flat_map(|f| self.params[f].iter().cloned())
            .collect();
        for p in escaped.iter() {
            self.give(p, &[ANY.into()])
        }
        let (writable, through) =
            (std::mem::take(&mut self.writable), std::mem::take(&mut self.through_refs));
        for w in writable.iter() {
            self.give(w, &through)
        }
        self.give(REFERENT, &through)
    }

    fn reaches(&self, start: &[CompactString], target: &str) -> bool {
        let mut seen: LPooled<AHashSet<&str>> = LPooled::take();
        let mut work: LPooled<Vec<&str>> = start.iter().map(|n| n.as_str()).collect();
        while let Some(n) = work.pop() {
            if n == target || n == ANY {
                return true;
            }
            if seen.insert(n)
                && let Some(from) = self.from.get(n)
            {
                work.extend(from.iter().map(|n| n.as_str()))
            }
        }
        false
    }
}
