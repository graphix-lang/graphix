//! What a debugger asks of a node graph (`design/debugger.md`): the `let`
//! that gives a binding its value, and the fused regions that compute a
//! place. Both ride [`crate::Update::refs`], the one walk that reaches
//! every node, callee instances included.

use crate::{BindId, Refs, explain::Because, stack, typ::Type};
use anyhow::Result;
use graphix_types::{
    SourcePosition,
    expr::{Expr, Origin, Source, parser},
};
use netidx_value::Value;
use std::{mem, path::Path};
use triomphe::Arc;

/// What gives a binding its value, as a debugger reads it: the `let`
/// that binds it, the argument a call passes for it, the select whose
/// arm's pattern names it, or a fused region, which is a box with inputs
/// and an output.
#[derive(Debug, Clone)]
pub struct BindSource {
    pub spec: Arc<Expr>,
    pub by: BoundBy,
    /// Every binding `spec` reads from outside itself.
    pub reads: Vec<BindId>,
    /// For a fused region, its type and the value it last produced.
    pub fused: Option<(Type, Option<Value>)>,
    /// Why its value has none, as far as its nodes tell: nothing where
    /// it has one.
    pub because: Vec<Because>,
    /// The calls it is inside, outermost first: one for each function
    /// instance around it.
    pub path: Vec<Arc<Expr>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoundBy {
    Let,
    /// A parameter: `spec` is the call that passes it.
    Param,
    /// A name in the pattern of a select's arm `arm`: `spec` is the
    /// select.
    Pattern {
        arm: usize,
    },
    Fused,
}

#[derive(Debug, Clone)]
pub(crate) enum Seek {
    /// What binds one binding; `calls` is the calls the walk is inside.
    Bind { id: BindId, found: Option<BindSource>, calls: Vec<Arc<Expr>> },
    /// The fused regions that compute a place: those whose expression is
    /// one of `covering`, and those that call a definition in `defs`,
    /// which the walk extends with each binding whose `let` is one of
    /// `covering`.
    Place { covering: Arc<[Expr]>, defs: Vec<BindId>, found: Vec<BindSource> },
}

/// The expressions of the source `ori` whose text covers `line`, or the
/// place `line`:`column`: read from its text again, since an expression
/// restored from an image does not say where it ends.
pub fn covering(ori: &Origin, line: i32, column: Option<i32>) -> Result<Arc<[Expr]>> {
    fn collect(e: &Expr, covers: &impl Fn(&Expr) -> bool, out: &mut Vec<Expr>) {
        if covers(e) {
            out.push(e.clone());
            stack::ensure_sufficient(|| {
                e.for_each_child(&mut |c| collect(c, covers, out))
            })
        }
    }
    let covers = |e: &Expr| {
        e.end.get().is_some_and(|end| match column {
            None => e.pos.line <= line && line <= end.line,
            Some(column) => {
                let at = SourcePosition { line, column };
                e.pos <= at && at < end
            }
        })
    };
    let mut out = Vec::new();
    for e in parser::parse(ori.clone())?.iter() {
        collect(e, &covers, &mut out)
    }
    Ok(Arc::from(out))
}

/// Whether `ori` is a file whose path ends in `file`.
pub fn in_file(ori: &Origin, file: &Path) -> bool {
    matches!(&ori.source, Source::File(p) if p.ends_with(file))
}

impl Refs {
    /// A walk of a whole tree that finds what binds `id`
    /// ([`Refs::take_sources`]): none where nothing in the tree does.
    pub fn seeking(id: BindId) -> Self {
        let seek = Seek::Bind { id, found: None, calls: Vec::new() };
        Self { seek: Some(Box::new(seek)), ..Self::default() }
    }

    /// The walk enters the body the call `spec` calls.
    pub(crate) fn enter_call(&mut self, spec: &Arc<Expr>) {
        if let Some(Seek::Bind { calls, .. }) = self.seek.as_deref_mut() {
            calls.push(spec.clone())
        }
    }

    pub(crate) fn leave_call(&mut self) {
        if let Some(Seek::Bind { calls, .. }) = self.seek.as_deref_mut() {
            calls.pop();
        }
    }

    /// A walk of a whole tree that finds the fused regions that compute a
    /// place, one for each instance fused ([`Refs::take_sources`]):
    /// `covering` is [`covering`] of the place, and `defs` what an
    /// earlier walk collected ([`Refs::take_defs`]).
    pub fn seeking_place(covering: Arc<[Expr]>, defs: Vec<BindId>) -> Self {
        let seek = Seek::Place { covering, defs, found: Vec::new() };
        Self { seek: Some(Box::new(seek)), ..Self::default() }
    }

    /// What the walk a seek made found.
    pub fn take_sources(&mut self) -> Vec<BindSource> {
        match self.seek.as_deref_mut() {
            Some(Seek::Bind { found, .. }) => found.take().into_iter().collect(),
            Some(Seek::Place { found, .. }) => mem::take(found),
            None => Vec::new(),
        }
    }

    /// The bindings whose `let` covers the place sought.
    pub fn take_defs(&mut self) -> Vec<BindId> {
        match self.seek.as_deref_mut() {
            Some(Seek::Place { defs, .. }) => mem::take(defs),
            Some(Seek::Bind { .. }) | None => Vec::new(),
        }
    }

    /// Whether the walk seeks what binds some binding: the test a node
    /// makes before it looks at what it binds.
    pub(crate) fn seeks_binding(&self) -> bool {
        matches!(self.seek.as_deref(), Some(Seek::Bind { .. }))
    }

    /// Whether the walk seeks the `let` that binds `id`.
    pub(crate) fn seeks(&self, id: BindId) -> bool {
        matches!(self.seek.as_deref(), Some(Seek::Bind { id: sought, .. }) if *sought == id)
    }

    /// Record that the `let` of `id` covers the place sought.
    pub(crate) fn defines(&mut self, id: BindId) {
        if let Some(Seek::Place { defs, .. }) = self.seek.as_deref_mut()
            && !defs.contains(&id)
        {
            defs.push(id)
        }
    }

    /// Whether the walk seeks a place and `id` is a definition covering
    /// it.
    pub(crate) fn seeks_callee(&self, id: BindId) -> bool {
        matches!(self.seek.as_deref(), Some(Seek::Place { defs, .. }) if defs.contains(&id))
    }

    /// Whether the walk seeks a place the text of `spec` covers.
    pub(crate) fn seeks_inside(&self, spec: &Expr) -> bool {
        let Some(Seek::Place { covering, .. }) = self.seek.as_deref() else {
            return false;
        };
        let same = |e: &Expr| {
            e.pos == spec.pos
                && e == spec
                && (Arc::ptr_eq(&e.ori, &spec.ori) || e.ori.source == spec.ori.source)
        };
        covering.iter().any(same)
    }

    /// Record `spec` as a source the walk seeks: what binds the binding
    /// sought, reading what `walk` collects, with `because` its value has
    /// none.
    pub(crate) fn sought(
        &mut self,
        spec: Arc<Expr>,
        by: BoundBy,
        fused: Option<(Type, Option<Value>)>,
        because: Vec<Because>,
        walk: impl FnOnce(&mut Refs),
    ) {
        let mut inner = Refs::default();
        walk(&mut inner);
        let mut reads = Vec::new();
        inner.with_external_refs(|id| reads.push(id));
        reads.sort_unstable();
        match self.seek.as_deref_mut() {
            Some(Seek::Bind { found, calls, .. }) => {
                let path = calls.clone();
                *found = Some(BindSource { spec, by, reads, fused, because, path })
            }
            Some(Seek::Place { found, .. }) => {
                let path = Vec::new();
                found.push(BindSource { spec, by, reads, fused, because, path })
            }
            None => (),
        }
    }
}
