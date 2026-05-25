//! Unified fusion: discovery walker (Phase 2 prototype).
//!
//! See `design/unified_fusion.md` for the full architecture.
//!
//! Discovery half of the unified pass: walk the post-compile node
//! graph and produce a flat list of `KernelCandidate`s. Each
//! candidate identifies a piece of code the build phase will turn
//! into a `KirKernel`.
//!
//! The walker dispatches on `NodeView` (see `lib.rs`'s `NodeView`).
//! Compared to the AST-based `for_each_child` walker the legacy
//! fusion pass uses, this one steps inside `Lambda` bodies and
//! reads scope/BindIds directly from compiled nodes instead of
//! reconstructing them.
//!
//! **Phase 2 scope**: the walker handles the four fusion-relevant
//! `NodeView` variants (`Bind`, `Lambda`, `Block`, `Module`) and
//! registers `Region` candidates for module-top-level non-Bind
//! nodes. Recursion into nested anonymous lambdas inside Apply
//! args / Select arms / etc. is deferred to Phase 3 (the build
//! phase already walks via `node.spec()` Exprs for KIR emit).

use crate::{
    expr::{ExprId, ModPath},
    node::{
        bind::Bind, lambda::Lambda, module::Module, Block,
    },
    BindId, Node, NodeView, Rt, Scope, UserEvent,
};
use arcstr::ArcStr;

/// A piece of code the build phase will lower into a `KirKernel`.
/// Lifetimes tie back to the source node graph — candidates borrow
/// into it and are consumed in the same pass.
#[derive(Debug)]
pub struct KernelCandidate<'a, R: Rt, E: UserEvent> {
    pub kind: CandidateKind<'a, R, E>,
    /// Module scope at the candidate's declaration point.
    pub scope: Scope,
    /// `ExprId` of the source expression. Used by the splice step
    /// to locate the node in the runtime's `nodes` map.
    pub source_id: ExprId,
}

#[derive(Debug)]
pub enum CandidateKind<'a, R: Rt, E: UserEvent> {
    /// `let f = |args| body` at any scope. Build a lambda-shape
    /// kernel keyed by `name`.
    LambdaBind {
        name: ArcStr,
        bind_id: BindId,
        bind: &'a Bind<R, E>,
        lambda: &'a Lambda,
    },
    /// `|args| body` at a non-Bind position. Synth name from the
    /// lambda's identity. (Deferred to Phase 3 — the walker
    /// doesn't currently descend into HOF args.)
    #[allow(dead_code)]
    AnonymousLambda {
        synth_name: ArcStr,
        lambda: &'a Lambda,
    },
    /// `let x = <sync expr>` at module-top-level. The runtime
    /// publishes the result via `bind_id`.
    ValueBind {
        name: ArcStr,
        bind_id: BindId,
        bind: &'a Bind<R, E>,
    },
    /// A top-level Sync subexpression that isn't a Bind. Splice
    /// replaces the node identified by `source_id` in the
    /// runtime's `nodes` map. `body` is the spec Expr the build
    /// phase feeds to KIR emission.
    Region {
        source_id: ExprId,
        body: &'a crate::expr::Expr,
    },
}

#[derive(Debug, Default)]
pub struct Candidates<'a, R: Rt, E: UserEvent> {
    pub items: Vec<KernelCandidate<'a, R, E>>,
}

impl<'a, R: Rt, E: UserEvent> Candidates<'a, R, E> {
    pub fn counts(&self) -> CandidateCounts {
        let mut c = CandidateCounts::default();
        for item in &self.items {
            match &item.kind {
                CandidateKind::LambdaBind { .. } => c.lambda_binds += 1,
                CandidateKind::AnonymousLambda { .. } => c.anonymous_lambdas += 1,
                CandidateKind::ValueBind { .. } => c.value_binds += 1,
                CandidateKind::Region { .. } => c.regions += 1,
            }
        }
        c
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct CandidateCounts {
    pub lambda_binds: usize,
    pub anonymous_lambdas: usize,
    pub value_binds: usize,
    pub regions: usize,
}

/// Entry point. Walk every top-level `Node` in `nodes`, producing a
/// flat candidate list. `root_scope` is the module scope to start
/// at (typically `ModPath::root()`).
pub fn walk<'a, R: Rt, E: UserEvent>(
    nodes: &'a [Node<R, E>],
    root_scope: ModPath,
) -> Candidates<'a, R, E> {
    let mut w = Walker {
        scope: root_scope,
        candidates: Vec::new(),
        module_top_level: true,
    };
    for n in nodes {
        w.visit_top_level(&**n);
    }
    Candidates { items: w.candidates }
}

struct Walker<'a, R: Rt, E: UserEvent> {
    /// Current module scope. Updated on Module entry/exit using the
    /// Module/Block-module node's own `scope` field.
    scope: ModPath,
    candidates: Vec<KernelCandidate<'a, R, E>>,
    /// True at file root and inside Module / Block(module=true).
    /// False inside Block(module=false) (Do blocks) and Lambda
    /// bodies. Drives `ValueBind` vs in-line-let-of-surrounding-
    /// kernel and `Region` vs not.
    module_top_level: bool,
}

impl<'a, R: Rt, E: UserEvent> Walker<'a, R, E> {
    fn visit_top_level(&mut self, n: &'a dyn crate::Update<R, E>) {
        match n.view() {
            NodeView::Bind(b) => self.visit_bind(b),
            NodeView::Block(blk) if blk.module => self.visit_module_block(blk),
            // Top-level `Do` block (non-module) — descend into its
            // children so each statement gets its own visit instead
            // of registering the whole `Do` as one Region. The
            // trailing expression-shape child is what produces the
            // file's value, so it gets the Region candidate;
            // earlier `Bind`/`Module`/etc. statements get their own
            // appropriate candidates (or none).
            NodeView::Block(blk) => self.visit_do_block(blk),
            NodeView::Module(m) => self.visit_sig_module(m),
            NodeView::Use(_) | NodeView::TypeDef(_) | NodeView::Nop(_) => {}
            _ => {
                if self.module_top_level {
                    let spec = n.spec();
                    self.candidates.push(KernelCandidate {
                        kind: CandidateKind::Region {
                            source_id: spec.id,
                            body: spec,
                        },
                        scope: Scope {
                            lexical: self.scope.clone(),
                            dynamic: self.scope.clone(),
                        },
                        source_id: spec.id,
                    });
                }
                // Phase 2: don't descend into non-container nodes
                // looking for anonymous lambdas. Phase 3 will add
                // per-node child accessors (or a generic descent)
                // so e.g. `array::map(arr, |x| ...)` registers an
                // AnonymousLambda candidate.
            }
        }
    }

    /// Descend into a `Block { module: false }` (Do block) at a
    /// top-level dispatch position. Each child is dispatched via
    /// `visit_top_level`, but `module_top_level` is suppressed for
    /// every child except the trailing one — only the Do's trailing
    /// expression can usefully be a Region candidate (it carries
    /// the block's value). Earlier statements get their own
    /// appropriate visits: `Bind` → `visit_bind`, `Module` →
    /// `visit_sig_module`, etc.
    fn visit_do_block(&mut self, blk: &'a Block<R, E>) {
        let n = blk.children.len();
        let saved_top = self.module_top_level;
        for (i, child) in blk.children.iter().enumerate() {
            let is_last = i + 1 == n;
            self.module_top_level = is_last && saved_top;
            self.visit_top_level(&**child);
        }
        self.module_top_level = saved_top;
    }

    fn visit_bind(&mut self, b: &'a Bind<R, E>) {
        // Get name and bind_id from the bind's pattern.
        let name = match &b.spec.kind {
            crate::expr::ExprKind::Bind(be) => be.pattern.single_bind().cloned(),
            _ => None,
        };
        let bind_id = single_bind_id(b);
        // Is the value a Lambda?
        match (b.node.view(), name, bind_id) {
            (NodeView::Lambda(l), Some(name), Some(bind_id)) => {
                self.candidates.push(KernelCandidate {
                    kind: CandidateKind::LambdaBind {
                        name,
                        bind_id,
                        bind: b,
                        lambda: l,
                    },
                    scope: b.scope.clone(),
                    source_id: b.spec.id,
                });
                // Lambda body resets module_top_level. The compiled
                // body lives inside the Lambda's InitFn, not yet
                // accessible at walk time — Phase 3 will descend via
                // node.spec() Expr. For now: nothing more to do.
            }
            (_, Some(name), Some(bind_id)) => {
                if self.module_top_level {
                    self.candidates.push(KernelCandidate {
                        kind: CandidateKind::ValueBind {
                            name,
                            bind_id,
                            bind: b,
                        },
                        scope: b.scope.clone(),
                        source_id: b.spec.id,
                    });
                }
                // Descend into the value: if it's a Block, that
                // becomes a Do-block scope reset; if it's a nested
                // Bind/Lambda/Module, we register those. Other node
                // types: no-op (Phase 3).
                self.descend_into_value(&*b.node);
            }
            _ => {
                // Multi-binding pattern: no single name/bind_id.
                // Still descend.
                self.descend_into_value(&*b.node);
            }
        }
    }

    /// Visit a `Block { module: true }` node — an unsig'd module
    /// declaration. Push scope, re-enter module-top-level, recurse.
    fn visit_module_block(&mut self, blk: &'a Block<R, E>) {
        let mod_name = match &blk.spec.kind {
            crate::expr::ExprKind::Module { name, .. } => name.clone(),
            _ => ArcStr::new(), // shouldn't happen; defensive
        };
        let new_scope = ModPath(self.scope.append(mod_name.as_str()));
        let saved_scope = std::mem::replace(&mut self.scope, new_scope);
        let saved_top = std::mem::replace(&mut self.module_top_level, true);
        for child in blk.children.iter() {
            self.visit_top_level(&**child);
        }
        self.scope = saved_scope;
        self.module_top_level = saved_top;
    }

    fn visit_sig_module(&mut self, m: &'a Module<R, E>) {
        // The Module node already has the *inner* scope (post-
        // append of the module name) stored in `scope`. Use it.
        let saved_scope =
            std::mem::replace(&mut self.scope, m.scope.lexical.clone());
        let saved_top = std::mem::replace(&mut self.module_top_level, true);
        for child in module_children(m).iter() {
            self.visit_top_level(&**child);
        }
        self.scope = saved_scope;
        self.module_top_level = saved_top;
    }

    /// Descend into a bind's value node. Only special-cases the
    /// container variants — Bind, Block, Lambda — which need
    /// scope-aware traversal. Other variants: no-op (Phase 3 will
    /// add nested-lambda discovery via spec-Expr walk).
    fn descend_into_value(&mut self, n: &'a dyn crate::Update<R, E>) {
        match n.view() {
            NodeView::Bind(b) => self.visit_bind(b),
            NodeView::Block(blk) => {
                // Do-block (module=false) introduces a lexical scope.
                if blk.module {
                    self.visit_module_block(blk);
                } else {
                    let saved_top =
                        std::mem::replace(&mut self.module_top_level, false);
                    for child in blk.children.iter() {
                        self.visit_top_level(&**child);
                    }
                    self.module_top_level = saved_top;
                }
            }
            NodeView::Module(m) => self.visit_sig_module(m),
            // Phase 3 will add descent for nested anonymous-lambda
            // discovery into other variants.
            _ => {}
        }
    }
}

fn single_bind_id<R: Rt, E: UserEvent>(b: &Bind<R, E>) -> Option<BindId> {
    let mut id: Option<BindId> = None;
    let mut count = 0usize;
    b.pattern.ids(&mut |i| {
        count += 1;
        if id.is_none() {
            id = Some(i);
        }
    });
    if count == 1 {
        id
    } else {
        None
    }
}

/// Accessor for `Module<R, E>::nodes`. Reads the `nodes` field
/// (now `pub(crate)`) directly.
fn module_children<R: Rt, E: UserEvent>(
    m: &Module<R, E>,
) -> &[Node<R, E>] {
    &m.nodes
}

// Walker tests live in `stdlib/graphix-tests/src/lang/unified_walker.rs`
// — they require the full parse → compile pipeline to produce real
// Node graphs, which depends on `GXRt` from the graphix-rt crate
// (downstream of graphix-compiler).
