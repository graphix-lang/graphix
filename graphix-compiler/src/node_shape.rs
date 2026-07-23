//! Declarative graph-shape assertions — the third test axis.
//!
//! `run!` checks a program's *value*; `FuseExpect` checks *whether*
//! fusion fired. Neither can see *what fused into what*. A [`NodeShape`]
//! is a declarative *specification* of a compiled (sub)graph: either a
//! standard node (a `NodeView` kind plus child shapes), a fused kernel
//! matched by partial [`KernelMatcher`] criteria, or [`NodeShape::Any`]
//! (a don't-care wildcard). [`match_node`] checks a real `Node` against
//! a spec and returns a precise mismatch reason on failure.
//!
//! The runtime drives this via `GXHandle::match_shape(eid, spec)`: the
//! walk-and-compare runs once, in-task, against the *live* post-fusion
//! graph — so the spec is checked against exactly what the compiler
//! produced, and can't drift from reality the way a hand-reconstructed
//! expectation can. `GXHandle::describe_shape(eid)` renders the actual
//! graph as text — an authoring aid for writing the spec in the first
//! place, not part of the assertion path.

use crate::{Node, NodeView, Rt, UserEvent, fusion::kernel_abi::KernelSig, typ::Type};
use arcstr::ArcStr;
use smallvec::SmallVec;

/// A declarative specification of a (sub)graph's shape.
#[derive(Debug, Clone)]
pub enum NodeShape {
    /// Matches any single node/subtree. Don't-care.
    Any,
    /// A non-fused node. `kind` (a `NodeView` variant name, see
    /// [`kind_name`]) must match when `Some`; `None` matches any kind.
    /// `children` match the node's children positionally — use
    /// [`NodeShape::Any`] to skip a child you don't care about.
    Node { kind: Option<ArcStr>, children: Vec<NodeShape> },
    /// A fused kernel matched against partial [`KernelMatcher`] criteria.
    Fused(KernelMatcher),
    /// Matches if *any* node in the subtree (this node or a
    /// descendant) matches the inner spec. Lets a test assert "this
    /// program contains a kernel shaped like X" without spelling out
    /// the path through wrapper nodes (module Do, binds, …).
    Contains(Box<NodeShape>),
}

impl NodeShape {
    /// A `Node` of the given `NodeView` kind, no child constraints
    /// (must still match child *count* — add children via [`child`]).
    ///
    /// [`child`]: NodeShape::child
    pub fn node(kind: &str) -> Self {
        NodeShape::Node { kind: Some(ArcStr::from(kind)), children: vec![] }
    }

    /// A `Node` of any kind.
    pub fn any_node() -> Self {
        NodeShape::Node { kind: None, children: vec![] }
    }

    /// A fused kernel matching `m`.
    pub fn fused(m: KernelMatcher) -> Self {
        NodeShape::Fused(m)
    }

    /// Matches if any node in the subtree matches `inner`. Use to find
    /// a kernel anywhere under the wrapper nodes a fixture compiles to.
    pub fn contains(inner: NodeShape) -> Self {
        NodeShape::Contains(Box::new(inner))
    }

    /// Convenience: `contains(fused(m))` — a kernel matching `m`
    /// somewhere in the program.
    pub fn contains_fused(m: KernelMatcher) -> Self {
        NodeShape::contains(NodeShape::fused(m))
    }

    /// Append a child shape (only meaningful on a `Node`).
    pub fn child(mut self, c: NodeShape) -> Self {
        if let NodeShape::Node { children, .. } = &mut self {
            children.push(c);
        }
        self
    }
}

/// Partial match criteria for a fused kernel. Every field is optional
/// (or additive); an unset field is a wildcard, so a spec asserts only
/// what it cares about — no brittle full-IR transcript.
#[derive(Debug, Clone, Default)]
pub struct KernelMatcher {
    /// Require this exact kernel return type.
    pub return_type: Option<Type>,
    /// Require exactly these scalar param names, in order.
    pub param_names: Option<Vec<ArcStr>>,
}

impl KernelMatcher {
    pub fn new() -> Self {
        Self::default()
    }

    /// Require the kernel's return type.
    pub fn returns(mut self, t: Type) -> Self {
        self.return_type = Some(t);
        self
    }

    /// Require exactly these scalar param names (in declaration order).
    pub fn params(mut self, names: &[&str]) -> Self {
        self.param_names = Some(names.iter().map(|s| ArcStr::from(*s)).collect());
        self
    }

    /// Check this matcher against a real kernel. `Ok` on match, `Err`
    /// with a human reason on the first failing criterion.
    fn check(&self, k: &KernelSig) -> Result<(), String> {
        if let Some(rt) = &self.return_type {
            if &k.return_type != rt {
                return Err(format!(
                    "return type: expected {rt:?}, got {:?}",
                    k.return_type
                ));
            }
        }
        if let Some(names) = &self.param_names {
            let actual: Vec<ArcStr> = k.params.iter().map(|p| p.name.clone()).collect();
            if actual != *names {
                return Err(format!("param names: expected {names:?}, got {actual:?}"));
            }
        }
        // There is deliberately NO body-op matcher (the old F4/#213
        // "EmitTags" idea — per-op tags recorded during emission —
        // was retired unbuilt). Threading a closed op-tag enum through
        // every `emit_clif` would resurrect the deleted GIR IR's
        // vocabulary tax to assert internal structure that the
        // differential value check, these signature facts, and the
        // `#[native]` attribute (an expr fully fused, zero node-walk
        // residue, usable at any source location) already cover.
        Ok(())
    }
}

// ─── Matching ─────────────────────────────────────────────────────

/// Check a compiled node against a [`NodeShape`] spec. `Ok(())` on
/// match; `Err(reason)` names the path and the first mismatch.
pub fn match_node<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    spec: &NodeShape,
) -> Result<(), String> {
    match_at(node, spec, "root")
}

fn match_at<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    spec: &NodeShape,
    path: &str,
) -> Result<(), String> {
    match spec {
        NodeShape::Any => Ok(()),
        NodeShape::Contains(inner) => {
            if find_match(node, inner) {
                Ok(())
            } else {
                Err(format!(
                    "at {path}: no node in the subtree matches the \
                     contained spec"
                ))
            }
        }
        NodeShape::Fused(gm) => match node.view() {
            NodeView::FusedKernel(fk) => {
                gm.check(fk.kernel()).map_err(|e| format!("at {path}: {e}"))
            }
            other => Err(format!(
                "at {path}: expected a Fused kernel, got a {} node",
                kind_name(&other)
            )),
        },
        NodeShape::Node { kind, children } => {
            let view = node.view();
            if let NodeView::FusedKernel(_) = view {
                return Err(format!(
                    "at {path}: expected a {} node, got a Fused kernel",
                    kind.as_deref().unwrap_or("(any)")
                ));
            }
            if let Some(k) = kind {
                let actual = kind_name(&view);
                if actual != *k {
                    return Err(format!("at {path}: expected kind {k}, got {actual}"));
                }
            }
            match node_children(&view) {
                None => Err(format!(
                    "at {path}: children of {} are not enumerated by \
                     node_shape yet (add an arm to node_children)",
                    kind_name(&view)
                )),
                Some(kids) => {
                    if kids.len() != children.len() {
                        return Err(format!(
                            "at {path}: {} has {} children, spec expects {}",
                            kind_name(&view),
                            kids.len(),
                            children.len()
                        ));
                    }
                    for (i, (child, cspec)) in kids.iter().zip(children).enumerate() {
                        match_at(child, cspec, &format!("{path}/{i}"))?;
                    }
                    Ok(())
                }
            }
        }
    }
}

/// True if `node` or any descendant matches `spec`. Used by
/// [`NodeShape::Contains`].
fn find_match<R: Rt, E: UserEvent>(node: &Node<R, E>, spec: &NodeShape) -> bool {
    if match_at(node, spec, "").is_ok() {
        return true;
    }
    match node_children(&node.view()) {
        Some(kids) => kids.iter().any(|c| find_match(c, spec)),
        None => false,
    }
}

// ─── Authoring aid: render the actual graph ───────────────────────

/// Render a compiled node as an indented text tree — for *writing* a
/// [`NodeShape`] spec (run it once, read the real shape). Not part of
/// the assertion path.
pub fn describe_node<R: Rt, E: UserEvent>(node: &Node<R, E>) -> String {
    let mut out = String::new();
    describe_at(node, 0, &mut out);
    out
}

fn describe_at<R: Rt, E: UserEvent>(node: &Node<R, E>, depth: usize, out: &mut String) {
    let pad = "  ".repeat(depth);
    match node.view() {
        NodeView::FusedKernel(fk) => {
            let k = fk.kernel();
            let params: Vec<&str> = k.params.iter().map(|p| p.name.as_str()).collect();
            out.push_str(&format!(
                "{pad}Fused(returns={:?}, params={:?})\n",
                k.return_type, params
            ));
            for feeder in fk.feeders() {
                describe_at(feeder, depth + 1, out);
            }
        }
        view => {
            out.push_str(&format!("{pad}{}\n", kind_name(&view)));
            match node_children(&view) {
                Some(kids) => {
                    for c in &kids {
                        describe_at(c, depth + 1, out);
                    }
                }
                None => out.push_str(&format!("{pad}  <children not enumerated>\n")),
            }
        }
    }
}

// ─── Graph traversal primitives ───────────────────────────────────

/// The child nodes of a non-fused view, in a deterministic order, or
/// `None` if this variant carries children that aren't enumerated here
/// (then a `Node` spec with children can't match it — the mismatch
/// reason says so). True leaves return `Some(empty)`.
fn node_children<'a, R: Rt, E: UserEvent>(
    view: &NodeView<'a, R, E>,
) -> Option<SmallVec<[&'a Node<R, E>; 4]>> {
    use NodeView as V;

    // Arithmetic / comparison / boolean ops all share `lhs`/`rhs`
    // (`Cached`); handle the whole family in one place.
    macro_rules! binop {
        ($n:expr) => {{
            let mut s: SmallVec<[&'a Node<R, E>; 4]> = SmallVec::new();
            s.push(&$n.lhs.node);
            s.push(&$n.rhs.node);
            return Some(s);
        }};
    }
    match view {
        V::Add(n) => binop!(n),
        V::Sub(n) => binop!(n),
        V::Mul(n) => binop!(n),
        V::Div(n) => binop!(n),
        V::Mod(n) => binop!(n),
        V::CheckedAdd(n) => binop!(n),
        V::CheckedSub(n) => binop!(n),
        V::CheckedMul(n) => binop!(n),
        V::CheckedDiv(n) => binop!(n),
        V::CheckedMod(n) => binop!(n),
        V::Eq(n) => binop!(n),
        V::Ne(n) => binop!(n),
        V::Lt(n) => binop!(n),
        V::Gt(n) => binop!(n),
        V::Lte(n) => binop!(n),
        V::Gte(n) => binop!(n),
        V::And(n) => binop!(n),
        V::Or(n) => binop!(n),
        _ => {}
    }

    let mut kids: SmallVec<[&'a Node<R, E>; 4]> = SmallVec::new();
    match view {
        // Containers / navigation.
        V::Block(b) => kids.extend(b.children.iter()),
        V::Bind(b) => kids.push(&b.node),
        V::MapQ(m) => {
            kids.push(&m.source.node);
            kids.push(&m.prototype);
        }
        V::FoldQ(m) => {
            kids.push(&m.source.node);
            kids.push(&m.init.node);
            kids.push(&m.prototype);
        }
        V::Module(m) => kids.push(m.source()),
        V::CallSite(cs) => {
            kids.push(cs.fnode());
            // Args live in a hashmap; sort by key (positional by index,
            // then named) for a stable child order.
            let mut entries: SmallVec<[(_, &'a Node<R, E>); 4]> = cs
                .args
                .iter()
                .filter_map(|(k, a)| a.node.as_ref().map(|n| (k, n)))
                .collect();
            entries.sort_by(|(a, _), (b, _)| a.cmp(b));
            kids.extend(entries.into_iter().map(|(_, n)| n));
        }
        V::Select(s) => {
            kids.push(&s.arg.node);
            kids.extend(s.arms.iter().map(|(_, c)| &c.node));
        }
        // Single/double-child wrappers.
        V::ExplicitParens(n) => kids.push(&n.n),
        V::TypeCast(n) => kids.push(&n.n),
        V::Qop(n) => kids.push(&n.n),
        V::OrNever(n) => kids.push(&n.n),
        V::Not(n) => kids.push(&n.n),
        V::Neg(n) => kids.push(&n.n),
        V::Connect(n) => kids.push(&n.node),
        V::ConnectDeref(n) => kids.push(&n.rhs.node),
        V::Sample(n) => {
            kids.push(&n.trigger);
            kids.push(&n.arg.node);
        }
        V::TryCatch(n) => {
            kids.extend(n.nodes.iter());
            kids.push(&n.handler);
        }
        V::ByRef(n) => kids.push(&n.child),
        V::Deref(n) => kids.push(&n.child),
        // Producers.
        V::Struct(n) => kids.extend(n.n.iter().map(|c| &c.node)),
        V::Tuple(n) => kids.extend(n.n.iter().map(|c| &c.node)),
        V::Variant(n) => kids.extend(n.n.iter().map(|c| &c.node)),
        V::Array(n) => kids.extend(n.n.iter().map(|c| &c.node)),
        V::Map(n) => {
            kids.extend(n.keys.iter().map(|c| &c.node));
            kids.extend(n.vals.iter().map(|c| &c.node));
        }
        V::StructWith(n) => {
            kids.push(&n.source);
            kids.extend(n.replace.iter().map(|r| &r.n.node));
        }
        V::StringInterpolate(n) => kids.extend(n.args.iter().map(|c| &c.node)),
        V::Any(n) => kids.extend(n.n.iter()),
        // Accessors.
        V::StructRef(n) => kids.push(&n.source),
        V::TupleRef(n) => kids.push(&n.source),
        V::ArrayRef(n) => {
            kids.push(&n.source.node);
            kids.push(&n.i.node);
        }
        V::ArraySlice(n) => {
            kids.push(&n.source.node);
            if let Some(s) = &n.start {
                kids.push(&s.node);
            }
            if let Some(e) = &n.end {
                kids.push(&e.node);
            }
        }
        V::MapRef(n) => {
            kids.push(&n.source.node);
            kids.push(&n.key.node);
        }
        // A fused kernel's children are its input feeders (lets
        // `Contains` descend through a kernel into what feeds it).
        V::FusedKernel(fk) => kids.extend(fk.feeders().iter()),
        // True leaves — no child nodes.
        V::Ref(_)
        | V::Constant(_)
        | V::Use(_)
        | V::TypeDef(_)
        | V::Nop(_)
        | V::Lambda(_) => {}
        // Binops returned above via the first match.
        V::Add(_)
        | V::Sub(_)
        | V::Mul(_)
        | V::Div(_)
        | V::Mod(_)
        | V::CheckedAdd(_)
        | V::CheckedSub(_)
        | V::CheckedMul(_)
        | V::CheckedDiv(_)
        | V::CheckedMod(_)
        | V::Eq(_)
        | V::Ne(_)
        | V::Lt(_)
        | V::Gt(_)
        | V::Lte(_)
        | V::Gte(_)
        | V::And(_)
        | V::Or(_) => unreachable!("handled above"),
    }
    Some(kids)
}

/// The `NodeView` variant name, used as the `Node { kind }` tag.
pub fn kind_name<R: Rt, E: UserEvent>(view: &NodeView<'_, R, E>) -> ArcStr {
    use arcstr::literal;
    match view {
        NodeView::FusedKernel(_) => literal!("FusedKernel"),
        NodeView::Bind(_) => literal!("Bind"),
        NodeView::Lambda(_) => literal!("Lambda"),
        NodeView::Block(b) if b.module => literal!("ModuleBlock"),
        NodeView::Block(_) => literal!("Block"),
        NodeView::Module(_) => literal!("Module"),
        NodeView::CallSite(_) => literal!("CallSite"),
        NodeView::Select(_) => literal!("Select"),
        NodeView::Ref(_) => literal!("Ref"),
        NodeView::ByRef(_) => literal!("ByRef"),
        NodeView::Deref(_) => literal!("Deref"),
        NodeView::Connect(_) => literal!("Connect"),
        NodeView::Sample(_) => literal!("Sample"),
        NodeView::StringInterpolate(_) => literal!("StringInterpolate"),
        NodeView::TypeCast(_) => literal!("TypeCast"),
        NodeView::Qop(_) => literal!("Qop"),
        NodeView::OrNever(_) => literal!("OrNever"),
        NodeView::TryCatch(_) => literal!("TryCatch"),
        _ => literal!("Other"),
    }
}
