//! Declarative graph-shape assertions for tests. A [`NodeShape`]
//! specifies a compiled (sub)graph — a node kind with child shapes, a
//! fused kernel matched by [`KernelMatcher`], or a wildcard — and
//! [`match_node`] checks a live post-fusion `Node` against it, naming
//! the first mismatch. Driven by `GXHandle::match_shape`;
//! `GXHandle::describe_shape` renders the actual graph as an authoring aid.

use crate::{
    Node, NodeView, Rt, UserEvent, fusion::kernel_abi::KernelSig, stack, typ::Type,
};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use smallvec::SmallVec;
use std::fmt::{self, Write};

/// A declarative specification of a (sub)graph's shape.
#[derive(Debug, Clone)]
pub enum NodeShape {
    /// Matches any single node/subtree. Don't-care.
    Any,
    /// A non-fused node. `kind` (a [`kind_name`]) must match when
    /// `Some`; `None` matches any kind. `children` match the node's
    /// children positionally — use [`NodeShape::Any`] to skip a child
    /// you don't care about.
    Node { kind: Option<ArcStr>, children: Vec<NodeShape> },
    /// A fused kernel matched against partial [`KernelMatcher`] criteria.
    Fused(KernelMatcher),
    /// Matches if any node in the subtree matches the inner spec.
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

    /// Matches if any node in the subtree matches `inner`.
    pub fn contains(inner: NodeShape) -> Self {
        NodeShape::Contains(Box::new(inner))
    }

    /// `contains(fused(m))`.
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

/// Partial match criteria for a fused kernel; an unset field is a
/// wildcard.
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

    /// The first criterion `k` fails, if any.
    fn mismatch<'a>(&'a self, k: &'a KernelSig) -> Option<Why<'a>> {
        if let Some(rt) = &self.return_type
            && &k.return_type != rt
        {
            return Some(Why::ReturnType { expected: rt, got: &k.return_type });
        }
        if let Some(names) = &self.param_names
            && !k.params.iter().map(|p| &p.name).eq(names.iter())
        {
            return Some(Why::ParamNames { expected: names, got: k });
        }
        None
    }
}

/// Why a node does not match a spec.
enum Why<'a> {
    NotContained,
    ExpectedFused { got: &'static str },
    ExpectedNode { kind: Option<&'a str> },
    Kind { expected: &'a str, got: &'static str },
    Children { kind: &'static str, got: usize, expected: usize },
    ReturnType { expected: &'a Type, got: &'a Type },
    ParamNames { expected: &'a [ArcStr], got: &'a KernelSig },
}

impl fmt::Display for Why<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Why::NotContained => {
                write!(f, "no node in the subtree matches the contained spec")
            }
            Why::ExpectedFused { got } => {
                write!(f, "expected a Fused kernel, got a {got} node")
            }
            Why::ExpectedNode { kind } => {
                write!(
                    f,
                    "expected a {} node, got a Fused kernel",
                    kind.unwrap_or("(any)")
                )
            }
            Why::Kind { expected, got } => {
                write!(f, "expected kind {expected}, got {got}")
            }
            Why::Children { kind, got, expected } => {
                write!(f, "{kind} has {got} children, spec expects {expected}")
            }
            Why::ReturnType { expected, got } => {
                write!(f, "return type: expected {expected:?}, got {got:?}")
            }
            Why::ParamNames { expected, got } => {
                write!(f, "param names: expected {expected:?}, got [")?;
                for (i, p) in got.params.iter().enumerate() {
                    let sep = if i == 0 { "" } else { ", " };
                    write!(f, "{sep}{:?}", p.name)?;
                }
                write!(f, "]")
            }
        }
    }
}

/// A mismatch and where it is: child indices from the matched root.
struct Mismatch<'a> {
    at: SmallVec<[usize; 8]>,
    why: Why<'a>,
}

impl<'a> Mismatch<'a> {
    fn here(why: Why<'a>) -> Self {
        Self { at: SmallVec::new(), why }
    }
}

/// Check a compiled node against a [`NodeShape`] spec, naming the path
/// and the first mismatch.
pub fn match_node<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    spec: &NodeShape,
) -> Result<()> {
    match_at(node, spec).map_err(|m| {
        let mut path = compact_str::CompactString::const_new("root");
        for i in m.at.iter().rev() {
            let _ = write!(path, "/{i}");
        }
        anyhow!("at {path}: {}", m.why)
    })
}

fn match_at<'a, R: Rt, E: UserEvent>(
    node: &'a Node<R, E>,
    spec: &'a NodeShape,
) -> std::result::Result<(), Mismatch<'a>> {
    stack::ensure_sufficient(|| match spec {
        NodeShape::Any => Ok(()),
        NodeShape::Contains(inner) => match find_match(node, inner) {
            true => Ok(()),
            false => Err(Mismatch::here(Why::NotContained)),
        },
        NodeShape::Fused(gm) => match node.view() {
            NodeView::FusedKernel(fk) => match gm.mismatch(fk.kernel()) {
                None => Ok(()),
                Some(why) => Err(Mismatch::here(why)),
            },
            other => Err(Mismatch::here(Why::ExpectedFused { got: kind_name(&other) })),
        },
        NodeShape::Node { kind, children } => {
            let view = node.view();
            if let NodeView::FusedKernel(_) = view {
                return Err(Mismatch::here(Why::ExpectedNode { kind: kind.as_deref() }));
            }
            let got = kind_name(&view);
            if let Some(expected) = kind
                && got != expected.as_str()
            {
                return Err(Mismatch::here(Why::Kind { expected, got }));
            }
            let kids = node_children(&view);
            if kids.len() != children.len() {
                return Err(Mismatch::here(Why::Children {
                    kind: got,
                    got: kids.len(),
                    expected: children.len(),
                }));
            }
            for (i, (child, cspec)) in kids.iter().zip(children).enumerate() {
                match_at(child, cspec).map_err(|mut m| {
                    m.at.push(i);
                    m
                })?;
            }
            Ok(())
        }
    })
}

/// True if `node` or any descendant matches `spec`. Used by
/// [`NodeShape::Contains`].
fn find_match<R: Rt, E: UserEvent>(node: &Node<R, E>, spec: &NodeShape) -> bool {
    stack::ensure_sufficient(|| {
        match_at(node, spec).is_ok()
            || node_children(&node.view()).iter().any(|c| find_match(c, spec))
    })
}

/// Render a compiled node as an indented text tree, for writing a
/// [`NodeShape`] spec.
pub fn describe_node<R: Rt, E: UserEvent>(node: &Node<R, E>) -> String {
    let mut out = String::new();
    describe_at(node, 0, &mut out);
    out
}

fn describe_at<R: Rt, E: UserEvent>(node: &Node<R, E>, depth: usize, out: &mut String) {
    stack::ensure_sufficient(|| {
        let view = node.view();
        let _ = write!(out, "{:w$}{}", "", kind_name(&view), w = depth * 2);
        if let NodeView::FusedKernel(fk) = &view {
            let k = fk.kernel();
            let _ = write!(out, "(returns={:?}, params=[", k.return_type);
            for (i, p) in k.params.iter().enumerate() {
                let sep = if i == 0 { "" } else { ", " };
                let _ = write!(out, "{sep}{:?}", p.name.as_str());
            }
            out.push_str("])");
        }
        out.push('\n');
        for c in node_children(&view) {
            describe_at(c, depth + 1, out);
        }
    })
}

// XCR claude for eric: agreed, one direct-children walk should serve both: split
// `fusion::for_each_node`'s match into a `for_each_child` it recurses through.
// Not this round: fusion-b is changing that match (Module source, ByRef
// recursion) and the split must land on the result, fusecheck-verified.
/// The child nodes of a view in a deterministic order; a kernel's are
/// its input feeders.
fn node_children<'a, R: Rt, E: UserEvent>(
    view: &NodeView<'a, R, E>,
) -> SmallVec<[&'a Node<R, E>; 4]> {
    use NodeView as V;
    let mut kids: SmallVec<[&'a Node<R, E>; 4]> = SmallVec::new();
    match view {
        V::Add(n) => kids.extend([&n.lhs, &n.rhs]),
        V::Sub(n) => kids.extend([&n.lhs, &n.rhs]),
        V::Mul(n) => kids.extend([&n.lhs, &n.rhs]),
        V::Div(n) => kids.extend([&n.lhs, &n.rhs]),
        V::Mod(n) => kids.extend([&n.lhs, &n.rhs]),
        V::CheckedAdd(n) => kids.extend([&n.lhs, &n.rhs]),
        V::CheckedSub(n) => kids.extend([&n.lhs, &n.rhs]),
        V::CheckedMul(n) => kids.extend([&n.lhs, &n.rhs]),
        V::CheckedDiv(n) => kids.extend([&n.lhs, &n.rhs]),
        V::CheckedMod(n) => kids.extend([&n.lhs, &n.rhs]),
        V::Eq(n) => kids.extend([&n.lhs, &n.rhs]),
        V::Ne(n) => kids.extend([&n.lhs, &n.rhs]),
        V::Lt(n) => kids.extend([&n.lhs, &n.rhs]),
        V::Gt(n) => kids.extend([&n.lhs, &n.rhs]),
        V::Lte(n) => kids.extend([&n.lhs, &n.rhs]),
        V::Gte(n) => kids.extend([&n.lhs, &n.rhs]),
        V::And(n) => kids.extend([&n.lhs, &n.rhs]),
        V::Or(n) => kids.extend([&n.lhs, &n.rhs]),
        V::Block(b) => kids.extend(b.children.iter()),
        V::Bind(b) => kids.push(&b.node),
        V::MapQ(m) => kids.extend([&m.source, &m.prototype]),
        V::FoldQ(m) => kids.extend([&m.source, &m.init, &m.prototype]),
        V::Module(m) => kids.extend(m.source()),
        V::CallSite(cs) => {
            kids.push(cs.fnode());
            // Sorted by key for a stable child order.
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
            kids.extend(s.arms.iter().map(|(_, c)| c));
        }
        V::ExplicitParens(n) => kids.push(&n.n),
        V::TypeCast(n) => kids.push(&n.n),
        V::Qop(n) => kids.push(&n.n),
        V::SeqGuard(n) => kids.push(&n.n),
        V::SeqAbort(n) => kids.push(&n.n),
        V::OrNever(n) => kids.push(&n.n),
        V::Not(n) => kids.push(&n.n),
        V::Neg(n) => kids.push(&n.n),
        V::Connect(n) => kids.push(&n.node),
        V::ConnectDeref(n) => kids.push(&n.rhs),
        V::Sample(n) => kids.extend([&n.trigger, &n.arg.node]),
        V::Catch(n) => {
            kids.push(&n.handler);
            if let Some(abort) = &n.seq_abort {
                kids.push(&abort.node);
                kids.extend(abort.manual.iter());
            }
        }
        V::ByRef(n) => n.for_each_child(&mut |c| kids.push(c)),
        V::Deref(n) => kids.push(&n.child),
        V::Struct(n) => kids.extend(n.n.iter()),
        V::Tuple(n) => kids.extend(n.n.iter()),
        V::Variant(n) => kids.extend(n.n.iter()),
        V::Construct(n) => kids.push(&n.arg),
        V::Array(n) => kids.extend(n.n.iter()),
        V::ListLit(n) => kids.extend(n.n.iter()),
        V::Map(n) => {
            kids.extend(n.entries.iter().map(|(k, _)| k));
            kids.extend(n.entries.iter().map(|(_, v)| v));
        }
        V::StructWith(n) => {
            kids.push(&n.source);
            kids.extend(n.replace.iter().map(|r| &r.n));
        }
        V::StringInterpolate(n) => kids.extend(n.args.iter()),
        V::Any(n) => kids.extend(n.n.iter()),
        V::Never(n) => kids.extend(n.n.iter()),
        V::StructRef(n) => kids.push(&n.source),
        V::TupleRef(n) => kids.push(&n.source),
        V::ArrayRef(n) => kids.extend([&n.source, &n.i]),
        V::ArraySlice(n) => {
            kids.push(&n.source);
            kids.extend(n.start.iter());
            kids.extend(n.end.iter());
        }
        V::MapRef(n) => kids.extend([&n.source, &n.key]),
        V::FusedKernel(fk) => kids.extend(fk.feeders().iter()),
        V::Impl(i) => kids.push(&i.body),
        V::Ref(_) | V::Constant(_) | V::TypeDef(_) | V::Nop(_) | V::Lambda(_) => {}
    }
    kids
}

/// The name a [`NodeShape::Node`] kind matches: the `NodeView` variant
/// name, `ModuleBlock` for a module's block.
pub fn kind_name<R: Rt, E: UserEvent>(view: &NodeView<'_, R, E>) -> &'static str {
    use NodeView as V;
    match view {
        V::Bind(_) => "Bind",
        V::Lambda(_) => "Lambda",
        V::Block(b) if b.module => "ModuleBlock",
        V::Block(_) => "Block",
        V::Module(_) => "Module",
        V::CallSite(_) => "CallSite",
        V::MapQ(_) => "MapQ",
        V::FoldQ(_) => "FoldQ",
        V::Select(_) => "Select",
        V::Catch(_) => "Catch",
        V::SeqGuard(_) => "SeqGuard",
        V::SeqAbort(_) => "SeqAbort",
        V::Qop(_) => "Qop",
        V::OrNever(_) => "OrNever",
        V::ExplicitParens(_) => "ExplicitParens",
        V::TypeCast(_) => "TypeCast",
        V::Connect(_) => "Connect",
        V::ConnectDeref(_) => "ConnectDeref",
        V::StringInterpolate(_) => "StringInterpolate",
        V::Any(_) => "Any",
        V::Never(_) => "Never",
        V::Sample(_) => "Sample",
        V::Struct(_) => "Struct",
        V::StructWith(_) => "StructWith",
        V::Tuple(_) => "Tuple",
        V::Variant(_) => "Variant",
        V::Construct(_) => "Construct",
        V::Array(_) => "Array",
        V::ListLit(_) => "ListLit",
        V::Map(_) => "Map",
        V::StructRef(_) => "StructRef",
        V::TupleRef(_) => "TupleRef",
        V::ArrayRef(_) => "ArrayRef",
        V::ArraySlice(_) => "ArraySlice",
        V::MapRef(_) => "MapRef",
        V::Ref(_) => "Ref",
        V::ByRef(_) => "ByRef",
        V::Deref(_) => "Deref",
        V::Add(_) => "Add",
        V::Sub(_) => "Sub",
        V::Mul(_) => "Mul",
        V::Div(_) => "Div",
        V::Mod(_) => "Mod",
        V::CheckedAdd(_) => "CheckedAdd",
        V::CheckedSub(_) => "CheckedSub",
        V::CheckedMul(_) => "CheckedMul",
        V::CheckedDiv(_) => "CheckedDiv",
        V::CheckedMod(_) => "CheckedMod",
        V::Eq(_) => "Eq",
        V::Ne(_) => "Ne",
        V::Lt(_) => "Lt",
        V::Gt(_) => "Gt",
        V::Lte(_) => "Lte",
        V::Gte(_) => "Gte",
        V::And(_) => "And",
        V::Or(_) => "Or",
        V::Not(_) => "Not",
        V::Neg(_) => "Neg",
        V::Constant(_) => "Constant",
        V::TypeDef(_) => "TypeDef",
        V::Impl(_) => "Impl",
        V::Nop(_) => "Nop",
        V::FusedKernel(_) => "FusedKernel",
    }
}
