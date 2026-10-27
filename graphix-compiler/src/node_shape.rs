//! Declarative graph-shape assertions for tests. A [`NodeShape`]
//! specifies a compiled (sub)graph — a node kind, a fused kernel
//! matched by [`KernelMatcher`], or a subtree holding either — and
//! [`match_node`] checks a live post-fusion `Node` against it, naming
//! the first mismatch. Driven by `GXHandle::match_shape`;
//! `GXHandle::describe_shape` renders the actual graph as an authoring aid.

use crate::{Node, NodeView, Rt, fusion::kernel_abi::KernelSig, stack, typ::Type};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use smallvec::SmallVec;
use std::fmt::{self, Write};

/// A declarative specification of a (sub)graph's shape.
#[derive(Debug, Clone)]
pub enum NodeShape {
    /// A non-fused node of this kind (a [`kind_name`]).
    Node(ArcStr),
    /// A fused kernel matched against partial [`KernelMatcher`] criteria.
    Fused(KernelMatcher),
    /// Matches if any node in the subtree matches the inner spec.
    Contains(Box<NodeShape>),
}

impl NodeShape {
    /// A non-fused node of the given `NodeView` kind.
    pub fn node(kind: &str) -> Self {
        NodeShape::Node(ArcStr::from(kind))
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
    ExpectedNode { kind: &'a str },
    Kind { expected: &'a str, got: &'static str },
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
                write!(f, "expected a {kind} node, got a Fused kernel")
            }
            Why::Kind { expected, got } => {
                write!(f, "expected kind {expected}, got {got}")
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

/// Check a compiled node against a [`NodeShape`] spec, naming the first
/// mismatch.
pub fn match_node<R: Rt>(node: &Node<R>, spec: &NodeShape) -> Result<()> {
    match_at(node, spec).map_err(|why| anyhow!("{why}"))
}

fn match_at<'a, R: Rt>(
    node: &'a Node<R>,
    spec: &'a NodeShape,
) -> std::result::Result<(), Why<'a>> {
    match spec {
        NodeShape::Contains(inner) => match find_match(node, inner) {
            true => Ok(()),
            false => Err(Why::NotContained),
        },
        NodeShape::Fused(gm) => match node.view() {
            NodeView::FusedKernel(fk) => match gm.mismatch(fk.kernel()) {
                None => Ok(()),
                Some(why) => Err(why),
            },
            other => Err(Why::ExpectedFused { got: kind_name(&other) }),
        },
        NodeShape::Node(kind) => match node.view() {
            NodeView::FusedKernel(_) => Err(Why::ExpectedNode { kind }),
            view => match kind_name(&view) {
                got if got == kind.as_str() => Ok(()),
                got => Err(Why::Kind { expected: kind, got }),
            },
        },
    }
}

/// True if `node` or any descendant matches `spec`. Used by
/// [`NodeShape::Contains`].
fn find_match<R: Rt>(node: &Node<R>, spec: &NodeShape) -> bool {
    stack::ensure_sufficient(|| {
        match_at(node, spec).is_ok()
            || node_children(node).iter().any(|c| find_match(c, spec))
    })
}

/// Render a compiled node as an indented text tree, for writing a
/// [`NodeShape`] spec.
pub fn describe_node<R: Rt>(node: &Node<R>) -> String {
    let mut out = String::new();
    describe_at(node, 0, &mut out);
    out
}

fn describe_at<R: Rt>(node: &Node<R>, depth: usize, out: &mut String) {
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
        for c in node_children(node) {
            describe_at(c, depth + 1, out);
        }
    })
}

/// The child nodes of a view in a deterministic order; a kernel's are
/// its input feeders.
fn node_children<'a, R: Rt>(node: &'a Node<R>) -> SmallVec<[&'a Node<R>; 4]> {
    let mut kids: SmallVec<[&'a Node<R>; 4]> = SmallVec::new();
    node.for_each_child(&mut |c| kids.push(c));
    kids
}

/// The name a [`NodeShape::Node`] kind matches: the `NodeView` variant
/// name, `ModuleBlock` for a module's block.
pub fn kind_name<R: Rt>(view: &NodeView<'_, R>) -> &'static str {
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
        V::SeqMachine(_) => "SeqMachine",
        V::SeqCapture(_) => "SeqCapture",
        V::Qop(q) if q.raises() => "Qop",
        V::Qop(_) => "OrNever",
        V::ExplicitParens(_) => "ExplicitParens",
        V::ForkControl(_) => "ForkControl",
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
