//! Why an expression has no value, as far as its nodes can tell
//! (`design/debugger.md`, §6): a tree over the parts the nothing can come
//! through. A node that has a value says nothing; one a part of which has
//! none passes that part's account up; a select, a seq, a call, a sample
//! and a `$` add what they know of themselves. The leaves are the
//! bindings read, which a debugger looks up and follows in turn.

use crate::{BindId, Node, NodeView, Rt, TagValue, stack, typ::Type};
use graphix_types::expr::Expr;
use netidx_value::Value;
use nohash::{IntMap, IntSet};
use triomphe::Arc;

/// The calls one account follows into before it stops.
const CALLS: usize = 32;

#[derive(Debug, Clone)]
pub enum Because {
    /// It reads this binding.
    Reads(BindId),
    /// What the node of `spec` says of itself, over the parts it waits
    /// on in turn.
    Of { spec: Arc<Expr>, note: Note, parts: Vec<Because> },
}

#[derive(Debug, Clone)]
pub enum Note {
    /// A select: the last value its argument had, and the arm it holds.
    Select {
        arg: Option<(Value, Type)>,
        arm: Option<usize>,
        arms: usize,
    },
    /// A seq: the step it is at, or none where it is not running.
    Seq {
        step: Option<usize>,
        steps: usize,
    },
    /// A sample: `held` where what it samples has had a value, so it is
    /// the trigger it waits on.
    Sample {
        held: bool,
    },
    /// A `$` (or a `?` where `raised`) whose operand is this error, or
    /// this null.
    Dropped {
        value: Value,
        raised: bool,
    },
    Call {
        called: Reached,
        fresh: bool,
        awaits: bool,
    },
    /// Computed in fused code, which reads `parts`.
    Fused,
    /// `never()`.
    Never,
}

/// What a call has reached.
#[derive(Debug, Clone)]
pub enum Reached {
    /// Nothing yet: the call has not been made.
    Nothing,
    /// A function, whose body `parts` follows into.
    Function,
    BuiltIn,
}

/// Whether a node's last production is a value.
pub(crate) fn valued(tv: &TagValue) -> bool {
    !tv.tag().is_bottom() && !tv.is_sentinel()
}

/// One account in the making.
pub struct Explain<'a, R: Rt> {
    /// The lets a lowering wrote inside the expression, by what they
    /// bind: a read of one is accounted for by its value.
    lowered: IntMap<BindId, &'a Node<R>>,
    /// The lowered lets accounted for: each says its piece once.
    told: IntSet<BindId>,
    /// The parameters of the calls the walk is inside: each call accounts
    /// for its arguments.
    params: IntSet<BindId>,
    calls: usize,
}

impl<'a, R: Rt> Explain<'a, R> {
    /// Why `node` has no value.
    pub fn of(node: &'a Node<R>) -> Vec<Because> {
        let mut cx = Self {
            lowered: IntMap::default(),
            told: IntSet::default(),
            params: IntSet::default(),
            calls: 0,
        };
        let mut out = Vec::new();
        cx.explain(node, &mut out);
        out
    }

    /// Note the lets a lowering wrote among `nodes`.
    pub(crate) fn lets(&mut self, nodes: impl Iterator<Item = &'a Node<R>>) {
        for n in nodes {
            if let NodeView::Bind(b) = n.view()
                && b.spec.is_lowered()
            {
                b.pattern.ids(&mut |id| {
                    self.lowered.insert(id, &b.node);
                })
            }
        }
    }

    pub(crate) fn param(&mut self, id: BindId) {
        self.params.insert(id);
    }

    /// Enter a call's body; false where the account has followed enough.
    pub(crate) fn enter_call(&mut self) -> bool {
        self.calls += 1;
        self.calls <= CALLS
    }

    fn read(&mut self, id: BindId, out: &mut Vec<Because>) {
        if self.params.contains(&id) {
            return;
        }
        match self.lowered.get(&id).copied() {
            None => out.push(Because::Reads(id)),
            Some(n) => {
                if self.told.insert(id) {
                    self.explain(n, out)
                }
            }
        }
    }

    pub(crate) fn explain(&mut self, n: &'a Node<R>, out: &mut Vec<Because>) {
        stack::ensure_sufficient(|| match n.view() {
            NodeView::Ref(r) => self.read(r.id, out),
            NodeView::Bind(b) => self.explain(&b.node, out),
            NodeView::Block(b) => b.explain(self, out),
            NodeView::Select(s) => s.explain(self, out),
            NodeView::SeqMachine(m) => m.explain(self, [].iter(), out),
            NodeView::Sample(s) => s.explain(self, out),
            NodeView::Qop(q) => q.explain(self, out),
            NodeView::CallSite(c) => c.explain(self, out),
            NodeView::FusedKernel(k) => k.explain(self, out),
            NodeView::Never(v) => {
                let spec = v.spec.clone();
                out.push(Because::Of { spec, note: Note::Never, parts: Vec::new() })
            }
            // always a value, or a statement that has none
            NodeView::Lambda(_)
            | NodeView::Constant(_)
            | NodeView::Nop(_)
            | NodeView::TypeDef(_)
            | NodeView::Impl(_)
            | NodeView::Module(_)
            | NodeView::Connect(_)
            | NodeView::ConnectDeref(_) => (),
            // a value when its parts are
            NodeView::MapQ(_)
            | NodeView::FoldQ(_)
            | NodeView::Catch(_)
            | NodeView::SeqGuard(_)
            | NodeView::SeqAbort(_)
            | NodeView::SeqCapture(_)
            | NodeView::ExplicitParens(_)
            | NodeView::ForkControl(_)
            | NodeView::TypeCast(_)
            | NodeView::StringInterpolate(_)
            | NodeView::Any(_)
            | NodeView::Struct(_)
            | NodeView::StructWith(_)
            | NodeView::Tuple(_)
            | NodeView::Variant(_)
            | NodeView::Construct(_)
            | NodeView::Array(_)
            | NodeView::ListLit(_)
            | NodeView::Map(_)
            | NodeView::StructRef(_)
            | NodeView::TupleRef(_)
            | NodeView::ArrayRef(_)
            | NodeView::ArraySlice(_)
            | NodeView::MapRef(_)
            | NodeView::ByRef(_)
            | NodeView::Deref(_)
            | NodeView::Add(_)
            | NodeView::Sub(_)
            | NodeView::Mul(_)
            | NodeView::Div(_)
            | NodeView::Mod(_)
            | NodeView::CheckedAdd(_)
            | NodeView::CheckedSub(_)
            | NodeView::CheckedMul(_)
            | NodeView::CheckedDiv(_)
            | NodeView::CheckedMod(_)
            | NodeView::Eq(_)
            | NodeView::Ne(_)
            | NodeView::Lt(_)
            | NodeView::Gt(_)
            | NodeView::Lte(_)
            | NodeView::Gte(_)
            | NodeView::And(_)
            | NodeView::Or(_)
            | NodeView::Not(_)
            | NodeView::Neg(_) => n.for_each_child(&mut |c| self.explain(c, out)),
        })
    }
}

/// The bindings `parts` read, each once, in order.
pub fn reads(parts: &[Because], out: &mut Vec<BindId>) {
    for p in parts {
        match p {
            Because::Reads(id) if !out.contains(id) => out.push(*id),
            Because::Reads(_) => (),
            Because::Of { parts, .. } => reads(parts, out),
        }
    }
}
