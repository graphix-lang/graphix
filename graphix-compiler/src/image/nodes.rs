//! Node graphs under an image. Every node kind that can be imaged
//! writes a [`NodeTag`] and its compile-time data through
//! `Update::image_encode`, owned by the node's own file; the tag
//! dispatches to that kind's `image_decode`, which rebuilds the node
//! with pristine state and replays the runtime registrations its
//! compile performed. A kind without a codec fails the write with
//! [`NOT_IMAGED`]; nothing is skipped.

use crate::{ExecCtx, Node, Rt, UserEvent, image::ImageBuf, node};
use bytes::{Buf, BufMut};
use netidx_core::pack::{PackError, decode_varint, encode_varint, varint_len};

/// `PackError::Application` payload: a node kind with no image codec.
pub const NOT_IMAGED: u64 = 1;

macro_rules! node_tags {
    ($($tag:ident),+ $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[repr(u8)]
        pub(crate) enum NodeTag { $($tag),+ }

        impl NodeTag {
            fn from_u8(b: u8) -> Option<Self> {
                const ALL: &[NodeTag] = &[$(NodeTag::$tag),+];
                ALL.get(b as usize).copied()
            }
        }
    };
}

node_tags! {
    Bind, Lambda, Block, Module, Constant, TypeDef, Impl, Trait, Nop, Never,
    Struct, Ref, Add, Sub, Mul, Div, Mod, CheckedAdd, CheckedSub, CheckedMul,
    CheckedDiv, CheckedMod, Eq, Ne, Lt, Gt, Lte, Gte, And, Or, Not, Neg, Array,
    ListLit, Tuple, Variant, ExplicitParens, StringInterpolate, Connect,
    ConnectDeref, TypeCast, Any, Sample, ArrayRef, ArraySlice, StructWith,
    StructRef, Construct, TupleRef, ByRef, Deref, Map, MapRef, Catch, Qop,
    SeqGuard, OrNever, CallSite, Select, Collection, Fused,
}

pub(crate) fn tag_len() -> usize {
    1
}

pub(crate) fn put_tag(tag: NodeTag, buf: &mut ImageBuf) {
    buf.put_u8(tag as u8)
}

pub(crate) fn nodes_len<R: Rt, E: UserEvent>(nodes: &[Node<R, E>]) -> usize {
    varint_len(nodes.len() as u64) + nodes.iter().map(|n| n.image_len()).sum::<usize>()
}

pub(crate) fn encode_nodes<R: Rt, E: UserEvent>(
    nodes: &[Node<R, E>],
    buf: &mut ImageBuf,
) -> Result<(), PackError> {
    encode_varint(nodes.len() as u64, buf);
    for n in nodes {
        n.image_encode(buf)?;
    }
    Ok(())
}

pub(crate) fn opt_node_len<R: Rt, E: UserEvent>(node: Option<&Node<R, E>>) -> usize {
    1 + node.map_or(0, |n| n.image_len())
}

pub(crate) fn opt_node_encode<R: Rt, E: UserEvent>(
    node: Option<&Node<R, E>>,
    buf: &mut ImageBuf,
) -> Result<(), PackError> {
    match node {
        Some(n) => {
            buf.put_u8(1);
            n.image_encode(buf)
        }
        None => {
            buf.put_u8(0);
            Ok(())
        }
    }
}

pub(crate) fn opt_node_decode<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    buf: &mut &[u8],
) -> Result<Option<Node<R, E>>, PackError> {
    if !buf.has_remaining() {
        return Err(PackError::BufferShort);
    }
    match buf.get_u8() {
        0 => Ok(None),
        1 => Ok(Some(decode_node(ctx, buf)?)),
        _ => Err(PackError::UnknownTag),
    }
}

pub(crate) fn decode_nodes<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    buf: &mut &[u8],
) -> Result<Vec<Node<R, E>>, PackError> {
    let n = decode_varint(buf)? as usize;
    let mut out = Vec::with_capacity(n);
    for _ in 0..n {
        out.push(decode_node(ctx, buf)?);
    }
    Ok(out)
}

pub(crate) fn decode_node<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    buf: &mut &[u8],
) -> Result<Node<R, E>, PackError> {
    use node::{
        array::{Array, ArrayRef, ArraySlice, ListLit},
        bind::{Bind, ByRef, Deref, Ref},
        callsite::CallSite,
        data::{Construct, Struct, StructRef, StructWith, Tuple, TupleRef, Variant},
        error::{Catch, OrNever, Qop, SeqGuard},
        lambda::Lambda,
        map::{Map, MapRef},
        module::Module,
        op::{
            Add, And, CheckedAdd, CheckedDiv, CheckedMod, CheckedMul, CheckedSub, Div,
            Eq, Gt, Gte, Lt, Lte, Mod, Mul, Ne, Neg, Not, Or, Sub,
        },
        select::Select,
        traits::{Impl, Trait},
        {
            Any, Block, Connect, ConnectDeref, Constant, ExplicitParens, Never, Nop,
            Sample, StringInterpolate, TypeCast, TypeDef,
        },
    };
    if !buf.has_remaining() {
        return Err(PackError::BufferShort);
    }
    let tag = NodeTag::from_u8(buf.get_u8()).ok_or(PackError::UnknownTag)?;
    match tag {
        NodeTag::Bind => Bind::image_decode(ctx, buf),
        NodeTag::Lambda => Lambda::image_decode(ctx, buf),
        NodeTag::Block => Block::image_decode(ctx, buf),
        NodeTag::Module => Module::image_decode(ctx, buf),
        NodeTag::Constant => Constant::image_decode(ctx, buf),
        NodeTag::TypeDef => TypeDef::image_decode(ctx, buf),
        NodeTag::Impl => Impl::image_decode(ctx, buf),
        NodeTag::Trait => Trait::image_decode(ctx, buf),
        NodeTag::Nop => Nop::image_decode(ctx, buf),
        NodeTag::Never => Never::image_decode(ctx, buf),
        NodeTag::Struct => Struct::image_decode(ctx, buf),
        NodeTag::Ref => Ref::image_decode(ctx, buf),
        NodeTag::Add => Add::image_decode(ctx, buf),
        NodeTag::Sub => Sub::image_decode(ctx, buf),
        NodeTag::Mul => Mul::image_decode(ctx, buf),
        NodeTag::Div => Div::image_decode(ctx, buf),
        NodeTag::Mod => Mod::image_decode(ctx, buf),
        NodeTag::CheckedAdd => CheckedAdd::image_decode(ctx, buf),
        NodeTag::CheckedSub => CheckedSub::image_decode(ctx, buf),
        NodeTag::CheckedMul => CheckedMul::image_decode(ctx, buf),
        NodeTag::CheckedDiv => CheckedDiv::image_decode(ctx, buf),
        NodeTag::CheckedMod => CheckedMod::image_decode(ctx, buf),
        NodeTag::Eq => Eq::image_decode(ctx, buf),
        NodeTag::Ne => Ne::image_decode(ctx, buf),
        NodeTag::Lt => Lt::image_decode(ctx, buf),
        NodeTag::Gt => Gt::image_decode(ctx, buf),
        NodeTag::Lte => Lte::image_decode(ctx, buf),
        NodeTag::Gte => Gte::image_decode(ctx, buf),
        NodeTag::And => And::image_decode(ctx, buf),
        NodeTag::Or => Or::image_decode(ctx, buf),
        NodeTag::Not => Not::image_decode(ctx, buf),
        NodeTag::Neg => Neg::image_decode(ctx, buf),
        NodeTag::Array => Array::image_decode(ctx, buf),
        NodeTag::ListLit => ListLit::image_decode(ctx, buf),
        NodeTag::Tuple => Tuple::image_decode(ctx, buf),
        NodeTag::Variant => Variant::image_decode(ctx, buf),
        NodeTag::ExplicitParens => ExplicitParens::image_decode(ctx, buf),
        NodeTag::StringInterpolate => StringInterpolate::image_decode(ctx, buf),
        NodeTag::Connect => Connect::image_decode(ctx, buf),
        NodeTag::ConnectDeref => ConnectDeref::image_decode(ctx, buf),
        NodeTag::TypeCast => TypeCast::image_decode(ctx, buf),
        NodeTag::Any => Any::image_decode(ctx, buf),
        NodeTag::Sample => Sample::image_decode(ctx, buf),
        NodeTag::ArrayRef => ArrayRef::image_decode(ctx, buf),
        NodeTag::ArraySlice => ArraySlice::image_decode(ctx, buf),
        NodeTag::StructWith => StructWith::image_decode(ctx, buf),
        NodeTag::StructRef => StructRef::image_decode(ctx, buf),
        NodeTag::Construct => Construct::image_decode(ctx, buf),
        NodeTag::TupleRef => TupleRef::image_decode(ctx, buf),
        NodeTag::ByRef => ByRef::image_decode(ctx, buf),
        NodeTag::Deref => Deref::image_decode(ctx, buf),
        NodeTag::Map => Map::image_decode(ctx, buf),
        NodeTag::MapRef => MapRef::image_decode(ctx, buf),
        NodeTag::Catch => Catch::image_decode(ctx, buf),
        NodeTag::Qop => Qop::image_decode(ctx, buf),
        NodeTag::SeqGuard => SeqGuard::image_decode(ctx, buf),
        NodeTag::OrNever => OrNever::image_decode(ctx, buf),
        NodeTag::CallSite => CallSite::image_decode(ctx, buf),
        NodeTag::Select => Select::image_decode(ctx, buf),
        NodeTag::Collection => {
            node::collection::CollectionIntrinsic::image_decode(ctx, buf)
        }
        NodeTag::Fused => crate::fusion::FusedKernel::image_decode(ctx, buf),
    }
}
