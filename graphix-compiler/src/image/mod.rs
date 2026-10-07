//! The compiler's part of an image: the handlers a `?` sees, kernel
//! signatures, body records, definitions' check tables and the instance
//! heap, kept in the session core ([`graphix_types::image`]) as its
//! extension.

mod defs;
pub mod nodes;
mod registration;

pub use graphix_types::image::*;
// named, not only globbed: a match pattern reads a name the glob does
// not bring in as a catch-all binding
pub use graphix_types::image::{DEF, REF};
pub use nodes::{NOT_IMAGED, decode_node, decode_nodes, encode_nodes};
pub use registration::{NOT_QUIESCENT, ProgramRoot, REGISTRATION_FORMAT, Registration};

use crate::{
    BindId, DynScope, ErrorHandler, FastCall, LambdaInstanceId, Scope,
    expr::ExprId,
    fusion::{
        emit::BodyRecord,
        kernel_abi::{KernelSig, SiteLeaf},
    },
    node::{callsite::RefsSummary, lambda::DefTable},
};
use ahash::AHashMap;
use bytes::{Buf, BufMut};
use netidx_core::pack::{Pack, PackError};
use std::sync;

/// A dynamic scope's tag for the root, beside [`REF`] and [`DEF`].
const ROOT: u8 = 2;

/// An instance body the eager part of the image skipped: written
/// after it, in the heap, at an offset the instance table records.
type Deferred = Box<dyn FnOnce(&mut ImageBuf) -> Result<(), PackError>>;

/// The compiler's objects in an encode session, and its instance heap.
#[derive(Default)]
pub(crate) struct Compiled {
    handlers: Table<usize, ErrorHandler>,
    /// Kernel signatures, slot-chain leaves and body records by `Arc`.
    pub(crate) kernel_sigs: Table<usize, triomphe::Arc<KernelSig>>,
    pub(crate) site_leaves: Table<usize, triomphe::Arc<SiteLeaf>>,
    pub(crate) records: Table<usize, triomphe::Arc<BodyRecord>>,
    /// Definitions' check tables by `Arc`.
    pub(crate) def_tables: Table<usize, sync::Arc<DefTable>>,
    /// Whether a call site writes its instance into the heap, for a
    /// first dispatch to decode, rather than inline.
    pub(crate) defer_instances: bool,
    pub(crate) deferred: Vec<(LambdaInstanceId, Deferred)>,
    pub(crate) instances: AHashMap<LambdaInstanceId, u64>,
    /// Every deferred instance's reference summary, walked once
    /// however many sites write it.
    pub(crate) instance_refs: AHashMap<LambdaInstanceId, RefsSummary>,
}

impl SessionExt for Compiled {
    fn end_session(&mut self) {
        self.deferred.clear();
    }
}

/// What the compiler reads from a restored image beside its objects.
#[derive(Default)]
pub(crate) struct Restored {
    /// The builtins' fast fns by name, for a kernel constant's recipe.
    pub(crate) fastcalls: AHashMap<&'static str, FastCall>,
    /// Where each instance written to the heap starts in the image.
    pub(crate) instances: AHashMap<LambdaInstanceId, u64>,
}

/// A dynamic scope is the chain of handlers a `?` sees; each handler is
/// shared by every node under its catch, so it is an object: written
/// once, parent first, its counters pristine before any cycle.
fn dynscope_len(scope: &DynScope) -> usize {
    match scope.handler() {
        None => 1,
        Some(h) => {
            let key = h.identity();
            object_len(&key, |k| (*k, h.clone()), |e| &mut e.ext::<Compiled>().handlers)
        }
    }
}

/// A handler outside its scope (a catch's own, a `?`'s resolved one)
/// is the same object the scope codec shares.
pub(crate) fn handler_len(h: &ErrorHandler) -> usize {
    dynscope_len(&DynScope::from_handler(h.clone()))
}

pub(crate) fn handler_encode(
    h: &ErrorHandler,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    dynscope_encode(&DynScope::from_handler(h.clone()), buf)
}

pub(crate) fn handler_decode(buf: &mut impl Buf) -> Result<ErrorHandler, PackError> {
    dynscope_decode(buf)?.handler().ok_or(PackError::InvalidFormat)
}

fn dynscope_encode(scope: &DynScope, buf: &mut impl BufMut) -> Result<(), PackError> {
    let Some(h) = scope.handler() else {
        buf.put_u8(ROOT);
        return Ok(());
    };
    let key = h.identity();
    object_encode(
        &key,
        |k| (*k, h.clone()),
        |e| &mut e.ext::<Compiled>().handlers,
        buf,
        |buf| {
            if h.generation() != 0 || h.has_nested_errors() {
                return Err(PackError::Application(registration::NOT_QUIESCENT));
            }
            let (bind, expr) = h.id();
            bind.encode(buf)?;
            expr.encode(buf)?;
            h.is_machine().encode(buf)?;
            dynscope_encode(&h.parent(), buf)
        },
    )
}

fn dynscope_decode(buf: &mut impl Buf) -> Result<DynScope, PackError> {
    shared_decode(
        buf,
        |ord| {
            built::<Foreign<ErrorHandler>>(ord)
                .map(|Foreign(h)| DynScope::from_handler(h))
        },
        |sub| {
            let bind = BindId::decode(sub)?;
            let expr = ExprId::decode(sub)?;
            let machine = bool::decode(sub)?;
            let parent = dynscope_decode(sub)?;
            let scope = parent.with_catch((bind, expr), machine);
            let h = scope.handler().expect("with_catch installs a handler");
            enter(Foreign(h).into_obj())?;
            Ok(scope)
        },
        |b| dynscope_decode(b),
        |tag| match tag {
            ROOT => Ok(DynScope::root()),
            _ => Err(PackError::UnknownTag),
        },
    )
}

pub fn scope_encode(scope: &Scope, buf: &mut impl BufMut) -> Result<(), PackError> {
    scope.lexical.encode(buf)?;
    dynscope_encode(&scope.dynamic, buf)
}

pub fn scope_decode(buf: &mut impl Buf) -> Result<Scope, PackError> {
    let lexical = Pack::decode(buf)?;
    let dynamic = dynscope_decode(buf)?;
    Ok(Scope { lexical, dynamic })
}
