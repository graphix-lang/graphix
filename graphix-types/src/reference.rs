//! A reference's runtime form: an abstract value holding its cell, which
//! no number and no other type's value can pass for.

use crate::BindId;
use anyhow::anyhow;
use bytes::{Buf, BufMut};
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Abstract, FromValue, Value, abstract_type::AbstractWrapper};
use std::{fmt, sync::LazyLock};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Reference(BindId);

impl fmt::Debug for Reference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "&ref")
    }
}

impl Pack for Reference {
    fn encoded_len(&self) -> usize {
        self.0.encoded_len()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        self.0.encode(buf)
    }

    // XCR claude for claude: [bug/medium] Reference::decode has no image-session gate,
    // unlike rep::Marker::decode and GxAbstract's decode. Outside a session
    // BindId::decode returns the raw id, so a packed value from outside the program
    // decodes a reference to an arbitrary cell. pack::read into Array<Any> returns
    // [&ref]. The same bytes arrive from any netidx publisher through
    // sys::net::subscribe, sys::net::call replies and rpc arguments, all of which
    // decode with Value::decode and cast through CastTarget under Any. That breaks the
    // rule that data is never a reference and the run-time cast refuses one. Fix:
    // return PackError::InvalidFormat here unless crate::ids::in_session(), as Marker
    // does, and pin it beside abstract_value_decodes_only_under_an_image. An
    // unknown-uuid abstract also passes under Any (`abstract:...`); refusing every
    // Value::Abstract in a data read would close both. probe:
    // design/review-2026-10-19/repro/x-security-forged-reference.gx (x-security-01)
    // 2026-10-20 claude: Reference::decode refuses outside an image session
    // (PackError::InvalidFormat), as Marker and GxAbstract do. The unknown-uuid
    // abstract under Any is not closed here. Pin:
    // reference::test::reference_decodes_only_under_an_image (graphix-types).
    /// Data never carries a reference, so bytes holding one are forged or
    /// stale: only an image session reads them.
    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if !crate::ids::in_session() {
            return Err(PackError::InvalidFormat);
        }
        Ok(Reference(BindId::decode(buf)?))
    }
}

static WRAPPER: LazyLock<AbstractWrapper<Reference>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0x3e, 0x71, 0x0a, 0xd4, 0x5c, 0x28, 0x4f, 0x9b, 0x86, 0x1e, 0xb2, 0x47, 0x0d,
        0x93, 0xe6, 0x5a,
    ]);
    Abstract::register::<Reference>(id).expect("failed to register Reference")
});

/// Register the wrapper with netidx before any value decodes: a value
/// read before the first reference is made must still downcast.
#[doc(hidden)]
pub fn register() {
    LazyLock::force(&WRAPPER);
}

/// The reference whose cell is `cell`.
pub fn reference(cell: BindId) -> Value {
    WRAPPER.wrap(Reference(cell))
}

/// The cell of `v`, if `v` is a reference.
pub fn cell(v: &Value) -> Option<BindId> {
    match v {
        Value::Abstract(a) => a.downcast_ref::<Reference>().map(|r| r.0),
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn reference_decodes_only_under_an_image() {
        let mut buf = Vec::new();
        Reference(BindId::new()).encode(&mut buf).unwrap();
        assert!(matches!(
            Reference::decode(&mut &buf[..]),
            Err(PackError::InvalidFormat)
        ));
    }
}

/// A reference read as its cell.
impl FromValue for BindId {
    fn from_value(v: Value) -> anyhow::Result<Self> {
        cell(&v)
            .ok_or_else(|| anyhow!("expected a reference, got {}", crate::typ::Naked(&v)))
    }
}
