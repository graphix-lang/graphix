//! The runtime box of a Graphix-minted abstract type
//! (`design/nominal_abstract_types.md`): a value of `type T =
//! Abstract<rep>` is a `Value::Abstract` carrying the type's identity
//! and its payload, minted only by the constructor `T(..)`. The tag is
//! what makes the nominal type honest at runtime — `T as t` is a tag
//! comparison, `T(x)` destructures the payload, and nothing else can
//! forge one.

use crate::typ::AbstractId;
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Abstract, Value, abstract_type::AbstractWrapper};
use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    sync::LazyLock,
};

#[derive(Clone)]
pub struct GxAbstract {
    pub id: AbstractId,
    /// The type's name, for rendering (`Counter(5)`); identity is `id`.
    pub name: ArcStr,
    pub payload: Value,
}

impl fmt::Debug for GxAbstract {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.name, self.payload)
    }
}

impl PartialEq for GxAbstract {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.payload == other.payload
    }
}

impl Eq for GxAbstract {}

impl PartialOrd for GxAbstract {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for GxAbstract {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id).then_with(|| self.payload.cmp(&other.payload))
    }
}

impl Hash for GxAbstract {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.payload.hash(state);
    }
}

impl Pack for GxAbstract {
    fn encoded_len(&self) -> usize {
        Pack::encoded_len(&self.id)
            + Pack::encoded_len(&self.name)
            + Pack::encoded_len(&self.payload)
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        Pack::encode(&self.id, buf)?;
        Pack::encode(&self.name, buf)?;
        Pack::encode(&self.payload, buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let id = Pack::decode(buf)?;
        let name = Pack::decode(buf)?;
        let payload = Pack::decode(buf)?;
        Ok(GxAbstract { id, name, payload })
    }
}

static WRAPPER: LazyLock<AbstractWrapper<GxAbstract>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0x5a, 0x0c, 0x31, 0x7e, 0x92, 0x4b, 0x4e, 0x61, 0xb8, 0x2d, 0x6f, 0x13, 0xc9,
        0xa4, 0x7d, 0x08,
    ]);
    Abstract::register::<GxAbstract>(id).expect("failed to register GxAbstract")
});

/// Mint a value of the abstract type `id` around `payload`.
pub fn wrap(id: AbstractId, name: ArcStr, payload: Value) -> Value {
    WRAPPER.wrap(GxAbstract { id, name, payload })
}

/// The box inside `v`, if `v` is a Graphix-minted abstract value.
pub fn get(v: &Value) -> Option<&GxAbstract> {
    match v {
        Value::Abstract(a) => a.downcast_ref::<GxAbstract>(),
        _ => None,
    }
}

/// The payload of a Graphix-minted abstract value — for Rust code
/// that consumes a type whose constructor lives in Graphix.
pub fn payload(v: &Value) -> Option<&Value> {
    get(v).map(|g| &g.payload)
}

/// Is `v` a value of the abstract type `id`? A Graphix-minted box
/// answers by its tag; a Rust-backed abstract value answers by the
/// wrapper UUID its package registered, which is [`crate::typ::abstract_uuid`]
/// of the type's path.
pub fn is_instance(v: &Value, id: AbstractId) -> bool {
    match v {
        Value::Abstract(a) => match a.downcast_ref::<GxAbstract>() {
            Some(g) => g.id == id,
            None => a.id().as_u64_pair().1 == id.inner(),
        },
        _ => false,
    }
}
