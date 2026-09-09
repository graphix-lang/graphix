//! The runtime box of a Graphix-minted abstract type: a value of
//! `type T = Abstract<rep>` is a `Value::Abstract` carrying the type's
//! identity and its payload, minted only by the constructor `T(..)`.

use crate::typ::AbstractId;
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Abstract, Value, abstract_type::AbstractWrapper};
use std::{
    cell::Cell,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ptr,
    sync::LazyLock,
};

/// The seam through which user `Eq`/`Ord`/`Display` impls reach
/// `Value`'s own comparison and printing. A frame holding `&mut
/// ExecCtx` loans a type-erased dispatch handle into a thread-local
/// for the duration of an operation (`node::coretraits::
/// with_value_hooks`); with no loan installed the structural case
/// applies.
#[repr(C)]
pub struct ValueHookDispatch {
    /// Type-erased pointer to the monomorphized dispatch state
    /// (`node::coretraits::HookState<R, E>`).
    pub state: *mut u8,
    /// `None` means no implementation: take the structural case.
    /// `Some` is always a definite answer.
    pub eq: fn(*mut u8, &GxAbstract, &GxAbstract) -> Option<bool>,
    pub cmp: fn(*mut u8, &GxAbstract, &GxAbstract) -> Option<Ordering>,
    pub fmt: fn(*mut u8, &GxAbstract) -> Option<ArcStr>,
}

thread_local! {
    static VALUE_HOOKS: Cell<*const ValueHookDispatch> = const { Cell::new(ptr::null()) };
}

/// Install `h` as the thread's value-hook dispatch until the guard
/// drops (loans nest). The caller must keep the handle and its state
/// alive and unmoved for the guard's lifetime.
pub(crate) fn arm_value_hooks(h: *const ValueHookDispatch) -> ValueHookGuard {
    ValueHookGuard { prev: VALUE_HOOKS.with(|c| c.replace(h)) }
}

pub(crate) struct ValueHookGuard {
    prev: *const ValueHookDispatch,
}

impl Drop for ValueHookGuard {
    fn drop(&mut self) {
        VALUE_HOOKS.with(|c| c.set(self.prev));
    }
}

fn hooked<T>(f: impl FnOnce(&ValueHookDispatch) -> Option<T>) -> Option<T> {
    let p = VALUE_HOOKS.with(|c| c.get());
    if p.is_null() {
        None
    } else {
        // SAFETY: the pointer was installed by `arm_value_hooks`, whose
        // guard is alive in a caller frame that owns the handle.
        f(unsafe { &*p })
    }
}

#[derive(Clone)]
pub struct GxAbstract {
    pub id: AbstractId,
    /// The type's name, for rendering (`Counter(5)`); identity is `id`.
    pub name: ArcStr,
    pub payload: Value,
}

impl fmt::Debug for GxAbstract {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Debug is the printed form of an abstract value; every
        // printer converges here, so a user Display impl is consulted here.
        crate::stack::ensure_sufficient(|| {
            if let Some(s) = hooked(|h| (h.fmt)(h.state, self)) {
                return f.write_str(&s);
            }
            write!(f, "{}(", self.name)?;
            crate::typ::tval::fmt_naked(f, &self.payload)?;
            write!(f, ")")
        })
    }
}

impl PartialEq for GxAbstract {
    fn eq(&self, other: &Self) -> bool {
        if self.id != other.id {
            return false;
        }
        if let Some(b) = hooked(|h| (h.eq)(h.state, self, other)) {
            return b;
        }
        self.payload == other.payload
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
        match self.id.cmp(&other.id) {
            Ordering::Equal => {}
            o => return o,
        }
        if let Some(o) = hooked(|h| (h.cmp)(h.state, self, other)) {
            return o;
        }
        self.payload.cmp(&other.payload)
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
/// answers by its tag; a Rust-backed value by its registered wrapper
/// UUID ([`crate::typ::abstract_uuid`] of the type's path).
pub fn is_instance(v: &Value, id: AbstractId) -> bool {
    match v {
        Value::Abstract(a) => match a.downcast_ref::<GxAbstract>() {
            Some(g) => g.id == id,
            None => a.id().as_u64_pair().1 == id.inner(),
        },
        _ => false,
    }
}
