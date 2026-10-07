//! The runtime box of a Graphix-minted abstract type: a value of
//! `type T = Abstract<rep>` is a `Value::Abstract` carrying the type's
//! identity and its payload, minted only by the constructor `T(..)`.

use crate::typ::{AbstractId, Type};
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
use triomphe::Arc;

/// The seam through which user `Eq`/`Ord`/`Display` impls reach
/// `Value`'s own comparison and printing. A frame holding `&mut
/// ExecCtx` loans a type-erased dispatch handle into a thread-local
/// for the duration of an operation (`node::coretraits::
/// with_hooks`); with no loan installed the structural case
/// applies.
#[repr(C)]
#[doc(hidden)]
pub struct ValueHookDispatch {
    /// Type-erased pointer to the monomorphized dispatch state
    /// (`node::coretraits::HookState<R, E>`).
    #[doc(hidden)]
    pub state: *mut u8,
    /// `None` means no implementation: take the structural case.
    /// `Some` is always a definite answer.
    #[doc(hidden)]
    pub eq: fn(*mut u8, &GxAbstract, &GxAbstract) -> Option<bool>,
    /// Whether an implementation of `Eq` may decide this value's
    /// equality; when not, equality is the payload's.
    #[doc(hidden)]
    pub eq_applies: fn(*mut u8, &GxAbstract) -> bool,
    #[doc(hidden)]
    pub cmp: fn(*mut u8, &GxAbstract, &GxAbstract) -> Option<Ordering>,
    #[doc(hidden)]
    pub fmt: fn(*mut u8, &GxAbstract) -> Option<ArcStr>,
}

thread_local! {
    static VALUE_HOOKS: Cell<*const ValueHookDispatch> = const { Cell::new(ptr::null()) };
}

/// Run `f` with `h` as the thread's value-hook dispatch (loans nest);
/// the previous dispatch is back when `f` returns or unwinds. `h.state`
/// must stay valid while `f` runs.
#[doc(hidden)]
pub fn with_value_hooks<T>(h: &ValueHookDispatch, f: impl FnOnce() -> T) -> T {
    let _restore = Restore(VALUE_HOOKS.with(|c| c.replace(h)));
    f()
}

/// Whether this thread runs under a value-hook loan, which no other
/// thread can share.
pub fn value_hooks_loaned() -> bool {
    VALUE_HOOKS.with(|c| !c.get().is_null())
}

struct Restore(*const ValueHookDispatch);

impl Drop for Restore {
    fn drop(&mut self) {
        VALUE_HOOKS.with(|c| c.set(self.0));
    }
}

/// Dispatch through the installed handle, which is suspended for the
/// dispatch's duration: the code an implementation runs sees no loan
/// but one it arms itself, so nothing inherits a context it was not
/// lent. The handle is restored on return and on unwind.
fn hooked<T>(f: impl FnOnce(&ValueHookDispatch) -> Option<T>) -> Option<T> {
    let p = VALUE_HOOKS.with(|c| c.replace(ptr::null()));
    if p.is_null() {
        return None;
    }
    let _restore = Restore(p);
    // SAFETY: only `with_value_hooks` installs a pointer, and it is the
    // borrow of a handle alive for the whole of that call's `f`.
    f(unsafe { &*p })
}

#[derive(Clone)]
pub struct GxAbstract {
    #[doc(hidden)]
    pub id: AbstractId,
    /// The type's name, for rendering (`Counter(5)`); identity is `id`.
    #[doc(hidden)]
    pub name: ArcStr,
    /// The type arguments the value was constructed at, so a core-trait
    /// implementation for one instantiation is told from another's.
    #[doc(hidden)]
    pub params: Arc<[Type]>,
    #[doc(hidden)]
    pub payload: Value,
}

impl GxAbstract {
    /// The value's type.
    pub fn typ(&self) -> Type {
        Type::Abstract { id: self.id, params: self.params.clone() }
    }
}

impl GxAbstract {
    /// The value as a user Display impl prints it, when the hooks are
    /// armed and one applies.
    pub(crate) fn displayed(&self) -> Option<ArcStr> {
        hooked(|h| (h.fmt)(h.state, self))
    }
}

impl fmt::Debug for GxAbstract {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Debug is the printed form of an abstract value; every
        // printer converges here, so a user Display impl is consulted here.
        crate::stack::ensure_sufficient(|| {
            if let Some(s) = self.displayed() {
                return f.write_str(&s);
            }
            write!(f, "{}(", self.name)?;
            crate::typ::tval::fmt_naked(f, &self.payload)?;
            write!(f, ")")
        })
    }
}

/// How deeply abstract values may nest in bytes being decoded: each
/// level is a reader around the last, which netidx walks recursively.
const MAX_DECODE_DEPTH: usize = 1024;

thread_local! {
    static DECODE_DEPTH: Cell<usize> = const { Cell::new(0) };
}

/// The fields in order, as the derived codec wrote them; every walk of
/// the payload, which may hold the next level, is guarded.
impl Pack for GxAbstract {
    fn encoded_len(&self) -> usize {
        crate::stack::ensure_sufficient(|| {
            self.id.encoded_len()
                + self.name.encoded_len()
                + self.params.encoded_len()
                + self.payload.encoded_len()
        })
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        crate::stack::ensure_sufficient(|| {
            self.id.encode(buf)?;
            self.name.encode(buf)?;
            self.params.encode(buf)?;
            self.payload.encode(buf)
        })
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let depth = DECODE_DEPTH.with(|d| d.get());
        if depth >= MAX_DECODE_DEPTH {
            return Err(PackError::TooBig);
        }
        DECODE_DEPTH.with(|d| d.set(depth + 1));
        let r = crate::stack::ensure_sufficient(|| {
            Ok(GxAbstract {
                id: Pack::decode(buf)?,
                name: Pack::decode(buf)?,
                params: Pack::decode(buf)?,
                payload: Pack::decode(buf)?,
            })
        });
        DECODE_DEPTH.with(|d| d.set(depth));
        r
    }
}

/// The payload may hold the next level: dropped under the guard.
impl Drop for GxAbstract {
    fn drop(&mut self) {
        let payload = std::mem::replace(&mut self.payload, Value::Null);
        crate::stack::ensure_sufficient(move || drop(payload))
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
        crate::stack::ensure_sufficient(|| self.payload == other.payload)
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
        crate::stack::ensure_sufficient(|| self.payload.cmp(&other.payload))
    }
}

/// Where an `Eq` implementation may decide equality only the id is
/// hashed, since no hash of the payload can agree with it; elsewhere
/// equality is the payload's, and so is the hash.
impl Hash for GxAbstract {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        if hooked(|h| (h.eq_applies)(h.state, self).then_some(())).is_none() {
            crate::stack::ensure_sufficient(|| self.payload.hash(state))
        }
    }
}

static WRAPPER: LazyLock<AbstractWrapper<GxAbstract>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0x5a, 0x0c, 0x31, 0x7e, 0x92, 0x4b, 0x4e, 0x61, 0xb8, 0x2d, 0x6f, 0x13, 0xc9,
        0xa4, 0x7d, 0x08,
    ]);
    Abstract::register::<GxAbstract>(id).expect("failed to register GxAbstract")
});

/// Mint a value of the abstract type `id<params>` around `payload`.
#[doc(hidden)]
pub fn wrap(id: AbstractId, name: ArcStr, params: Arc<[Type]>, payload: Value) -> Value {
    WRAPPER.wrap(GxAbstract { id, name, params, payload })
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
