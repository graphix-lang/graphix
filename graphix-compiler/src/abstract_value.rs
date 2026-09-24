//! The runtime box of a Graphix-minted abstract type: a value of
//! `type T = Abstract<rep>` is a `Value::Abstract` carrying the type's
//! identity and its payload, minted only by the constructor `T(..)`.

use crate::typ::{AbstractId, Type};
use arcstr::ArcStr;
use netidx_derive::Pack;
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
pub(crate) struct ValueHookDispatch {
    /// Type-erased pointer to the monomorphized dispatch state
    /// (`node::coretraits::HookState<R, E>`).
    pub(crate) state: *mut u8,
    /// `None` means no implementation: take the structural case.
    /// `Some` is always a definite answer.
    pub(crate) eq: fn(*mut u8, &GxAbstract, &GxAbstract) -> Option<bool>,
    pub(crate) cmp: fn(*mut u8, &GxAbstract, &GxAbstract) -> Option<Ordering>,
    pub(crate) fmt: fn(*mut u8, &GxAbstract) -> Option<ArcStr>,
}

thread_local! {
    static VALUE_HOOKS: Cell<*const ValueHookDispatch> = const { Cell::new(ptr::null()) };
}

/// Run `f` with `h` as the thread's value-hook dispatch (loans nest);
/// the previous dispatch is back when `f` returns or unwinds. `h.state`
/// must stay valid while `f` runs.
pub(crate) fn with_value_hooks<T>(h: &ValueHookDispatch, f: impl FnOnce() -> T) -> T {
    let _restore = Restore(VALUE_HOOKS.with(|c| c.replace(h)));
    f()
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

#[derive(Clone, Pack)]
#[pack(unwrapped)]
pub struct GxAbstract {
    pub(crate) id: AbstractId,
    /// The type's name, for rendering (`Counter(5)`); identity is `id`.
    pub(crate) name: ArcStr,
    /// The type arguments the value was constructed at, so a core-trait
    /// implementation for one instantiation is told from another's.
    pub(crate) params: Arc<[Type]>,
    pub(crate) payload: Value,
}

impl GxAbstract {
    /// The value's type.
    pub fn typ(&self) -> Type {
        Type::Abstract { id: self.id, params: self.params.clone() }
    }
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

// Only the id: `eq` may consult a user `Eq` impl, which no hash of the
// payload can agree with.
impl Hash for GxAbstract {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
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
pub(crate) fn wrap(
    id: AbstractId,
    name: ArcStr,
    params: Arc<[Type]>,
    payload: Value,
) -> Value {
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
