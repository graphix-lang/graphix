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
    cell::Cell,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ptr,
    sync::LazyLock,
};

/// THE VALUE SEAM for the core traits (`design/traits.md` §12, Eric's
/// call 2026-08-23): `Value`'s own `eq`/`partial_cmp`/`Debug` reach a
/// `GxAbstract` through the netidx abstract vtable, which lands in the
/// impls below — so a user implementation of core `Eq`/`Ord`/`Display`
/// hooked HERE is honored by every consumer of Value comparison and
/// printing at once: chunkmap map keys, `array::sort`, `min`/`max`,
/// `uniq`, the comparison operators, the JIT's `graphix_value_eq`
/// helper, the typed and naked printers.
///
/// The hurdle is `ExecCtx` access: these impls are called from
/// arbitrary depth inside operations that can't take a context. The
/// answer is the [`crate::fusion::DynDispatchHandle`] pattern — the
/// frame that HOLDS `&mut ExecCtx`/`&mut Event` and is about to run a
/// comparing/printing operation loans them into this thread-local as
/// a type-erased dispatch handle for the duration of that operation
/// (`node::coretraits::with_value_hooks`). No loan installed — an
/// off-cycle comparison on another thread, a context with no core
/// impls — means the structural case, exactly as before.
#[repr(C)]
pub struct ValueHookDispatch {
    /// Type-erased pointer to the monomorphized dispatch state
    /// (`node::coretraits::HookState<R, E>`).
    pub state: *mut u8,
    /// `None` = no implementation (or no answer is possible) — take
    /// the structural case. `Some` is always a definite answer: a
    /// bottoming implementation resolves by the bottom-key rule
    /// (`node::coretraits`).
    pub eq: fn(*mut u8, &GxAbstract, &GxAbstract) -> Option<bool>,
    pub cmp: fn(*mut u8, &GxAbstract, &GxAbstract) -> Option<Ordering>,
    pub fmt: fn(*mut u8, &GxAbstract) -> Option<ArcStr>,
}

thread_local! {
    static VALUE_HOOKS: Cell<*const ValueHookDispatch> = const { Cell::new(ptr::null()) };
}

/// Install `h` as the thread's value-hook dispatch until the guard
/// drops (save/restore — loans nest). The caller owns the pointed-to
/// handle and state and must keep them alive and unmoved for the
/// guard's lifetime; `node::coretraits::with_value_hooks` is the safe
/// wrapper.
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
        // Debug IS the printed form of an abstract value (Eric's call):
        // every printer — the typed walk, the naked walk, netidx's own
        // `{:?}` — converges here, so a `Display` implementation
        // consulted here covers them all. Guarded: abstract payloads
        // may nest abstracts, one Debug frame per level.
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
