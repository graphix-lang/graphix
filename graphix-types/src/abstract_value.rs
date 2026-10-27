//! The runtime box of a Graphix-minted abstract type: a value of
//! `type T = Abstract<rep>` is a `Value::Abstract` carrying the type's
//! identity and its payload, minted only by the constructor `T(..)`.

use crate::typ::{AbstractId, Type};
use anyhow::{Result, anyhow};
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
/// `Value`'s own comparison and printing, and `Cast` impls the cast walk
/// and the writers. A frame holding `&mut
/// ExecCtx` loans a type-erased dispatch handle into a thread-local
/// for the duration of an operation (`node::coretraits::
/// with_hooks`); with no loan installed the structural case
/// applies.
#[repr(C)]
#[doc(hidden)]
pub struct ValueHookDispatch {
    /// Type-erased pointer to the context the dispatch borrows
    /// (`ExecCtx<R>`).
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
    /// `None` means no `Cast` implementation.
    #[doc(hidden)]
    pub to_value: fn(*mut u8, &GxAbstract) -> Option<CastHook>,
    /// The value read as the abstract type, the `Type::Abstract` given;
    /// `None` means no `Cast` implementation.
    #[doc(hidden)]
    pub from_value: fn(*mut u8, &Type, &Value) -> Option<CastHook>,
}

/// What a `Cast` implementation made of a value.
#[doc(hidden)]
pub enum CastHook {
    /// The implementation leaves the method out: the representation.
    Representation,
    /// Its result, `None` when it produced none.
    Ran(Option<Value>),
}

thread_local! {
    static VALUE_HOOKS: Cell<*const ValueHookDispatch> = const { Cell::new(ptr::null()) };
}

/// Run `f` with `h` as the thread's value-hook dispatch (loans nest);
/// the previous dispatch is back when `f` returns or unwinds. `h.state`
/// must stay valid while `f` runs.
#[doc(hidden)]
pub fn with_value_hooks<T>(h: &ValueHookDispatch, f: impl FnOnce() -> T) -> T {
    let _restore = crate::Restore::replace(&VALUE_HOOKS, h as *const ValueHookDispatch);
    f()
}

/// Whether this thread runs under a value-hook loan, which no other
/// thread can share.
pub fn value_hooks_loaned() -> bool {
    VALUE_HOOKS.with(|c| !c.get().is_null())
}

/// Dispatch through the installed handle, which is suspended for the
/// dispatch's duration: the code an implementation runs sees no loan
/// but one it arms itself, so nothing inherits a context it was not
/// lent. The handle is restored on return and on unwind.
fn hooked<T>(f: impl FnOnce(&ValueHookDispatch) -> Option<T>) -> Option<T> {
    let restore = crate::Restore::replace(&VALUE_HOOKS, ptr::null());
    let p = restore.prev();
    if p.is_null() {
        return None;
    }
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

    /// The data this value is written as, by its `Cast` implementation;
    /// `None` when there is none or the hooks are not armed.
    pub fn to_value(&self) -> Option<CastHook> {
        hooked(|h| (h.to_value)(h.state, self))
    }
}

/// `v` as a writer writes it: every abstract value in it replaced by the
/// data its `Cast` implementation writes it as, through the hooks, and
/// every tuple, struct, variant and list in its data form
/// ([`crate::rep::strip`]). An abstract value with no implementation, a
/// Rust-backed one included, is refused, and so is an implementation
/// that writes its own type inside itself without end, and a map two of
/// whose keys write as the same data (`"A"` and `` `A ``).
pub fn to_data(v: &Value) -> Result<Value> {
    Ok(rewrite(v, 0, Forms::Strip)?.unwrap_or_else(|| v.clone()))
}

/// [`to_data`] with every tuple, struct, variant and list kept as a
/// value, so what differs or orders apart as a value still does: a
/// store's keys.
pub fn to_marked_data(v: &Value) -> Result<Value> {
    Ok(rewrite(v, 0, Forms::Keep)?.unwrap_or_else(|| v.clone()))
}

#[derive(Clone, Copy, PartialEq)]
enum Forms {
    Strip,
    Keep,
}

/// [`to_data`] or [`to_marked_data`] of `v`, `None` when nothing in it
/// changed; `depth` counts the implementations written through so far.
/// One walk: each level's markers go ([`crate::rep::strip_with`]) or stay
/// ([`crate::rep::map_parts`]) and each abstract writes as its data.
fn rewrite(v: &Value, depth: usize, forms: Forms) -> Result<Option<Value>> {
    match v {
        Value::Abstract(_)
            if forms == Forms::Keep && crate::rep::marker_kind(v).is_some() =>
        {
            Ok(None)
        }
        Value::Abstract(_) => {
            let data = match written_as(v, depth)? {
                Written::Representation(g) => g.payload.clone(),
                Written::Ran(d) => d,
            };
            Ok(Some(rewrite(&data, depth + 1, forms)?.unwrap_or(data)))
        }
        v => {
            let mut part = |c: &Value| rewrite(c, depth, forms);
            let data = match forms {
                Forms::Strip => crate::rep::strip_with(v, &mut part)?,
                Forms::Keep => crate::rep::map_parts(v, &mut part)?,
            };
            // keys that differ as values may write as the same data
            if let (Value::Map(m), Some(Value::Map(d))) = (v, &data)
                && d.len() < m.len()
            {
                return Err(anyhow!("two keys of a map write as the same data"));
            }
            Ok(data)
        }
    }
}

/// What one abstract value is written as.
pub enum Written<'a> {
    /// Its representation, the payload of the value held.
    Representation(&'a GxAbstract),
    /// What its `Cast` implementation made of it.
    Ran(Value),
}

/// The abstract value `v`, `depth` implementations into a write, as its
/// `Cast` implementation writes it. A reference, a function, a value of a
/// type backed by Rust, a type with no implementation, an implementation that
/// produced nothing and a write through more implementations than a
/// reader could read are refused.
pub fn written_as(v: &Value, depth: usize) -> Result<Written<'_>> {
    if crate::reference::cell(v).is_some() {
        return Err(anyhow!("a reference is not data"));
    }
    let Some(g) = get(v) else {
        return Err(anyhow!(
            "a function or a value of a type backed by Rust is not data"
        ));
    };
    if depth >= MAX_DEPTH {
        return Err(anyhow!(
            "{} is written as data more than {MAX_DEPTH} abstract values deep",
            g.name
        ));
    }
    match g.to_value() {
        None => Err(anyhow!("{} has no Cast implementation", g.name)),
        Some(CastHook::Representation) => Ok(Written::Representation(g)),
        Some(CastHook::Ran(Some(d))) => Ok(Written::Ran(d)),
        Some(CastHook::Ran(None)) => {
            Err(anyhow!("the Cast implementation of {} produced nothing", g.name))
        }
    }
}

/// `v` read as the abstract type `t` by its `Cast` implementation;
/// `None` when there is none or the hooks are not armed.
pub fn from_value(t: &Type, v: &Value) -> Option<CastHook> {
    hooked(|h| (h.from_value)(h.state, t, v))
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

/// How deeply abstract values may nest in bytes: each level is a reader
/// around the last, which netidx walks recursively, so the writer refuses
/// what no reader could read.
const MAX_DEPTH: usize = 1024;

thread_local! {
    static DECODE_DEPTH: Cell<usize> = const { Cell::new(0) };
    static ENCODE_DEPTH: Cell<usize> = const { Cell::new(0) };
}

/// `f` one level deeper in the walk `depth` counts, refused past
/// [`MAX_DEPTH`].
fn nested<T>(
    depth: &'static std::thread::LocalKey<Cell<usize>>,
    f: impl FnOnce() -> Result<T, PackError>,
) -> Result<T, PackError> {
    let d = depth.with(|d| d.get());
    if d >= MAX_DEPTH {
        return Err(PackError::TooBig);
    }
    depth.with(|c| c.set(d + 1));
    let r = crate::stack::ensure_sufficient(f);
    depth.with(|c| c.set(d));
    r
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
        nested(&ENCODE_DEPTH, || {
            self.id.encode(buf)?;
            self.name.encode(buf)?;
            self.params.encode(buf)?;
            self.payload.encode(buf)
        })
    }

    /// Only an image restores an abstract value: everything else is data,
    /// where an abstract type comes from its `Cast` implementation, and
    /// bytes holding one are forged or stale.
    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if !crate::ids::in_session() {
            return Err(PackError::InvalidFormat);
        }
        nested(&DECODE_DEPTH, || {
            Ok(GxAbstract {
                id: Pack::decode(buf)?,
                name: Pack::decode(buf)?,
                params: Pack::decode(buf)?,
                payload: Pack::decode(buf)?,
            })
        })
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
        // CR claude for eric: [bug/medium] The structural case ignores params, but a
        // type test does not. Box<i64>(1) and Box<[i64, null]>(1) are told apart by a
        // select, yet compare == and merge into one map key (coretraits::pair also
        // sends two instantiations here). Either compare params after the id in eq, cmp
        // and hash, or rule that a union of two instantiations of one minted abstract
        // is not Discernible. probe:
        // design/review-2026-10-26/repro/c-module-traits-abstract-instantiations-equal.gx
        // (c-module-traits-04)
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

/// Register the wrapper with netidx before any value decodes: a value
/// read before the first mint must still downcast to a [`GxAbstract`].
#[doc(hidden)]
pub fn register() {
    LazyLock::force(&WRAPPER);
}

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

#[cfg(test)]
mod test {
    use super::*;

    /// Writers write an abstract value as data, so bytes holding one are
    /// forged or stale: only an image session reads them.
    #[test]
    fn abstract_value_decodes_only_under_an_image() {
        let v = GxAbstract {
            id: AbstractId::of(&crate::expr::ModPath::from(["m"]), "T"),
            name: arcstr::literal!("T"),
            params: Arc::from_iter([]),
            payload: Value::I64(1),
        };
        let mut buf = Vec::new();
        v.encode(&mut buf).unwrap();
        assert!(matches!(
            GxAbstract::decode(&mut &buf[..]),
            Err(PackError::InvalidFormat)
        ));
    }
}
