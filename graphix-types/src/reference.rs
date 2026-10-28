//! A reference's runtime form: an abstract value holding its cell, which
//! no number and no other type's value can pass for, and what keeps the
//! cell and the place it names alive while the reference does.

use crate::{BindId, abstract_value, list, stack::ensure_sufficient};
use anyhow::anyhow;
use bytes::{Buf, BufMut};
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Abstract, FromValue, Value, abstract_type::AbstractWrapper};
use nohash::{IntMap, IntSet};
use parking_lot::Mutex;
use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    mem,
    sync::LazyLock,
};
use triomphe::Arc;

/// The references alive in a runtime: a reference keeps its cell, and the
/// binding it names, until its last copy drops ([`Keep`]).
#[derive(Debug, Default)]
pub struct Lives(Mutex<LivesState>);

#[derive(Debug, Default)]
struct LivesState {
    /// Per binding a reference names, how many keeps name it.
    pins: IntMap<BindId, u32>,
    /// Per cell whose keep names a binding, that binding.
    targets: IntMap<BindId, BindId>,
    /// Bindings deleted while pinned: their values go with the last pin.
    orphaned: IntSet<BindId>,
    /// Cells, and orphans unpinned, whose values may go.
    released: Vec<BindId>,
}

impl LivesState {
    fn unpin(&mut self, t: BindId) {
        if let Some(n) = self.pins.get_mut(&t) {
            *n -= 1;
            if *n == 0 {
                self.pins.remove(&t);
                if self.orphaned.remove(&t) {
                    self.released.push(t);
                }
            }
        }
    }

    /// The keep of `cell` names `target`, in place of what it named.
    fn name(&mut self, cell: BindId, target: BindId) {
        let old = self.targets.insert(cell, target);
        if old != Some(target) {
            *self.pins.entry(target).or_default() += 1;
            if let Some(o) = old {
                self.unpin(o)
            }
        }
    }
}

impl Lives {
    /// What every reference value of the cell `cell`, naming `target`,
    /// shares: the last one to drop releases them.
    pub fn keep(lives: &Arc<Self>, cell: BindId, target: Option<BindId>) -> Arc<Keep> {
        if let Some(t) = target {
            lives.0.lock().name(cell, t);
        }
        Arc::new(Keep { cell, lives: lives.clone() })
    }

    /// The binding `id` is deleted: true when a reference still names it,
    /// whose value then goes with the last such reference.
    // CR claude for claude: [bug/medium] An orphan whose own stored value holds a copy of
    // the reference that pins it is never released. Its value keeps the Keep alive, and
    // the Keep keeps the value. A slot doing `x <- {me: &x, data: ..}` leaks x's value
    // and the cell's entries each time it is deleted (about 800 MB per 100 cycles in
    // the probe, against a flat run with `me: null`). Either collect such cycles in
    // ExecCtx::release_references, for example by releasing orphans pinned only from
    // orphaned values, or rule the leak acceptable and document it in
    // design/place_references.md. probe:
    // design/review-2026-10-26/repro/x-refs-catchup-self-ref-leak.gx
    // (x-refs-catchup-02)
    // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q61, A reference
    // cycle through an orphan's own value): yes, collect them (the recommendation:
    // collect them (orphans pinned only from orphaned values).)
    pub fn orphan_if_pinned(&self, id: BindId) -> bool {
        let mut st = self.0.lock();
        let pinned = st.pins.contains_key(&id);
        if pinned {
            st.orphaned.insert(id);
        }
        pinned
    }

    /// The cells and orphaned bindings released since the last call, whose
    /// stored values may go; the lock is not held while `f` runs.
    pub fn released(&self, mut f: impl FnMut(BindId)) {
        loop {
            let released = mem::take(&mut self.0.lock().released);
            if released.is_empty() {
                break;
            }
            for id in released {
                f(id)
            }
        }
    }
}

/// Shared by every reference value of one cell.
pub struct Keep {
    cell: BindId,
    lives: Arc<Lives>,
}

impl Keep {
    /// The cell names `target` now, a place whose root moved: the keep
    /// pins it in place of the binding it named.
    pub fn retarget(&self, target: BindId) {
        self.lives.0.lock().name(self.cell, target)
    }
}

impl fmt::Debug for Keep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Keep").field("cell", &self.cell).finish()
    }
}

impl Drop for Keep {
    fn drop(&mut self) {
        let mut st = self.lives.0.lock();
        st.released.push(self.cell);
        if let Some(t) = st.targets.remove(&self.cell) {
            st.unpin(t)
        }
    }
}

/// A reference, known by its cell; one minted by a `&` keeps the cell and
/// what it names alive.
#[derive(Clone)]
struct Reference {
    cell: BindId,
    _keep: Option<Arc<Keep>>,
}

impl PartialEq for Reference {
    fn eq(&self, other: &Self) -> bool {
        self.cell == other.cell
    }
}

impl Eq for Reference {}

impl PartialOrd for Reference {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// XCR claude for claude: [bug/medium] References typed Any still sort, key maps and dedup
// through this impl, by the cell id that ByRef::compile mints. Slot instances compiled
// or bound on forked branches mint those ids in scheduling order, so array::sort of an
// Array<Any> of references differs between serial and parallel runs and from run to run
// (force: a different order on each of 5 runs; off: always slot order). This
// contradicts design/parallel_eval.md §8, which says reference ids reach no sort.
// CLAUDE.md admits the problem only for functions under Any, and the fuzzer misses it
// because oracle_tier excludes Any + sort programs. Needs a ruling: refuse ordering a
// reference under Any, order references deterministically, or document the case beside
// functions. probe:
// design/review-2026-10-26/repro/x-parallel-any-ref-sort-nondeterministic.gx
// (x-parallel-02)
// 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q34, References
// under Any): yes (the recommendation: walk for `==`, refuse the order.)
// 2026-10-28 codex: Implemented q34: equality descends through Any and replaces
// references by their named places; statically ref-free subtrees retain the typed fast
// path. Ordering refuses a reference reached under Any, including singleton
// sort/min/max/dedup, map construction, queries, edits, casts, collection results,
// native map helpers and reference paths. An actual matching Eq/Ord implementation
// remains authoritative; reference walks match its type arguments, so an impl for
// another instantiation cannot hide a reference. Any comparisons remain unfused as the
// ruling explicitly recommended. Pins lang::byref::any_reference_* plus scalar/Ord
// controls cover all four modes; the final focused byref::any_ run passed 48 tests.
// Equality probes failed before the implementation; ordering and cast pins failed all
// four modes with the implementation removed; compiler map_helpers_refuse_reference_keys
// and map_paths_refuse_reference_keys both failed with it removed. The additional
// instantiated-impl equality and ordering probes failed eight cases before the exact
// lookup correction and passed afterward. Rules, book, design and language skill
// updated. Full-suite and fuzzer results are recorded in the completion report.
// 2026-10-28 codex: Gate reference-order walks on resolved argument/key types. Map nodes,
// collection builders, by-reference map paths and ordered builtins retain the decision
// from typecheck1 (and image it); native helpers receive constant gates. The fast builtin
// path is used only when its argument type cannot conceal references. Cast nodes and
// reader targets retain a conservative target-level gate; the generic Type cast API also
// checks the static map-key type once before iterating entries, so an unrelated Any in a
// map value never causes a key walk. Existing netidx-facing signatures are unchanged.
// Pins: ordered_builtin_checks_only_unknown_types and typed_map_cast_skips_reference_walk
// both failed with their guards removed, then passed restored. Extended
// map_helpers_refuse_reference_keys covers both native gate values;
// key_type_check_does_not_change_place_identity pins that gate metadata is not reference
// identity. The complete graphix-types, graphix-compiler and graphix-package-core unit
// suites pass (336, 54 and 6 tests), and all 48 lang::byref::any_ integration tests pass.
// No socket tests are involved in these focused runs.
// 2026-10-28 codex: Follow-up validation found an abstract target can hide a map
// representation behind a custom Ord. The outer type's comparison walk is not its cast's
// representation walk. Pretyped casts now keep the key guard for any abstract target (and
// Any); each actual map still checks only keys whose static type can conceal references.
// The new any_reference_cast_abstract_map_is_refused pin failed in all four modes with
// the earlier target gate. This does not change the unchecked path for concrete
// structural targets such as Map<string, i64>.
impl Ord for Reference {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cell.cmp(&other.cell)
    }
}

impl Hash for Reference {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.cell.hash(state)
    }
}

impl fmt::Debug for Reference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "&ref")
    }
}

impl Pack for Reference {
    fn encoded_len(&self) -> usize {
        self.cell.encoded_len()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        self.cell.encode(buf)
    }

    /// Data never carries a reference, so bytes holding one are forged or
    /// stale: only an image session reads them.
    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if !crate::ids::in_session() {
            return Err(PackError::InvalidFormat);
        }
        Ok(Reference { cell: BindId::decode(buf)?, _keep: None })
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

/// The reference whose cell is `cell`, kept alive by `keep`.
pub fn reference(cell: BindId, keep: Option<Arc<Keep>>) -> Value {
    WRAPPER.wrap(Reference { cell, _keep: keep })
}

/// The cell of `v`, if `v` is a reference.
pub fn cell(v: &Value) -> Option<BindId> {
    match v {
        Value::Abstract(a) => a.downcast_ref::<Reference>().map(|r| r.cell),
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn reference_decodes_only_under_an_image() {
        let mut buf = Vec::new();
        Reference { cell: BindId::new(), _keep: None }.encode(&mut buf).unwrap();
        assert!(matches!(
            Reference::decode(&mut &buf[..]),
            Err(PackError::InvalidFormat)
        ));
    }

    #[test]
    fn a_retargeted_keep_pins_its_new_binding_alone() {
        let lives = Arc::new(Lives::default());
        let (cell, a, b) = (BindId::new(), BindId::new(), BindId::new());
        let keep = Lives::keep(&lives, cell, Some(a));
        keep.retarget(b);
        let mut released = Vec::new();
        assert!(!lives.orphan_if_pinned(a));
        assert!(lives.orphan_if_pinned(b));
        lives.released(|id| released.push(id));
        assert!(released.is_empty());
        drop(keep);
        lives.released(|id| released.push(id));
        assert_eq!(released, [cell, b]);
    }
}

/// A reference read as its cell.
impl FromValue for BindId {
    fn from_value(v: Value) -> anyhow::Result<Self> {
        cell(&v)
            .ok_or_else(|| anyhow!("expected a reference, got {}", crate::typ::Naked(&v)))
    }
}

#[cfg(test)]
thread_local! {
    pub(crate) static ORDER_CHECKS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Whether ordering this value can reach a reference outside an `Ord` implementation.
/// The caller lends its core-trait hooks.
pub fn orderable(v: &Value) -> bool {
    #[cfg(test)]
    ORDER_CHECKS.set(ORDER_CHECKS.get() + 1);
    ensure_sufficient(|| {
        if cell(v).is_some() {
            return false;
        }
        if let Some(g) = abstract_value::get(v) {
            return g.has_order_impl() || orderable(&g.payload);
        }
        if list::split(v).is_some() {
            return list::Iter::new(v.clone()).all(|v| orderable(&v));
        }
        match v {
            Value::Array(a) => a.iter().all(orderable),
            Value::Map(m) => m.into_iter().all(|(k, v)| orderable(k) && orderable(v)),
            Value::Error(e) => orderable(e),
            _ => true,
        }
    })
}

#[cfg(test)]
mod order_check_test {
    use super::*;
    use crate::{env::Env, typ::Type};
    use triomphe::Arc as TArc;

    #[test]
    fn typed_map_cast_skips_reference_walk() {
        let env = Env::default();
        let target = Type::Map {
            key: TArc::new(Type::Primitive(netidx_value::Typ::I64.into())),
            value: TArc::new(Type::Primitive(netidx_value::Typ::I64.into())),
        };
        let value = Value::Map([(Value::I64(1), Value::I64(2))].into_iter().collect());
        ORDER_CHECKS.set(0);
        assert_eq!(target.cast_value(&env, value.clone()), value);
        assert_eq!(ORDER_CHECKS.get(), 0);
    }
}
