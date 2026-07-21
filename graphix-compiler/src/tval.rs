//! The tagged Value — the shared value currency of the interpreter and
//! the JIT (`design/replay_frames.md` "v2: TagValue as the interpreter
//! currency").
//!
//! A `TagValue` is bit-identical to `Value` (16 bytes, two integer
//! eightbytes → two registers, so the JIT marshals it exactly like a
//! `Value`), EXCEPT the upper 8 bits of the discriminant word are
//! reserved for a tag. The real `Value` discriminant only uses the low
//! bits (`value_disc` tops out at `0x8000_0000`), so the tag and the
//! discriminant never collide. Two tag bits carry the two channels the
//! kernel's disc word carries (`fusion/emit.rs` `STALE`/`TAINT`):
//!
//! - **STALE** — "did not fire this cycle; the payload is a cached
//!   prior value". Result of an op fires iff ANY consumed operand
//!   fired (AND-reduce); forced fresh at the kernel output and at
//!   `set_var` writes, and in the interpreter at loop-driver body
//!   results and the runtime delivery boundary.
//! - **TAINT** — a BOTTOM PLACEHOLDER (#219: a div0, a handler-less
//!   `?`, a missing input). The payload is never a usable value;
//!   anything that consumes it is bottom. The word can still FLOW
//!   without bottoming anything — straight-line evaluation computes
//!   untaken paths, so whether the RESULT is bottom is decided at the
//!   force points (output, `set_var`, destructuring consumers), not at
//!   the producing site. Propagates by OR through consumption.
//!   Invariant: TAINT ⟹ STALE (the constructors make the violation
//!   unrepresentable).
//!
//! The BOUNDARY GUARANTEE: a `TagValue` is *uninterpreted* raw words —
//! the only ways to recover a `Value` are [`TagValue::value`] and
//! [`TagValue::with_value`], which MASK the tag first. So a tagged
//! disc can never be read AS a `Value` discriminant — which is the UB
//! that turned a forgotten disc clean into a process abort (a corrupt
//! disc → `Value::clone`/`drop` → `unreachable_unchecked`). The tag
//! rides through [`Clone`] so taint survives a refcount bump with no
//! manual re-attach. Lives here (not in `netidx-value`): it needs only
//! `Value`'s public `Clone`/`Drop` plus the 16-byte/two-eightbyte
//! layout `fusion/emit_helpers.rs` pins.

use netidx_value::Value;
use std::fmt;

/// The reserved tag byte — the upper 8 bits of the discriminant word.
const TAG_MASK: u64 = 0xFF00_0000_0000_0000;

/// The tag byte of a [`TagValue`], as a first-class type so tag
/// plumbing in the interpreter is compiler-checked rather than raw-u8
/// discipline. The bit values are THE authority — the kernel's i64
/// disc constants (`fusion/emit.rs` `STALE`/`TAINT`) are derived from
/// them by `<< 56`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Tag(u8);

impl Tag {
    /// "did not fire this cycle" (kernel disc bit 61).
    pub const STALE_BIT: u8 = 0x20;
    /// A bottom placeholder — consuming it makes the consumer bottom;
    /// the result bottoms only if a taken path consumes it (#219;
    /// kernel disc bit 62).
    pub const TAINT_BIT: u8 = 0x40;

    /// Fired this cycle, not a bottom — the ordinary production.
    pub const FIRED: Tag = Tag(0);
    /// A value-channel refresh: present, valid, did not fire.
    pub const STALE: Tag = Tag(Self::STALE_BIT);
    /// A possible bottom. TAINT ⟹ STALE by construction.
    pub const TAINT: Tag = Tag(Self::TAINT_BIT | Self::STALE_BIT);

    /// Wrap a raw tag byte from the JIT boundary, restoring the
    /// TAINT ⟹ STALE invariant (the kernel maintains it in CLIF, but
    /// a helper-minted byte might carry TAINT alone).
    pub fn from_raw(bits: u8) -> Self {
        if bits & Self::TAINT_BIT != 0 { Self::TAINT } else { Tag(bits) }
    }

    pub fn bits(self) -> u8 {
        self.0
    }

    /// Fired this cycle (neither stale nor tainted).
    pub fn is_fired(self) -> bool {
        self.0 & (Self::STALE_BIT | Self::TAINT_BIT) == 0
    }

    pub fn is_tainted(self) -> bool {
        self.0 & Self::TAINT_BIT != 0
    }

    /// Should this production trigger the consumer's evaluation?
    /// True for fired productions AND tainted ones (taint must ride
    /// toward its force point); false for merely-stale refreshes.
    pub fn triggers(self) -> bool {
        self.is_fired() || self.is_tainted()
    }

    /// Combine per the kernel's propagation rules: taint ORs (any
    /// consumed bottom taints the result), stale AND-reduces (a result
    /// fires iff ANY operand fired). `FIRED` is the identity for
    /// taint accumulation and the absorbing element for firing.
    pub fn join(self, other: Tag) -> Tag {
        let taint = (self.0 | other.0) & Self::TAINT_BIT;
        let stale = (self.0 & other.0) & Self::STALE_BIT;
        Self::from_raw(taint | stale)
    }

    /// OR `other`'s taint into self, leaving self's firing alone —
    /// the op-result rule for a consumed operand cache (the operand's
    /// staleness doesn't matter mid-expression; its taint does).
    pub fn with_taint_of(self, other: Tag) -> Tag {
        if other.is_tainted() { Self::TAINT } else { self }
    }
}

#[repr(C)]
pub struct TagValue {
    disc: u64,
    payload: u64,
}

impl TagValue {
    /// Wrap the two raw words the JIT produced (a kernel return's `out`
    /// slot, a helper result) — the gateway INTO `TagValue` from
    /// untrusted JIT output. Recover the clean `Value` via
    /// [`TagValue::value`] (which masks the tag), never a bare
    /// `transmute`: that's the whole point — a tainted disc the kernel
    /// leaked can't materialize as a corrupt `Value`.
    ///
    /// SAFETY: the masked words `(disc & !TAG_MASK, payload)` must be a
    /// valid `Value` bit pattern (or the zero sentinel, checked via
    /// [`Self::is_sentinel`] before any clone/drop): `value`/`drop`/
    /// `clone` transmute them into a `Value`, so arbitrary words are
    /// UB there, not here.
    // XCR estokes: This should be marked unsafe, you can use it to construct
    // and invalid Value.
    // (done — unsafe with the contract above; both callers are the JIT
    // out-slot decodes in fusion/kernel.rs, which guard the sentinel)
    #[inline]
    pub unsafe fn from_raw(disc: u64, payload: u64) -> Self {
        TagValue { disc, payload }
    }

    /// Stuff `tag` into the upper 8 bits of `v`'s discriminant.
    #[inline]
    pub fn tagged(v: Value, tag: Tag) -> Self {
        let [disc, payload] = unsafe { std::mem::transmute::<Value, [u64; 2]>(v) };
        debug_assert_eq!(disc & TAG_MASK, 0, "Value discriminant overlaps the tag byte");
        TagValue { disc: disc | ((tag.bits() as u64) << 56), payload }
    }

    /// An untagged `TagValue` (tag = 0) — fired this cycle.
    #[inline]
    pub fn clean(v: Value) -> Self {
        Self::tagged(v, Tag::FIRED)
    }

    /// Alias of [`Self::clean`] under the interpreter's vocabulary.
    #[inline]
    pub fn fired(v: Value) -> Self {
        Self::clean(v)
    }

    /// A value-channel refresh: present and valid, did not fire.
    #[inline]
    pub fn stale(v: Value) -> Self {
        Self::tagged(v, Tag::STALE)
    }

    /// A possible-bottom placeholder (tainted, hence also stale).
    #[inline]
    pub fn tainted(v: Value) -> Self {
        Self::tagged(v, Tag::TAINT)
    }

    /// The tag byte, invariant-restored.
    #[inline]
    pub fn tag(&self) -> Tag {
        Tag::from_raw((self.disc >> 56) as u8)
    }

    /// The raw tag byte as the JIT wrote it (boundary code only).
    #[inline]
    pub fn raw_tag(&self) -> u8 {
        (self.disc >> 56) as u8
    }

    #[inline]
    pub fn is_fired(&self) -> bool {
        self.tag().is_fired()
    }

    #[inline]
    pub fn is_tainted(&self) -> bool {
        self.tag().is_tainted()
    }

    /// Recover the clean `Value`, MASKING the tag. The sole raw-words →
    /// `Value` gateway; consumes self, transferring payload ownership.
    #[inline]
    pub fn value(self) -> Value {
        let me = std::mem::ManuallyDrop::new(self);
        unsafe {
            std::mem::transmute::<[u64; 2], Value>([me.disc & !TAG_MASK, me.payload])
        }
    }

    /// Split into the clean `Value` and its tag.
    #[inline]
    pub fn into_parts(self) -> (Value, Tag) {
        let tag = self.tag();
        (self.value(), tag)
    }

    /// True iff the masked discriminant is zero — the pending-sentinel
    /// word pair a JIT pending path leaves in the `out` slot, which must
    /// never reach a clone/drop (it isn't a valid `Value`).
    #[inline]
    pub fn is_sentinel(&self) -> bool {
        self.disc & !TAG_MASK == 0
    }

    /// Borrow the masked `Value` for a read-only operation WITHOUT
    /// consuming (no refcount change). For helpers the JIT passes a
    /// borrowed operand (tag-eq, is-null, payload reads): the JIT keeps
    /// owning its copy.
    #[inline]
    pub fn with_value<T>(&self, f: impl FnOnce(&Value) -> T) -> T {
        let v = std::mem::ManuallyDrop::new(unsafe {
            std::mem::transmute::<[u64; 2], Value>([self.disc & !TAG_MASK, self.payload])
        });
        f(&v)
    }

    /// Clone out the clean `Value` (refcount bump), keeping self.
    #[inline]
    pub fn value_cloned(&self) -> Value {
        self.with_value(|v| v.clone())
    }
}

impl Clone for TagValue {
    #[inline]
    fn clone(&self) -> Self {
        // Clone the MASKED Value (refcount bump) and re-apply the tag.
        // `view` is a borrowed view of our own bits — must not drop it.
        let view = std::mem::ManuallyDrop::new(unsafe {
            std::mem::transmute::<[u64; 2], Value>([self.disc & !TAG_MASK, self.payload])
        });
        let dup: Value = (*view).clone();
        let [disc, payload] = unsafe { std::mem::transmute::<Value, [u64; 2]>(dup) };
        TagValue { disc: disc | (self.disc & TAG_MASK), payload }
    }
}

impl Drop for TagValue {
    #[inline]
    fn drop(&mut self) {
        // Mask the tag and drop as a clean Value, releasing the payload.
        let v = unsafe {
            std::mem::transmute::<[u64; 2], Value>([self.disc & !TAG_MASK, self.payload])
        };
        drop(v);
    }
}

impl fmt::Debug for TagValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_sentinel() {
            return write!(f, "TagValue(<sentinel>, {:?})", self.tag());
        }
        self.with_value(|v| write!(f, "TagValue({v:?}, {:?})", self.tag()))
    }
}

impl fmt::Display for TagValue {
    /// The masked value's Display — tags are carried by `Debug`, not
    /// leaked into user-facing output.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.with_value(|v| write!(f, "{v}"))
    }
}

impl From<Value> for TagValue {
    fn from(v: Value) -> Self {
        Self::clean(v)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tag_join_follows_kernel_propagation() {
        use Tag as T;
        // stale AND-reduces: any fired operand fires the result
        assert_eq!(T::FIRED.join(T::STALE), T::FIRED);
        assert_eq!(T::STALE.join(T::STALE), T::STALE);
        // taint ORs and implies stale
        assert_eq!(T::FIRED.join(T::TAINT), T::TAINT);
        assert_eq!(T::STALE.join(T::TAINT), T::TAINT);
        assert!(T::TAINT.is_tainted() && !T::TAINT.is_fired());
        // a raw taint-only byte is invariant-restored
        assert_eq!(T::from_raw(T::TAINT_BIT), T::TAINT);
    }

    #[test]
    fn tags_ride_clone_and_mask_on_value() {
        let tv = TagValue::tainted(Value::from("boo"));
        let dup = tv.clone();
        assert!(dup.is_tainted());
        assert_eq!(dup.value(), Value::from("boo"));
        assert_eq!(tv.value_cloned(), Value::from("boo"));
    }
}
