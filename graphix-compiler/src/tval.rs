//! The tagged Value: what every awake node delivers every cycle, on
//! both engines. Bit-identical to `Value` except that the upper 8 bits
//! of the discriminant word carry a tag of two orthogonal bits, STALE
//! (not an event this cycle) and TAINT (no usable value), giving the
//! four states of [`TagView`]. Both bits join by OR over consumed
//! inputs ([`Tag::join`]). Consume through [`TagValue::view`]; the
//! only ways to recover a `Value` mask the tag first, so a tagged disc
//! is never read as a `Value` discriminant.

use netidx_value::Value;
use std::fmt;

/// The reserved tag byte — the upper 8 bits of the discriminant word.
const TAG_MASK: u64 = 0xFF00_0000_0000_0000;

/// The tag byte of a [`TagValue`]. The kernel's disc constants
/// (`fusion/emit.rs` `STALE`/`TAINT`) are these bits `<< 56`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Tag(u8);

impl Tag {
    /// "not an event this cycle" (kernel disc bit 61).
    pub const STALE_BIT: u8 = 0x20;
    /// "no usable value" (kernel disc bit 62). The payload under this
    /// bit is a placeholder, never usable.
    pub const TAINT_BIT: u8 = 0x40;
    /// Fired this cycle, not a bottom — the ordinary production.
    pub const FIRED: Tag = Tag(0);
    /// A value-channel refresh: present, valid, did not fire.
    pub const STALE: Tag = Tag(Self::STALE_BIT);
    /// A bottom that is an event.
    pub const FRESH_BOTTOM: Tag = Tag(Self::TAINT_BIT);
    /// A standing bottom, and the phantom initial state of a slot that
    /// has never produced.
    pub const STALE_BOTTOM: Tag = Tag(Self::TAINT_BIT | Self::STALE_BIT);
    /// Legacy name for [`Self::STALE_BOTTOM`].
    pub const TAINT: Tag = Self::STALE_BOTTOM;

    /// Wrap a raw tag byte.
    pub fn from_raw(bits: u8) -> Self {
        Tag(bits)
    }

    pub fn bits(self) -> u8 {
        self.0
    }

    /// Fired this cycle and carrying a usable value.
    pub fn is_fired(self) -> bool {
        self.0 & (Self::STALE_BIT | Self::TAINT_BIT) == 0
    }

    /// Bottom — no usable value (fresh or standing).
    pub fn is_bottom(self) -> bool {
        self.0 & Self::TAINT_BIT != 0
    }

    /// Legacy name for [`Self::is_bottom`].
    pub fn is_tainted(self) -> bool {
        self.is_bottom()
    }

    /// Should this production trigger the consumer's evaluation?
    /// `Fired` and `FreshBottom` are events, the stale states are not.
    pub fn triggers(self) -> bool {
        self.0 & Self::STALE_BIT == 0
    }

    /// Bottom ORs, fired ORs (so the STALE bit ANDs). `FIRED` is the
    /// identity for bottom and absorbing for firing.
    pub fn join(self, other: Tag) -> Tag {
        let taint = (self.0 | other.0) & Self::TAINT_BIT;
        let stale = (self.0 & other.0) & Self::STALE_BIT;
        Self::from_raw(taint | stale)
    }

    /// OR `other`'s bottom into self, leaving self's firing alone.
    pub fn with_taint_of(self, other: Tag) -> Tag {
        if other.is_tainted() { Self::TAINT } else { self }
    }

    /// Set the STALE bit, keeping bottomness: the tag a resident
    /// re-surfaces under when nothing triggered this cycle.
    pub fn quiet(self) -> Tag {
        Tag(self.0 | Self::STALE_BIT)
    }

    /// Clear the STALE bit, keeping bottomness: a fresh reader sees a
    /// standing value as new.
    pub fn fresh(self) -> Tag {
        Tag(self.0 & !Self::STALE_BIT)
    }
}

/// The exhaustive view of a production, the way to consume a
/// [`TagValue`]. The value-bearing variants carry `&TagValue` (no
/// untagged `Value` exists to lend); read it through
/// [`TagValue::with_value`] or [`TagValue::value_cloned`].
#[derive(Debug)]
pub enum TagView<'a> {
    /// An event carrying a value.
    Fired(&'a TagValue),
    /// Present, not an event — the value channel.
    Stale(&'a TagValue),
    /// An event with no usable value.
    FreshBottom,
    /// A standing bottom / the never-produced phantom.
    StaleBottom,
}

/// The `(disc, payload)` words of a `Value` with every byte defined:
/// the only sanctioned `Value` → words read. A raw transmute reads the
/// payload lane of dataless and narrow variants as padding (poison to
/// LLVM). Narrow scalars widen with `pack_value_to_u64`'s conventions.
pub fn value_words(v: &Value) -> [u64; 2] {
    use std::mem::MaybeUninit;
    let w: [MaybeUninit<u64>; 2] = unsafe { std::mem::transmute_copy(v) };
    let disc = unsafe { w[0].assume_init() };
    let payload = match v {
        Value::Null => 0,
        Value::Bool(x) => *x as u64,
        Value::U8(x) => *x as u64,
        Value::I8(x) => *x as i64 as u64,
        Value::U16(x) => *x as u64,
        Value::I16(x) => *x as i64 as u64,
        Value::U32(x) | Value::V32(x) => *x as u64,
        Value::I32(x) | Value::Z32(x) => *x as i64 as u64,
        Value::F32(x) => x.to_bits() as u64,
        Value::U64(_)
        | Value::V64(_)
        | Value::I64(_)
        | Value::Z64(_)
        | Value::F64(_)
        | Value::String(_)
        | Value::Bytes(_)
        | Value::Error(_)
        | Value::Array(_)
        | Value::Map(_)
        | Value::Decimal(_)
        | Value::DateTime(_)
        | Value::Duration(_)
        | Value::Abstract(_) => unsafe { w[1].assume_init() },
    };
    [disc, payload]
}

#[repr(C)]
pub struct TagValue {
    disc: u64,
    payload: u64,
}

impl TagValue {
    /// Wrap the two raw words the JIT produced; recover the `Value`
    /// via [`TagValue::value`].
    ///
    /// SAFETY: the masked words `(disc & !TAG_MASK, payload)` must be a
    /// valid `Value` bit pattern, or the zero sentinel checked via
    /// [`Self::is_sentinel`] before any clone/drop.
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
        let [disc, payload] = value_words(&v);
        std::mem::forget(v);
        debug_assert_eq!(disc & TAG_MASK, 0, "Value discriminant overlaps the tag byte");
        TagValue { disc: disc | ((tag.bits() as u64) << 56), payload }
    }

    /// An untagged `TagValue`: fired this cycle.
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

    /// The initial state of a slot that has never produced.
    #[inline]
    pub fn phantom() -> Self {
        Self::tagged(Value::Null, Tag::STALE_BOTTOM)
    }

    /// Store a production into this slot and hand back the borrow.
    #[inline]
    pub fn set(&mut self, tv: TagValue) -> &TagValue {
        *self = tv;
        self
    }

    /// Rewrite the tag byte in place, keeping the payload, and hand
    /// back the borrow.
    #[inline]
    pub fn retag(&mut self, tag: Tag) -> &TagValue {
        self.disc = (self.disc & !TAG_MASK) | ((tag.bits() as u64) << 56);
        self
    }

    /// The quiet-cycle production: set STALE in place, keep
    /// bottomness, hand back the borrow.
    #[inline]
    pub fn ride(&mut self) -> &TagValue {
        let t = self.tag().quiet();
        self.retag(t)
    }

    /// The shared production of a node that never produces.
    pub fn phantom_ref() -> &'static TagValue {
        static PHANTOM: std::sync::LazyLock<TagValue> =
            std::sync::LazyLock::new(TagValue::phantom);
        &PHANTOM
    }

    /// The shared tainted-placeholder production, for a return path
    /// that must deliver a bottom without clobbering its resident.
    pub fn tainted_null() -> &'static TagValue {
        static TAINTED: std::sync::LazyLock<TagValue> =
            std::sync::LazyLock::new(|| TagValue::tainted(Value::Null));
        &TAINTED
    }

    /// The shared bottom production for a wrapper that bottoms an
    /// invocation without clobbering its resident: `FreshBottom` when
    /// `triggering`, else `StaleBottom`.
    pub fn bottom_null(triggering: bool) -> &'static TagValue {
        static FRESH: std::sync::LazyLock<TagValue> =
            std::sync::LazyLock::new(|| TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM));
        static STALE: std::sync::LazyLock<TagValue> =
            std::sync::LazyLock::new(|| TagValue::tagged(Value::Null, Tag::STALE_BOTTOM));
        if triggering { &FRESH } else { &STALE }
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

    /// The exhaustive production view — see [`TagView`].
    #[inline]
    pub fn view(&self) -> TagView<'_> {
        let tag = self.tag();
        match (tag.is_bottom(), tag.bits() & Tag::STALE_BIT != 0) {
            (false, false) => TagView::Fired(self),
            (false, true) => TagView::Stale(self),
            (true, false) => TagView::FreshBottom,
            (true, true) => TagView::StaleBottom,
        }
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

    /// True iff the masked discriminant is zero: the sentinel a JIT
    /// pending path leaves in the `out` slot, not a valid `Value`.
    #[inline]
    pub fn is_sentinel(&self) -> bool {
        self.disc & !TAG_MASK == 0
    }

    /// Borrow the masked `Value` for a read-only operation without
    /// consuming or touching the refcount.
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

impl Default for TagValue {
    /// The phantom.
    fn default() -> Self {
        Self::phantom()
    }
}

impl Clone for TagValue {
    #[inline]
    fn clone(&self) -> Self {
        // `view` is a borrowed view of our own bits and must not drop.
        let view = std::mem::ManuallyDrop::new(unsafe {
            std::mem::transmute::<[u64; 2], Value>([self.disc & !TAG_MASK, self.payload])
        });
        let dup: Value = (*view).clone();
        let [disc, payload] = value_words(&dup);
        std::mem::forget(dup);
        TagValue { disc: disc | (self.disc & TAG_MASK), payload }
    }
}

impl Drop for TagValue {
    #[inline]
    fn drop(&mut self) {
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
    /// The masked value's Display; tags show only in `Debug`.
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
        assert_eq!(T::FIRED.join(T::STALE), T::FIRED);
        assert_eq!(T::STALE.join(T::STALE), T::STALE);
        assert_eq!(T::FIRED.join(T::TAINT), T::FRESH_BOTTOM);
        assert_eq!(T::STALE.join(T::TAINT), T::STALE_BOTTOM);
        assert!(T::TAINT.is_tainted() && !T::TAINT.is_fired());
        assert_eq!(T::from_raw(T::TAINT_BIT), T::FRESH_BOTTOM);
    }

    #[test]
    fn tags_ride_clone_and_mask_on_value() {
        let tv = TagValue::tainted(Value::from("boo"));
        let dup = tv.clone();
        assert!(dup.is_tainted());
        assert_eq!(dup.value(), Value::from("boo"));
        assert_eq!(tv.value_cloned(), Value::from("boo"));
    }

    #[test]
    fn view_maps_the_observable_states() {
        let fired = TagValue::fired(Value::from(1i64));
        assert!(
            matches!(fired.view(), TagView::Fired(tv) if tv.value_cloned() == Value::from(1i64))
        );
        let stale = TagValue::stale(Value::from(2i64));
        assert!(
            matches!(stale.view(), TagView::Stale(tv) if tv.value_cloned() == Value::from(2i64))
        );
        let bottom = TagValue::tainted(Value::Null);
        assert!(matches!(bottom.view(), TagView::StaleBottom));
    }

    #[test]
    fn fresh_bottom_is_observable() {
        let tv = TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM);
        assert_eq!(tv.tag(), Tag::FRESH_BOTTOM);
        assert!(matches!(tv.view(), TagView::FreshBottom));
        assert!(tv.tag().triggers() && tv.tag().is_bottom());
    }
}
