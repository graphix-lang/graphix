//! The compiler's id types: `netidx-core`'s `atomic_id!` plus what an
//! image needs, kept local so netidx's own ids and wire format are
//! untouched. Each domain has two regions: ids are minted from
//! [`MINT_BASE`] up, and the blocks image readers reserve come from
//! below it. An image records the span of its ids in each region; a
//! reader reserves one block the size of both, above every reserved id
//! the image wrote, and maps its reserved ids then its minted ids into
//! it, so the same image loads into several runtimes in one process
//! without touching anything already minted. A runtime's image never
//! spans another runtime's reservation, so a chain of restores keeps
//! its size.

/// The first minted id; every reserved block lies below it.
pub const MINT_BASE: u64 = 1 << 62;

/// One past the last id an image may hold: a minted id's wire form is
/// its offset from [`MINT_BASE`] shifted up a bit.
const MINT_LIMIT: u64 = 1 << 63;

/// The one write an identity key hashes to. Minted ids share their high
/// bits and addresses their high and low bits, which a hash table's tag
/// and bucket index read: the multiply spreads them over both.
pub const fn mix(n: u64) -> u64 {
    n.wrapping_mul(0x9E37_79B9_7F4A_7C15)
}

/// The hasher of a map keyed by an address: one [`mix`]ed write.
#[derive(Default)]
pub struct AddrHasher(u64);

impl std::hash::Hasher for AddrHasher {
    fn write(&mut self, _: &[u8]) {
        unreachable!("an address key is written as a usize")
    }

    fn write_usize(&mut self, n: usize) {
        self.0 = mix(n as u64)
    }

    fn finish(&self) -> u64 {
        self.0
    }
}

pub type AddrMap<V> =
    std::collections::HashMap<usize, V, std::hash::BuildHasherDefault<AddrHasher>>;
pub type AddrSet =
    std::collections::HashSet<usize, std::hash::BuildHasherDefault<AddrHasher>>;

/// The ids of one region an image holds: `floor` is the smallest,
/// `extent` one past the largest.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IdSpan {
    pub floor: u64,
    pub extent: u64,
}

impl Default for IdSpan {
    fn default() -> Self {
        IdSpan { floor: u64::MAX, extent: 0 }
    }
}

impl IdSpan {
    pub fn len(&self) -> u64 {
        self.extent.saturating_sub(self.floor)
    }

    /// Empty, or ids in `[lo, hi)`.
    fn within(&self, lo: u64, hi: u64) -> bool {
        *self == IdSpan::default()
            || (lo <= self.floor && self.floor < self.extent && self.extent <= hi)
    }

    pub(crate) fn contains(&self, raw: u64) -> bool {
        self.floor <= raw && raw < self.extent
    }

    fn count(&mut self, raw: u64) {
        self.floor = self.floor.min(raw);
        self.extent = self.extent.max(raw + 1);
    }
}

/// The ids of one domain an image holds, by region.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct IdSpans {
    pub reserved: IdSpan,
    pub minted: IdSpan,
}

impl IdSpans {
    /// Each region's span lies in its region, as any image written holds:
    /// a read refuses other counts before any counter moves.
    pub(crate) fn valid(&self) -> bool {
        self.reserved.within(0, MINT_BASE) && self.minted.within(MINT_BASE, MINT_LIMIT)
    }

    /// The size of the block a reader reserves.
    pub fn len(&self) -> u64 {
        self.reserved.len() + self.minted.len()
    }

    pub(crate) fn count(&mut self, raw: u64) {
        match raw < MINT_BASE {
            true => self.reserved.count(raw),
            false => self.minted.count(raw),
        }
    }

    /// Where the written id `raw` goes in the block at `start`: the
    /// reserved span first, then the minted one, each in order.
    pub(crate) fn relocate(&self, start: u64, raw: u64) -> Option<u64> {
        if self.reserved.contains(raw) {
            Some(start + (raw - self.reserved.floor))
        } else if self.minted.contains(raw) {
            Some(start + self.reserved.len() + (raw - self.minted.floor))
        } else {
            None
        }
    }
}

/// Reserve below [`MINT_BASE`] a block for the ids of `spans`, above
/// every reserved id they hold, and lift `minted` past every minted id
/// they hold, so neither a relocated id nor a new one spells an id the
/// image's text holds. `None` when the reserved region is full.
pub(crate) fn reserve_above(
    reserved: &std::sync::atomic::AtomicU64,
    minted: &std::sync::atomic::AtomicU64,
    spans: IdSpans,
) -> Option<IdRelocation> {
    use std::sync::atomic::Ordering::Relaxed;
    minted.fetch_max(spans.minted.extent, Relaxed);
    let len = spans.len();
    if len == 0 {
        return Some(IdRelocation::Decode { start: 0, spans });
    }
    let start = |c: u64| c.max(spans.reserved.extent);
    let prev = reserved
        .fetch_update(Relaxed, Relaxed, |c| {
            start(c).checked_add(len).filter(|end| *end <= MINT_BASE)
        })
        .ok()?;
    Some(IdRelocation::Decode { start: start(prev), spans })
}

/// An id as written: its region in the low bit, then its offset in the
/// region, so a minted id costs a varint of its offset from
/// [`MINT_BASE`].
pub(crate) fn to_wire(raw: u64) -> u64 {
    match raw.checked_sub(MINT_BASE) {
        Some(offset) => (offset << 1) | 1,
        None => raw << 1,
    }
}

pub(crate) fn from_wire(wire: u64) -> Option<u64> {
    match wire & 1 {
        1 => MINT_BASE.checked_add(wire >> 1),
        _ => Some(wire >> 1),
    }
}

thread_local! {
    /// Whether an image session's relocations are installed on this
    /// thread: then every [`image_id!`] type must have one.
    static IN_SESSION: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Mark whether an image session's relocations are installed on this
/// thread, returning the previous mark.
pub(crate) fn set_in_session(on: bool) -> bool {
    IN_SESSION.with(|s| s.replace(on))
}

/// Whether an image session is encoding or decoding on this thread.
pub(crate) fn in_session() -> bool {
    IN_SESSION.with(|s| s.get())
}

/// An [`image_id!`] type with no relocation under an image session is
/// missing from the session's domains: written or read raw, its ids would
/// collide with the reader's.
pub(crate) fn unrelocated(slot: Option<IdRelocation>) -> bool {
    slot.is_none() && IN_SESSION.with(|s| s.get())
}

/// How an [`image_id!`] type is written while an image is encoded or
/// decoded on this thread; `None` (the default) writes the id as is.
#[derive(Debug, Clone, Copy)]
pub(crate) enum IdRelocation {
    /// The spans of the ids written so far.
    Encode(IdSpans),
    /// A written id in `spans` goes into the block at `start`
    /// ([`IdSpans::relocate`]); any other is refused.
    Decode { start: u64, spans: IdSpans },
}

macro_rules! image_id {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
        pub struct $name(u64);

        /// One mixed write (`ids::mix`), so `nohash` maps take it as is.
        impl std::hash::Hash for $name {
            fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
                h.write_u64($crate::ids::mix(self.0))
            }
        }

        impl nohash::IsEnabled for $name {}

        impl $name {
            fn counter() -> &'static std::sync::atomic::AtomicU64 {
                static NEXT: std::sync::atomic::AtomicU64 =
                    std::sync::atomic::AtomicU64::new($crate::ids::MINT_BASE);
                &NEXT
            }

            fn reserved() -> &'static std::sync::atomic::AtomicU64 {
                static NEXT: std::sync::atomic::AtomicU64 =
                    std::sync::atomic::AtomicU64::new(0);
                &NEXT
            }

            pub fn new() -> Self {
                $name(Self::counter().fetch_add(1, std::sync::atomic::Ordering::Relaxed))
            }

            /// Reserve a block for the ids of `spans`; see
            /// [`crate::ids::reserve_above`].
            pub(crate) fn reserve(
                spans: $crate::ids::IdSpans,
            ) -> Option<$crate::ids::IdRelocation> {
                $crate::ids::reserve_above(Self::reserved(), Self::counter(), spans)
            }

            pub fn inner(&self) -> u64 {
                self.0
            }

            /// Reconstruct an id `new()` minted from its raw value. It
            /// round-trips only within the compile that holds it (a
            /// synthesized `#bind::N` path read back in the same compile),
            /// never through bytes an image keeps: only `Pack` relocates.
            pub fn from_inner(i: u64) -> Self {
                $name(i)
            }

            fn relocation_slot() -> &'static std::thread::LocalKey<
                std::cell::Cell<Option<$crate::ids::IdRelocation>>,
            > {
                thread_local! {
                    static RELOCATION: std::cell::Cell<
                        Option<$crate::ids::IdRelocation>,
                    > = const { std::cell::Cell::new(None) };
                }
                &RELOCATION
            }

            /// Install `r` as this id type's relocation on this thread
            /// and return the previous one.
            pub(crate) fn set_relocation(
                r: Option<$crate::ids::IdRelocation>,
            ) -> Option<$crate::ids::IdRelocation> {
                Self::relocation_slot().replace(r)
            }

            /// The id as written; an encode relocation counts it toward
            /// its region's span.
            fn wire(&self) -> u64 {
                let slot = Self::relocation_slot();
                if let Some($crate::ids::IdRelocation::Encode(mut spans)) = slot.get() {
                    spans.count(self.0);
                    slot.set(Some($crate::ids::IdRelocation::Encode(spans)));
                }
                $crate::ids::to_wire(self.0)
            }
        }

        impl netidx_core::pack::Pack for $name {
            fn encoded_len(&self) -> usize {
                netidx_core::pack::varint_len(self.wire())
            }

            fn encode(
                &self,
                buf: &mut impl bytes::BufMut,
            ) -> std::result::Result<(), netidx_core::pack::PackError> {
                if $crate::ids::unrelocated(Self::relocation_slot().get()) {
                    return Err(netidx_core::pack::PackError::InvalidFormat);
                }
                Ok(netidx_core::pack::encode_varint(self.wire(), buf))
            }

            fn decode(
                buf: &mut impl bytes::Buf,
            ) -> std::result::Result<Self, netidx_core::pack::PackError> {
                let raw = $crate::ids::from_wire(netidx_core::pack::decode_varint(buf)?)
                    .ok_or(netidx_core::pack::PackError::InvalidFormat)?;
                match Self::relocation_slot().get() {
                    Some($crate::ids::IdRelocation::Decode { start, spans }) => spans
                        .relocate(start, raw)
                        .map(Self)
                        .ok_or(netidx_core::pack::PackError::InvalidFormat),
                    slot if $crate::ids::unrelocated(slot) => {
                        Err(netidx_core::pack::PackError::InvalidFormat)
                    }
                    _ => Ok(Self(raw)),
                }
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use netidx_core::pack::Pack;
    use stray::Stray;

    /// An id type no session lists; only what the test calls is used.
    #[allow(dead_code)]
    mod stray {
        image_id!(Stray);
    }

    /// An id type no session relocates is refused under one, never
    /// written or read raw.
    #[test]
    fn an_unrelocated_id_refuses_under_a_session() {
        let id = Stray::new();
        let mut buf = bytes::BytesMut::new();
        id.encode(&mut buf).unwrap();
        let prev = super::set_in_session(true);
        assert!(id.encode(&mut bytes::BytesMut::new()).is_err());
        assert!(Stray::decode(&mut &buf[..]).is_err());
        super::set_in_session(prev);
        assert_eq!(Stray::decode(&mut &buf[..]).unwrap(), id);
    }

    /// Heap addresses share their high bits and their aligned low bits:
    /// an address key's hash spreads them over the 7 top bits a hash
    /// table's tag reads.
    #[test]
    fn address_keys_spread_over_the_tag() {
        use std::hash::BuildHasher;
        let build = std::hash::BuildHasherDefault::<super::AddrHasher>::default();
        let tags: std::collections::HashSet<u64> = (0..1024usize)
            .map(|i| build.hash_one(0x5555_5555_0000 + i * 48) >> 57)
            .collect();
        assert!(tags.len() > 100, "{} tags", tags.len());
    }
}
