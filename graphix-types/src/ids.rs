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
pub(crate) const MINT_BASE: u64 = 1 << 62;

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
    // CR claude for claude: [bug] A failed read has a side effect: this line lifts the
    // process-wide minted counter to whatever minted extent the image's trailer claims,
    // before the fit check below can refuse the block. IdCounts::decode
    // (image/mod.rs:95) also accepts any floor and extent, so len (:55) can overflow
    // too. With a corrupt trailer, the read panics in debug at :55, or the cold
    // fallback mints ids near u64::MAX and writing the program image panics at :41.
    // With an extent of 3*2^62 the fallback runs, but to_wire (:107) shifts the top bit
    // out of its ids, so the program entry it writes never reads again and every later
    // run is cold. This breaks the rule that an entry which fails to read leaves the
    // session untouched and starts cold. Validate the counts before any counter moves
    // (floor <= extent, reserved spans below MINT_BASE, minted extents well below
    // 3*2^62, checked sum); probe: design/review-2026-10-05/repro/x-image-06.sh
    // (x-image-06)
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
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(u64);

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
                    _ => Ok(Self(raw)),
                }
            }
        }
    };
}
