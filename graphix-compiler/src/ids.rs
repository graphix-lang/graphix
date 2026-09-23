//! The compiler's id types: `netidx-core`'s `atomic_id!` plus what an
//! image needs, kept local so netidx's own ids and wire format are
//! untouched. An image writes ids as minted and records each domain's
//! span; a reader reserves a block that size on the counter, above
//! every id the image wrote, and offsets every id into it, so the same
//! image loads into several runtimes in one process without touching
//! anything already minted.

/// The ids of one domain an image holds: `floor` is the smallest,
/// `extent` one past the largest; the reader reserves the difference.
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
}

/// No reservation takes a counter past this, so no counter wraps.
const COUNTER_LIMIT: u64 = 1 << 62;

/// Reserve on `counter` a block for the ids of `span`, starting at or
/// above `span.extent`, and return what a written id is offset by.
/// `None` when the block would take the counter past its limit.
pub(crate) fn reserve_above(
    counter: &std::sync::atomic::AtomicU64,
    span: IdSpan,
) -> Option<IdRelocation> {
    use std::sync::atomic::Ordering::Relaxed;
    let len = span.len();
    if len == 0 {
        return Some(IdRelocation::Decode { base: 0, span });
    }
    let start = |c: u64| c.max(span.extent);
    let prev = counter
        .fetch_update(Relaxed, Relaxed, |c| {
            start(c).checked_add(len).filter(|end| *end <= COUNTER_LIMIT)
        })
        .ok()?;
    Some(IdRelocation::Decode { base: start(prev) - span.floor, span })
}

/// How an [`image_id!`] type is written while an image is encoded or
/// decoded on this thread; `None` (the default) writes the raw id.
#[derive(Debug, Clone, Copy)]
pub(crate) enum IdRelocation {
    /// The span of the ids written so far.
    Encode(IdSpan),
    /// A written id in `span` is offset by `base` into the reserved
    /// block; any other is refused.
    Decode { base: u64, span: IdSpan },
}

macro_rules! image_id {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(u64);

        impl nohash::IsEnabled for $name {}

        impl $name {
            fn counter() -> &'static std::sync::atomic::AtomicU64 {
                static NEXT: std::sync::atomic::AtomicU64 =
                    std::sync::atomic::AtomicU64::new(0);
                &NEXT
            }

            pub fn new() -> Self {
                $name(Self::counter().fetch_add(1, std::sync::atomic::Ordering::Relaxed))
            }

            /// Reserve a block for the ids of `span`; see
            /// [`crate::ids::reserve_above`].
            pub(crate) fn reserve(
                span: $crate::ids::IdSpan,
            ) -> Option<$crate::ids::IdRelocation> {
                $crate::ids::reserve_above(Self::counter(), span)
            }

            pub fn inner(&self) -> u64 {
                self.0
            }

            /// Reconstruct from a raw inner value. The counter field is
            /// otherwise private precisely so distinct ID domains can't be
            /// mixed; this exists ONLY to round-trip an id that was already
            /// minted by `new()` across a boundary that can't carry the
            /// typed value (e.g. a JIT'd kernel emitting `inner()` as a
            /// constant and reconstructing it on the other side). Do not
            /// use it to forge ids.
            // XCR claude for eric: `mk` and lib.rs's unused `From<u64> for LambdaId`
            // and `TryFrom<Value> for BindId` are deleted. The allow is not stale:
            // TVarId's copy is in crate-private `typ::tvar`, unused. `From<u64> for
            // BindId` stays: six callers in four crates decode a reference value.
            #[allow(dead_code)]
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

            /// The id as written, which is the id; an encode relocation
            /// counts it toward the span.
            fn wire(&self) -> u64 {
                let slot = Self::relocation_slot();
                if let Some($crate::ids::IdRelocation::Encode(mut span)) = slot.get() {
                    span.floor = span.floor.min(self.0);
                    span.extent = span.extent.max(self.0 + 1);
                    slot.set(Some($crate::ids::IdRelocation::Encode(span)));
                }
                self.0
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
                let raw = netidx_core::pack::decode_varint(buf)?;
                match Self::relocation_slot().get() {
                    Some($crate::ids::IdRelocation::Decode { base, span }) => {
                        if span.contains(raw) {
                            Ok(Self(base + raw))
                        } else {
                            Err(netidx_core::pack::PackError::InvalidFormat)
                        }
                    }
                    _ => Ok(Self(raw)),
                }
            }
        }
    };
}
