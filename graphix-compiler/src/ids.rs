//! The compiler's id types: `netidx-core`'s `atomic_id!` plus what an
//! image needs, kept local so netidx's own ids and wire format are
//! untouched. An image writes ids as minted and records each domain's
//! span; a reader reserves a block that size on the counter and offsets
//! every id into it, so the same image loads into several runtimes in
//! one process without touching anything already minted.

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
}

/// How an [`image_id!`] type is written while an image is encoded or
/// decoded on this thread; `None` (the default) writes the raw id.
#[derive(Debug, Clone)]
pub enum IdRelocation {
    /// The span of the ids written so far.
    Encode(IdSpan),
    /// What a written id is offset by: the reserved block's base less
    /// the span's floor.
    Decode { base: u64 },
}

macro_rules! image_id {
    ($name:ident) => {
        // CR claude for eric: [risk] The serde derives write the raw id,
        // bypassing the relocation `Pack` applies, and I found nothing in the
        // workspace that serde-serializes these ids (Expr serializes as text).
        // Drop them so no path can carry an unrelocated id out of an image.
        #[derive(
            Debug,
            Clone,
            Copy,
            PartialEq,
            Eq,
            PartialOrd,
            Ord,
            Hash,
            Serialize,
            Deserialize,
        )]
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

            /// Reserve `n` consecutive ids nobody else will mint and
            /// return the first.
            pub fn reserve(n: u64) -> Self {
                $name(Self::counter().fetch_add(n, std::sync::atomic::Ordering::Relaxed))
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
            // CR claude for eric: [dead] The `allow(dead_code)` is stale (a pub
            // fn; used by node/bind.rs:471), and `mk` below is an unused
            // test-only copy of this. lib.rs:276/286/292 add three more
            // forging doors (`From<u64>`, `TryFrom<Value>`) beside this one.
            #[allow(dead_code)]
            pub fn from_inner(i: u64) -> Self {
                $name(i)
            }

            #[cfg(test)]
            #[allow(dead_code)]
            pub fn mk(i: u64) -> Self {
                $name(i)
            }

            fn relocation_slot() -> &'static std::thread::LocalKey<
                std::cell::RefCell<Option<$crate::ids::IdRelocation>>,
            > {
                thread_local! {
                    static RELOCATION: std::cell::RefCell<
                        Option<$crate::ids::IdRelocation>,
                    > = const { std::cell::RefCell::new(None) };
                }
                &RELOCATION
            }

            /// Install `r` as this id type's relocation on this thread
            /// and return the previous one.
            // CR claude for eric: [style] `set_relocation`, `reserve` and the
            // `IdRelocation` re-export (lib.rs:24) are public, but only
            // image/mod.rs uses them; any crate can repoint every id this
            // thread writes. Make them pub(crate).
            pub fn set_relocation(
                r: Option<$crate::ids::IdRelocation>,
            ) -> Option<$crate::ids::IdRelocation> {
                Self::relocation_slot().with_borrow_mut(|slot| std::mem::replace(slot, r))
            }

            /// The id as written, which is the id; an encode relocation
            /// counts it toward the span.
            fn wire(&self) -> u64 {
                Self::relocation_slot().with_borrow_mut(|slot| {
                    if let Some($crate::ids::IdRelocation::Encode(span)) = slot {
                        span.floor = span.floor.min(self.0);
                        span.extent = span.extent.max(self.0 + 1);
                    }
                });
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
                Ok(Self::relocation_slot().with_borrow(|slot| match slot {
                    Some($crate::ids::IdRelocation::Decode { base }) => {
                        // CR claude for eric: [bug] no span check: a wire id outside the image's
                        // reserved span (a corrupt cache) relocates onto a live id, and wrapping_add
                        // hides overflow (companion to the CR at image/mod.rs on IdCounts). Refuse
                        // `raw >= span` as a decode error.
                        Self(base.wrapping_add(raw))
                    }
                    _ => Self(raw),
                }))
            }
        }
    };
}
