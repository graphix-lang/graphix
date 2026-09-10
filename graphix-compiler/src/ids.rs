//! The compiler's id types: `netidx-core`'s `atomic_id!` plus what an
//! image needs, kept local so netidx's own ids and wire format are
//! untouched. An image writes ids densely in first-seen order and reads
//! them offset into a block reserved on the counter, so the same image
//! loads into several runtimes in one process without touching anything
//! already minted.

use std::collections::HashMap;

/// How an [`image_id!`] type is written while an image is encoded or
/// decoded on this thread; `None` (the default) writes the raw id.
#[derive(Debug, Clone)]
pub enum IdRelocation {
    Encode(HashMap<u64, u64>),
    Decode { base: u64 },
}

macro_rules! image_id {
    ($name:ident) => {
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
            pub fn set_relocation(
                r: Option<$crate::ids::IdRelocation>,
            ) -> Option<$crate::ids::IdRelocation> {
                Self::relocation_slot().with_borrow_mut(|slot| std::mem::replace(slot, r))
            }

            /// The id as written: dense in first-seen order under an
            /// encode relocation, raw otherwise.
            fn wire(&self) -> u64 {
                Self::relocation_slot().with_borrow_mut(|slot| match slot {
                    Some($crate::ids::IdRelocation::Encode(dense)) => {
                        let next = dense.len() as u64;
                        *dense.entry(self.0).or_insert(next)
                    }
                    _ => self.0,
                })
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
                    Some($crate::ids::IdRelocation::Decode { base }) => Self(base + raw),
                    _ => Self(raw),
                }))
            }
        }
    };
}
