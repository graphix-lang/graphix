#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]

/// Generates `PartialEq`, `Eq`, `PartialOrd`, `Ord`, `Hash`, `impl_no_pack!`,
/// and the `LazyLock<AbstractWrapper<T>>` static for an abstract value type
/// whose identity is determined by `Arc::as_ptr(&self.inner)`.
macro_rules! impl_abstract_arc {
    ($name:ident, $wrapper_vis:vis static $wrapper:ident = [$($uuid:expr),* $(,)?]) => {
        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                std::sync::Arc::as_ptr(&self.inner) == std::sync::Arc::as_ptr(&other.inner)
            }
        }
        impl Eq for $name {}
        impl PartialOrd for $name {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }
        impl Ord for $name {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                std::sync::Arc::as_ptr(&self.inner)
                    .cmp(&std::sync::Arc::as_ptr(&other.inner))
            }
        }
        impl std::hash::Hash for $name {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                std::sync::Arc::as_ptr(&self.inner).hash(state)
            }
        }
        graphix_package_core::impl_no_pack!($name);
        $wrapper_vis static $wrapper: std::sync::LazyLock<
            netidx_value::abstract_type::AbstractWrapper<$name>,
        > = std::sync::LazyLock::new(|| {
            let id = uuid::Uuid::from_bytes([$($uuid),*]);
            netidx_value::Abstract::register::<$name>(id)
                .expect(concat!("failed to register ", stringify!($name)))
        });
    };
}

mod encoding;
mod tree;
mod cursor;
mod txn;
mod subscribe;

use tree::{
    DbGetType, DbOpen, DbFlush, DbGenerateId, DbTreeNames, DbDropTree, DbTree,
    DbGet, DbInsert, DbRemove, DbContainsKey, DbGetMany,
    DbFirst, DbLast, DbPopMin, DbPopMax, DbGetLt, DbGetGt,
    DbCompareAndSwap, DbBatch, DbLen, DbIsEmpty,
    DbSizeOnDisk, DbWasRecovered, DbChecksum, DbExport, DbImport,
};
use cursor::{DbCursorNew, DbCursorRead, DbCursorReadMany, DbCursorRange};
use txn::{
    DbTxnBegin, DbTxnTree, DbTxnGet, DbTxnInsert, DbTxnRemove,
    DbTxnBatch, DbTxnCommit, DbTxnRollback,
};
use subscribe::{DbSubscribe, DbOnInsert, DbOnRemove};

pub use tree::{DbValue, TreeValue};

// ── Package registration ──────────────────────────────────────────

graphix_derive::defpackage! {
    builtins => [
        DbGetType,
        DbOpen,
        DbFlush,
        DbGenerateId,
        DbTreeNames,
        DbDropTree,
        DbTree,
        DbGet,
        DbInsert,
        DbRemove,
        DbContainsKey,
        DbGetMany,
        DbFirst,
        DbLast,
        DbPopMin,
        DbPopMax,
        DbGetLt,
        DbGetGt,
        DbCompareAndSwap,
        DbBatch,
        DbLen,
        DbIsEmpty,
        DbSizeOnDisk,
        DbWasRecovered,
        DbChecksum,
        DbExport,
        DbImport,
        DbCursorNew,
        DbCursorRead,
        DbCursorReadMany,
        DbCursorRange,
        DbSubscribe,
        DbOnInsert,
        DbOnRemove,
        DbTxnBegin,
        DbTxnTree,
        DbTxnGet,
        DbTxnInsert,
        DbTxnRemove,
        DbTxnBatch,
        DbTxnCommit,
        DbTxnRollback,
    ],
}
