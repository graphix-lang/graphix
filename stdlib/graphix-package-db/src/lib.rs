#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]

mod cursor;
mod encoding;
mod subscribe;
mod tree;
mod txn;

use cursor::{DbCursorNew, DbCursorRange, DbCursorRead, DbCursorReadMany};
use subscribe::{DbOnInsert, DbOnRemove, DbSubscribe};
use tree::{
    DbBatch, DbChecksum, DbCompareAndSwap, DbContainsKey, DbDropTree, DbExport, DbFirst,
    DbFlush, DbGenerateId, DbGet, DbGetGt, DbGetLt, DbGetMany, DbGetType, DbImport,
    DbInsert, DbIsEmpty, DbLast, DbLen, DbOpen, DbPopMax, DbPopMin, DbRemove,
    DbSizeOnDisk, DbTree, DbTreeNames, DbWasRecovered,
};
use txn::{
    DbTxnBatch, DbTxnBegin, DbTxnCommit, DbTxnGet, DbTxnInsert, DbTxnRemove,
    DbTxnRollback, DbTxnTree,
};

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
