#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]

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
