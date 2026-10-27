use anyhow::Result;
use graphix_package_core::run_with_tempdir;
use netidx::subscriber::Value;

fn assert_tree_type(v: &Value, expected_key: &str, expected_val: &str) {
    let ty = match v {
        Value::Array(a) => a,
        _ => panic!("type info not array: {v:?}"),
    };
    match (&ty[0], &ty[1]) {
        (Value::String(k), Value::String(v)) => {
            assert_eq!(&**k, expected_key, "key type mismatch");
            assert_eq!(&**v, expected_val, "val type mismatch");
        }
        _ => panic!("type entries not strings: {ty:?}"),
    }
}

run_with_tempdir!(
    name: db_open,
    code: r#"{{
        let db = db::open("{}");
        (is_err(db), is_err(db::flush(db$)))
    }}"#,
    setup: |td| {
        td.path().join("test_open.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[false, false]"#);
        Ok(())
    }
);

run_with_tempdir!(
    name: db_insert_get,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::tree(db, null)?;
        let ty = db::get_type(db, t ~ null)?;
        let old = db::insert(t, "hello", 42)$;
        let result = db::get(t, old ~ "hello")?;
        (result, ty)
    }}"#,
    setup: |td| {
        td.path().join("test_insert_get.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[i64:42, ["string", "i64"]]"#);
        Ok(())
    }
);

run_with_tempdir!(
    name: db_get_missing,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, null)?;
        db::get(t, "nonexistent")?
    }}"#,
    setup: |td| {
        td.path().join("test_get_missing.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"null"#);
        Ok(())
    }
);

run_with_tempdir!(
    name: db_remove,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::tree(db, null)?;
        let ty = db::get_type(db, t ~ null)?;
        let old = db::insert(t, "key", 99)$;
        let result = db::remove(t, old ~ "key")?;
        (result, ty)
    }}"#,
    setup: |td| {
        td.path().join("test_remove.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[i64:99, ["string", "i64"]]"#);
        Ok(())
    }
);

run_with_tempdir!(
    name: db_contains_key,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::tree(db, null)?;
        let ty = db::get_type(db, t ~ null)?;
        let old = db::insert(t, "exists", 1)$;
        let result = db::contains_key(t, old ~ "exists")?;
        (result, ty)
    }}"#,
    setup: |td| {
        td.path().join("test_contains.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[true, ["string", "i64"]]"#);
        Ok(())
    }
);

run_with_tempdir!(
    name: db_named_tree,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::tree(db, "mytree")?;
        let ty = db::get_type(db, t ~ "mytree")?;
        let old = db::insert(t, "x", "hello")$;
        let result = db::get(t, old ~ "x")?;
        (result, ty)
    }}"#,
    setup: |td| {
        td.path().join("test_named_tree.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"["hello", ["string", "string"]]"#);
        Ok(())
    }
);

run_with_tempdir!(
    name: db_integer_key_order,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<i64, string> = db::tree(db, "ints")?;
        let ty = db::get_type(db, t ~ "ints")?;
        let ins = db::batch(t, [
            `Insert(100, "hundred"),
            `Insert(-50, "neg fifty"),
            `Insert(0, "zero"),
            `Insert(-1, "neg one"),
            `Insert(50, "fifty")
        ])?;
        let cursor = db::cursor::new(ins ~ t);
        let entries = db::cursor::read_many(cursor, cursor ~ i64:6)?;
        (entries, ty)
    }}"#,
    setup: |td| {
        td.path().join("test_int_order.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[[[i64:-50, "neg fifty"], [i64:-1, "neg one"], [i64:0, "zero"], [i64:50, "fifty"], [i64:100, "hundred"]], ["i64", "string"]]"#);
        Ok(())
    }
);

// Insert string keys, use cursor with prefix filter
run_with_tempdir!(
    name: db_cursor_prefix,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::tree(db, "pfx")?;
        let ty = db::get_type(db, t ~ "pfx")?;
        let a = db::insert(t, "aaa", 1)$;
        let b = db::insert(t, a ~ "aab", 2)$;
        let c = db::insert(t, b ~ "bbb", 3)$;
        let cursor = db::cursor::new(#prefix: "aa", c ~ t);
        let r1 = db::cursor::read(cursor, cursor)$;
        let r2 = db::cursor::read(cursor, r1)$;
        let r3 = db::cursor::read(cursor, r2)$;
        ([r1, r2, r3], ty)
    }}"#,
    setup: |td| {
        td.path().join("test_cursor_prefix.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[[["aaa", i64:1], ["aab", i64:2], null], ["string", "i64"]]"#);
        Ok(())
    }
);

// Atomic batch of inserts and removes
run_with_tempdir!(
    name: db_batch,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, null)?;
        let a = db::insert(t, "a", 1)$;
        let b = db::insert(t, a ~ "b", 2)$;
        let batched = db::batch(t, b ~ [`Insert("c", 3), `Insert("d", 4), `Remove("a")])?;
        let va = db::get(t, batched ~ "a")?;
        let vb = db::get(t, va ~ "b")?;
        let vc = db::get(t, vb ~ "c")?;
        let vd = db::get(t, vc ~ "d")?;
        (va, vb, vc, vd)
    }}"#,
    setup: |td| {
        td.path().join("test_batch.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[null, i64:2, i64:3, i64:4]"#);
        Ok(())
    }
);

// Batch get, verify order, null for missing, and inferred types
run_with_tempdir!(
    name: db_get_many,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::tree(db, null)?;
        let ty = db::get_type(db, t ~ null)?;
        let a = db::insert(t, "x", 10)$;
        let b = db::insert(t, a ~ "y", 20)$;
        let keys = b ~ ["y", "missing", "x"];
        let result = db::get_many(t, keys)?;
        (result, ty)
    }}"#,
    setup: |td| {
        td.path().join("test_get_many.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[[i64:20, null, i64:10], ["string", "i64"]]"#);
        Ok(())
    }
);

// first/last on empty and populated tree
run_with_tempdir!(
    name: db_first_last,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<i64, string> = db::tree(db, null)?;
        let ef = db::first(t)?;
        let el = db::last(ef ~ t)?;
        let a = db::insert(t, el ~ 10, "ten")$;
        let b = db::insert(t, a ~ 5, "five")$;
        let c = db::insert(t, b ~ 20, "twenty")$;
        let first = db::first(c ~ t)?;
        let last = db::last(first ~ t)?;
        (ef, el, first, last)
    }}"#,
    setup: |td| {
        td.path().join("test_first_last.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[null, null, [i64:5, "five"], [i64:20, "twenty"]]"#);
        Ok(())
    }
);

// pop_min/pop_max atomically remove and return
run_with_tempdir!(
    name: db_pop_min_max,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<i64, string> = db::tree(db, null)?;
        let a = db::insert(t, 10, "ten")$;
        let b = db::insert(t, a ~ 5, "five")$;
        let c = db::insert(t, b ~ 20, "twenty")$;
        let pmin = db::pop_min(c ~ t)?;
        let pmax = db::pop_max(pmin ~ t)?;
        let remain = db::first(pmax ~ t)?;
        let last_pop = db::pop_min(remain ~ t)?;
        let empty_pop = db::pop_min(last_pop ~ t)?;
        (pmin, pmax, remain, empty_pop)
    }}"#,
    setup: |td| {
        td.path().join("test_pop.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[[i64:5, "five"], [i64:20, "twenty"], [i64:10, "ten"], null]"#);
        Ok(())
    }
);

// get_lt/get_gt nearest-neighbor lookup
run_with_tempdir!(
    name: db_get_lt_gt,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<i64, string> = db::tree(db, null)?;
        let a = db::insert(t, 10, "ten")$;
        let b = db::insert(t, a ~ 20, "twenty")$;
        let c = db::insert(t, b ~ 30, "thirty")$;
        let lt = db::get_lt(c ~ t, 20)?;
        let gt = db::get_gt(lt ~ t, 20)?;
        let no_lt = db::get_lt(gt ~ t, 5)?;
        let no_gt = db::get_gt(no_lt ~ t, 100)?;
        (lt, gt, no_lt, no_gt)
    }}"#,
    setup: |td| {
        td.path().join("test_lt_gt.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[[i64:10, "ten"], [i64:30, "thirty"], null, null]"#);
        Ok(())
    }
);

// compare_and_swap: success, success, mismatch
run_with_tempdir!(
    name: db_compare_and_swap,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, null)?;
        let r1 = db::compare_and_swap(t, "key", null, 42)?;
        let r2 = db::compare_and_swap(r1 ~ t, "key", 42, 100)?;
        let r3 = db::compare_and_swap(r2 ~ t, "key", 42, 200)?;
        let final_val = db::get(r3 ~ t, "key")?;
        (r1, r2, r3, final_val)
    }}"#,
    setup: |td| {
        td.path().join("test_cas.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[null, null, ["Mismatch", i64:100], i64:100]"#);
        Ok(())
    }
);

// len/is_empty introspection
run_with_tempdir!(
    name: db_len_is_empty,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, null)?;
        let empty = db::is_empty(t)?;
        let len0 = db::len(empty ~ t)?;
        let a = db::insert(t, len0 ~ "a", 1)$;
        let b = db::insert(t, a ~ "b", 2)$;
        let c = db::insert(t, b ~ "c", 3)$;
        let len3 = db::len(c ~ t)?;
        let not_empty = db::is_empty(len3 ~ t)?;
        (empty, len0, len3, not_empty)
    }}"#,
    setup: |td| {
        td.path().join("test_len.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[true, u64:0, u64:3, false]"#);
        Ok(())
    }
);

// size_on_disk, was_recovered, checksum smoke tests
run_with_tempdir!(
    name: db_introspection,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, null)?;
        let ins = db::insert(t, "k", 1)$;
        let flushed = db::flush(ins ~ db)?;
        let size = db::size_on_disk(flushed ~ db)?;
        let recovered = db::was_recovered(size ~ db)?;
        let crc = db::checksum(recovered ~ db)?;
        (size, recovered, crc)
    }}"#,
    setup: |td| {
        td.path().join("test_introspection.db")
    },
    expect: |v: Value| -> Result<()> {
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        match &arr[0] {
            Value::U64(n) => assert!(*n > 0, "size > 0, got {n}"),
            other => panic!("size: {other:?}"),
        }
        assert!(matches!(&arr[1], Value::Bool(false)), "recovered: {:?}", arr[1]);
        assert!(matches!(&arr[2], Value::U32(_)), "checksum: {:?}", arr[2]);
        Ok(())
    }
);

// export/import round-trip
run_with_tempdir!(
    name: db_export_import,
    code: r#"{{
        let base = "{}";
        let src = db::open("[base]/src.db")$;
        let t: db::Tree<string, i64> = db::tree(src, null)?;
        let a = db::insert(t, "x", 10)$;
        let b = db::insert(t, a ~ "y", 20)$;
        let exported = db::export(b ~ src, "[base]/export.bin")?;
        let dst = db::open(exported ~ "[base]/dst.db")$;
        let imported = db::import(dst, "[base]/export.bin")?;
        let t2: db::Tree<string, i64> = db::tree(imported ~ dst, null)?;
        let vx = db::get(t2, "x")?;
        let vy = db::get(t2, vx ~ "y")?;
        (vx, vy)
    }}"#,
    setup: |td| {
        td.path().to_path_buf()
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[i64:10, i64:20]"#);
        Ok(())
    }
);

// Cursor read_many, verify short array on exhaustion and inferred types
run_with_tempdir!(
    name: db_cursor_read_many,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::tree(db, null)?;
        let ty = db::get_type(db, t ~ null)?;
        let a = db::insert(t, "a", 1)$;
        let b = db::insert(t, a ~ "b", 2)$;
        let c = db::insert(t, b ~ "c", 3)$;
        let cursor = db::cursor::new(c ~ t);
        let batch1 = db::cursor::read_many(cursor, cursor ~ i64:2)?;
        let batch2 = db::cursor::read_many(cursor, batch1 ~ i64:2)?;
        let batch3 = db::cursor::read_many(cursor, batch2 ~ i64:2)?;
        ([batch1, batch2, batch3], ty)
    }}"#,
    setup: |td| {
        td.path().join("test_cursor_read_many.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[[[["a", i64:1], ["b", i64:2]], [["c", i64:3]], []], ["string", "i64"]]"#);
        Ok(())
    }
);

// get_type with explicit annotations and a missing tree. `t2` is
// created after `t1` (`t1 ~ "other"`) so the read chain gated on `t2`
// runs once both trees exist.
run_with_tempdir!(
    name: db_get_type,
    code: r#"{{
        let db = db::open("{}")$;
        let t1: db::Tree<i64, string> = db::tree(db, "typed")?;
        let t2: db::Tree<string, bool> = db::tree(db, t1 ~ "other")?;
        let missing = db::get_type(db, t2 ~ "nonexistent")?;
        let ty1 = db::get_type(db, missing ~ "typed")?;
        let ty2 = db::get_type(db, ty1 ~ "other")?;
        [missing, ty1, ty2]
    }}"#,
    setup: |td| {
        td.path().join("test_get_type.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[null, ["i64", "string"], ["string", "bool"]]"#);
        Ok(())
    }
);

// Reserved tree names are rejected
run_with_tempdir!(
    name: db_reserved_tree_name,
    code: r#"{{
        let db = db::open("{}")$;
        let r1: Result<db::Tree<string, string>, `DbErr(string)> = db::tree(db, "$$__graphix_default__$$");
        let r2: Result<db::Tree<string, string>, `DbErr(string)> = db::tree(db, r1 ~ "$$__graphix_meta__$$");
        (is_err(r1), is_err(r2))
    }}"#,
    setup: |td| {
        td.path().join("test_reserved.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[true, true]"#);
        Ok(())
    }
);

// Opening a tree with mismatched types must fail
run_with_tempdir!(
    name: db_type_mismatch_named,
    code: r#"{{
        let db = db::open("{}")$;
        let t1: db::Tree<string, i64> = db::tree(db, "t")?;
        let ins = db::insert(t1, "x", 1)$;
        let t2: [db::Tree<i64, string>, Error<`DbErr(string)>] = db::tree(db, ins ~ "t");
        (is_err(t1), is_err(t2))
    }}"#,
    setup: |td| {
        td.path().join("test_type_mismatch_named.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[false, true]"#);
        Ok(())
    }
);

// Opening the default tree with mismatched types must fail
run_with_tempdir!(
    name: db_type_mismatch_default,
    code: r#"{{
        let db = db::open("{}")$;
        let t1: db::Tree<string, i64> = db::tree(db, null)?;
        let ins = db::insert(t1, "x", 1)$;
        let t2: [db::Tree<i64, string>, Error<`DbErr(string)>] = db::tree(ins ~ db, null);
        (is_err(t1), is_err(t2))
    }}"#,
    setup: |td| {
        td.path().join("test_type_mismatch_default.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[false, true]"#);
        Ok(())
    }
);

// Verify default tree stores type metadata when types are annotated
run_with_tempdir!(
    name: db_default_typed,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, null)?;
        let ty = db::get_type(db, t ~ null)?;
        let old = db::insert(t, "x", 42)$;
        let result = db::get(t, old ~ "x")?;
        (result, ty)
    }}"#,
    setup: |td| {
        td.path().join("test_default_typed.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[i64:42, ["string", "i64"]]"#);
        Ok(())
    }
);

// Range with integer keys: insert 5 values, range [5, 15) → 2 entries
run_with_tempdir!(
    name: db_cursor_range_inclusive_exclusive,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<i64, string> = db::tree(db, "range")?;
        let ins = db::batch(t, [
            `Insert(1, "one"),
            `Insert(5, "five"),
            `Insert(10, "ten"),
            `Insert(15, "fifteen"),
            `Insert(20, "twenty")
        ])?;
        let cursor = db::cursor::range(#start: `Included(5), #end: `Excluded(15), ins ~ t);
        db::cursor::read_many(cursor, cursor ~ i64:10)?
    }}"#,
    setup: |td| {
        td.path().join("test_range_incl_excl.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[[i64:5, "five"], [i64:10, "ten"]]"#);
        Ok(())
    }
);

// Range unbounded start: ..=10 (Included end)
run_with_tempdir!(
    name: db_cursor_range_unbounded_start,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<i64, string> = db::tree(db, "range")?;
        let ins = db::batch(t, [
            `Insert(1, "one"),
            `Insert(5, "five"),
            `Insert(10, "ten"),
            `Insert(15, "fifteen"),
            `Insert(20, "twenty")
        ])?;
        let cursor = db::cursor::range(#end: `Included(i64:10), ins ~ t);
        db::cursor::read_many(cursor, cursor ~ i64:10)?
    }}"#,
    setup: |td| {
        td.path().join("test_range_unbound_start.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[[i64:1, "one"], [i64:5, "five"], [i64:10, "ten"]]"#);
        Ok(())
    }
);

// Range unbounded end: 15..
run_with_tempdir!(
    name: db_cursor_range_unbounded_end,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<i64, string> = db::tree(db, "range")?;
        let ins = db::batch(t, [
            `Insert(1, "one"),
            `Insert(5, "five"),
            `Insert(10, "ten"),
            `Insert(15, "fifteen"),
            `Insert(20, "twenty")
        ])?;
        let cursor = db::cursor::range(#start: `Included(15), ins ~ t);
        db::cursor::read_many(cursor, cursor ~ i64:10)?
    }}"#,
    setup: |td| {
        td.path().join("test_range_unbound_end.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[[i64:15, "fifteen"], [i64:20, "twenty"]]"#);
        Ok(())
    }
);

// Empty range → empty cursor
run_with_tempdir!(
    name: db_cursor_range_empty,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<i64, string> = db::tree(db, "range")?;
        let ins = db::batch(t, [
            `Insert(1, "one"),
            `Insert(5, "five"),
            `Insert(10, "ten")
        ])?;
        let cursor = db::cursor::range(#start: `Included(100), #end: `Excluded(200), ins ~ t);
        db::cursor::read_many(cursor, cursor ~ i64:10)?
    }}"#,
    setup: |td| {
        td.path().join("test_range_empty.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[]"#);
        Ok(())
    }
);

// Commit: begin, insert, commit, verify with db::get
run_with_tempdir!(
    name: db_txn_commit,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, "txn")?;
        let txn = db::txn::begin(t ~ db)?;
        let tt = db::txn::tree(txn, txn ~ "txn")?;
        let ins = db::txn::insert(tt, "k1", 42)?;
        let committed = db::txn::commit(ins ~ txn)?;
        db::get(t, committed ~ "k1")?
    }}"#,
    setup: |td| {
        td.path().join("test_txn_commit.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"i64:42"#);
        Ok(())
    }
);

// Rollback: begin, insert, rollback, verify value unchanged
run_with_tempdir!(
    name: db_txn_rollback,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, "txn")?;
        let ins = db::insert(t, "k1", 100)$;
        let txn = db::txn::begin(ins ~ db)?;
        let tt = db::txn::tree(txn, txn ~ "txn")?;
        let txn_ins = db::txn::insert(tt, "k1", 999)?;
        let rolled = db::txn::rollback(txn_ins ~ txn)?;
        db::get(t, rolled ~ "k1")?
    }}"#,
    setup: |td| {
        td.path().join("test_txn_rollback.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"i64:100"#);
        Ok(())
    }
);

// Multi-tree transaction: insert in two trees, commit, verify both
run_with_tempdir!(
    name: db_txn_multi_tree,
    code: r#"{{
        let db = db::open("{}")$;
        let t1: db::Tree<string, i64> = db::tree(db, "tree1")?;
        let t2: db::Tree<string, string> = db::tree(db, t1 ~ "tree2")?;
        let txn = db::txn::begin(t2 ~ db)?;
        let tt1 = db::txn::tree(txn, txn ~ "tree1")?;
        let tt2: db::txn::TxnTree<string, string> = db::txn::tree(txn, tt1 ~ "tree2")?;
        let ins1 = db::txn::insert(tt1, "a", 1)?;
        let ins2 = db::txn::insert(tt2, ins1 ~ "b", "hello")?;
        let committed = db::txn::commit(ins2 ~ txn)?;
        let v1 = db::get(t1, committed ~ "a")?;
        let v2 = db::get(t2, v1 ~ "b")?;
        (v1, v2)
    }}"#,
    setup: |td| {
        td.path().join("test_txn_multi_tree.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[i64:1, "hello"]"#);
        Ok(())
    }
);

// Txn batch: begin, open tree, batch insert+remove, commit, verify
run_with_tempdir!(
    name: db_txn_batch,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, "txb")?;
        let pre = db::insert(t, "a", 1)$;
        let txn = db::txn::begin(pre ~ db)?;
        let tt = db::txn::tree(txn, txn ~ "txb")?;
        let batched = db::txn::batch(tt, tt ~ [`Insert("b", 2), `Insert("c", 3), `Remove("a")])?;
        let committed = db::txn::commit(batched ~ txn)?;
        let va = db::get(t, committed ~ "a")?;
        let vb = db::get(t, va ~ "b")?;
        let vc = db::get(t, vb ~ "c")?;
        (va, vb, vc)
    }}"#,
    setup: |td| {
        td.path().join("test_txn_batch.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[null, i64:2, i64:3]"#);
        Ok(())
    }
);

// Subscribe to a tree and verify on_insert fires
run_with_tempdir!(
    name: db_subscribe_on_insert,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, null)?;
        let sub = db::subscription::new(t);
        let key = never();
        key <- sub ~ "mykey";
        let ins = db::insert(t, key, 99)$;
        db::subscription::on_insert(sub)?
    }}"#,
    setup: |td| {
        td.path().join("test_sub_insert.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[[["key", "mykey"], ["value", i64:99]]]"#);
        Ok(())
    }
);

// Subscribe to a tree and verify on_remove fires
run_with_tempdir!(
    name: db_subscribe_on_remove,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, null)?;
        let sub = db::subscription::new(t);
        let key = never();
        key <- sub ~ "delme";
        let ins = db::insert(t, key, 7)$;
        let del = db::remove(t, ins ~ "delme")?;
        db::subscription::on_remove(sub)?
    }}"#,
    setup: |td| {
        td.path().join("test_sub_remove.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[[["key", "delme"]]]"#);
        Ok(())
    }
);

// on_insert and on_remove active on one subscription: the first handler
// does not consume the event.
run_with_tempdir!(
    name: db_subscribe_both_handlers,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, null)?;
        let sub = db::subscription::new(t);
        let key = never();
        key <- sub ~ "shared";
        let ins = db::insert(t, key, 42)$;
        let on_ins = db::subscription::on_insert(sub)?;
        let ins0 = on_ins[0]$;
        let del = db::remove(t, ins0 ~ "shared")?;
        let on_rm = db::subscription::on_remove(sub)?;
        let rm0 = on_rm[0]$;
        (ins0.key, rm0.key)
    }}"#,
    setup: |td| {
        td.path().join("test_sub_both.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"["shared", "shared"]"#);
        Ok(())
    }
);

fn arr<const N: usize>(vs: [Value; N]) -> Value {
    Value::Array(vs.into())
}

fn s(x: &str) -> Value {
    Value::String(x.into())
}

// Keys sort as the language orders them: negative floats before positive,
// and -0.0 is the key 0.0.
run_with_tempdir!(
    name: db_float_keys_order,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<f64, string> = db::tree(db, null)$;
        let b = db::batch(t, [`Insert(2.0, "2"), `Insert(0.0 - 3.0, "-3"), `Insert(0.5, "0.5"), `Insert(0.0 - 1.0, "-1"), `Insert(0.0, "0")])$;
        let first = db::first(b ~ t)$;
        let lt = db::get_lt(t, b ~ 0.0)$;
        let z = db::get(t, b ~ 0.0 * (0.0 - 1.0))$;
        (first, lt, z)
    }}"#,
    setup: |td| { td.path().join("float_keys.db") },
    expect: |v: Value| -> Result<()> {
        let e = arr([
            arr([Value::F64(-3.0), s("-3")]),
            arr([Value::F64(-1.0), s("-1")]),
            s("0"),
        ]);
        assert_eq!(v, e);
        Ok(())
    }
);

// The meta tree and sled's name for the default tree are not a program's
// to open, drop or list.
run_with_tempdir!(
    name: db_reserved_names,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, null)$;
        let ins = db::insert(t, "k", 4096)$;
        let r1: Result<db::Tree<string, string>, `DbErr(string)> = db::tree(db, ins ~ "__sled__default");
        let r2 = db::drop_tree(db, ins ~ "$$__graphix_meta__$$");
        let names = db::tree_names(r2 ~ db)$;
        (is_err(r1), is_err(r2), names)
    }}"#,
    setup: |td| { td.path().join("reserved.db") },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v, arr([Value::Bool(true), Value::Bool(true), arr([])]));
        Ok(())
    }
);

// A dropped tree's name is free for any types.
run_with_tempdir!(
    name: db_drop_tree_frees_name,
    code: r#"{{
        let db = db::open("{}")$;
        let x: db::Tree<string, i64> = db::tree(db, "x")$;
        let ins = db::insert(x, "k", 1)$;
        let dropped = db::drop_tree(db, ins ~ "x")$;
        let y: db::Tree<i64, f64> = db::tree(db, dropped ~ "x")$;
        db::get_type(db, y ~ "x")$
    }}"#,
    setup: |td| { td.path().join("drop.db") },
    expect: |v: Value| -> Result<()> {
        assert_tree_type(&v, "i64", "f64");
        Ok(())
    }
);

// A tree opened twice in a transaction is one tree: each handle sees the
// other's writes and the last write is the one committed.
run_with_tempdir!(
    name: db_txn_tree_opened_twice,
    code: r#"{{
        let db = db::open("{}")$;
        let txn = db::txn::begin(db)$;
        let t1: db::txn::TxnTree<string, i64> = db::txn::tree(txn, "x")$;
        let t2: db::txn::TxnTree<string, i64> = db::txn::tree(txn, t1 ~ "x")$;
        let i1 = db::txn::insert(t1, t2 ~ "k", 1)$;
        let r = db::txn::get(t2, i1 ~ "k")$;
        let i2 = db::txn::insert(t1, r ~ "k", 2)$;
        let i3 = db::txn::insert(t2, i2 ~ "k", 3)$;
        let c = db::txn::commit(i3 ~ txn)$;
        let t: db::Tree<string, i64> = db::tree(db, c ~ "x")$;
        (r, db::get(t, t ~ "k")$)
    }}"#,
    setup: |td| { td.path().join("txn_twice.db") },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v, arr([Value::I64(1), Value::I64(3)]));
        Ok(())
    }
);

// A tree first opened in a transaction exists only once the transaction
// commits: a rollback leaves neither the tree nor its types.
run_with_tempdir!(
    name: db_txn_rollback_leaves_no_tree,
    code: r#"{{
        let db = db::open("{}")$;
        let txn = db::txn::begin(db)$;
        let t: db::txn::TxnTree<string, i64> = db::txn::tree(txn, "fresh")$;
        let ins = db::txn::insert(t, "k", 1)$;
        let r = db::txn::rollback(ins ~ txn)$;
        let names = db::tree_names(r ~ db)$;
        (names, db::get_type(names ~ db, "fresh")$)
    }}"#,
    setup: |td| { td.path().join("txn_fresh.db") },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v, arr([arr([]), Value::Null]));
        Ok(())
    }
);

// A constant #prefix filters the events, and the write issued on the
// subscription's fire is seen.
run_with_tempdir!(
    name: db_subscription_prefix,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, null)$;
        let sub = db::subscription::new(#prefix: "aa", t);
        let b = db::batch(t, sub ~ [`Insert("bb", 1), `Insert("aa1", 2)])$;
        db::subscription::on_insert(sub)$
    }}"#,
    setup: |td| { td.path().join("prefix.db") },
    expect: |v: Value| -> Result<()> {
        let ev = arr([arr([s("key"), s("aa1")]), arr([s("value"), Value::I64(2)])]);
        assert_eq!(v, arr([ev]));
        Ok(())
    }
);

// A value that cannot be stored is a DbErr, not silence: a tree of `Array<Any>`
// holding a handle backed by Rust (a tree of `db::Db` is refused by the
// check, `lang::cast_trait::db_tree_of_a_handle_refused`).
run_with_tempdir!(
    name: db_unstorable_value,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, Array<Any>> = db::tree(db, null)$;
        is_err(db::insert(t, "k", [db]))
    }}"#,
    setup: |td| { td.path().join("unstorable.db") },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v, Value::Bool(true));
        Ok(())
    }
);

// A tree's types are stored with their typedefs expanded, so a program
// whose typedef differs is refused; the stored text is the db format.
run_with_tempdir!(
    name: db_typedef_types_stored_expanded,
    code: r#"{{
        let db = db::open("{}")$;
        let wrote = {{
            type Rec = {{x: i64, label: string}};
            let t: db::Tree<string, Array<Rec>> = db::tree(db, "arr")$;
            db::insert(t, "k", [{{x: 42, label: "a"}}])$
        }};
        let refused = {{
            type Rec = {{x: string, label: string}};
            let t: Result<db::Tree<string, Array<Rec>>, `DbErr(string)> = db::tree(db, wrote ~ "arr");
            is_err(t)
        }};
        (refused, db::get_type(db, wrote ~ "arr")$)
    }}"#,
    setup: |td| { td.path().join("typedef.db") },
    expect: |v: Value| -> Result<()> {
        let e = arr([Value::Bool(true), arr([s("string"), s("Array<{ label: string, x: i64 }>")])]);
        assert_eq!(v, e);
        Ok(())
    }
);

// Opening an open database hands back the live handle; sled's lock would
// refuse a second.
run_with_tempdir!(
    name: db_open_twice,
    code: r#"{{
        let p = "{}";
        let a = db::open(p);
        let b = db::open(a ~ p);
        (is_err(a), is_err(b))
    }}"#,
    setup: |td| { td.path().join("twice.db") },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v, arr([Value::Bool(false), Value::Bool(false)]));
        Ok(())
    }
);

// The last handle of one database drops while a transaction on another is
// open: the drop must not wait on the transaction's lock in the cycle.
run_with_tempdir!(
    name: db_drop_under_open_txn,
    code: r#"{{
        let dba = db::open("{0}/a.db")$;
        let ta: db::Tree<string, i64> = db::tree(dba, "a")$;
        let pre = db::insert(ta, "x", 1)$;
        let txn = db::txn::begin(pre ~ dba)$;
        let tt: db::txn::TxnTree<string, i64> = db::txn::tree(txn, txn ~ "a")$;
        let ins = db::txn::insert(tt, "k", 7)$;
        let pb = "{0}/b1.db";
        pb <- ins ~ "{0}/b2.db";
        let opened = count(db::open(pb)$);
        let c = db::txn::commit(select opened {{ 2 => txn, _ => never() }})$;
        c ~ "committed"
    }}"#,
    setup: |td| { td.path() },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v, Value::String("committed".into()));
        Ok(())
    }
);

// A commit is on disk when it answers: a copy of the files taken then holds it.
run_with_tempdir!(
    name: db_commit_is_durable,
    code: r#"{{
        let db = db::open("{0}/db")$;
        let t: db::Tree<string, i64> = db::tree(db, "t")$;
        let txn = db::txn::begin(t ~ db)$;
        let tt: db::txn::TxnTree<string, i64> = db::txn::tree(txn, txn ~ "t")$;
        db::txn::commit(db::txn::insert(tt, "k", 42)$ ~ txn)
    }}"#,
    setup: |td| { td.path() },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v, Value::Null);
        Ok(())
    },
    verify: |td| {
        // Windows locks byte ranges of an open database: its files cannot
        // be read until it closes
        if cfg!(not(windows)) {
            let src = td.path().join("db");
            let dst = td.path().join("copy");
            copy_dir(&src, &dst)?;
            let t = sled::open(&dst)?.open_tree("t")?;
            assert_eq!(t.len(), 1, "the commit is not in the files");
        }
    }
);

fn copy_dir(src: &std::path::Path, dst: &std::path::Path) -> std::io::Result<()> {
    std::fs::create_dir_all(dst)?;
    for e in std::fs::read_dir(src)? {
        let e = e?;
        let to = dst.join(e.file_name());
        if e.file_type()?.is_dir() {
            copy_dir(&e.path(), &to)?;
        } else {
            std::fs::copy(e.path(), to)?;
        }
    }
    Ok(())
}

// A subscription read in an arm that sleeps and wakes, or made in one,
// delivers the inserts after the wake.
run_with_tempdir!(
    name: db_subscription_hears_after_a_wake,
    code: r#"{{
        let db = db::open("{0}/db")$;
        let t: db::Tree<string, i64> = db::tree(db, "a")?;
        let sub = db::subscription::new(t);
        let on = true;
        on <- sys::time::timer(duration:200.ms, false) ~ false;
        on <- sys::time::timer(duration:400.ms, false) ~ true;
        let i1 = db::insert(t, sys::time::timer(duration:100.ms, false) ~ "a", 1)$;
        let i2 = db::insert(t, sys::time::timer(duration:600.ms, false) ~ "b", 2)$;
        let hits = select on {{ true => db::subscription::on_insert(sub)?, false => [] }};
        let b = array::filter(hits, |e| e.key == "b");
        select array::len(b) {{ 0 => never(), _ => b[0]$.value }}
    }}"#,
    setup: |td| { td.path() },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v, Value::I64(2));
        Ok(())
    }
);

run_with_tempdir!(
    name: db_subscription_made_in_an_arm_hears_after_a_wake,
    code: r#"{{
        let db = db::open("{0}/db")$;
        let t: db::Tree<string, i64> = db::tree(db, "a")?;
        let pre: [string, null] = null;
        let on = true;
        on <- sys::time::timer(duration:200.ms, false) ~ false;
        on <- sys::time::timer(duration:400.ms, false) ~ true;
        let i1 = db::insert(t, sys::time::timer(duration:100.ms, false) ~ "a", 1)$;
        let i2 = db::insert(t, sys::time::timer(duration:600.ms, false) ~ "b", 2)$;
        let hits = select on {{
            true => db::subscription::on_insert(db::subscription::new(#prefix: pre, t))?,
            false => []
        }};
        let b = array::filter(hits, |e| e.key == "b");
        select array::len(b) {{ 0 => never(), _ => b[0]$.value }}
    }}"#,
    setup: |td| { td.path() },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v, Value::I64(2));
        Ok(())
    }
);

// An open transaction locks nothing: plain ops run while it is open, and
// its write is invisible to them until the commit.
run_with_tempdir!(
    name: db_txn_open_locks_nothing,
    code: r#"{{
        let db = db::open("{0}/x.db")$;
        let t: db::Tree<string, i64> = db::tree(db, "a")$;
        let pre = db::insert(t, "x", 1)$;
        let txn = db::txn::begin(pre ~ db)$;
        let tt: db::txn::TxnTree<string, i64> = db::txn::tree(txn, txn ~ "a")$;
        let ins = db::txn::insert(tt, "k", 7)$;
        let before = db::get(t, ins ~ "k")$;
        let x = db::get(t, before ~ "x")$;
        let c = db::txn::commit(x ~ txn)$;
        let after = db::get(t, c ~ "k")$;
        after ~ [before, x, after]
    }}"#,
    setup: |td| { td.path() },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v, arr([Value::Null, Value::I64(1), Value::I64(7)]));
        Ok(())
    }
);

// A key the transaction read changes before the commit: the commit is a
// `Conflict and writes nothing.
run_with_tempdir!(
    name: db_txn_conflict,
    code: r#"{{
        let db = db::open("{0}/x.db")$;
        let t: db::Tree<string, i64> = db::tree(db, "a")$;
        let pre = db::insert(t, "x", 1)$;
        let txn = db::txn::begin(pre ~ db)$;
        let tt: db::txn::TxnTree<string, i64> = db::txn::tree(txn, txn ~ "a")$;
        let r = db::txn::get(tt, tt ~ "x")$;
        let w = db::insert(t, r ~ "x", 2)$;
        let ins = db::txn::insert(tt, w ~ "y", 3)$;
        let c = db::txn::commit(ins ~ txn);
        let y = db::get(t, c ~ "y")$;
        y ~ (c, y)
    }}"#,
    setup: |td| { td.path() },
    expect: |v: Value| -> Result<()> {
        let conflict = Value::Error(Value::String("Conflict".into()).into());
        assert_eq!(v, arr([conflict, Value::Null]));
        Ok(())
    }
);

// Two transactions on two databases, interleaved: write A, write B,
// commit A, commit B. Both commit.
run_with_tempdir!(
    name: db_txn_interleaved_databases,
    code: r#"{{
        let da = db::open("{0}/a.db")$;
        let dbb = db::open("{0}/b.db")$;
        let ta: db::Tree<string, i64> = db::tree(da, "t")$;
        let tb: db::Tree<string, i64> = db::tree(dbb, "t")$;
        let xa = db::txn::begin(ta ~ da)$;
        let xb = db::txn::begin(tb ~ dbb)$;
        let tta: db::txn::TxnTree<string, i64> = db::txn::tree(xa, xa ~ "t")$;
        let ttb: db::txn::TxnTree<string, i64> = db::txn::tree(xb, xb ~ "t")$;
        let ia = db::txn::insert(tta, "k", 1)$;
        let ib = db::txn::insert(ttb, ia ~ "k", 2)$;
        let ca = db::txn::commit(ib ~ xa)$;
        let cb = db::txn::commit(ca ~ xb)$;
        let va = db::get(ta, cb ~ "k")$;
        let vb = db::get(tb, va ~ "k")$;
        vb ~ [va, vb]
    }}"#,
    setup: |td| { td.path() },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v, arr([Value::I64(1), Value::I64(2)]));
        Ok(())
    }
);

// on_change carries inserts and removes in one stream, in order, as the
// ops db::batch takes.
run_with_tempdir!(
    name: db_subscription_on_change,
    code: r#"{{
        let db = db::open("{0}/x.db")$;
        let t: db::Tree<string, i64> = db::tree(db, null)$;
        let sub = db::subscription::new(t);
        let changes = db::subscription::on_change(sub)$;
        let log = [];
        log <- changes ~ array::concat(log, changes);
        let a = db::insert(t, sub ~ "a", 1)$;
        let r = db::remove(t, a ~ "a")$;
        let b = db::insert(t, r ~ "b", 2)$;
        sys::time::after_idle(duration:200.ms, b ~ log) ~ log
    }}"#,
    setup: |td| { td.path() },
    expect: |v: Value| -> Result<()> {
        let ins = |k: &str, v: i64| arr([Value::String("Insert".into()), Value::String(k.into()), Value::I64(v)]);
        let rm = |k: &str| arr([Value::String("Remove".into()), Value::String(k.into())]);
        assert_eq!(v, arr([ins("a", 1), rm("a"), ins("b", 2)]));
        Ok(())
    }
);

run_with_tempdir!(
    name: db_cursor_tagged_prefix,
    code: r#"{{
        let d = db::open("{}")?;
        seq {{
            let t: db::Tree<[string, i64], i64> = db::tree(d, "strings")?;
            db::batch(t, [`Insert("aa1", 1), `Insert("aa2", 2), `Insert("ab", 3)])?;
            let prefix: [string, i64] = "aa";
            let c = db::cursor::new(#prefix: prefix, t);
            let strings = db::cursor::read_many(c, 10)?;
            let a: db::Tree<Array<string>, i64> = db::tree(d, "arrays")?;
            db::batch(a, [`Insert(["x", "a"], 1), `Insert(["x", "b"], 2), `Insert(["xy"], 3)])?;
            let c = db::cursor::new(#prefix: ["x"], a);
            let arrays = db::cursor::read_many(c, 10)?;
            (array::map(strings, |(_, v)| v), array::map(arrays, |(_, v)| v))
        }}
    }}"#,
    setup: |td| { td.path().join("tagged_prefix.db") },
    expect: |v: Value| -> Result<()> {
        let entries = Value::Array([Value::I64(1), Value::I64(2)].into());
        assert_eq!(v, Value::Array([entries.clone(), entries].into()));
        Ok(())
    }
);

run_with_tempdir!(
    name: db_subscription_tagged_prefix,
    code: r#"{{
        let d = db::open("{}")?;
        let t: db::Tree<[string, i64], i64> = db::tree(d, "strings")?;
        let prefix: [string, i64] = "aa";
        let sub = db::subscription::new(#prefix: prefix, t);
        db::insert(sub ~ t, "aa1", 1)?;
        db::subscription::on_insert(sub)?
    }}"#,
    setup: |td| { td.path().join("tagged_subscription.db") },
    expect: |v: Value| -> Result<()> {
        let Value::Array(events) = v else { panic!("expected events") };
        assert_eq!(events.len(), 1);
        assert_eq!(events[0], Value::Array([
            Value::Array([Value::from("key"), Value::from("aa1")].into()),
            Value::Array([Value::from("value"), Value::I64(1)].into()),
        ].into()));
        Ok(())
    }
);

run_with_tempdir!(
    name: db_subscription_identical_inputs_keep_identity,
    code: r#"{{
        let d = db::open("{}")?;
        let t: db::Tree<string, i64> = db::tree(d, null)?;
        let prefix: [string, null] = null;
        let sub = db::subscription::new(#prefix: prefix, t);
        let first = once(sub);
        prefix <- first ~ null;
        sys::time::after_idle(duration:50.ms, first == sub)
    }}"#,
    setup: |td| { td.path().join("stable_subscription.db") },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v, Value::Bool(true));
        Ok(())
    }
);

// An async op in an arm runs on its trigger's fire, never again at the
// arm's wake: one fire of `b` pops one entry, though the arm sleeps and
// wakes before the count.
run_with_tempdir!(
    name: db_op_not_repeated_at_wake,
    code: r#"{{
        let d = db::open("{}")?;
        let t: db::Tree<i64, i64> = db::tree(d, null)?;
        let b = db::batch(t, [`Insert(1, 1), `Insert(2, 2), `Insert(3, 3)])?;
        let on = true;
        on <- sys::time::after_idle(duration:100.ms, b) ~ false;
        on <- sys::time::after_idle(duration:200.ms, b) ~ true;
        let popped = select on {{ true => db::pop_min(b ~ t)?, false => never() }};
        db::len(sys::time::after_idle(duration:400.ms, popped) ~ t)?
    }}"#,
    setup: |td| { td.path().join("pop_wake.db") },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v, Value::U64(2));
        Ok(())
    }
);

run_with_tempdir!(
    name: db_subscription_accessors_do_not_replay_on_wake,
    code: r#"{{
        let d = db::open("{}")?;
        let t: db::Tree<string, i64> = db::tree(d, null)?;
        let sub = db::subscription::new(t);
        let on = true;
        let inserts = 0;
        inserts <- count(select on {{ true => db::subscription::on_insert(sub)?, false => never() }});
        let removes = 0;
        removes <- count(select on {{ true => db::subscription::on_remove(sub)?, false => never() }});
        let changes = 0;
        changes <- count(select on {{ true => db::subscription::on_change(sub)?, false => never() }});
        seq {{
            sub;
            db::insert(t, "k", 1)?;
            until live(inserts == 1);
            db::remove(t, "k")?;
            until live(removes == 1 && changes == 2);
            on <- false;
            sys::time::timer(duration:20.ms, false);
            on <- true;
            sys::time::after_idle(duration:50.ms, on);
            (inserts, removes, changes)
        }}
    }}"#,
    setup: |td| { td.path().join("accessor_wake.db") },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v, arr([Value::I64(1), Value::I64(1), Value::I64(2)]));
        Ok(())
    }
);

// A tree stores an abstract value as the data its `Cast` writes and reads
// it back through `from_value`; a tree holding other data refuses to open
// as one of `Sorted`, so nothing reads it as one.
run_with_tempdir!(
    name: db_abstract_round_trip_through_cast,
    code: r#"{{
        type Sorted = Abstract<Array<i64>>;
        impl Cast for Sorted {{
            let from_value = |v| select cast<Array<i64>>(v) {{
                error as e => e,
                a => select array::sort(a) == a {{
                    true => Sorted(a),
                    false => error(`InvalidCast("not sorted"))
                }}
            }}
        }};
        let db = db::open("{}")$;
        let t: db::Tree<string, Sorted> = db::tree(db, "sorted")$;
        let raw: db::Tree<string, Array<i64>> = db::tree(db, "raw")$;
        let a = db::insert(t, "a", Sorted([1, 3]));
        let b = db::insert(raw, "b", [3, 1]);
        let r: Result<db::Tree<string, Sorted>, `DbErr(string)> = db::tree(db, b ~ "raw");
        ("[db::get(t, a ~ "a")]", is_err(r))
    }}"#,
    setup: |td| {
        td.path().join("test_abstract.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(
            v.to_string(),
            r#"["Sorted([1, 3])", true]"#
        );
        Ok(())
    }
);

// Keys keep their forms: `A and "A" are two keys, and a bare variant
// orders before a payload variant as `<` orders them.
run_with_tempdir!(
    name: db_keys_differ_as_values,
    code: r#"{{
        type K = [string, `A, `B(i64)];
        let s: K = "A";
        let a: K = `A;
        let b: K = `B(1);
        let db = db::open("{}")$;
        let t: db::Tree<K, i64> = db::tree(db, null)$;
        let u: db::Tree<[`A, `B(i64)], i64> = db::tree(db, "variants")$;
        seq {{
            let _ = db::insert(t, s, 1)$;
            let _ = db::insert(t, a, 2)$;
            let _ = db::insert(u, `B(1), 3)$;
            let _ = db::insert(u, `A, 4)$;
            (db::len(t)$, db::get(t, s)$, db::get(t, a)$, db::first(u)$)
        }}
    }}"#,
    setup: |td| {
        td.path().join("test_key_forms.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[u64:2, i64:1, i64:2, ["A", i64:4]]"#);
        Ok(())
    }
);

// A cursor yields a union's keys in `array::sort`'s order, each in its
// own form: the tuple (1, 2) and the list [<1, 2>] are two keys.
run_with_tempdir!(
    name: db_keys_order_as_values,
    code: r#"{{
        type K = [List<i64>, (i64, i64), {{a: i64, b: string}}, `A(i64), `B];
        let ks: Array<K> = [
            [<1>], [<1, 2>], [<0, 5>], [<>], (1, 2), (0, 9),
            {{a: 1, b: "x"}}, {{a: 0, b: "y"}}, `A(3), `B, `A(1)
        ];
        let db = db::open("{}")$;
        let t: db::Tree<K, i64> = db::tree(db, null)$;
        let list: K = [<1>];
        let tuple: K = (1, 2);
        seq {{
            let _ = db::batch(t, array::map(ks, |k| `Insert(k, 0)))$;
            let read = db::cursor::read_many(db::cursor::new(t), 100)$;
            let keys = array::map(read, |(k, _)| k);
            let prefixed = db::cursor::read_many(db::cursor::new(#prefix: list, t), 100)$;
            let exact = db::cursor::read_many(db::cursor::new(#prefix: tuple, t), 100)$;
            (keys == array::sort(ks), array::len(prefixed), array::len(exact))
        }}
    }}"#,
    setup: |td| {
        td.path().join("test_key_order.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[true, i64:2, i64:1]"#);
        Ok(())
    }
);

// Dropping a tree ends its subscriptions: on_change delivers the insert
// made before the drop, then one DbErr.
run_with_tempdir!(
    name: db_drop_tree_ends_subscriptions,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<string, i64> = db::tree(db, "t")$;
        let sub = db::subscription::new(t);
        let ch = db::subscription::on_change(sub);
        let _ = db::insert(t, sub ~ "a", 1)$;
        let heard = select ch {{ error as _ => never(), _ => true }};
        let _ = db::drop_tree(db, heard ~ "t")$;
        select ch {{ error as e => e, _ => never() }}
    }}"#,
    setup: |td| {
        td.path().join("test_drop_subscribed.db")
    },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"error:["DbErr", "the tree was dropped"]"#);
        Ok(())
    }
);

// A call whose arguments are not data answers its error without losing
// a reply that lands in the same cycle: the insert finishes while its arm
// sleeps, and the wake both answers it and catches up a value whose Cast
// writes a function. The reply comes first, the error next cycle.
run_with_tempdir!(
    name: db_not_data_keeps_the_reply_of_its_cycle,
    code: r#"{{
        type V = Abstract<i64>;
        impl Cast for V {{
            let to_value = |v: V| -> Any select v.0 {{ 0 => |y: i64| y, n => n }}
        }};
        let d = db::open("{}")?;
        let t: db::Tree<string, V> = db::tree(d, null)?;
        let v = V(1);
        let on = true;
        let r = select on {{ true => db::insert(t, "k", v), false => never() }};
        let off = t ~ false;
        on <- off;
        v <- off ~ V(0);
        let landed = db::get(t, sys::time::timer(duration: 10.ms, true) ~ "k")?;
        on <- once(select landed {{ null as _ => never(), _ => true }});
        let log: Array<string> = [];
        log <- r ~ array::push(log, "[r]");
        select r {{ error as e => e ~ log, _ => never() }}
    }}"#,
    setup: |td| { td.path().join("not_data_wake.db") },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"["null"]"#);
        Ok(())
    }
);

// A db op runs on a blocking thread no abort stops, so an op whose arm
// sleeps while it runs finishes asleep and answers at the wake: the
// popped entry, and a commit's outcome, are not lost.
run_with_tempdir!(
    name: db_pop_answers_at_the_wake,
    code: r#"{{
        let d = db::open("{}")?;
        let t: db::Tree<i64, string> = db::tree(d, null)?;
        let ins = db::insert(t, 1, "one")$;
        let on = true;
        let popped = select on {{ true => db::pop_min(ins ~ t)?, false => never() }};
        on <- ins ~ false;
        on <- on ~ select on {{ false => true, true => never() }};
        (popped, db::len(popped ~ t)?)
    }}"#,
    setup: |td| { td.path().join("pop_asleep.db") },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[[i64:1, "one"], u64:0]"#);
        Ok(())
    }
);

run_with_tempdir!(
    name: db_txn_commit_answers_at_the_wake,
    code: r#"{{
        let d = db::open("{}")?;
        let t: db::Tree<string, i64> = db::tree(d, "txn")?;
        let txn = db::txn::begin(t ~ d)?;
        let tt = db::txn::tree(txn, txn ~ "txn")?;
        let ins = db::txn::insert(tt, "k1", 42)?;
        let on = true;
        let committed = select on {{ true => db::txn::commit(ins ~ txn), false => never() }};
        on <- ins ~ false;
        on <- on ~ select on {{ false => true, true => never() }};
        (committed, db::get(t, committed ~ "k1")?)
    }}"#,
    setup: |td| { td.path().join("commit_asleep.db") },
    expect: |v: Value| -> Result<()> {
        assert_eq!(v.to_string(), r#"[null, i64:42]"#);
        Ok(())
    }
);
