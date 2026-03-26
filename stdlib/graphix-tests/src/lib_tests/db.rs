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
        db::flush(db$)$;
        is_err(db)
    }}"#,
    setup: |td| {
        td.path().join("test_open.db")
    },
    expect: |v: Value| -> Result<()> {
        assert!(matches!(v, Value::Bool(false)), "expected open+flush to succeed, got: {v:?}");
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert!(matches!(&arr[0], Value::I64(42)), "expected I64(42), got: {:?}", arr[0]);
        assert_tree_type(&arr[1], "string", "i64");
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
        assert!(matches!(v, Value::Null), "expected Null, got: {v:?}");
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert!(matches!(&arr[0], Value::I64(99)), "expected I64(99), got: {:?}", arr[0]);
        assert_tree_type(&arr[1], "string", "i64");
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert!(matches!(&arr[0], Value::Bool(true)), "expected Bool(true), got: {:?}", arr[0]);
        assert_tree_type(&arr[1], "string", "i64");
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        match &arr[0] {
            Value::String(s) if &**s == "hello" => (),
            _ => panic!("expected String(\"hello\"), got: {:?}", arr[0]),
        }
        assert_tree_type(&arr[1], "string", "string");
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
        let outer = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        let arr = match &outer[0] { Value::Array(a) => a, _ => panic!("results not array: {:?}", outer[0]) };
        // 5 entries returned, read_many(6) should not over-read
        assert_eq!(arr.len(), 5, "expected 5 entries from read_many(6), got {}", arr.len());
        // Expected order: -50, -1, 0, 50, 100
        let expected: &[(i64, &str)] = &[
            (-50, "neg fifty"), (-1, "neg one"), (0, "zero"), (50, "fifty"), (100, "hundred"),
        ];
        for (i, (ek, ev)) in expected.iter().enumerate() {
            match &arr[i] {
                Value::Array(pair) => {
                    match &pair[0] {
                        Value::I64(k) => assert_eq!(*k, *ek, "key {i} mismatch"),
                        other => panic!("key {i}: expected I64, got {other:?}"),
                    }
                    match &pair[1] {
                        Value::String(s) => assert_eq!(&**s, *ev, "val {i} mismatch"),
                        other => panic!("val {i}: expected String, got {other:?}"),
                    }
                }
                other => panic!("entry {i}: expected tuple array, got {other:?}"),
            }
        }
        assert_tree_type(&outer[1], "i64", "string");
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
        let outer = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        let arr = match &outer[0] { Value::Array(a) => a, _ => panic!("results not array: {:?}", outer[0]) };
        // Should get "aaa" and "aab" entries, then null
        match &arr[0] {
            Value::Array(pair) => match &pair[0] {
                Value::String(s) if &**s == "aaa" => (),
                other => panic!("entry 0 key: expected 'aaa', got {other:?}"),
            },
            other => panic!("entry 0: expected tuple array, got {other:?}"),
        }
        match &arr[1] {
            Value::Array(pair) => match &pair[0] {
                Value::String(s) if &**s == "aab" => (),
                other => panic!("entry 1 key: expected 'aab', got {other:?}"),
            },
            other => panic!("entry 1: expected tuple array, got {other:?}"),
        }
        assert!(matches!(arr[2], Value::Null), "entry 2: expected Null (end), got {:?}", arr[2]);
        assert_tree_type(&outer[1], "string", "i64");
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert!(matches!(&arr[0], Value::Null), "'a' should be removed, got: {:?}", arr[0]);
        assert!(matches!(&arr[1], Value::I64(2)), "'b' should be 2, got: {:?}", arr[1]);
        assert!(matches!(&arr[2], Value::I64(3)), "'c' should be 3, got: {:?}", arr[2]);
        assert!(matches!(&arr[3], Value::I64(4)), "'d' should be 4, got: {:?}", arr[3]);
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
        let outer = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        let arr = match &outer[0] { Value::Array(a) => a, _ => panic!("results not array: {:?}", outer[0]) };
        assert_eq!(arr.len(), 3);
        assert!(matches!(&arr[0], Value::I64(20)), "expected I64(20), got: {:?}", arr[0]);
        assert!(matches!(&arr[1], Value::Null), "expected Null for missing, got: {:?}", arr[1]);
        assert!(matches!(&arr[2], Value::I64(10)), "expected I64(10), got: {:?}", arr[2]);
        assert_tree_type(&outer[1], "string", "i64");
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert!(matches!(&arr[0], Value::Null), "empty first: {:?}", arr[0]);
        assert!(matches!(&arr[1], Value::Null), "empty last: {:?}", arr[1]);
        match &arr[2] {
            Value::Array(p) => assert!(matches!(&p[0], Value::I64(5)), "first key: {:?}", p[0]),
            other => panic!("first not tuple: {other:?}"),
        }
        match &arr[3] {
            Value::Array(p) => assert!(matches!(&p[0], Value::I64(20)), "last key: {:?}", p[0]),
            other => panic!("last not tuple: {other:?}"),
        }
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        match &arr[0] {
            Value::Array(p) => assert!(matches!(&p[0], Value::I64(5)), "pop_min: {:?}", p[0]),
            other => panic!("pop_min: {other:?}"),
        }
        match &arr[1] {
            Value::Array(p) => assert!(matches!(&p[0], Value::I64(20)), "pop_max: {:?}", p[0]),
            other => panic!("pop_max: {other:?}"),
        }
        match &arr[2] {
            Value::Array(p) => assert!(matches!(&p[0], Value::I64(10)), "remain: {:?}", p[0]),
            other => panic!("remain: {other:?}"),
        }
        assert!(matches!(&arr[3], Value::Null), "empty pop: {:?}", arr[3]);
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        match &arr[0] {
            Value::Array(p) => assert!(matches!(&p[0], Value::I64(10)), "lt: {:?}", p[0]),
            other => panic!("lt: {other:?}"),
        }
        match &arr[1] {
            Value::Array(p) => assert!(matches!(&p[0], Value::I64(30)), "gt: {:?}", p[0]),
            other => panic!("gt: {other:?}"),
        }
        assert!(matches!(&arr[2], Value::Null), "no_lt: {:?}", arr[2]);
        assert!(matches!(&arr[3], Value::Null), "no_gt: {:?}", arr[3]);
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert!(matches!(&arr[0], Value::Null), "cas1 success: {:?}", arr[0]);
        assert!(matches!(&arr[1], Value::Null), "cas2 success: {:?}", arr[1]);
        match &arr[2] {
            Value::Array(a) => {
                match &a[0] {
                    Value::String(s) if &**s == "Mismatch" => (),
                    other => panic!("cas3 tag: {other:?}"),
                }
                assert!(matches!(&a[1], Value::I64(100)), "cas3 current: {:?}", a[1]);
            }
            other => panic!("cas3: {other:?}"),
        }
        assert!(matches!(&arr[3], Value::I64(100)), "final: {:?}", arr[3]);
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert!(matches!(&arr[0], Value::Bool(true)), "empty: {:?}", arr[0]);
        assert!(matches!(&arr[1], Value::U64(0)), "len0: {:?}", arr[1]);
        assert!(matches!(&arr[2], Value::U64(3)), "len3: {:?}", arr[2]);
        assert!(matches!(&arr[3], Value::Bool(false)), "not_empty: {:?}", arr[3]);
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert!(matches!(&arr[0], Value::I64(10)), "x: {:?}", arr[0]);
        assert!(matches!(&arr[1], Value::I64(20)), "y: {:?}", arr[1]);
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
        let outer = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        let arr = match &outer[0] { Value::Array(a) => a, _ => panic!("results not array: {:?}", outer[0]) };
        assert_eq!(arr.len(), 3);
        // batch1: 2 entries
        let b1 = match &arr[0] { Value::Array(a) => a, _ => panic!("batch1 not array") };
        assert_eq!(b1.len(), 2, "batch1 should have 2 entries");
        // batch2: 1 entry (only "c" left)
        let b2 = match &arr[1] { Value::Array(a) => a, _ => panic!("batch2 not array") };
        assert_eq!(b2.len(), 1, "batch2 should have 1 entry");
        // batch3: 0 entries (exhausted)
        let b3 = match &arr[2] { Value::Array(a) => a, _ => panic!("batch3 not array") };
        assert_eq!(b3.len(), 0, "batch3 should be empty");
        assert_tree_type(&outer[1], "string", "i64");
        Ok(())
    }
);

// Verify get_type with explicit annotations and missing tree
run_with_tempdir!(
    name: db_get_type,
    code: r#"{{
        let db = db::open("{}")$;
        let t1: db::Tree<i64, string> = db::tree(db, "typed")?;
        let t2: db::Tree<string, bool> = db::tree(db, "other")?;
        let missing = db::get_type(db, t2 ~ "nonexistent")?;
        let ty1 = db::get_type(db, missing ~ "typed")?;
        let ty2 = db::get_type(db, ty1 ~ "other")?;
        [missing, ty1, ty2]
    }}"#,
    setup: |td| {
        td.path().join("test_get_type.db")
    },
    expect: |v: Value| -> Result<()> {
        let arr = match &v {
            Value::Array(a) => a,
            _ => panic!("expected Array, got: {v:?}"),
        };
        assert_eq!(arr.len(), 3);
        // missing tree returns null
        assert!(matches!(&arr[0], Value::Null), "expected Null for missing, got: {:?}", arr[0]);
        // typed tree: (i64, string)
        assert_tree_type(&arr[1], "i64", "string");
        // other tree: (string, bool)
        assert_tree_type(&arr[2], "string", "bool");
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert!(matches!(&arr[0], Value::Bool(true)), "expected $$__graphix_default__$$ to be rejected, got: {:?}", arr[0]);
        assert!(matches!(&arr[1], Value::Bool(true)), "expected $$__graphix_meta__$$ to be rejected, got: {:?}", arr[1]);
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert!(matches!(&arr[0], Value::Bool(false)), "first open should succeed, got: {:?}", arr[0]);
        assert!(matches!(&arr[1], Value::Bool(true)), "second open should fail, got: {:?}", arr[1]);
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert!(matches!(&arr[0], Value::Bool(false)), "first open should succeed, got: {:?}", arr[0]);
        assert!(matches!(&arr[1], Value::Bool(true)), "second open should fail, got: {:?}", arr[1]);
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert!(matches!(&arr[0], Value::I64(42)), "expected I64(42), got: {:?}", arr[0]);
        assert_tree_type(&arr[1], "string", "i64");
        Ok(())
    }
);

// ── Range query tests ─────────────────────────────────────────────

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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert_eq!(arr.len(), 2, "expected 2 entries, got {}", arr.len());
        let expected: &[(i64, &str)] = &[(5, "five"), (10, "ten")];
        for (i, (ek, ev)) in expected.iter().enumerate() {
            match &arr[i] {
                Value::Array(pair) => {
                    match &pair[0] {
                        Value::I64(k) => assert_eq!(*k, *ek, "key {i} mismatch"),
                        other => panic!("key {i}: expected I64, got {other:?}"),
                    }
                    match &pair[1] {
                        Value::String(s) => assert_eq!(&**s, *ev, "val {i} mismatch"),
                        other => panic!("val {i}: expected String, got {other:?}"),
                    }
                }
                other => panic!("entry {i}: expected tuple, got {other:?}"),
            }
        }
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert_eq!(arr.len(), 3, "expected 3 entries (1,5,10), got {}", arr.len());
        let expected: &[i64] = &[1, 5, 10];
        for (i, ek) in expected.iter().enumerate() {
            match &arr[i] {
                Value::Array(pair) => match &pair[0] {
                    Value::I64(k) => assert_eq!(*k, *ek, "key {i} mismatch"),
                    other => panic!("key {i}: expected I64, got {other:?}"),
                },
                other => panic!("entry {i}: expected tuple, got {other:?}"),
            }
        }
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert_eq!(arr.len(), 2, "expected 2 entries (15,20), got {}", arr.len());
        let expected: &[i64] = &[15, 20];
        for (i, ek) in expected.iter().enumerate() {
            match &arr[i] {
                Value::Array(pair) => match &pair[0] {
                    Value::I64(k) => assert_eq!(*k, *ek, "key {i} mismatch"),
                    other => panic!("key {i}: expected I64, got {other:?}"),
                },
                other => panic!("entry {i}: expected tuple, got {other:?}"),
            }
        }
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert_eq!(arr.len(), 0, "expected empty result, got {}", arr.len());
        Ok(())
    }
);

// ── Transaction tests ─────────────────────────────────────────────

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
        assert!(matches!(v, Value::I64(42)), "expected I64(42), got: {v:?}");
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
        assert!(matches!(v, Value::I64(100)), "expected I64(100) (original), got: {v:?}");
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert!(matches!(&arr[0], Value::I64(1)), "expected I64(1), got: {:?}", arr[0]);
        match &arr[1] {
            Value::String(s) if &**s == "hello" => (),
            other => panic!("expected String(\"hello\"), got: {other:?}"),
        }
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        assert!(matches!(&arr[0], Value::Null), "'a' should be removed, got: {:?}", arr[0]);
        assert!(matches!(&arr[1], Value::I64(2)), "'b' should be 2, got: {:?}", arr[1]);
        assert!(matches!(&arr[2], Value::I64(3)), "'c' should be 3, got: {:?}", arr[2]);
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
        // on_insert returns Array<{key: 'k, value: 'v}>
        let outer = match &v { Value::Array(a) => a, _ => panic!("expected array, got: {v:?}") };
        assert_eq!(outer.len(), 1, "expected 1 insert event, got: {}", outer.len());
        let s = match &outer[0] { Value::Array(a) => a, _ => panic!("expected struct, got: {:?}", outer[0]) };
        match &s[0] {
            Value::Array(kv) => match &kv[1] {
                Value::String(k) if &**k == "mykey" => (),
                other => panic!("expected key='mykey', got: {other:?}"),
            },
            other => panic!("expected key field, got: {other:?}"),
        }
        match &s[1] {
            Value::Array(kv) => assert!(matches!(&kv[1], Value::I64(99)), "expected value=99, got: {:?}", kv[1]),
            other => panic!("expected value field, got: {other:?}"),
        }
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
        // on_remove returns Array<{key: 'k}>
        let outer = match &v { Value::Array(a) => a, _ => panic!("expected array, got: {v:?}") };
        assert_eq!(outer.len(), 1, "expected 1 remove event, got: {}", outer.len());
        let s = match &outer[0] { Value::Array(a) => a, _ => panic!("expected struct, got: {:?}", outer[0]) };
        match &s[0] {
            Value::Array(kv) => match &kv[1] {
                Value::String(k) if &**k == "delme" => (),
                other => panic!("expected key='delme', got: {other:?}"),
            },
            other => panic!("expected key field, got: {other:?}"),
        }
        Ok(())
    }
);

// Both on_insert and on_remove active on the same subscription.
// Tests that the event is not consumed by the first handler (the fix
// for scan_db_events using get instead of remove).
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
        let arr = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        match &arr[0] {
            Value::String(s) if &**s == "shared" => (),
            other => panic!("expected 'shared' from on_insert, got: {other:?}"),
        }
        match &arr[1] {
            Value::String(s) if &**s == "shared" => (),
            other => panic!("expected 'shared' from on_remove, got: {other:?}"),
        }
        Ok(())
    }
);
