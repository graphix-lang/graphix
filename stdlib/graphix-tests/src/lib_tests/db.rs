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
        let t = db::default(db)?;
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
        let t = db::default(db)?;
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
        let t = db::default(db)?;
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
        let t = db::default(db)?;
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
        let ins = db::insert_many(t, [
            (100, "hundred"),
            (-50, "neg fifty"),
            (0, "zero"),
            (-1, "neg one"),
            (50, "fifty")
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

// Batch insert, verify returned previous values and inferred types
run_with_tempdir!(
    name: db_insert_many,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::default(db)?;
        let ty = db::get_type(db, t ~ null)?;
        let first = db::insert(t, "a", 1)$;
        let pairs = first ~ [("a", 10), ("b", 20), ("c", 30)];
        let result = db::insert_many(t, pairs)?;
        (result, ty)
    }}"#,
    setup: |td| {
        td.path().join("test_insert_many.db")
    },
    expect: |v: Value| -> Result<()> {
        let outer = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        let arr = match &outer[0] { Value::Array(a) => a, _ => panic!("results not array: {:?}", outer[0]) };
        assert_eq!(arr.len(), 3, "expected 3 results, got {}", arr.len());
        // "a" had previous value 1
        assert!(matches!(&arr[0], Value::I64(1)), "expected I64(1) for 'a' prev, got: {:?}", arr[0]);
        // "b" and "c" are new
        assert!(matches!(&arr[1], Value::Null), "expected Null for 'b' prev, got: {:?}", arr[1]);
        assert!(matches!(&arr[2], Value::Null), "expected Null for 'c' prev, got: {:?}", arr[2]);
        assert_tree_type(&outer[1], "string", "i64");
        Ok(())
    }
);

// Batch get, verify order, null for missing, and inferred types
run_with_tempdir!(
    name: db_get_many,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::default(db)?;
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

// Batch remove, verify returned previous values and inferred types
run_with_tempdir!(
    name: db_remove_many,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::default(db)?;
        let ty = db::get_type(db, t ~ null)?;
        let a = db::insert(t, "p", 100)$;
        let b = db::insert(t, a ~ "q", 200)$;
        let keys = b ~ ["q", "nonexistent", "p"];
        let result = db::remove_many(t, keys)?;
        (result, ty)
    }}"#,
    setup: |td| {
        td.path().join("test_remove_many.db")
    },
    expect: |v: Value| -> Result<()> {
        let outer = match &v { Value::Array(a) => a, _ => panic!("not array: {v:?}") };
        let arr = match &outer[0] { Value::Array(a) => a, _ => panic!("results not array: {:?}", outer[0]) };
        assert_eq!(arr.len(), 3);
        assert!(matches!(&arr[0], Value::I64(200)), "expected I64(200), got: {:?}", arr[0]);
        assert!(matches!(&arr[1], Value::Null), "expected Null, got: {:?}", arr[1]);
        assert!(matches!(&arr[2], Value::I64(100)), "expected I64(100), got: {:?}", arr[2]);
        assert_tree_type(&outer[1], "string", "i64");
        Ok(())
    }
);

// Cursor read_many, verify short array on exhaustion and inferred types
run_with_tempdir!(
    name: db_cursor_read_many,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::default(db)?;
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
        let r1 = db::tree(db, "$$__graphix_default__$$");
        let r2 = db::tree(db, r1 ~ "$$__graphix_meta__$$");
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
        let t1: db::Tree<string, i64> = db::default(db)?;
        let ins = db::insert(t1, "x", 1)$;
        let t2: [db::Tree<i64, string>, Error<`DbErr(string)>] = db::default(ins ~ db);
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
        let t: db::Tree<string, i64> = db::default(db)?;
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
