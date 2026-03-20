use anyhow::Result;
use graphix_package_core::run_with_tempdir;
use netidx::subscriber::Value;

run_with_tempdir!(
    name: db_open,
    code: r#"{{
        let db = db::open("{}")$;
        db::flush(db)$;
        null
    }}"#,
    setup: |td| {
        td.path().join("test_open.db")
    },
    expect: |v: Value| -> Result<()> {
        assert!(matches!(v, Value::Null), "expected Null, got: {v:?}");
        Ok(())
    }
);

run_with_tempdir!(
    name: db_insert_get,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::default(db);
        let old = db::insert(t, "hello", 42)$;
        db::get(t, old ~ "hello")?
    }}"#,
    setup: |td| {
        td.path().join("test_insert_get.db")
    },
    expect: |v: Value| -> Result<()> {
        assert!(matches!(v, Value::I64(42)), "expected I64(42), got: {v:?}");
        Ok(())
    }
);

run_with_tempdir!(
    name: db_get_missing,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::default(db);
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
        let t = db::default(db);
        let old = db::insert(t, "key", 99)$;
        db::remove(t, old ~ "key")?
    }}"#,
    setup: |td| {
        td.path().join("test_remove.db")
    },
    expect: |v: Value| -> Result<()> {
        assert!(matches!(v, Value::I64(99)), "expected I64(99), got: {v:?}");
        Ok(())
    }
);

run_with_tempdir!(
    name: db_contains_key,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::default(db);
        let old = db::insert(t, "exists", 1)$;
        db::contains_key(t, old ~ "exists")?
    }}"#,
    setup: |td| {
        td.path().join("test_contains.db")
    },
    expect: |v: Value| -> Result<()> {
        assert!(matches!(v, Value::Bool(true)), "expected Bool(true), got: {v:?}");
        Ok(())
    }
);

run_with_tempdir!(
    name: db_named_tree,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::tree(db, "mytree")?;
        let old = db::insert(t, "x", "hello")$;
        db::get(t, old ~ "x")?
    }}"#,
    setup: |td| {
        td.path().join("test_named_tree.db")
    },
    expect: |v: Value| -> Result<()> {
        match &v {
            Value::String(s) if &**s == "hello" => Ok(()),
            _ => panic!("expected String(\"hello\"), got: {v:?}"),
        }
    }
);

// Insert i64 keys including negatives, iterate via cursor, verify sorted order
run_with_tempdir!(
    name: db_integer_key_order,
    code: r#"{{
        let db = db::open("{}")$;
        let t: db::Tree<i64, string> = db::tree(db, "ints")?;
        let a = db::insert(t, 100, "hundred")$;
        let b = db::insert(t, a ~ -50, "neg fifty")$;
        let c = db::insert(t, b ~ 0, "zero")$;
        let d = db::insert(t, c ~ -1, "neg one")$;
        let e = db::insert(t, d ~ 50, "fifty")$;
        let cursor = db::cursor::new(e ~ t);
        let r1 = db::cursor::read(cursor, cursor)$;
        let r2 = db::cursor::read(cursor, r1)$;
        let r3 = db::cursor::read(cursor, r2)$;
        let r4 = db::cursor::read(cursor, r3)$;
        let r5 = db::cursor::read(cursor, r4)$;
        [r1, r2, r3, r4, r5]
    }}"#,
    setup: |td| {
        td.path().join("test_int_order.db")
    },
    expect: |v: Value| -> Result<()> {
        let arr = match &v {
            Value::Array(a) => a,
            _ => panic!("expected Array, got: {v:?}"),
        };
        // Expected order: -50, -1, 0, 50, 100
        let expected_keys: &[i64] = &[-50, -1, 0, 50, 100];
        for (i, expected) in expected_keys.iter().enumerate() {
            let entry = &arr[i];
            match entry {
                Value::Array(pair) => match &pair[0] {
                    Value::I64(k) => assert_eq!(*k, *expected, "key {i} mismatch: got {k}, expected {expected}"),
                    other => panic!("key {i}: expected I64, got {other:?}"),
                },
                other => panic!("entry {i}: expected (key, val) tuple array, got {other:?}"),
            }
        }
        Ok(())
    }
);

// Insert string keys, use cursor with prefix filter
run_with_tempdir!(
    name: db_cursor_prefix,
    code: r#"{{
        let db = db::open("{}")$;
        let t = db::tree(db, "pfx")?;
        let a = db::insert(t, "aaa", 1)$;
        let b = db::insert(t, a ~ "aab", 2)$;
        let c = db::insert(t, b ~ "bbb", 3)$;
        let cursor = db::cursor::new(#prefix: "aa", c ~ t);
        let r1 = db::cursor::read(cursor, cursor)$;
        let r2 = db::cursor::read(cursor, r1)$;
        let r3 = db::cursor::read(cursor, r2)$;
        [r1, r2, r3]
    }}"#,
    setup: |td| {
        td.path().join("test_cursor_prefix.db")
    },
    expect: |v: Value| -> Result<()> {
        let arr = match &v {
            Value::Array(a) => a,
            _ => panic!("expected Array, got: {v:?}"),
        };
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
        Ok(())
    }
);
