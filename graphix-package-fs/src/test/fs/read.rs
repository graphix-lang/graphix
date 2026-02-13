use crate::test::{REGISTER, ROOT};
use anyhow::Result;
use graphix_package_core::run_with_tempdir;
use netidx::subscriber::Value;
use tokio::fs;

run_with_tempdir! {
    name: test_read_all_basic,
    code: r#"fs::read_all("{}")"#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test.txt");
        let content = "Hello, World!";
        fs::write(&test_file, content).await?;
        test_file
    },
    expect: |v: Value| -> Result<()> {
        if let Value::String(s) = v {
            assert_eq!(&*s, "Hello, World!");
            Ok(())
        } else {
            panic!("expected String value, got: {v:?}")
        }
    }
}

run_with_tempdir! {
    name: test_read_all_nonexistent,
    code: r#"fs::read_all("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent.txt")
    },
    expect_error
}

run_with_tempdir! {
    name: test_read_all_utf8,
    code: r#"fs::read_all("{}")"#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("utf8.txt");
        let content = "Hello, 世界! 🦀";
        fs::write(&test_file, content).await?;
        test_file
    },
    expect: |v: Value| -> Result<()> {
        if let Value::String(s) = v {
            assert_eq!(&*s, "Hello, 世界! 🦀");
            Ok(())
        } else {
            panic!("expected String value, got: {v:?}")
        }
    }
}

run_with_tempdir! {
    name: test_read_all_bin_basic,
    code: r#"fs::read_all_bin("{}")"#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test.bin");
        let content = b"Binary data \x00\x01\x02\xff";
        fs::write(&test_file, content).await?;
        test_file
    },
    expect: |v: Value| -> Result<()> {
        if let Value::Bytes(b) = v {
            assert_eq!(b.as_ref(), b"Binary data \x00\x01\x02\xff");
            Ok(())
        } else {
            panic!("expected Bytes value, got: {v:?}")
        }
    }
}

run_with_tempdir! {
    name: test_read_all_bin_nonexistent,
    code: r#"fs::read_all_bin("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent.bin")
    },
    expect_error
}
