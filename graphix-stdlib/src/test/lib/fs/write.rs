use crate::test::{escape_path, init};
use anyhow::Result;
use arcstr::ArcStr;
use graphix_rt::GXEvent;
use netidx::subscriber::Value;
use poolshark::global::GPooled;
use tokio::{fs, sync::mpsc, time::Duration};

/// Macro to create fs::write_* tests with common setup/teardown logic
macro_rules! write_test {
    // Error expectation case - delegates to main pattern
    (
        name: $test_name:ident,
        function: $func:expr,
        content: $content:expr,
        setup: |$temp_dir:ident| $setup:block,
        expect_error
    ) => {
        write_test! {
            name: $test_name,
            function: $func,
            content: $content,
            setup: |$temp_dir| $setup,
            expect: |_v: Value| -> Result<()> {
                if matches!(_v, Value::Error(_)) {
                    Ok(())
                } else {
                    panic!("expected Error value, got: {_v:?}")
                }
            }
        }
    };
    // Success case with verification - delegates to main pattern
    (
        name: $test_name:ident,
        function: $func:expr,
        content: $content:expr,
        setup: |$temp_dir:ident| $setup:block,
        verify: |$verify_dir:ident| $verify:block
    ) => {
        write_test! {
            name: $test_name,
            function: $func,
            content: $content,
            setup: |$temp_dir| $setup,
            expect: |_v: Value| -> Result<()> {
                // Check write succeeded (returns Ok(null))
                if !matches!(_v, Value::Null) {
                    panic!("expected Null (success), got: {_v:?}");
                }
                // Verify file contents - need to recreate temp_dir reference
                Ok(())
            },
            verify: |$verify_dir| $verify
        }
    };
    // Main pattern with custom expectation and optional verification
    (
        name: $test_name:ident,
        function: $func:expr,
        content: $content:expr,
        setup: |$temp_dir:ident| $setup:block,
        expect: $expect_handler:expr
        $(, verify: |$verify_dir:ident| $verify:block)?
    ) => {
        #[tokio::test(flavor = "current_thread")]
        async fn $test_name() -> Result<()> {
            let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(10);
            let ctx = init(tx).await?;
            let $temp_dir = tempfile::tempdir()?;

            // Run setup block which should return test_file
            let test_file = { $setup };

            let code = format!(
                r#"{}(#path: "{}", {})"#,
                $func,
                escape_path(test_file.display()),
                $content
            );
            eprintln!("{code}");
            let compiled = ctx.rt.compile(ArcStr::from(code)).await?;
            let eid = compiled.exprs[0].id;

            let timeout = tokio::time::sleep(Duration::from_secs(2));
            tokio::pin!(timeout);

            loop {
                tokio::select! {
                    _ = &mut timeout => panic!("timeout waiting for result"),
                    Some(mut batch) = rx.recv() => {
                        for event in batch.drain(..) {
                            if let GXEvent::Updated(id, v) = event {
                                if id == eid {
                                    $expect_handler(v)?;
                                    $(
                                        let $verify_dir = &$temp_dir;
                                        $verify
                                    )?
                                    return Ok(());
                                }
                            }
                        }
                    }
                }
            }
        }
    };
}

write_test! {
    name: test_write_all_basic,
    function: "fs::write_all",
    content: r#""Hello, World!""#,
    setup: |temp_dir| {
        temp_dir.path().join("test.txt")
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("test.txt");
        let content = fs::read_to_string(&test_file).await?;
        assert_eq!(content, "Hello, World!");
    }
}

write_test! {
    name: test_write_all_overwrite_existing,
    function: "fs::write_all",
    content: r#""Overwritten content""#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("existing.txt");
        fs::write(&test_file, "Original content").await?;
        test_file
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("existing.txt");
        let content = fs::read_to_string(&test_file).await?;
        assert_eq!(content, "Overwritten content");
    }
}

write_test! {
    name: test_write_all_utf8,
    function: "fs::write_all",
    content: r#""Hello, 世界! 🦀""#,
    setup: |temp_dir| {
        temp_dir.path().join("utf8.txt")
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("utf8.txt");
        let content = fs::read_to_string(&test_file).await?;
        assert_eq!(content, "Hello, 世界! 🦀");
    }
}

write_test! {
    name: test_write_all_empty_string,
    function: "fs::write_all",
    content: r#""""#,
    setup: |temp_dir| {
        temp_dir.path().join("empty.txt")
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("empty.txt");
        let content = fs::read_to_string(&test_file).await?;
        assert_eq!(content, "");
    }
}

write_test! {
    name: test_write_all_bin_basic,
    function: "fs::write_all_bin",
    content: r#"bytes:SGVsbG8="#,
    setup: |temp_dir| {
        temp_dir.path().join("test.bin")
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("test.bin");
        let content = fs::read(&test_file).await?;
        assert_eq!(content, b"Hello");
    }
}

write_test! {
    name: test_write_all_bin_with_nulls,
    function: "fs::write_all_bin",
    content: r#"bytes:AAECqg=="#,
    setup: |temp_dir| {
        temp_dir.path().join("binary.bin")
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("binary.bin");
        let content = fs::read(&test_file).await?;
        assert_eq!(content, b"\x00\x01\x02\xaa");
    }
}

write_test! {
    name: test_write_all_bin_overwrite,
    function: "fs::write_all_bin",
    content: r#"bytes:AQI="#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("overwrite.bin");
        fs::write(&test_file, b"\x00\x00\x00\x00").await?;
        test_file
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("overwrite.bin");
        let content = fs::read(&test_file).await?;
        assert_eq!(content, b"\x01\x02");
    }
}

write_test! {
    name: test_write_all_invalid_path,
    function: "fs::write_all",
    content: r#""content""#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent_dir").join("test.txt")
    },
    expect_error
}
