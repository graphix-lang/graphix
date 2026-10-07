use graphix_package_core::run_with_tempdir;
use tokio::fs;

run_with_tempdir! {
    name: test_write_all_basic,
    code: r#"sys::fs::write_all(#path: "{}", "Hello, World!")"#,
    setup: |temp_dir| {
        temp_dir.path().join("test.txt")
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("test.txt");
        let content = fs::read_to_string(&test_file).await?;
        assert_eq!(content, "Hello, World!");
    }
}

run_with_tempdir! {
    name: test_write_all_overwrite_existing,
    code: r#"sys::fs::write_all(#path: "{}", "Overwritten content")"#,
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

run_with_tempdir! {
    name: test_write_all_utf8,
    code: r#"sys::fs::write_all(#path: "{}", "Hello, 世界! 🦀")"#,
    setup: |temp_dir| {
        temp_dir.path().join("utf8.txt")
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("utf8.txt");
        let content = fs::read_to_string(&test_file).await?;
        assert_eq!(content, "Hello, 世界! 🦀");
    }
}

run_with_tempdir! {
    name: test_write_all_empty_string,
    code: r#"sys::fs::write_all(#path: "{}", "")"#,
    setup: |temp_dir| {
        temp_dir.path().join("empty.txt")
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("empty.txt");
        let content = fs::read_to_string(&test_file).await?;
        assert_eq!(content, "");
    }
}

run_with_tempdir! {
    name: test_write_all_bin_basic,
    code: r#"sys::fs::write_all_bin(#path: "{}", bytes:SGVsbG8=)"#,
    setup: |temp_dir| {
        temp_dir.path().join("test.bin")
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("test.bin");
        let content = fs::read(&test_file).await?;
        assert_eq!(content, b"Hello");
    }
}

run_with_tempdir! {
    name: test_write_all_bin_with_nulls,
    code: r#"sys::fs::write_all_bin(#path: "{}", bytes:AAECqg==)"#,
    setup: |temp_dir| {
        temp_dir.path().join("binary.bin")
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("binary.bin");
        let content = fs::read(&test_file).await?;
        assert_eq!(content, b"\x00\x01\x02\xaa");
    }
}

run_with_tempdir! {
    name: test_write_all_bin_overwrite,
    code: r#"sys::fs::write_all_bin(#path: "{}", bytes:AQI=)"#,
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

run_with_tempdir! {
    name: test_write_all_invalid_path,
    code: r#"sys::fs::write_all(#path: "{}", "content")"#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent_dir").join("test.txt")
    },
    expect_error
}
