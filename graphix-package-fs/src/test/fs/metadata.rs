use crate::test::{REGISTER, ROOT};
use anyhow::Result;
use graphix_package_core::run_with_tempdir;
use netidx::subscriber::Value;
use std::collections::HashMap;
use tokio::fs;
use tokio::time::Duration;

/// Helper to convert metadata array to a hashmap for easier testing
fn metadata_to_map(v: &Value) -> Option<HashMap<String, Value>> {
    if let Value::Array(arr) = v {
        let mut map = HashMap::new();
        for item in arr.iter() {
            if let Value::Array(pair) = item {
                if pair.len() == 2 {
                    if let (Value::String(key), val) = (&pair[0], &pair[1]) {
                        map.insert(key.to_string(), val.clone());
                    }
                }
            }
        }
        Some(map)
    } else {
        None
    }
}

// ===== is_file tests =====

run_with_tempdir! {
    name: test_is_file_basic,
    code: r#"fs::is_file("{}")"#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test.txt");
        fs::write(&test_file, "content").await?;
        test_file
    },
    expect: |v: Value| -> Result<()> {
        if let Value::String(s) = v {
            assert!(s.ends_with("test.txt"));
            Ok(())
        } else {
            panic!("expected String value, got: {v:?}")
        }
    }
}

run_with_tempdir! {
    name: test_is_file_on_directory,
    code: r#"fs::is_file("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().to_path_buf()
    },
    expect_error
}

run_with_tempdir! {
    name: test_is_file_nonexistent,
    code: r#"fs::is_file("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent.txt")
    },
    expect_error
}

#[cfg(unix)]
run_with_tempdir! {
    name: test_is_file_symlink_to_file,
    code: r#"fs::is_file("{}")"#,
    setup: |temp_dir| {
        let target = temp_dir.path().join("target.txt");
        fs::write(&target, "content").await?;
        let link = temp_dir.path().join("link.txt");
        std::os::unix::fs::symlink(&target, &link)?;
        link
    },
    expect: |v: Value| -> Result<()> {
        if let Value::String(s) = v {
            assert!(s.ends_with("link.txt"));
            Ok(())
        } else {
            panic!("expected String value, got: {v:?}")
        }
    }
}

// ===== is_dir tests =====

run_with_tempdir! {
    name: test_is_dir_basic,
    code: r#"fs::is_dir("{}")"#,
    setup: |temp_dir| {
        let test_dir = temp_dir.path().join("test_dir");
        fs::create_dir(&test_dir).await?;
        test_dir
    },
    expect: |v: Value| -> Result<()> {
        if let Value::String(s) = v {
            assert!(s.ends_with("test_dir"));
            Ok(())
        } else {
            panic!("expected String value, got: {v:?}")
        }
    }
}

run_with_tempdir! {
    name: test_is_dir_on_file,
    code: r#"fs::is_dir("{}")"#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test.txt");
        fs::write(&test_file, "content").await?;
        test_file
    },
    expect_error
}

run_with_tempdir! {
    name: test_is_dir_nonexistent,
    code: r#"fs::is_dir("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent_dir")
    },
    expect_error
}

run_with_tempdir! {
    name: test_is_dir_temp_dir,
    code: r#"fs::is_dir("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().to_path_buf()
    },
    expect: |v: Value| -> Result<()> {
        matches!(v, Value::String(_))
            .then_some(())
            .ok_or_else(|| anyhow::anyhow!("expected String value, got: {v:?}"))
    }
}

#[cfg(unix)]
run_with_tempdir! {
    name: test_is_dir_symlink_to_dir,
    code: r#"fs::is_dir("{}")"#,
    setup: |temp_dir| {
        let target = temp_dir.path().join("target_dir");
        fs::create_dir(&target).await?;
        let link = temp_dir.path().join("link_dir");
        std::os::unix::fs::symlink(&target, &link)?;
        link
    },
    expect: |v: Value| -> Result<()> {
        if let Value::String(s) = v {
            assert!(s.ends_with("link_dir"));
            Ok(())
        } else {
            panic!("expected String value, got: {v:?}")
        }
    }
}

// ===== metadata tests =====

run_with_tempdir! {
    name: test_metadata_file_basic,
    code: r#"fs::metadata("{}")"#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test.txt");
        fs::write(&test_file, "hello world").await?;
        test_file
    },
    expect: |v: Value| -> Result<()> {
        let fields = metadata_to_map(&v).expect("expected metadata array");

        // Check that kind is File
        let kind = fields.get("kind").expect("kind field missing");
        assert!(matches!(kind, Value::String(s) if s.as_str() == "File"), "expected kind=File, got: {kind:?}");

        // Check that len is 11 (length of "hello world")
        let len = fields.get("len").expect("len field missing");
        assert!(matches!(len, Value::U64(11)), "expected len=11, got: {len:?}");

        Ok(())
    }
}

run_with_tempdir! {
    name: test_metadata_dir_basic,
    code: r#"fs::metadata("{}")"#,
    setup: |temp_dir| {
        let test_dir = temp_dir.path().join("test_dir");
        fs::create_dir(&test_dir).await?;
        test_dir
    },
    expect: |v: Value| -> Result<()> {
        let fields = metadata_to_map(&v).expect("expected metadata array");
        let kind = fields.get("kind").expect("kind field missing");
        assert!(matches!(kind, Value::String(s) if s.as_str() == "Dir"), "expected kind=Dir, got: {kind:?}");
        Ok(())
    }
}

run_with_tempdir! {
    name: test_metadata_nonexistent,
    code: r#"fs::metadata("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent.txt")
    },
    expect_error
}

#[cfg(unix)]
run_with_tempdir! {
    name: test_metadata_symlink_follow,
    code: r#"fs::metadata("{}")"#,
    setup: |temp_dir| {
        let target = temp_dir.path().join("target.txt");
        fs::write(&target, "content").await?;
        let link = temp_dir.path().join("link.txt");
        std::os::unix::fs::symlink(&target, &link)?;
        link
    },
    expect: |v: Value| -> Result<()> {
        let fields = metadata_to_map(&v).expect("expected metadata array");
        let kind = fields.get("kind").expect("kind field missing");
        // With follow_symlinks=true (default), should see File not Symlink
        assert!(matches!(kind, Value::String(s) if s.as_str() == "File"), "expected kind=File (followed), got: {kind:?}");
        Ok(())
    }
}

#[cfg(unix)]
run_with_tempdir! {
    name: test_metadata_symlink_nofollow,
    code: r#"fs::metadata(#follow_symlinks: false, "{}")"#,
    setup: |temp_dir| {
        let target = temp_dir.path().join("target.txt");
        fs::write(&target, "content").await?;
        let link = temp_dir.path().join("link.txt");
        std::os::unix::fs::symlink(&target, &link)?;
        link
    },
    expect: |v: Value| -> Result<()> {
        let fields = metadata_to_map(&v).expect("expected metadata array");
        let kind = fields.get("kind").expect("kind field missing");
        // With follow_symlinks=false, should see Symlink
        assert!(
            matches!(kind, Value::String(s) if s.as_str() == "Symlink"),
            "expected kind=Symlink (not followed), got: {kind:?}"
        );
        Ok(())
    }
}

#[cfg(unix)]
run_with_tempdir! {
    name: test_metadata_permissions_unix,
    code: r#"fs::metadata("{}")"#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test.txt");
        fs::write(&test_file, "content").await?;
        // Set specific permissions (0o644)
        let mut perms = fs::metadata(&test_file).await?.permissions();
        use std::os::unix::fs::PermissionsExt;
        perms.set_mode(0o644);
        fs::set_permissions(&test_file, perms).await?;
        test_file
    },
    expect: |v: Value| -> Result<()> {
        let fields = metadata_to_map(&v).expect("expected metadata array");
        let permissions = fields.get("permissions").expect("permissions field missing");
        if let Value::U32(mode) = permissions {
            // Check that at least the lower bits match 0o644
            assert_eq!(mode & 0o777, 0o644, "expected mode 0o644, got: {mode:#o}");
            Ok(())
        } else {
            panic!("expected U32 permissions on Unix, got: {permissions:?}")
        }
    }
}

run_with_tempdir! {
    name: test_metadata_timestamps,
    code: r#"fs::metadata("{}")"#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test.txt");
        fs::write(&test_file, "content").await?;
        // Give filesystem time to set timestamps
        tokio::time::sleep(Duration::from_millis(10)).await;
        test_file
    },
    expect: |v: Value| -> Result<()> {
        let fields = metadata_to_map(&v).expect("expected metadata array");
        // Just verify that the timestamp fields exist
        // We can't check exact values, but we can check they're present
        assert!(fields.contains_key("accessed"), "missing accessed field");
        assert!(fields.contains_key("created"), "missing created field");
        assert!(fields.contains_key("modified"), "missing modified field");
        Ok(())
    }
}
