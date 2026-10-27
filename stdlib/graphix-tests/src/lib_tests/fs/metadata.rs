use crate::lib_tests::struct_field;
use anyhow::Result;
use graphix_package_core::run_with_tempdir;
use netidx::subscriber::Value;
use std::time::{Duration, SystemTime};
use tokio::fs;

run_with_tempdir! {
    name: test_is_file_basic,
    code: r#"sys::fs::is_file("{}")"#,
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
    code: r#"sys::fs::is_file("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().to_path_buf()
    },
    expect_error
}

run_with_tempdir! {
    name: test_is_file_nonexistent,
    code: r#"sys::fs::is_file("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent.txt")
    },
    expect_error
}

#[cfg(unix)]
run_with_tempdir! {
    name: test_is_file_symlink_to_file,
    code: r#"sys::fs::is_file("{}")"#,
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

run_with_tempdir! {
    name: test_is_dir_basic,
    code: r#"sys::fs::is_dir("{}")"#,
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
    code: r#"sys::fs::is_dir("{}")"#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test.txt");
        fs::write(&test_file, "content").await?;
        test_file
    },
    expect_error
}

run_with_tempdir! {
    name: test_is_dir_nonexistent,
    code: r#"sys::fs::is_dir("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent_dir")
    },
    expect_error
}

run_with_tempdir! {
    name: test_is_dir_temp_dir,
    code: r#"sys::fs::is_dir("{}")"#,
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
    code: r#"sys::fs::is_dir("{}")"#,
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

run_with_tempdir! {
    name: test_metadata_file_basic,
    code: r#"sys::fs::metadata("{}")"#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test.txt");
        fs::write(&test_file, "hello world").await?;
        test_file
    },
    expect: |v: Value| -> Result<()> {

        let kind = struct_field(&v, "kind").expect("kind field missing");
        assert!(matches!(kind, Value::String(s) if s.as_str() == "File"), "expected kind=File, got: {kind:?}");

        let len = struct_field(&v, "len").expect("len field missing");
        assert!(matches!(len, Value::U64(11)), "expected len=11, got: {len:?}");

        Ok(())
    }
}

run_with_tempdir! {
    name: test_metadata_dir_basic,
    code: r#"sys::fs::metadata("{}")"#,
    setup: |temp_dir| {
        let test_dir = temp_dir.path().join("test_dir");
        fs::create_dir(&test_dir).await?;
        test_dir
    },
    expect: |v: Value| -> Result<()> {
        let kind = struct_field(&v, "kind").expect("kind field missing");
        assert!(matches!(kind, Value::String(s) if s.as_str() == "Dir"), "expected kind=Dir, got: {kind:?}");
        Ok(())
    }
}

run_with_tempdir! {
    name: test_metadata_nonexistent,
    code: r#"sys::fs::metadata("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent.txt")
    },
    expect_error
}

#[cfg(unix)]
run_with_tempdir! {
    name: test_metadata_symlink_follow,
    code: r#"sys::fs::metadata("{}")"#,
    setup: |temp_dir| {
        let target = temp_dir.path().join("target.txt");
        fs::write(&target, "content").await?;
        let link = temp_dir.path().join("link.txt");
        std::os::unix::fs::symlink(&target, &link)?;
        link
    },
    expect: |v: Value| -> Result<()> {
        let kind = struct_field(&v, "kind").expect("kind field missing");
        assert!(matches!(kind, Value::String(s) if s.as_str() == "File"), "expected kind=File (followed), got: {kind:?}");
        Ok(())
    }
}

#[cfg(unix)]
run_with_tempdir! {
    name: test_metadata_symlink_nofollow,
    code: r#"sys::fs::metadata(#follow_symlinks: false, "{}")"#,
    setup: |temp_dir| {
        let target = temp_dir.path().join("target.txt");
        fs::write(&target, "content").await?;
        let link = temp_dir.path().join("link.txt");
        std::os::unix::fs::symlink(&target, &link)?;
        link
    },
    expect: |v: Value| -> Result<()> {
        let kind = struct_field(&v, "kind").expect("kind field missing");
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
    code: r#"sys::fs::metadata("{}")"#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test.txt");
        fs::write(&test_file, "content").await?;
        let mut perms = fs::metadata(&test_file).await?.permissions();
        use std::os::unix::fs::PermissionsExt;
        perms.set_mode(0o644);
        fs::set_permissions(&test_file, perms).await?;
        test_file
    },
    expect: |v: Value| -> Result<()> {
        let permissions = struct_field(&v, "permissions").expect("permissions field missing");
        if let Value::U32(mode) = permissions {
            assert_eq!(mode & 0o777, 0o644, "expected mode 0o644, got: {mode:#o}");
            Ok(())
        } else {
            panic!("expected U32 permissions on Unix, got: {permissions:?}")
        }
    }
}

const ACCESSED: u64 = 1_000_000_000;
const MODIFIED: u64 = 1_200_000_000;

run_with_tempdir! {
    name: test_metadata_timestamps,
    code: r#"sys::fs::metadata("{}")"#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test.txt");
        fs::write(&test_file, "content").await?;
        std::fs::File::options().write(true).open(&test_file)?.set_times(
            std::fs::FileTimes::new()
                .set_accessed(SystemTime::UNIX_EPOCH + Duration::from_secs(ACCESSED))
                .set_modified(SystemTime::UNIX_EPOCH + Duration::from_secs(MODIFIED)),
        )?;
        test_file
    },
    expect: |v: Value| -> Result<()> {
        let at = |name: &str, secs: u64| match struct_field(&v, name) {
            Some(Value::DateTime(d)) => assert_eq!(d.timestamp(), secs as i64, "{name}"),
            f => panic!("expected {name} to be a datetime, got: {f:?}"),
        };
        at("accessed", ACCESSED);
        at("modified", MODIFIED);
        match struct_field(&v, "created") {
            Some(Value::DateTime(_) | Value::Null) => Ok(()),
            f => panic!("expected created to be a datetime or null, got: {f:?}"),
        }
    }
}
