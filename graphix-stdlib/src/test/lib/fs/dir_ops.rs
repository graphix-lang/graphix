use crate::{run_with_tempdir, test::init};
use anyhow::Result;
use arcstr::ArcStr;
use graphix_rt::GXEvent;
use netidx::subscriber::Value;
use poolshark::global::GPooled;
use tokio::{fs, sync::mpsc, time::Duration};

// ============================================================================
// fs::create_dir tests
// ============================================================================

run_with_tempdir! {
    name: test_create_dir_basic,
    code: r#"fs::create_dir("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("newdir")
    },
    verify: |temp_dir| {
        let test_dir = temp_dir.path().join("newdir");
        assert!(test_dir.exists());
        assert!(test_dir.is_dir());
    }
}

run_with_tempdir! {
    name: test_create_dir_already_exists,
    code: r#"fs::create_dir("{}")"#,
    setup: |temp_dir| {
        let test_dir = temp_dir.path().join("existing");
        fs::create_dir(&test_dir).await?;
        test_dir
    },
    expect_error
}

run_with_tempdir! {
    name: test_create_dir_all_basic,
    code: r#"fs::create_dir(#all: true, "{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("parent").join("child").join("grandchild")
    },
    verify: |temp_dir| {
        let test_dir = temp_dir.path().join("parent").join("child").join("grandchild");
        assert!(test_dir.exists());
        assert!(test_dir.is_dir());
    }
}

run_with_tempdir! {
    name: test_create_dir_all_idempotent,
    code: r#"fs::create_dir(#all: true, "{}")"#,
    setup: |temp_dir| {
        let test_dir = temp_dir.path().join("existing");
        fs::create_dir(&test_dir).await?;
        test_dir
    },
    verify: |temp_dir| {
        let test_dir = temp_dir.path().join("existing");
        assert!(test_dir.exists());
        assert!(test_dir.is_dir());
    }
}

run_with_tempdir! {
    name: test_create_dir_missing_parent,
    code: r#"fs::create_dir("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent").join("newdir")
    },
    expect_error
}

// ============================================================================
// fs::remove_dir tests
// ============================================================================

run_with_tempdir! {
    name: test_remove_dir_empty,
    code: r#"fs::remove_dir("{}")"#,
    setup: |temp_dir| {
        let test_dir = temp_dir.path().join("empty");
        fs::create_dir(&test_dir).await?;
        test_dir
    },
    verify: |temp_dir| {
        let test_dir = temp_dir.path().join("empty");
        assert!(!test_dir.exists());
    }
}

run_with_tempdir! {
    name: test_remove_dir_not_empty,
    code: r#"fs::remove_dir("{}")"#,
    setup: |temp_dir| {
        let test_dir = temp_dir.path().join("notempty");
        fs::create_dir(&test_dir).await?;
        fs::write(test_dir.join("file.txt"), "content").await?;
        test_dir
    },
    expect_error
}

run_with_tempdir! {
    name: test_remove_dir_nonexistent,
    code: r#"fs::remove_dir("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent")
    },
    expect_error
}

run_with_tempdir! {
    name: test_remove_dir_all_with_contents,
    code: r#"fs::remove_dir(#all: true, "{}")"#,
    setup: |temp_dir| {
        let test_dir = temp_dir.path().join("parent");
        fs::create_dir_all(test_dir.join("child")).await?;
        fs::write(test_dir.join("file1.txt"), "content1").await?;
        fs::write(test_dir.join("child").join("file2.txt"), "content2").await?;
        test_dir
    },
    verify: |temp_dir| {
        let test_dir = temp_dir.path().join("parent");
        assert!(!test_dir.exists());
    }
}

run_with_tempdir! {
    name: test_remove_dir_all_deeply_nested,
    code: r#"fs::remove_dir(#all: true, "{}")"#,
    setup: |temp_dir| {
        let test_dir = temp_dir.path().join("a");
        fs::create_dir_all(test_dir.join("b").join("c").join("d")).await?;
        fs::write(test_dir.join("b").join("c").join("d").join("file.txt"), "deep").await?;
        test_dir
    },
    verify: |temp_dir| {
        let test_dir = temp_dir.path().join("a");
        assert!(!test_dir.exists());
    }
}

run_with_tempdir! {
    name: test_remove_dir_all_nonexistent,
    code: r#"fs::remove_dir(#all: true, "{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent")
    },
    expect_error
}

// ============================================================================
// fs::remove_file tests
// ============================================================================

run_with_tempdir! {
    name: test_remove_file_basic,
    code: r#"fs::remove_file("{}")"#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test.txt");
        fs::write(&test_file, "content").await?;
        test_file
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("test.txt");
        assert!(!test_file.exists());
    }
}

run_with_tempdir! {
    name: test_remove_file_nonexistent,
    code: r#"fs::remove_file("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent.txt")
    },
    expect_error
}

run_with_tempdir! {
    name: test_remove_file_is_directory,
    code: r#"fs::remove_file("{}")"#,
    setup: |temp_dir| {
        let test_dir = temp_dir.path().join("dir");
        fs::create_dir(&test_dir).await?;
        test_dir
    },
    expect_error
}

run_with_tempdir! {
    name: test_remove_file_utf8_name,
    code: r#"fs::remove_file("{}")"#,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("テスト_🦀.txt");
        fs::write(&test_file, "content").await?;
        test_file
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("テスト_🦀.txt");
        assert!(!test_file.exists());
    }
}

// ============================================================================
// Integration tests combining operations
// ============================================================================

run_with_tempdir! {
    name: test_create_and_remove_dir_sequence,
    code: r#"{{
        let dir = "{}";
        let created = fs::create_dir(dir)?;
        fs::remove_dir(created ~ dir)
    }}"#,
    setup: |temp_dir| {
        temp_dir.path().join("seq_test")
    },
    verify: |temp_dir| {
        let test_dir = temp_dir.path().join("seq_test");
        assert!(!test_dir.exists());
    }
}

run_with_tempdir! {
    name: test_create_write_remove_file_sequence,
    code: r#"{{
        let dir = "{}";
        let created = fs::create_dir(dir)?;
        let file = fs::join_path(created ~ dir, "data.txt");
        let written = fs::write_all(#path: file, "test data")?;
        fs::remove_file(written ~ file)
    }}"#,
    setup: |temp_dir| {
        temp_dir.path().join("fileseq")
    },
    verify: |temp_dir| {
        let test_file = temp_dir.path().join("fileseq").join("data.txt");
        assert!(!test_file.exists());
        // Parent directory should still exist
        let test_dir = temp_dir.path().join("fileseq");
        assert!(test_dir.exists());
    }
}
