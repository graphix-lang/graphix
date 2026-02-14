use anyhow::{bail, Result};
use graphix_package_core::run_with_tempdir;
use netidx::subscriber::Value;
use std::collections::HashSet;
use tokio::fs;

/// Helper to extract DirEntry array from Result value
fn extract_direntry_array(v: &Value) -> Result<Vec<&Value>> {
    if let Value::Array(arr) = v {
        Ok(arr.iter().collect())
    } else {
        bail!("expected Array value, got: {v:?}")
    }
}

/// Helper to extract field from DirEntry struct
fn get_field<'a>(entry: &'a Value, field: &str) -> Result<&'a Value> {
    if let Value::Array(arr) = entry {
        for item in arr.iter() {
            if let Value::Array(pair) = item {
                if pair.len() == 2 {
                    if let Value::String(key) = &pair[0] {
                        if &**key == field {
                            return Ok(&pair[1]);
                        }
                    }
                }
            }
        }
        bail!("field {field} not found in entry")
    } else {
        bail!("expected struct (array), got: {entry:?}")
    }
}

// ===== Basic readdir tests =====

run_with_tempdir! {
    name: test_readdir_basic,
    code: r#"fs::readdir("{}")"#,
    setup: |temp_dir| {
        let root = temp_dir.path();
        fs::write(root.join("file1.txt"), "content1").await?;
        fs::write(root.join("file2.txt"), "content2").await?;
        fs::create_dir(root.join("subdir")).await?;
        root.to_path_buf()
    },
    expect: |v: Value| -> Result<()> {
        let entries = extract_direntry_array(&v)?;
        assert_eq!(entries.len(), 3, "expected 3 entries");

        // Check that we have the expected file names
        let mut names = HashSet::new();
        for entry in &entries {
            let file_name = get_field(entry, "file_name")?;
            if let Value::String(s) = file_name {
                names.insert(s.to_string());
            }
            // Verify depth is 1 (immediate children)
            let depth = get_field(entry, "depth")?;
            assert!(matches!(depth, Value::I64(1)), "expected depth=1, got: {depth:?}");
        }

        assert!(names.contains("file1.txt"), "missing file1.txt");
        assert!(names.contains("file2.txt"), "missing file2.txt");
        assert!(names.contains("subdir"), "missing subdir");
        Ok(())
    }
}

run_with_tempdir! {
    name: test_readdir_empty_dir,
    code: r#"fs::readdir("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().to_path_buf()
    },
    expect: |v: Value| -> Result<()> {
        let entries = extract_direntry_array(&v)?;
        assert_eq!(entries.len(), 0, "expected empty directory");
        Ok(())
    }
}

run_with_tempdir! {
    name: test_readdir_nonexistent,
    code: r#"fs::readdir("{}")"#,
    setup: |temp_dir| {
        temp_dir.path().join("nonexistent")
    },
    expect_error
}

// ===== Depth tests =====

run_with_tempdir! {
    name: test_readdir_max_depth_2,
    code: r#"fs::readdir(#max_depth: 2, "{}")"#,
    setup: |temp_dir| {
        let root = temp_dir.path();
        fs::write(root.join("root.txt"), "root").await?;
        fs::create_dir(root.join("sub1")).await?;
        fs::write(root.join("sub1").join("file1.txt"), "sub1").await?;
        fs::create_dir(root.join("sub1").join("sub2")).await?;
        fs::write(root.join("sub1").join("sub2").join("file2.txt"), "sub2").await?;
        root.to_path_buf()
    },
    expect: |v: Value| -> Result<()> {
        let entries = extract_direntry_array(&v)?;
        // Should have: root.txt, sub1, sub1/file1.txt, sub1/sub2
        assert!(entries.len() >= 4, "expected at least 4 entries, got {}", entries.len());

        let mut names = HashSet::new();
        let mut max_depth = 0i64;
        for entry in &entries {
            let file_name = get_field(entry, "file_name")?;
            if let Value::String(s) = file_name {
                names.insert(s.to_string());
            }
            let depth = get_field(entry, "depth")?;
            if let Value::I64(d) = depth {
                max_depth = max_depth.max(*d);
            }
        }

        assert!(names.contains("root.txt"), "missing root.txt");
        assert!(names.contains("file1.txt"), "missing file1.txt");
        assert_eq!(max_depth, 2, "should not traverse beyond depth 2");
        // file2.txt should NOT be in the results (it's at depth 3)
        assert!(!names.contains("file2.txt"), "file2.txt should not be included (depth 3)");
        Ok(())
    }
}

run_with_tempdir! {
    name: test_readdir_min_depth_2,
    code: r#"fs::readdir(#min_depth: 2, #max_depth: 3, "{}")"#,
    setup: |temp_dir| {
        let root = temp_dir.path();
        fs::write(root.join("root.txt"), "root").await?;
        fs::create_dir(root.join("sub1")).await?;
        fs::write(root.join("sub1").join("file1.txt"), "sub1").await?;
        fs::create_dir(root.join("sub1").join("sub2")).await?;
        fs::write(root.join("sub1").join("sub2").join("file2.txt"), "sub2").await?;
        root.to_path_buf()
    },
    expect: |v: Value| -> Result<()> {
        let entries = extract_direntry_array(&v)?;
        // Should skip depth 0 (root) and depth 1 (root.txt, sub1)
        // Should include depth 2 (file1.txt, sub2) and depth 3 (file2.txt)

        let mut names = HashSet::new();
        let mut min_depth = i64::MAX;
        for entry in &entries {
            let file_name = get_field(entry, "file_name")?;
            if let Value::String(s) = file_name {
                names.insert(s.to_string());
            }
            let depth = get_field(entry, "depth")?;
            if let Value::I64(d) = depth {
                min_depth = min_depth.min(*d);
            }
        }

        assert_eq!(min_depth, 2, "minimum depth should be 2");
        assert!(!names.contains("root.txt"), "root.txt should not be included (depth 1)");
        assert!(names.contains("file1.txt"), "missing file1.txt (depth 2)");
        assert!(names.contains("file2.txt"), "missing file2.txt (depth 3)");
        Ok(())
    }
}

// ===== Ordering tests =====

run_with_tempdir! {
    name: test_readdir_contents_first,
    code: r#"fs::readdir(#max_depth: 2, #contents_first: true, "{}")"#,
    setup: |temp_dir| {
        let root = temp_dir.path();
        fs::create_dir(root.join("dir1")).await?;
        fs::write(root.join("dir1").join("file.txt"), "content").await?;
        root.to_path_buf()
    },
    expect: |v: Value| -> Result<()> {
        let entries = extract_direntry_array(&v)?;
        assert!(entries.len() >= 2, "expected at least 2 entries");

        // With contents_first, file.txt should appear before dir1
        let mut file_idx = None;
        let mut dir_idx = None;
        for (i, entry) in entries.iter().enumerate() {
            let file_name = get_field(entry, "file_name")?;
            if let Value::String(s) = file_name {
                if &**s == "file.txt" {
                    file_idx = Some(i);
                } else if &**s == "dir1" {
                    dir_idx = Some(i);
                }
            }
        }

        assert!(file_idx.is_some(), "file.txt not found");
        assert!(dir_idx.is_some(), "dir1 not found");
        assert!(
            file_idx.unwrap() < dir_idx.unwrap(),
            "contents_first: file.txt should appear before dir1"
        );
        Ok(())
    }
}

// ===== Symlink tests (Unix only) =====

#[cfg(unix)]
run_with_tempdir! {
    name: test_readdir_follow_symlinks,
    code: r#"fs::readdir(#max_depth: 2, #follow_symlinks: true, "{}")"#,
    setup: |temp_dir| {
        let root = temp_dir.path();
        fs::create_dir(root.join("real_dir")).await?;
        fs::write(root.join("real_dir").join("file.txt"), "content").await?;
        std::os::unix::fs::symlink(root.join("real_dir"), root.join("link_dir"))?;
        root.to_path_buf()
    },
    expect: |v: Value| -> Result<()> {
        let entries = extract_direntry_array(&v)?;

        let mut names = HashSet::new();
        for entry in &entries {
            let file_name = get_field(entry, "file_name")?;
            if let Value::String(s) = file_name {
                names.insert(s.to_string());
            }
        }

        // With follow_symlinks=true, we should see file.txt twice:
        // once under real_dir and once under link_dir
        let file_count = entries.iter().filter(|e| {
            if let Ok(Value::String(s)) = get_field(e, "file_name") {
                &**s == "file.txt"
            } else {
                false
            }
        }).count();

        assert_eq!(file_count, 2, "should see file.txt twice when following symlinks");
        Ok(())
    }
}

#[cfg(unix)]
run_with_tempdir! {
    name: test_readdir_no_follow_symlinks,
    code: r#"fs::readdir(#max_depth: 2, #follow_symlinks: false, "{}")"#,
    setup: |temp_dir| {
        let root = temp_dir.path();
        fs::create_dir(root.join("real_dir")).await?;
        fs::write(root.join("real_dir").join("file.txt"), "content").await?;
        std::os::unix::fs::symlink(root.join("real_dir"), root.join("link_dir"))?;
        root.to_path_buf()
    },
    expect: |v: Value| -> Result<()> {
        let entries = extract_direntry_array(&v)?;

        // With follow_symlinks=false, we should only see file.txt once
        let file_count = entries.iter().filter(|e| {
            if let Ok(Value::String(s)) = get_field(e, "file_name") {
                &**s == "file.txt"
            } else {
                false
            }
        }).count();

        assert_eq!(file_count, 1, "should see file.txt only once when not following symlinks");
        Ok(())
    }
}

#[cfg(unix)]
run_with_tempdir! {
    name: test_readdir_follow_root_symlink,
    code: r#"fs::readdir("{}")"#,
    setup: |temp_dir| {
        let root = temp_dir.path();
        fs::create_dir(root.join("real_dir")).await?;
        fs::write(root.join("real_dir").join("file.txt"), "content").await?;
        std::os::unix::fs::symlink(root.join("real_dir"), root.join("link_to_dir"))?;
        root.join("link_to_dir")
    },
    expect: |v: Value| -> Result<()> {
        let entries = extract_direntry_array(&v)?;

        let mut names = HashSet::new();
        for entry in &entries {
            let file_name = get_field(entry, "file_name")?;
            if let Value::String(s) = file_name {
                names.insert(s.to_string());
            }
        }

        // By default, follow_root_symlink is true, so we should see the contents
        assert!(names.contains("file.txt"), "should follow root symlink and see file.txt");
        Ok(())
    }
}

#[cfg(unix)]
run_with_tempdir! {
    name: test_readdir_no_follow_root_symlink,
    code: r#"fs::readdir(#follow_root_symlink: false, "{}")"#,
    setup: |temp_dir| {
        let root = temp_dir.path();
        fs::create_dir(root.join("real_dir")).await?;
        fs::write(root.join("real_dir").join("file.txt"), "content").await?;
        std::os::unix::fs::symlink(root.join("real_dir"), root.join("link_to_dir"))?;
        root.join("link_to_dir")
    },
    expect: |v: Value| -> Result<()> {
        // With follow_root_symlink=false, walkdir returns an empty array
        // rather than an error when the root is a symlink
        let entries = extract_direntry_array(&v)?;
        assert_eq!(entries.len(), 0, "expected empty result when not following root symlink");
        Ok(())
    }
}

// ===== FileType tests =====

run_with_tempdir! {
    name: test_readdir_file_types,
    code: r#"fs::readdir("{}")"#,
    setup: |temp_dir| {
        let root = temp_dir.path();
        fs::write(root.join("file.txt"), "content").await?;
        fs::create_dir(root.join("dir")).await?;
        #[cfg(unix)]
        std::os::unix::fs::symlink(root.join("file.txt"), root.join("link"))?;
        root.to_path_buf()
    },
    expect: |v: Value| -> Result<()> {
        let entries = extract_direntry_array(&v)?;

        let mut found_file = false;
        let mut found_dir = false;
        #[cfg(unix)]
        let mut found_symlink = false;

        for entry in &entries {
            let file_name = get_field(entry, "file_name")?;
            let kind = get_field(entry, "kind")?;

            if let (Value::String(name), Value::String(k)) = (file_name, kind) {
                match &**name {
                    "file.txt" => {
                        assert_eq!(&**k, "File", "file.txt should have kind=File");
                        found_file = true;
                    }
                    "dir" => {
                        assert_eq!(&**k, "Dir", "dir should have kind=Dir");
                        found_dir = true;
                    }
                    #[cfg(unix)]
                    "link" => {
                        assert_eq!(&**k, "Symlink", "link should have kind=Symlink");
                        found_symlink = true;
                    }
                    _ => {}
                }
            }
        }

        assert!(found_file, "file.txt not found");
        assert!(found_dir, "dir not found");
        #[cfg(unix)]
        assert!(found_symlink, "link not found");
        Ok(())
    }
}

// ===== Path tests =====

run_with_tempdir! {
    name: test_readdir_full_paths,
    code: r#"fs::readdir("{}")"#,
    setup: |temp_dir| {
        let root = temp_dir.path();
        fs::write(root.join("file.txt"), "content").await?;
        root.to_path_buf()
    },
    expect: |v: Value| -> Result<()> {
        let entries = extract_direntry_array(&v)?;
        assert_eq!(entries.len(), 1, "expected 1 entry");

        let path = get_field(entries[0], "path")?;
        if let Value::String(s) = path {
            assert!(s.ends_with("file.txt"), "path should end with file.txt");
            assert!(s.contains('/') || s.contains('\\'), "path should be absolute/full");
        } else {
            bail!("expected String path, got: {path:?}");
        }
        Ok(())
    }
}

// ===== Error handling tests =====

run_with_tempdir! {
    name: test_readdir_invalid_depth_params,
    code: r#"fs::readdir(#max_depth: 1, #min_depth: 2, "{}")"#,
    setup: |temp_dir| {
        temp_dir.path().to_path_buf()
    },
    expect_error
}

run_with_tempdir! {
    name: test_readdir_negative_max_depth,
    code: r#"fs::readdir(#max_depth: -1, "{}")"#,
    setup: |temp_dir| {
        temp_dir.path().to_path_buf()
    },
    expect_error
}

run_with_tempdir! {
    name: test_readdir_negative_min_depth,
    code: r#"fs::readdir(#min_depth: -1, "{}")"#,
    setup: |temp_dir| {
        temp_dir.path().to_path_buf()
    },
    expect_error
}

// ===== Complex structure test =====

run_with_tempdir! {
    name: test_readdir_complex_structure,
    code: r#"fs::readdir(#max_depth: 3, "{}")"#,
    setup: |temp_dir| {
        let root = temp_dir.path();

        // Create a complex directory structure
        fs::write(root.join("root1.txt"), "root").await?;
        fs::write(root.join("root2.txt"), "root").await?;

        fs::create_dir(root.join("a")).await?;
        fs::write(root.join("a").join("a1.txt"), "a").await?;
        fs::write(root.join("a").join("a2.txt"), "a").await?;

        fs::create_dir(root.join("b")).await?;
        fs::write(root.join("b").join("b1.txt"), "b").await?;

        fs::create_dir(root.join("a").join("aa")).await?;
        fs::write(root.join("a").join("aa").join("aa1.txt"), "aa").await?;

        root.to_path_buf()
    },
    expect: |v: Value| -> Result<()> {
        let entries = extract_direntry_array(&v)?;

        // Count entries at each depth
        let mut depth_counts = std::collections::HashMap::new();
        for entry in &entries {
            let depth = get_field(entry, "depth")?;
            if let Value::I64(d) = depth {
                *depth_counts.entry(d).or_insert(0) += 1;
            }
        }

        // Depth 1: root1.txt, root2.txt, a, b = 4
        // Depth 2: a1.txt, a2.txt, b1.txt, aa = 4
        // Depth 3: aa1.txt = 1
        assert_eq!(depth_counts.get(&1).copied().unwrap_or(0), 4, "expected 4 entries at depth 1");
        assert_eq!(depth_counts.get(&2).copied().unwrap_or(0), 4, "expected 4 entries at depth 2");
        assert_eq!(depth_counts.get(&3).copied().unwrap_or(0), 1, "expected 1 entry at depth 3");

        assert_eq!(entries.len(), 9, "expected total of 9 entries");
        Ok(())
    }
}
