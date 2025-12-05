use crate::{
    run,
    test::{escape_path, init},
};
use anyhow::{bail, Result};
use arcstr::ArcStr;
use graphix_rt::GXEvent;
use netidx::subscriber::Value;
use poolshark::global::GPooled;
use tokio::{fs, sync::mpsc, time::Duration};

/// Macro to create fs::watch tests with common setup/teardown logic.
/// Supports both simple single-action tests and complex multi-event sequences.
macro_rules! watch_test {
    // Simple pattern: single action after establishment, boolean expectation
    // This delegates to the complex pattern with sensible defaults
    (
        name: $test_name:ident,
        interest: $interest:expr,
        setup: |$temp_dir:ident| $setup:block,
        action: |$action_dir:ident| $action:block,
        expect: $expect:expr
    ) => {
        watch_test! {
            name: $test_name,
            interest: $interest,
            timeout_secs: 2,
            setup: |$temp_dir| {
                $setup
                $temp_dir.path()
            },
            state: {
                _event_count: usize = 0,
            },
            on_event: |count, temp_dir, _event_count| {
                *_event_count = count;
                if count == 1 {
                    eprintln!("watch established, performing action");
                    let $action_dir = &temp_dir;
                    $action
                }
            },
            verify: {
                let got_event = _event_count > 1;
                assert_eq!(got_event, $expect,
                    "Expected event: {}, Got event: {}", $expect, got_event)
            }
        }
    };

    // Complex pattern: multi-event sequence with state tracking
    (
        name: $test_name:ident,
        interest: $interest:expr,
        timeout_secs: $timeout:expr,
        setup: |$setup_dir:ident| $setup:block,
        state: { $($state_name:ident: $state_type:ty = $state_init:expr),* $(,)? },
        on_event: |$event_count:ident, $event_dir:ident, $($state_param:ident),*| $on_event:block,
        verify: { $($verify:stmt);* $(;)? }
    ) => {
        #[tokio::test(flavor = "current_thread")]
        async fn $test_name() -> Result<()> {
            let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent<_>>>>(10);
            let ctx = init(tx).await?;
            let temp_dir = tempfile::tempdir()?;

            // Run setup
            let watch_path = {
                let $setup_dir = &temp_dir;
                $setup
            };

            // Start watching
            let code = format!(
                r#"fs::watch(#interest: {}, "{}")"#,
                $interest, escape_path(watch_path.display())
            );

            let compiled = ctx.rt.compile(ArcStr::from(code)).await?;
            let eid = compiled.exprs[0].id;

            let timeout = tokio::time::sleep(Duration::from_secs($timeout));
            tokio::pin!(timeout);
            let mut event_count = 0;
            $(let mut $state_name: $state_type = $state_init;)*

            loop {
                tokio::select! {
                    _ = &mut timeout => break,
                    Some(mut batch) = rx.recv() => {
                        for event in batch.drain(..) {
                            if let GXEvent::Updated(id, v) = event {
                                if id == eid && matches!(v, Value::String(_)) {
                                    event_count += 1;
                                    eprintln!("Event #{event_count}: {v}");

                                    let $event_count = event_count;
                                    let $event_dir = &temp_dir;
                                    $(let $state_param = &mut $state_param;)*
                                    $on_event
                                }
                            }
                        }
                    }
                }
            }

            $($verify;)*
            Ok(())
        }
    };
}

// Test file creation detection
watch_test! {
    name: test_watch_create_file,
    interest: "[`Established, `Create]",
    setup: |_temp_dir| {},
    action: |temp_dir| {
        let test_file = temp_dir.path().join("test_file.txt");
        fs::write(&test_file, b"hello").await?;
    },
    expect: true
}

// Test file modification detection
watch_test! {
    name: test_watch_modify_file,
    interest: "[`Established, `Modify]",
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test_file.txt");
        fs::write(&test_file, b"initial").await?;
    },
    action: |temp_dir| {
        let test_file = temp_dir.path().join("test_file.txt");
        fs::write(&test_file, b"modified content").await?;
    },
    expect: true
}

// Test file deletion detection
watch_test! {
    name: test_watch_delete_file,
    interest: "[`Established, `Delete]",
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test_file.txt");
        fs::write(&test_file, b"to be deleted").await?;
    },
    action: |temp_dir| {
        let test_file = temp_dir.path().join("test_file.txt");
        fs::remove_file(&test_file).await?;
    },
    expect: true
}

// Test interest filtering (should NOT detect events not matching interest)
watch_test! {
    name: test_watch_interest_filtering,
    interest: "[`Established, `Create]",
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("test_file.txt");
        fs::write(&test_file, b"initial").await?;
    },
    action: |temp_dir| {
        let test_file = temp_dir.path().join("test_file.txt");
        fs::write(&test_file, b"modified").await?;
    },
    expect: false
}

// Test watching a non-existent file that gets created
watch_test! {
    name: test_watch_nonexistent_file_created,
    interest: "[`Create, `Established]",
    timeout_secs: 5,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("nonexistent.txt");
        let _ = std::fs::remove_file(&test_file);
        test_file
    },
    state: {
        file_created: bool = false,
        got_create: bool = false,
    },
    on_event: |count, temp_dir, file_created, got_create| {
        let test_file = temp_dir.path().join("nonexistent.txt");
        if count == 1 && !*file_created {
            eprintln!("Creating file");
            fs::write(&test_file, b"hello").await?;
            *file_created = true;
        } else if count >= 2 {
            *got_create = true;
        }
    },
    verify: {
        assert!(got_create, "Did not receive create event for non-existent file");
    }
}

// Test watching existing file, deleting it, then recreating it
watch_test! {
    name: test_watch_delete_then_recreate,
    interest: "[`Established, `Create, `Delete, `Modify]",
    timeout_secs: 6,
    setup: |temp_dir| {
        let test_file = temp_dir.path().join("delete_recreate.txt");
        fs::write(&test_file, b"initial").await?;
        test_file
    },
    state: {
        got_delete: bool = false,
        got_create: bool = false,
    },
    on_event: |count, temp_dir, got_delete, got_create| {
        let test_file = temp_dir.path().join("delete_recreate.txt");
        if count == 1 {
            eprintln!("Deleting file");
            fs::remove_file(&test_file).await?;
        } else if count == 2 {
            *got_delete = true;
            eprintln!("Recreating file");
            tokio::time::sleep(Duration::from_millis(100)).await;
            fs::write(&test_file, b"recreated").await?;
        } else if count == 3 {
            *got_create = true;
        }
    },
    verify: {
        assert!(got_delete, "Did not receive delete event");
        assert!(got_create, "Did not receive create event after recreation");
    }
}

// Test renaming parent directory
watch_test! {
    name: test_watch_parent_rename,
    interest: "[`Established, `Delete, `Create]",
    timeout_secs: 4,
    setup: |temp_dir| {
        let parent_dir = temp_dir.path().join("parent");
        fs::create_dir(&parent_dir).await?;
        let test_file = parent_dir.join("file.txt");
        fs::write(&test_file, b"content").await?;
        test_file
    },
    state: {
        got_delete: bool = false,
    },
    on_event: |count, temp_dir, got_delete| {
        if count == 1 {
            let parent_dir = temp_dir.path().join("parent");
            let new_parent = temp_dir.path().join("parent_renamed");
            eprintln!("Renaming parent directory");
            fs::rename(&parent_dir, &new_parent).await?;
        } else {
            *got_delete = true;
        }
    },
    verify: {
        assert!(got_delete, "Did not receive delete event after parent rename");
    }
}

// Test multi-level parent creation
watch_test! {
    name: test_watch_multilevel_parent_creation,
    interest: "[`Established, `Create]",
    timeout_secs: 5,
    setup: |temp_dir| {
        let deep_file = temp_dir.path().join("a").join("b").join("c").join("file.txt");
        let _ = fs::remove_dir_all(temp_dir.path().join("a")).await;
        deep_file
    },
    state: {
        got_create: bool = false,
    },
    on_event: |count, temp_dir, got_create| {
        let deep_file = temp_dir.path().join("a").join("b").join("c").join("file.txt");
        if count == 1 {
            eprintln!("Creating /a");
            fs::create_dir(temp_dir.path().join("a")).await?;
            tokio::time::sleep(Duration::from_millis(100)).await;

            eprintln!("Creating /a/b");
            fs::create_dir(temp_dir.path().join("a/b")).await?;
            tokio::time::sleep(Duration::from_millis(100)).await;

            eprintln!("Creating /a/b/c");
            fs::create_dir(temp_dir.path().join("a/b/c")).await?;
            tokio::time::sleep(Duration::from_millis(100)).await;

            eprintln!("Creating file");
            fs::write(&deep_file, b"deep content").await?;
        } else {
            *got_create = true;
        }
    },
    verify: {
        assert!(got_create, "Did not receive create event for deep file");
    }
}

// Test deep parent rename (rename two levels up)
watch_test! {
    name: test_watch_deep_parent_rename,
    interest: "[`Established, `Delete]",
    timeout_secs: 4,
    setup: |temp_dir| {
        let d = temp_dir.path().join("a").join("b").join("c").join("d");
        fs::create_dir_all(&d).await?;
        let test_file = d.join("file.txt");
        fs::write(&test_file, b"content").await?;
        eprintln!("test file: {}", test_file.display());
        test_file
    },
    state: {
        got_delete: bool = false,
    },
    on_event: |count, temp_dir, got_delete| {
        if count == 1 {
            let a = temp_dir.path().join("a");
            let b = a.join("b");
            let b2 = a.join("b2");
            eprintln!("Renaming /a/b to /a/b2 (two levels up)");
            eprintln!("rename {} to {}", b.display(), b2.display());
            fs::rename(&b, &b2).await?;
        } else {
            *got_delete = true;
        }
    },
    verify: {
        assert!(got_delete, "Did not receive delete event after deep parent rename");
    }
}

// Test race with parent deletion
watch_test! {
    name: test_watch_parent_tree_deletion,
    interest: "[`Established, `Delete]",
    timeout_secs: 4,
    setup: |temp_dir| {
        let b = temp_dir.path().join("a").join("b");
        fs::create_dir_all(&b).await?;
        let test_file = b.join("file.txt");
        fs::write(&test_file, b"content").await?;
        test_file
    },
    state: {
        got_delete: bool = false,
    },
    on_event: |count, temp_dir, got_delete| {
        if count == 1 {
            let a = temp_dir.path().join("a");
            eprintln!("Deleting entire /a directory tree");
            fs::remove_dir_all(&a).await?;
        } else {
            *got_delete = true;
        }
    },
    verify: {
        assert!(got_delete, "Did not receive delete event after parent tree deletion");
    }
}

// Test multiple watches on related paths
#[tokio::test(flavor = "current_thread")]
async fn test_watch_multiple_related_paths() -> Result<()> {
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent<_>>>>(10);
    let ctx = init(tx).await?;
    let temp_dir = tempfile::tempdir()?;

    let file1 = temp_dir.path().join("a").join("b").join("file.txt");
    let file2 = temp_dir.path().join("a").join("b").join("c").join("file.txt");

    // Ensure nothing exists
    let _ = fs::remove_dir_all(temp_dir.path().join("a")).await;

    // Watch both files
    let code1 = format!(
        r#"fs::watch(#interest: [`Established, `Create], "{}")"#,
        escape_path(file1.display())
    );
    let code2 = format!(
        r#"fs::watch(#interest: [`Established, `Create], "{}")"#,
        escape_path(file2.display())
    );

    let compiled1 = ctx.rt.compile(ArcStr::from(code1)).await?;
    let compiled2 = ctx.rt.compile(ArcStr::from(code2)).await?;
    let eid1 = compiled1.exprs[0].id;
    let eid2 = compiled2.exprs[0].id;

    let timeout = tokio::time::sleep(Duration::from_secs(5));
    tokio::pin!(timeout);
    let mut watch1_ready = false;
    let mut watch2_ready = false;
    let mut got_create_file1 = false;
    let mut got_create_file2 = false;

    loop {
        tokio::select! {
            _ = &mut timeout => break,
            Some(mut batch) = rx.recv() => {
                for event in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = event {
                        if matches!(v, Value::String(_)) {
                            eprintln!("Event for {}: {v}", id.inner());

                            if id == eid1 && !watch1_ready {
                                watch1_ready = true;
                                eprintln!("Watch 1 ready");
                            } else if id == eid2 && !watch2_ready {
                                watch2_ready = true;
                                eprintln!("Watch 2 ready");
                            } else if id == eid1 {
                                got_create_file1 = true;
                            } else if id == eid2 {
                                got_create_file2 = true;
                            }

                            if watch1_ready && watch2_ready && !got_create_file1 && !got_create_file2 {
                                // Both watches ready, create only file2
                                eprintln!("Creating deep file only (file2)");
                                fs::create_dir_all(temp_dir.path().join("a/b/c")).await?;
                                fs::write(&file2, b"deep").await?;
                            }
                        }
                    }
                }
            }
        }
    }

    assert!(!got_create_file1, "Should not get create event for file1");
    assert!(got_create_file2, "Should get create event for file2");
    Ok(())
}

// Test established -> pending transition -> established
watch_test! {
    name: test_watch_established_to_pending,
    interest: "[`Delete, `Create, `Established]",
    timeout_secs: 4,
    setup: |temp_dir| {
        let subdir = temp_dir.path().join("subdir");
        fs::create_dir(&subdir).await?;
        let test_file = subdir.join("file.txt");
        fs::write(&test_file, b"content").await?;
        test_file
    },
    state: {
        got_established: bool = false,
        got_delete: bool = false,
        got_create: bool = false,
    },
    on_event: |count, temp_dir, got_established, got_delete, got_create| {
        let subdir = temp_dir.path().join("subdir");
        if count == 1 {
            *got_established = true;
            eprintln!("Deleting parent directory");
            fs::remove_dir_all(&subdir).await?;
            tokio::time::sleep(Duration::from_millis(100)).await;
        } else if count == 2 {
            eprintln!("Parent directory deleted, recreating");
            *got_delete = true;
            fs::create_dir(&subdir).await?;
            fs::write(&subdir.join("file.txt"), b"content").await?
        } else {
            *got_create = true;
        }
    },
    verify: {
        assert!(got_established, "Did not receive Established event");
        assert!(got_delete, "Did not receive Delete event after parent deletion");
        assert!(got_create, "Did not receive Create event after recreation");
    }
}

// Test file -> directory transition
watch_test! {
    name: test_watch_file_to_directory,
    interest: "[`Established, `Delete, `Create]",
    timeout_secs: 4,
    setup: |temp_dir| {
        let path = temp_dir.path().join("transform");
        fs::write(&path, b"file content").await?;
        path
    },
    state: {
        got_delete: bool = false,
        got_create: bool = false,
    },
    on_event: |count, temp_dir, got_delete, got_create| {
        let path = temp_dir.path().join("transform");
        if count == 1 {
            eprintln!("Deleting file and creating directory");
            fs::remove_file(&path).await?;
            tokio::time::sleep(Duration::from_millis(100)).await;
            fs::create_dir(&path).await?;
        } else if count == 2 {
            *got_delete = true;
        } else if count == 3 {
            *got_create = true;
        }
    },
    verify: {
        assert!(got_delete, "Did not receive delete event");
        assert!(got_create, "Did not receive create event for directory");
    }
}

// Test symlink with non-existent target
#[cfg(unix)]
watch_test! {
    name: test_watch_symlink_nonexistent_target,
    interest: "[`Established, `Create, `Modify]",
    timeout_secs: 4,
    setup: |temp_dir| {
        use std::os::unix::fs::symlink;
        let target = temp_dir.path().join("target.txt");
        let link = temp_dir.path().join("link.txt");
        symlink(&target, &link).unwrap();
        link
    },
    state: {
        got_event: bool = false,
    },
    on_event: |count, temp_dir, got_event| {
        let target = temp_dir.path().join("target.txt");
        if count == 1 {
            eprintln!("Creating symlink target");
            fs::write(&target, b"target content").await?;
        } else {
            *got_event = true;
        }
    },
    verify: {
        assert!(got_event, "Did not receive event when symlink target was created");
    }
}

// Test deleting and recreating symlink target (watches resolve through symlinks)
#[cfg(unix)]
watch_test! {
    name: test_watch_symlink_recreate,
    interest: "[`Established, `Delete, `Create]",
    timeout_secs: 4,
    setup: |temp_dir| {
        use std::os::unix::fs::symlink;
        let target = temp_dir.path().join("target.txt");
        let link = temp_dir.path().join("link.txt");
        fs::write(&target, b"content").await?;
        symlink(&target, &link)?;
        link
    },
    state: {
        got_delete: bool = false,
        got_create: bool = false,
    },
    on_event: |count, temp_dir, got_delete, got_create| {
        let target = temp_dir.path().join("target.txt");
        if count == 1 {
            eprintln!("Deleting target");
            fs::remove_file(&target).await?;
            tokio::time::sleep(Duration::from_millis(500)).await;
            eprintln!("Recreating target");
            fs::write(&target, b"content").await?;
        } else if count == 2 {
            *got_delete = true;
        } else if count == 3 {
            *got_create = true;
        }
    },
    verify: {
        assert!(got_delete, "Did not receive delete event for target");
        assert!(got_create, "Did not receive create event for target");
    }
}

// Test SetGlobals - disable polling
run!(
    test_watch_set_globals_disable_polling,
    r#"fs::set_global_watch_parameters(#poll_batch_size: 0, #poll_interval: duration:1.s)"#,
    |v: Result<&Value>| { matches!(v, Ok(Value::Null)) }
);
