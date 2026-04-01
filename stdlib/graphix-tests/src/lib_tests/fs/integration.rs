use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

// Test that writes and then reads a file using Graphix
const WRITE_THEN_READ: &str = r#"{
  let temp = sys::fs::tempdir::create(null)?;
  let path = sys::join_path(sys::fs::tempdir::path(temp), "write_read_test.txt");
  let write_result = sys::fs::write_all(#path: path, "Test content");
  sys::fs::read_all(write_result ~ path)
}"#;

run!(test_write_then_read, WRITE_THEN_READ, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "Test content")
});

// Test that watches a directory, writes to a file, and receives modify events.
// On macOS, FSEvents reports file writes as Create rather than Modify, so
// we include both in the interest set.
#[cfg(not(target_os = "macos"))]
const WRITE_THEN_WATCH_MODIFY: &str = r#"{
  let paths = {
    let temp = sys::fs::tempdir::create(null)?;
    let temp_path = sys::fs::tempdir::path(temp);
    let file_path = sys::join_path(temp_path, "watch_write_test.txt");
    let write_result = sys::fs::write_all(#path: file_path, "initial");
    {dir: temp_path, file: write_result ~ file_path}
  };

  use sys::fs::watch;
  let w = create(null)?;
  let handle = watch(#interest: [`Established, `Modify], w, paths.dir)?;
  let watch_path = path(handle);
  let established = once(watch_path);
  let modify_event = skip(#n:1, watch_path);
  let write_done = sys::fs::write_all(#path: established ~ paths.file, "modified by write_all");
  let content = sys::fs::read_all(write_done ~ paths.file);

  let content_ok = content == "modified by write_all";
  let modify_ok = modify_event != "";

  content_ok && modify_ok
}"#;

#[cfg(target_os = "macos")]
const WRITE_THEN_WATCH_MODIFY: &str = r#"{
  let paths = {
    let temp = sys::fs::tempdir::create(null)?;
    let temp_path = sys::fs::tempdir::path(temp);
    let file_path = sys::join_path(temp_path, "watch_write_test.txt");
    let write_result = sys::fs::write_all(#path: file_path, "initial");
    {dir: temp_path, file: write_result ~ file_path}
  };

  use sys::fs::watch;
  let w = create(null)?;
  let handle = watch(#interest: [`Established, `Modify, `Create], w, paths.dir)?;
  let watch_path = path(handle);
  let established = once(watch_path);
  let modify_event = skip(#n:1, watch_path);
  let write_done = sys::fs::write_all(#path: established ~ paths.file, "modified by write_all");
  let content = sys::fs::read_all(write_done ~ paths.file);

  let content_ok = content == "modified by write_all";
  let modify_ok = modify_event != "";

  content_ok && modify_ok
}"#;

run!(test_write_then_watch_modify, WRITE_THEN_WATCH_MODIFY, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// Test that writes binary data and then reads it back using Graphix
const WRITE_BIN_THEN_READ_BIN: &str = r#"{
  let temp = sys::fs::tempdir::create(null)?;
  let path = sys::join_path(sys::fs::tempdir::path(temp), "binary_cycle.bin");
  let write_result = sys::fs::write_all_bin(#path: path, bytes:SGVsbG8=);
  sys::fs::read_all_bin(write_result ~ path)
}"#;

run!(test_write_bin_then_read_bin, WRITE_BIN_THEN_READ_BIN, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bytes(b)) if b.as_ref() == b"Hello")
});
