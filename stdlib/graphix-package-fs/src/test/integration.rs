use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

// Test that writes and then reads a file using Graphix
const WRITE_THEN_READ: &str = r#"{
  let temp = fs::tempdir(null)?;
  let path = fs::join_path(temp, "write_read_test.txt");
  let write_result = fs::write_all(#path: path, "Test content");
  fs::read_all(write_result ~ path)
}"#;

run!(test_write_then_read, WRITE_THEN_READ, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "Test content")
});

// Test that watches a directory, writes to a file, and receives modify events
const WRITE_THEN_WATCH_MODIFY: &str = r#"{
  let paths = {
    let temp = fs::tempdir(null)?;
    let file_path = fs::join_path(temp, "watch_write_test.txt");
    let write_result = fs::write_all(#path: file_path, "initial");
    {dir: temp, file: write_result ~ file_path}
  };

  let watch_stream = fs::watch(#interest: [`Established, `Modify], paths.dir);
  let established = once(watch_stream);
  let modify_event = skip(#n:1, watch_stream);
  let write_done = fs::write_all(#path: established ~ paths.file, "modified by write_all");
  let content = fs::read_all(write_done ~ paths.file);

  let content_ok = content == "modified by write_all";
  let modify_ok = modify_event != "";

  content_ok && modify_ok
}"#;

run!(test_write_then_watch_modify, WRITE_THEN_WATCH_MODIFY, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// Test that writes binary data and then reads it back using Graphix
const WRITE_BIN_THEN_READ_BIN: &str = r#"{
  let temp = fs::tempdir(null)?;
  let path = fs::join_path(temp, "binary_cycle.bin");
  let write_result = fs::write_all_bin(#path: path, bytes:SGVsbG8=);
  fs::read_all_bin(write_result ~ path)
}"#;

run!(test_write_bin_then_read_bin, WRITE_BIN_THEN_READ_BIN, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bytes(b)) if b.as_ref() == b"Hello")
});
