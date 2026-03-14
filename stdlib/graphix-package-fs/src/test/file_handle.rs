use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

// write + seek + read round-trip
const WRITE_SEEK_READ: &str = r#"{
  use fs::file;
  let temp = fs::tempdir::create(null)?;
  let path = fs::join_path(fs::tempdir::path(temp), "test.txt");
  let f = open(`Create, path)?;
  let written = write(buffer::from_string("hello"), f)?;
  let pos = written ~ `Start(u64:0);
  let seeked = seek(pos, f)?;
  let n = seeked ~ written;
  buffer::to_string(read(n, f)?)
}"#;

run!(test_write_seek_read, WRITE_SEEK_READ, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
});

// write_exact + read_exact round-trip
const WRITE_EXACT_READ_EXACT: &str = r#"{
  use fs::file;
  let temp = fs::tempdir::create(null)?;
  let path = fs::join_path(fs::tempdir::path(temp), "test2.txt");
  let f = open(`Create, path)?;
  let written = write_exact(buffer::from_string("hello world"), f);
  let pos = written? ~ `Start(u64:0);
  let seeked = seek(pos, f)?;
  buffer::to_string(read_exact(u64:1024, seeked ~ f)?)
}"#;

run!(test_write_exact_read_exact, WRITE_EXACT_READ_EXACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello world")
});

// open non-existent with Read mode expects error
const OPEN_NONEXISTENT: &str = r#"{
  use fs::file;
  open(`Read, "/this/does/not/exist/at/all.txt")
}"#;

run!(test_open_nonexistent, OPEN_NONEXISTENT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(_)))
});

// fstat after write
const FSTAT_AFTER_WRITE: &str = r#"{
  use fs::file;
  let temp = fs::tempdir::create(null)?;
  let path = fs::join_path(fs::tempdir::path(temp), "fstat.txt");
  let f = open(`Create, path)?;
  let written = write_exact(buffer::from_string("12345"), f);
  let md = fstat(written? ~ f)?;
  md.len == u64:5
}"#;

run!(test_fstat_after_write, FSTAT_AFTER_WRITE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// truncate
const TRUNCATE_TEST: &str = r#"{
  use fs::file;
  let temp = fs::tempdir::create(null)?;
  let path = fs::join_path(fs::tempdir::path(temp), "trunc.txt");
  let f = open(`Create, path)?;
  let written = write_exact(buffer::from_string("hello world"), f);
  let tlen = written? ~ u64:5;
  let truncated = truncate(tlen, f);
  let pos = truncated? ~ `Start(u64:0);
  let seeked = seek(pos, f)?;
  buffer::to_string(read_exact(u64:1024, seeked ~ f)?)
}"#;

run!(test_truncate, TRUNCATE_TEST, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
});

// CreateNew on existing file expects error
const CREATE_NEW_EXISTING: &str = r#"{
  use fs::file;
  let temp = fs::tempdir::create(null)?;
  let path = fs::join_path(fs::tempdir::path(temp), "existing.txt");
  let f1 = open(`Create, path)?;
  let written = write_exact(buffer::from_string("first"), f1);
  written? ~ open(`CreateNew, path)
}"#;

run!(test_create_new_existing, CREATE_NEW_EXISTING, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(_)))
});
