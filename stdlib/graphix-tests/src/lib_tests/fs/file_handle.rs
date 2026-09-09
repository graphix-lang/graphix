use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

const WRITE_SEEK_READ: &str = r#"{
  use sys::fs::{self, *};
  use sys::io::{Read, Write};

  let temp = sys::fs::tempdir::create(null)?;
  let path = sys::join_path(sys::fs::tempdir::path(temp), "test.txt");
  let f = open(`Create, path)?;
  let written = Write::write(f, buffer::from_string("hello"))?;
  let pos = written ~ `Start(u64:0);
  let seeked = Seek::seek(f, pos)?;
  let n = seeked ~ written;
  buffer::to_string(Read::read(f, n)?)
}"#;

run!(test_write_seek_read, WRITE_SEEK_READ, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
});

const WRITE_EXACT_READ_EXACT: &str = r#"{
  use sys::fs::{self, *};
  use sys::io::{Read, Write};

  let temp = sys::fs::tempdir::create(null)?;
  let path = sys::join_path(sys::fs::tempdir::path(temp), "test2.txt");
  let f = open(`Create, path)?;
  let written = Write::write_exact(f, buffer::from_string("hello world"));
  let pos = written? ~ `Start(u64:0);
  let seeked = Seek::seek(f, pos)?;
  buffer::to_string(Read::read_exact(seeked ~ f, u64:1024)?)
}"#;

run!(test_write_exact_read_exact, WRITE_EXACT_READ_EXACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello world")
});

const OPEN_NONEXISTENT: &str = r#"{
  use sys::fs::{self, *};
  open(`Read, "/this/does/not/exist/at/all.txt")
}"#;

run!(test_open_nonexistent, OPEN_NONEXISTENT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(_)))
}; graphix_package_core::testing::FuseExpect::None);

// fstat after write; flush is required (macOS updates metadata on flush).
const FSTAT_AFTER_WRITE: &str = r#"{
  use sys::fs::{self, *};
  use sys::io::Write;

  let temp = sys::fs::tempdir::create(null)?;
  let path = sys::join_path(sys::fs::tempdir::path(temp), "fstat.txt");
  let f = open(`Create, path)?;
  let written = Write::write_exact(f, buffer::from_string("12345"));
  let flushed = Write::flush(written? ~ f);
  let md = fstat(flushed? ~ f)?;
  md.len == u64:5
}"#;

run!(test_fstat_after_write, FSTAT_AFTER_WRITE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

const TRUNCATE_TEST: &str = r#"{
  use sys::fs::{self, *};
  use sys::io::{Read, Write};

  let temp = sys::fs::tempdir::create(null)?;
  let path = sys::join_path(sys::fs::tempdir::path(temp), "trunc.txt");
  let f = open(`Create, path)?;
  let written = Write::write_exact(f, buffer::from_string("hello world"));
  let tlen = written? ~ u64:5;
  let truncated = truncate(f, tlen);
  let pos = truncated? ~ `Start(u64:0);
  let seeked = Seek::seek(f, pos)?;
  buffer::to_string(Read::read_exact(seeked ~ f, u64:1024)?)
}"#;

run!(test_truncate, TRUNCATE_TEST, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
});

// CreateNew on an existing file errors. `open(`CreateNew, written? ~ path)`
// samples the argument so the CreateNew open runs after the write, not
// concurrently with the Create open.
const CREATE_NEW_EXISTING: &str = r#"{
  use sys::fs::{self, *};
  use sys::io::Write;

  let temp = sys::fs::tempdir::create(null)?;
  let path = sys::join_path(sys::fs::tempdir::path(temp), "existing.txt");
  let f1 = open(`Create, path)?;
  let written = Write::write_exact(f1, buffer::from_string("first"));
  open(`CreateNew, written? ~ path)
}"#;

run!(test_create_new_existing, CREATE_NEW_EXISTING, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(_)))
});
