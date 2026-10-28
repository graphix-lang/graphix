use anyhow::Result;
use compact_str::format_compact;
use graphix_package_core::{run, testing::FuseExpect};
use netidx::subscriber::Value;

const WRITE_SEEK_READ: &str = r#"{
  use sys::fs::{Seek, open};
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
  use sys::fs::{Seek, open};
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
  use sys::fs::open;
  open(`Read, "/this/does/not/exist/at/all.txt")
}"#;

run!(test_open_nonexistent, OPEN_NONEXISTENT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(_)))
}; FuseExpect::None);

// fstat after write; flush is required (macOS updates metadata on flush).
const FSTAT_AFTER_WRITE: &str = r#"{
  use sys::fs::{fstat, open};
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
  use sys::fs::{Seek, open, truncate};
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
  use sys::fs::open;
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

// An fstat beside a line reader leaves it reading: only what consumes
// the stream or moves its position ends the reader (x-rerun-5-01).
run!(
    fstat_leaves_lines_reading,
    r#"{
  use sys::fs::{fstat, open, write_all};
  use sys::io::Lines;
  let temp = sys::fs::tempdir::create(null)?;
  let path = sys::join_path(sys::fs::tempdir::path(temp), "lines.txt");
  let body = str::join(#sep: "\n", array::init(20000, |i| "line [i]"));
  let wrote = write_all(#path: path, body)?;
  let f = open(`Read, wrote ~ path)?;
  let ls = Lines::lines_batched(f)?;
  let n = 0;
  n <- ls ~ n + array::len(ls);
  let st = fstat(ls ~ f)?;
  select n { 20000 => n, _ => never() }
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(20000)))
);

// An open whose arm sleeps while it runs finishes asleep and nothing
// answers it: a `CreateNew` makes its file, and the handle is closed unseen.
const OPEN_IS_NOT_ANSWERED_AFTER_A_SLEEP: &str = r#"{
  use sys::fs::open;
  let temp = sys::fs::tempdir::create(null)?;
  let path = sys::join_path(sys::fs::tempdir::path(temp), "new.txt");
  let on = true;
  let f = select on { true => open(`CreateNew, path)?, false => never() };
  on <- path ~ false;
  on <- on ~ select on { false => true, true => never() };
  let answered = false;
  answered <- f ~ true;
  let made = sys::fs::is_file(sys::time::timer(duration: 10.ms, true) ~ path);
  let there = select made { error as _ => never(), p => p };
  skip(#n: 4, there) ~ answered
}"#;

run!(test_open_is_not_answered_after_a_sleep, OPEN_IS_NOT_ANSWERED_AFTER_A_SLEEP, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(false)))
}; FuseExpect::Jit);

const LINES_RESTART_AFTER_SEEK: &str = r#"{
  use sys::fs::{Seek, open, write_all};
  use sys::io::Lines;
  let temp = sys::fs::tempdir::create(null)?;
  let path = sys::join_path(sys::fs::tempdir::path(temp), "lines.txt");
  let wrote = write_all(#path: path, "a\nb\n")?;
  let f = open(`Read, wrote ~ path)?;
  let src = f;
  let batch = select batched {
    true => Lines::lines_batched(src)?,
    false => [Lines::lines(src)?]
  };
  let all = [];
  all <- batch ~ array::concat(all, batch);
  let first = select all { ["a", "b"] => null, _ => never() };
  let rewind = Seek::seek(sys::time::after_idle(duration:20.ms, first) ~ f, `Start(u64:0))?;
  src <- rewind ~ f;
  select all { ["a", "b", "a", "b"] => true, _ => never() }
}"#;

run!(
    lines_restart_after_seek,
    format_compact!("{{ let batched = false; {LINES_RESTART_AFTER_SEEK} }}").as_str(),
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true)))
);

run!(
    batched_lines_restart_after_seek,
    format_compact!("{{ let batched = true; {LINES_RESTART_AFTER_SEEK} }}").as_str(),
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true)))
);

run!(
    lines_restart_after_appending,
    r#"{
  use sys::fs::{open, write_all};
  use sys::io::{Lines, Write};
  let temp = sys::fs::tempdir::create(null)?;
  let path = sys::join_path(sys::fs::tempdir::path(temp), "lines.txt");
  let wrote = write_all(#path: path, "a\nb\n")?;
  let f = open(`Read, wrote ~ path)?;
  let src = f;
  let line = Lines::lines(src)?;
  let all = [];
  all <- line ~ array::push(all, line);
  let first = select all { ["a", "b"] => null, _ => never() };
  let append = open(`Append, sys::time::after_idle(duration:20.ms, first) ~ path)?;
  let wrote_more = Write::write_exact(append, buffer::from_string("c\n"))?;
  let flushed = Write::flush(wrote_more ~ append)?;
  src <- flushed ~ f;
  select all { ["a", "b", "c"] => true, _ => never() }
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true)))
);

run!(
    lines_restart_after_error,
    r#"{
  use sys::fs::open;
  use sys::io::Lines;
  let temp = sys::fs::tempdir::create(null)?;
  let path = sys::join_path(sys::fs::tempdir::path(temp), "lines.txt");
  let f = open(`Write, path)?;
  let src = f;
  let line = Lines::lines(src);
  let n = 0;
  n <- line ~ select line { error as _ => n + 1, _ => never() };
  let first = select n { 1 => f, _ => never() };
  src <- sys::time::after_idle(duration:20.ms, first);
  select n { 2 => true, _ => never() }
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true)))
);

run!(
    lines_restart_does_not_rewind,
    r#"{
  use sys::fs::{open, write_all};
  use sys::io::Lines;
  let temp = sys::fs::tempdir::create(null)?;
  let path = sys::join_path(sys::fs::tempdir::path(temp), "lines.txt");
  let wrote = write_all(#path: path, "a\nb\n")?;
  let f = open(`Read, wrote ~ path)?;
  let src = f;
  let line = Lines::lines(src)?;
  let all = [];
  all <- line ~ array::push(all, line);
  let first = select all { ["a", "b"] => f, _ => never() };
  let retry = once(sys::time::after_idle(duration:20.ms, first));
  src <- retry;
  sys::time::after_idle(duration:50.ms, retry) ~ (all == ["a", "b"])
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Bool(true)))
);
