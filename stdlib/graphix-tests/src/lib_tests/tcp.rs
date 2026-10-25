use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx::subscriber::Value;

// Every fixture binds port `:0` and reads the assigned address via
// `sys::tcp::listener_addr`; the four modes run concurrently.

// listen + connect + accept.
const TCP_CONNECT_ACCEPT: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  server ~ true
}
"#;

run!(tcp_connect_accept, TCP_CONNECT_ACCEPT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

// Connecting to port 1 (reserved, nothing listening) fails.
const TCP_CONNECT_FAIL: &str = r#"
  is_err(sys::tcp::connect("127.0.0.1:1"))
"#;

run!(tcp_connect_fail, TCP_CONNECT_FAIL, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::None);

// Listening on an already-bound address fails.
const TCP_LISTEN_FAIL: &str = r#"
{
  let l1 = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(l1)?;
  is_err(sys::tcp::listen(l1 ~ addr))
}
"#;

run!(tcp_listen_fail, TCP_LISTEN_FAIL, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

// A read whose arm sleeps finishes asleep and answers at the wake: the
// bytes it took before the sleep are not lost (x-builtin-effects-03,
// sys-io-07).
const TCP_READ_ACROSS_SLEEP: &str = r#"
{
  use sys::io::{Read, Write};
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  let w1 = Write::write_exact(server, buffer::from_string("hello"))?;
  let on = true;
  on <- w1 ~ false;
  let later = select on { false => on ~ server, true => never() };
  let w2 = Write::write_exact(later, buffer::from_string("0123456789"))?;
  on <- w2 ~ true;
  let got = select on { true => Read::read_exact(client, u64:10)?, false => never() };
  buffer::to_string(got)?
}
"#;

run!(tcp_read_across_sleep, TCP_READ_ACROSS_SLEEP, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello01234")
}; FuseExpect::Jit);

// Write on the client, read on the server.
const TCP_WRITE_READ: &str = r#"
{
  use sys::io::{Read, Write};
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  Write::write(client, buffer::from_string("hello"))?;
  buffer::to_string(Read::read(server, u64:1024)?)?
}
"#;

run!(tcp_write_read, TCP_WRITE_READ, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
}; FuseExpect::Jit);

// write_exact on the client, read on the server.
const TCP_WRITE_EXACT: &str = r#"
{
  use sys::io::{Read, Write};
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  Write::write_exact(client, buffer::from_string("world"))?;
  buffer::to_string(Read::read(server, u64:1024)?)?
}
"#;

run!(tcp_write_exact, TCP_WRITE_EXACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "world")
}; FuseExpect::Jit);

// read_exact on the server.
const TCP_READ_EXACT: &str = r#"
{
  use sys::io::{Read, Write};
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  Write::write(client, buffer::from_string("exact"))?;
  buffer::to_string(Read::read_exact(server, u64:5)?)?
}
"#;

run!(tcp_read_exact, TCP_READ_EXACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "exact")
}; FuseExpect::Jit);

// shutdown returns null (after accept).
const TCP_SHUTDOWN: &str = r#"
{
  use sys::tcp::Socket;
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  Socket::shutdown(server ~ client)?
}
"#;

run!(tcp_shutdown, TCP_SHUTDOWN, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
}; FuseExpect::Jit);

// peer_addr on the client is the listener's bound address.
const TCP_PEER_ADDR: &str = r#"
{
  use sys::tcp::Socket;
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  (server ~ Socket::peer_addr(client)?) == addr
}
"#;

run!(tcp_peer_addr, TCP_PEER_ADDR, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

// local_addr on the server is the listener's address.
const TCP_LOCAL_ADDR: &str = r#"
{
  use sys::tcp::Socket;
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  Socket::local_addr(server)? == addr
}
"#;

run!(tcp_local_addr, TCP_LOCAL_ADDR, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

// write returns the number of bytes written.
const TCP_WRITE_RETURNS_LEN: &str = r#"
{
  use sys::io::Write;
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  Write::write(server ~ client, buffer::from_string("hello"))?
}
"#;

run!(tcp_write_returns_len, TCP_WRITE_RETURNS_LEN, |v: Result<&Value>| {
    matches!(v, Ok(Value::U64(5)))
}; FuseExpect::Jit);

// The listener is replaced while an accept on the old one is pending: the
// new invocation aborts it, so the new listener's client is accepted and
// the old listener, which nothing holds now, has closed its port.
const TCP_ACCEPT_REPLACED_LISTENER: &str = r#"
{
  use sys::tcp::Socket;
  let addr = "127.0.0.1:0";
  let l = sys::tcp::listen(addr)?;
  let la = sys::tcp::listener_addr(l)?;
  let first = once(la);
  addr <- first ~ "127.0.0.1:0";
  let srv = sys::tcp::accept(l, null)?;
  let settled = sys::time::after_idle(duration:300.ms, la);
  let cl = sys::tcp::connect(settled)?;
  let old = sys::tcp::connect(settled ~ first);
  Socket::local_addr(srv)? == (cl ~ la) && is_err(old)
}
"#;

run!(tcp_accept_replaced_listener, TCP_ACCEPT_REPLACED_LISTENER, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

// A deleted slot aborts its pending accept, which drops the listener and
// frees the port.
const TCP_ACCEPT_DELETED: &str = r#"
{
  let addrs = ["127.0.0.1:0"];
  let las = array::map(addrs, |a| {
    let l = sys::tcp::listen(a)?;
    let pending = sys::tcp::accept(l, null);
    sys::tcp::listener_addr(l)?
  });
  let first = never();
  first <- once(las[0]$);
  addrs <- first ~ [];
  let settled = sys::time::after_idle(duration:300.ms, array::len(las));
  is_err(sys::tcp::connect(settled ~ first))
}
"#;

run!(tcp_accept_deleted, TCP_ACCEPT_DELETED, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

// A trigger that fires every cycle replaces each accept with the next, and
// an accept that completed before it was replaced still answers: every
// client is accepted and none is closed.
const TCP_ACCEPT_BUSY_TRIGGER: &str = r#"
{
  use sys::io::Read;
  let l = sys::tcp::listen("127.0.0.1:0")$;
  let addr = sys::tcp::listener_addr(l)$;
  let clients = array::init(50, |i| sys::tcp::connect(addr)$);
  let ev = never();
  let held = array::map(clients, |c| { ev <- Read::read(c, u64:1) ~ 1; c });
  let lost = 0;
  lost <- ev ~ lost + 1;
  let tick = 0;
  tick <- tick + 1;
  let conn = sys::tcp::accept(l, tick)$;
  let all = [];
  all <- conn ~ array::push(all, conn);
  sys::time::timer(duration:1.s, false) ~ (array::len(all), lost)
}
"#;

run!(tcp_accept_busy_trigger, TCP_ACCEPT_BUSY_TRIGGER, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(50), Value::I64(0)])
});
