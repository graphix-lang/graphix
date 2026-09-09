use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

// Every fixture binds port `:0` and reads the assigned address via
// `sys::tcp::listener_addr`; the three modes run concurrently.

// listen + connect + accept.
const TCP_CONNECT_ACCEPT: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  sys::tcp::accept(listener, client)?;
  true
}
"#;

run!(tcp_connect_accept, TCP_CONNECT_ACCEPT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

// Connecting to port 1 (reserved, nothing listening) fails.
const TCP_CONNECT_FAIL: &str = r#"
  is_err(sys::tcp::connect("127.0.0.1:1"))
"#;

run!(tcp_connect_fail, TCP_CONNECT_FAIL, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::None);

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
}; graphix_package_core::testing::FuseExpect::None);

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
}; graphix_package_core::testing::FuseExpect::None);

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
}; graphix_package_core::testing::FuseExpect::None);

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
}; graphix_package_core::testing::FuseExpect::None);

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
}; graphix_package_core::testing::FuseExpect::None);

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
}; graphix_package_core::testing::FuseExpect::None);

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
}; graphix_package_core::testing::FuseExpect::None);

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
}; graphix_package_core::testing::FuseExpect::None);
