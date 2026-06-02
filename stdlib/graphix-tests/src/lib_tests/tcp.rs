use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

// Every fixture binds with port `:0` so the OS assigns a fresh
// ephemeral port per test invocation, then reads the actual
// address via `sys::tcp::listener_addr` before connecting. Fixed
// ports caused races between the three-mode expansion's
// concurrent `interp` / `fused` / `jit` runs of the same fixture.

// Basic listen + connect + accept
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

// Connect to unbound port fails. We can't easily pick a guaranteed-
// unbound ephemeral port, so use port 1 (universally reserved
// privileged port that nothing's listening on in a test
// environment).
const TCP_CONNECT_FAIL: &str = r#"
  is_err(sys::tcp::connect("127.0.0.1:1"))
"#;

run!(tcp_connect_fail, TCP_CONNECT_FAIL, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

// Listen on already-bound port fails. Bind once with port 0 to
// claim a fresh ephemeral, then try to listen on the same actual
// address — the second bind must error.
const TCP_LISTEN_FAIL: &str = r#"
{
  let l1 = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(l1)?;
  is_err(sys::tcp::listen(l1 ~ addr))
}
"#;

run!(tcp_listen_fail, TCP_LISTEN_FAIL, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

// Write on client, read on server
const TCP_WRITE_READ: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  sys::io::write(client, buffer::from_string("hello"))?;
  buffer::to_string(sys::io::read(server, u64:1024)?)?
}
"#;

run!(tcp_write_read, TCP_WRITE_READ, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
}; graphix_package_core::testing::FuseExpect::None);

// write_exact on client, read on server
const TCP_WRITE_EXACT: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  sys::io::write_exact(client, buffer::from_string("world"))?;
  buffer::to_string(sys::io::read(server, u64:1024)?)?
}
"#;

run!(tcp_write_exact, TCP_WRITE_EXACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "world")
}; graphix_package_core::testing::FuseExpect::None);

// Write known data, read_exact on server
const TCP_READ_EXACT: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  sys::io::write(client, buffer::from_string("exact"))?;
  buffer::to_string(sys::io::read_exact(server, u64:5)?)?
}
"#;

run!(tcp_read_exact, TCP_READ_EXACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "exact")
}; graphix_package_core::testing::FuseExpect::None);

// Shutdown returns null (wait for accept before shutting down)
const TCP_SHUTDOWN: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  sys::tcp::shutdown(server ~ client)?
}
"#;

run!(tcp_shutdown, TCP_SHUTDOWN, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
}; graphix_package_core::testing::FuseExpect::None);

// peer_addr on client returns server address. Compare the
// returned address against the listener's bound address inside
// graphix so the predicate doesn't need to know the exact port.
const TCP_PEER_ADDR: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  (server ~ sys::tcp::peer_addr(client)?) == addr
}
"#;

run!(tcp_peer_addr, TCP_PEER_ADDR, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

// local_addr on server matches listener address.
const TCP_LOCAL_ADDR: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  sys::tcp::local_addr(server)? == addr
}
"#;

run!(tcp_local_addr, TCP_LOCAL_ADDR, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

// write returns number of bytes written
const TCP_WRITE_RETURNS_LEN: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:0")?;
  let addr = sys::tcp::listener_addr(listener)?;
  let client = sys::tcp::connect(listener ~ addr)?;
  let server = sys::tcp::accept(listener, client)?;
  sys::io::write(server ~ client, buffer::from_string("hello"))?
}
"#;

run!(tcp_write_returns_len, TCP_WRITE_RETURNS_LEN, |v: Result<&Value>| {
    matches!(v, Ok(Value::U64(5)))
}; graphix_package_core::testing::FuseExpect::Jit);
