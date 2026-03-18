use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

// Basic listen + connect + accept
const TCP_CONNECT_ACCEPT: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:19801")?;
  let client = sys::tcp::connect(listener ~ "127.0.0.1:19801")?;
  sys::tcp::accept(listener, client)?;
  true
}
"#;

run!(tcp_connect_accept, TCP_CONNECT_ACCEPT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// Connect to unbound port fails
const TCP_CONNECT_FAIL: &str = r#"
  is_err(sys::tcp::connect("127.0.0.1:1"))
"#;

run!(tcp_connect_fail, TCP_CONNECT_FAIL, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// Listen on already-bound port fails
const TCP_LISTEN_FAIL: &str = r#"
{
  let l1 = sys::tcp::listen("127.0.0.1:19809")?;
  is_err(sys::tcp::listen(l1 ~ "127.0.0.1:19809"))
}
"#;

run!(tcp_listen_fail, TCP_LISTEN_FAIL, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// Write on client, read on server
const TCP_WRITE_READ: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:19802")?;
  let client = sys::tcp::connect(listener ~ "127.0.0.1:19802")?;
  let server = sys::tcp::accept(listener, client)?;
  sys::io::write(client, buffer::from_string("hello"))?;
  buffer::to_string(sys::io::read(server, u64:1024)?)?
}
"#;

run!(tcp_write_read, TCP_WRITE_READ, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
});

// write_exact on client, read on server
const TCP_WRITE_EXACT: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:19803")?;
  let client = sys::tcp::connect(listener ~ "127.0.0.1:19803")?;
  let server = sys::tcp::accept(listener, client)?;
  sys::io::write_exact(client, buffer::from_string("world"))?;
  buffer::to_string(sys::io::read(server, u64:1024)?)?
}
"#;

run!(tcp_write_exact, TCP_WRITE_EXACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "world")
});

// Write known data, read_exact on server
const TCP_READ_EXACT: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:19804")?;
  let client = sys::tcp::connect(listener ~ "127.0.0.1:19804")?;
  let server = sys::tcp::accept(listener, client)?;
  sys::io::write(client, buffer::from_string("exact"))?;
  buffer::to_string(sys::io::read_exact(server, u64:5)?)?
}
"#;

run!(tcp_read_exact, TCP_READ_EXACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "exact")
});

// Shutdown returns null (wait for accept before shutting down)
const TCP_SHUTDOWN: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:19805")?;
  let client = sys::tcp::connect(listener ~ "127.0.0.1:19805")?;
  let server = sys::tcp::accept(listener, client)?;
  sys::tcp::shutdown(server ~ client)?
}
"#;

run!(tcp_shutdown, TCP_SHUTDOWN, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
});

// peer_addr on client returns server address
const TCP_PEER_ADDR: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:19806")?;
  let client = sys::tcp::connect(listener ~ "127.0.0.1:19806")?;
  let server = sys::tcp::accept(listener, client)?;
  server ~ sys::tcp::peer_addr(client)?
}
"#;

run!(tcp_peer_addr, TCP_PEER_ADDR, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "127.0.0.1:19806")
});

// local_addr on server matches listener address
const TCP_LOCAL_ADDR: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:19807")?;
  let client = sys::tcp::connect(listener ~ "127.0.0.1:19807")?;
  let server = sys::tcp::accept(listener, client)?;
  sys::tcp::local_addr(server)?
}
"#;

run!(tcp_local_addr, TCP_LOCAL_ADDR, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "127.0.0.1:19807")
});

// write returns number of bytes written
const TCP_WRITE_RETURNS_LEN: &str = r#"
{
  let listener = sys::tcp::listen("127.0.0.1:19808")?;
  let client = sys::tcp::connect(listener ~ "127.0.0.1:19808")?;
  let server = sys::tcp::accept(listener, client)?;
  sys::io::write(server ~ client, buffer::from_string("hello"))?
}
"#;

run!(tcp_write_returns_len, TCP_WRITE_RETURNS_LEN, |v: Result<&Value>| {
    matches!(v, Ok(Value::U64(5)))
});
