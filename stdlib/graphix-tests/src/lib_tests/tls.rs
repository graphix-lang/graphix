use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx::subscriber::Value;

// TLS round-trip: connect + accept, then write/read through upgraded
// streams.
run!(tls_round_trip, { let cd = crate::lib_tests::cert_dir(); format!(r#"{{
    use sys::io::{{Read, Write}};
    let cert = sys::fs::read_all_bin("{cd}/server.pem")$;
    let key = sys::fs::read_all_bin("{cd}/server.key")$;
    let ca = sys::fs::read_all_bin("{cd}/ca.pem")$;
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client_tcp = sys::tcp::connect(addr)?;
    let server_tcp = sys::tcp::accept(listener, client_tcp)?;
    let server = sys::tls::accept(#cert: cert, #key: key, server_tcp)?;
    let client = sys::tls::connect(#ca_cert: ca, "127.0.0.1", client_tcp)?;
    Write::write_exact(client ~ server, buffer::from_string("hello tls"))?;
    buffer::to_string(Read::read(server ~ client, u64:1024)?)?
}}"#) }, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello tls")
}; FuseExpect::None);

// Trait dispatch over a union of two Rust-backed abstract types: each
// member's tag test answers by its registered wrapper UUID.
run!(socket_union_dispatch, { let cd = crate::lib_tests::cert_dir(); format!(r#"{{
    use sys::io::{{Read, Write}};
    use sys::tcp::Socket;
    let cert = sys::fs::read_all_bin("{cd}/server.pem")$;
    let key = sys::fs::read_all_bin("{cd}/server.key")$;
    let ca = sys::fs::read_all_bin("{cd}/ca.pem")$;
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client_tcp = sys::tcp::connect(addr)?;
    let server_tcp = sys::tcp::accept(listener, client_tcp)?;
    let server = sys::tls::accept(#cert: cert, #key: key, server_tcp)?;
    let client = sys::tls::connect(#ca_cert: ca, "127.0.0.1", client_tcp)?;
    let either: [sys::tcp::TcpStream, sys::tls::TlsStream] = client;
    str::len(Socket::peer_addr(server ~ either)?) > 0
}}"#) }, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::None);

// A client that never speaks connects first; the handshakes run at once,
// so client 2's completes while the silent one is still pending.
run!(tls_accept_silent_client, { let cd = crate::lib_tests::cert_dir(); format!(r#"{{
    let cert = sys::fs::read_all_bin("{cd}/server.pem")$;
    let key = sys::fs::read_all_bin("{cd}/server.key")$;
    let ca = sys::fs::read_all_bin("{cd}/ca.pem")$;
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let n = 0;
    let conn = sys::tcp::accept(listener, n)?;
    n <- conn ~ n + 1;
    let server = sys::tls::accept(#cert: cert, #key: key, conn);
    let silent = sys::tcp::connect(addr)?;
    let c2 = sys::tcp::connect(sys::time::timer(duration:200.ms, false) ~ addr)?;
    let client = sys::tls::connect(#ca_cert: ca, "127.0.0.1", c2)?;
    client ~ true
}}"#) }, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

// A new certificate, key or CA applies to later streams: it starts no
// handshake on a stream already upgraded, so each side answers once.
run!(tls_config_update_no_rehandshake, { let cd = crate::lib_tests::cert_dir(); format!(r#"{{
    let cert = sys::fs::read_all_bin("{cd}/server.pem")$;
    let key = sys::fs::read_all_bin("{cd}/server.key")$;
    let ca = sys::fs::read_all_bin("{cd}/ca.pem")$;
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client_tcp = sys::tcp::connect(addr)?;
    let server_tcp = sys::tcp::accept(listener, client_tcp)?;
    let server = sys::tls::accept(#cert: cert, #key: key, server_tcp);
    let client = sys::tls::connect(#ca_cert: ca, "127.0.0.1", client_tcp);
    cert <- client ~ cert;
    ca <- server ~ ca;
    let settled = sys::time::after_idle(duration:300.ms, (count(server), count(client)));
    settled ~ [count(server), count(client)]
}}"#) }, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a.len() == 2 && a[0] == Value::I64(1) && a[1] == Value::I64(1))
}; FuseExpect::None);

// The streams arrive before the certificates: each handshake starts when
// its site first holds everything, not only at the stream's own fire.
run!(tls_stream_before_cert, { let cd = crate::lib_tests::cert_dir(); format!(r#"{{
    let late = sys::time::timer(duration:200.ms, false);
    let cert = late ~ sys::fs::read_all_bin("{cd}/server.pem")$;
    let key = sys::fs::read_all_bin("{cd}/server.key")$;
    let ca = late ~ sys::fs::read_all_bin("{cd}/ca.pem")$;
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client_tcp = sys::tcp::connect(addr)?;
    let server_tcp = sys::tcp::accept(listener, client_tcp)?;
    let server = sys::tls::accept(#cert: cert, #key: key, server_tcp)?;
    let client = sys::tls::connect(#ca_cert: ca, "127.0.0.1", client_tcp)?;
    (server, client) ~ true
}}"#) }, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::None);
