use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

fn cert_dir() -> String {
    concat!(env!("CARGO_MANIFEST_DIR"), "/certs").replace('\\', "/")
}

// TLS round-trip: connect + accept, then write/read through upgraded streams
// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(tls_round_trip, { let cd = cert_dir(); format!(r#"{{
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
}; graphix_package_core::testing::FuseExpect::None);

// Trait dispatch over a UNION of two Rust-backed abstract types: the
// generated select tests each member's tag, which a Rust-backed value
// answers by the path-derived wrapper UUID its package registered.
run!(socket_union_dispatch, { let cd = cert_dir(); format!(r#"{{
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
}; graphix_package_core::testing::FuseExpect::None);
