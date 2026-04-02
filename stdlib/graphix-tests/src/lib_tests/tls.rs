use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

fn cert_dir() -> String {
    concat!(env!("CARGO_MANIFEST_DIR"), "/certs").replace('\\', "/")
}

// TLS round-trip: connect + accept, then write/read through upgraded streams
run!(tls_round_trip, { let cd = cert_dir(); format!(r#"{{
    let cert = sys::fs::read_all_bin("{cd}/server.pem")$;
    let key = sys::fs::read_all_bin("{cd}/server.key")$;
    let ca = sys::fs::read_all_bin("{cd}/ca.pem")$;
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client_tcp = sys::tcp::connect(addr)?;
    let server_tcp = sys::tcp::accept(listener, client_tcp)?;
    let server = sys::tls::accept(#cert: cert, #key: key, server_tcp)?;
    let client = sys::tls::connect(#ca_cert: ca, "127.0.0.1", client_tcp)?;
    sys::io::write_exact(client ~ server, buffer::from_string("hello tls"))?;
    buffer::to_string(sys::io::read(server ~ client, u64:1024)?)?
}}"#) }, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello tls")
});
