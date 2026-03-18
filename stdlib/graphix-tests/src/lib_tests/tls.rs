use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

const CERT_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/certs");

// TLS round-trip: connect + accept, then write/read through upgraded streams
run!(tls_round_trip, format!(r#"{{
    let cert = fs::read_all_bin("{CERT_DIR}/server.pem")$;
    let key = fs::read_all_bin("{CERT_DIR}/server.key")$;
    let ca = fs::read_all_bin("{CERT_DIR}/ca.pem")$;
    let listener = net::tcp::listen("127.0.0.1:0")?;
    let addr = net::tcp::listener_addr(listener)?;
    let client_tcp = net::tcp::connect(addr)?;
    let server_tcp = net::tcp::accept(listener, client_tcp)?;
    let server = net::tls::accept(#cert: cert, #key: key, server_tcp)?;
    let client = net::tls::connect(#ca_cert: ca, "127.0.0.1", client_tcp)?;
    net::tcp::write_exact(client ~ server, buffer::from_string("hello tls"))?;
    buffer::to_string(net::tcp::read(server ~ client, u64:1024)?)?
}}"#), |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello tls")
});
