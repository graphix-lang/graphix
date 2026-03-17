use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

const CERT_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/certs");

run!(http_round_trip, r#"{
    let handler = |req: net::http::Request| {
        body: "hello [req.method]",
        headers: [],
        status: u16:200,
        url: ""
    };
    let server = net::http::serve(
        #addr: "127.0.0.1:0",
        #handler: handler
    )$;
    let addr = net::http::server_addr(server);
    let client = net::http::default_client(server)$;
    let resp = net::http::request(client, "http://[addr]/")$;
    resp.body
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello GET")
});

run!(https_round_trip, format!(r#"{{
    let cert = fs::read_all_bin("{CERT_DIR}/server.pem")$;
    let key = fs::read_all_bin("{CERT_DIR}/server.key")$;
    let handler = |req: net::http::Request| {{
        body: "hello [req.method]",
        headers: [],
        status: u16:200,
        url: ""
    }};
    let server = net::http::serve(
        #addr: "127.0.0.1:0",
        #cert: cert,
        #key: key,
        #handler: handler
    )$;
    let addr = net::http::server_addr(server);
    let ca = fs::read_all_bin("{CERT_DIR}/ca.pem")$;
    let client = net::http::client(#ca_cert: ca, server)$;
    let resp = net::http::request(client, "https://[addr]/")$;
    resp.body
}}"#), |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello GET")
});
