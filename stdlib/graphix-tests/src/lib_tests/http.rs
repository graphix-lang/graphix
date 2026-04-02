use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

fn cert_dir() -> String {
    concat!(env!("CARGO_MANIFEST_DIR"), "/certs").replace('\\', "/")
}

run!(http_round_trip, r#"{
    let handler = |req: http::Request| {
        body: "hello [req.method]",
        headers: [],
        status: u16:200,
        url: ""
    };
    let server = http::serve(
        #addr: "127.0.0.1:0",
        #handler: handler
    )$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let resp = http::request(client, "http://[addr]/")$;
    resp.body
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello GET")
});

run!(https_round_trip, { let cd = cert_dir(); format!(r#"{{
    let cert = sys::fs::read_all_bin("{cd}/server.pem")$;
    let key = sys::fs::read_all_bin("{cd}/server.key")$;
    let handler = |req: http::Request| {{
        body: "hello [req.method]",
        headers: [],
        status: u16:200,
        url: ""
    }};
    let server = http::serve(
        #addr: "127.0.0.1:0",
        #cert: cert,
        #key: key,
        #handler: handler
    )$;
    let addr = http::server_addr(server);
    let ca = sys::fs::read_all_bin("{cd}/ca.pem")$;
    let client = http::client(#ca_cert: ca, server)$;
    let resp = http::request(client, "https://[addr]/")$;
    resp.body
}}"#) }, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello GET")
});
