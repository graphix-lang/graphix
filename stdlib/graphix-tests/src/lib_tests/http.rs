use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx::subscriber::Value;

// CR claude for claude: [test-gap] All five http tests send one request to a synchronous
// handler that reads req.method, the one shape under which the server's reply-pairing,
// wedge, restart and TLS-accept bugs cannot show. Add pins for: two sequential requests
// through a handler with two async lookups (each body must match its path), concurrent
// requests, a raising handler followed by a good request, a constant-body handler, a
// server restarted on the same fixed port, and an HTTPS request next to an idle TCP
// connection. (http-sqlite-db1-13)
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
}; FuseExpect::Jit);

run!(https_round_trip, { let cd = crate::lib_tests::cert_dir(); format!(r#"{{
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
}; FuseExpect::Jit);

run!(http_status_round_trip, r#"{
    let handler = |req: http::Request| {
        body: "created [req.method]",
        headers: [],
        status: u16:201,
        url: ""
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let resp = http::request(client, "http://[addr]/")$;
    resp.status
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::U16(201)))
}; FuseExpect::Jit);

run!(http_invalid_status_is_500, r#"{
    let handler = |req: http::Request| {
        body: "[req.method]",
        headers: [],
        status: u16:1000,
        url: ""
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let resp = http::request(client, "http://[addr]/")$;
    resp.status
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::U16(500)))
}; FuseExpect::Jit);

run!(http_invalid_header_is_500, r#"{
    let handler = |req: http::Request| {
        body: "[req.method]",
        headers: [("not a header", "x")],
        status: u16:200,
        url: ""
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let resp = http::request(client, "http://[addr]/")$;
    resp.status
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::U16(500)))
}; FuseExpect::Jit);
