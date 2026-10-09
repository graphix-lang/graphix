use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx::subscriber::Value;

// CR claude for eric: [test-gap] All five http tests send one request to a synchronous
// handler that reads req.method, the one shape under which the server's reply-pairing,
// wedge, restart and TLS-accept bugs cannot show. Add pins for: two sequential requests
// through a handler with two async lookups (each body must match its path), concurrent
// requests, a raising handler followed by a good request, a constant-body handler, a
// server restarted on the same fixed port, and an HTTPS request next to an idle TCP
// connection. (http-sqlite-db1-13)
// 2026-10-07 claude: pinned now: a handler that bottoms then a good request
// (http_bottom_handler_then_good), a restart on the same address
// (http_restart_same_address), #max_body, raw bodies and rest::post. Two async
// lookups in sequence and concurrent requests wait on the pairing (db1-01); the
// HTTPS-beside-an-idle-connection case has only its repro (x-panics-09.py).
// 2026-10-08 claude: re-addressed: the remaining pins (two async lookups in sequence,
// concurrent requests) wait on your db1-01 (reply pairing), and HTTPS beside an idle
// connection on x-panics-09.
// 2026-10-08 claude: re-addressed: the remaining pins (two async lookups in sequence,
// concurrent requests) wait on your db1-01 (reply pairing), and HTTPS beside an idle
// connection on x-panics-09.
// 2026-10-09 claude: http_bottom_handler_then_good is now http_raising_handler_then_good
// (a `?` raise answers 500); http_abandoned_request_then_good and
// http_async_handler_answers are new.
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

// A request the handler has no value for is answered 500 and does not
// hold up the next.
run!(http_raising_handler_then_good, r#"{
    let handler = |req: http::Request| {
        body: "hi [req.body?]",
        headers: [],
        status: u16:200,
        url: ""
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let r1 = http::request(client, "http://[addr]/")$;
    let r2 = http::request(#method: `POST, #body: "bob", r1 ~ client, "http://[addr]/")$;
    "[r1.status] [r2.status] [r2.body]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "500 200 hi bob")
}; FuseExpect::Jit);

// A handler with no value for a request leaves it unanswered; once its
// client gives up, the next request is served.
run!(http_abandoned_request_then_good, r#"{
    let handler = |req: http::Request| {
        body: "hi [req.body$]",
        headers: [],
        status: u16:200,
        url: ""
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let r1 = http::request(#timeout: duration:200.ms, client, "http://[addr]/");
    let r2 = http::request(#method: `POST, #body: "bob", r1 ~ client, "http://[addr]/")$;
    "[r2.status] [r2.body]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "200 hi bob")
}; FuseExpect::Jit);

// A reply that waits on an async value is sent when the value arrives.
run!(http_async_handler_answers, r#"{
    let handler = |req: http::Request| {
        let page = sys::time::after_idle(duration:50.ms, "p [req.path]");
        { body: "[req.method] [page]", headers: [], status: u16:200, url: "" }
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let r = http::request(client, "http://[addr]/a")$;
    "[r.status] [r.body]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "200 GET p /a")
}; FuseExpect::Jit);

// A restart on the same address keeps the socket: the port is the same and
// the restarted server answers.
run!(http_restart_same_address, r#"{
    let handler = |req: http::Request| {
        body: "ok [req.path]",
        headers: [],
        status: u16:200,
        url: ""
    };
    let mc = 10;
    let server = http::serve(#addr: "127.0.0.1:0", #max_connections: mc, #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(once(server))$;
    let r1 = http::request(client, "http://[once(addr)]/a")$;
    mc <- r1 ~ 20;
    let addr2 = skip(#n: 1, addr);
    let r2 = http::request(client, "http://[addr2]/b")$;
    "[once(addr) == addr2] [r2.body]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "true ok /b")
}; FuseExpect::Jit);

// A body over #max_body is answered 413 without reaching the handler.
run!(http_max_body, r#"{
    let handler = |req: http::Request| {
        body: "[req.body]",
        headers: [],
        status: u16:200,
        url: ""
    };
    let server = http::serve(#addr: "127.0.0.1:0", #max_body: 4, #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let r = http::request(#method: `POST, #body: "hello", client, "http://[addr]/")$;
    r.status
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::U16(413)))
}; FuseExpect::Jit);

// A body that is not UTF-8 arrives as raw bytes.
run!(http_raw_body, r#"{
    let handler = |req: http::Request| {
        body: "[req.body] [req.raw == bytes:/+4=]",
        headers: [],
        status: u16:200,
        url: ""
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let r = http::request_bin(#method: `POST, #body: bytes:/+4=, client, "http://[addr]/")$;
    r.body
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bytes(b)) if &***b == b"null true")
}; FuseExpect::Jit);

// rest::post sends JSON headers, the bearer and the body.
run!(http_rest_post, r#"{
    let handler = |req: http::Request| {
        let has = |name: string, value: string|
            array::len(array::filter(req.headers, |h: (string, string)| (h.0 == name) && (h.1 == value))) == 1;
        {
            body: "[req.method] [has("content-type", "application/json")] [has("authorization", "Bearer t")] [req.body$]",
            headers: [],
            status: u16:200,
            url: ""
        }
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let r = http::rest::post(#bearer: "t", #body: "x", client, "http://[addr]/")$;
    r.body
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "POST true true x")
}; FuseExpect::Jit);
