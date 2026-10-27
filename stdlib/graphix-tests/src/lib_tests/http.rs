use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{Collect, FuseExpect, Mode, collect},
};
use netidx::subscriber::Value;
use std::time::Duration;

run!(http_round_trip, r#"{
    let handler = |req: http::Request| {
        body: "hello [req.method]",
        headers: [],
        status: u16:200
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
        status: u16:200
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

// A client that connects and never speaks holds only its own handshake:
// a request beside it answers at once, well inside the handshake timeout.
run!(https_beside_a_silent_client, { let cd = crate::lib_tests::cert_dir(); format!(r#"{{
    let cert = sys::fs::read_all_bin("{cd}/server.pem")$;
    let key = sys::fs::read_all_bin("{cd}/server.key")$;
    let handler = |req: http::Request| {{
        body: "hello [req.method]",
        headers: [],
        status: u16:200
    }};
    let server = http::serve(
        #addr: "127.0.0.1:0",
        #cert: cert,
        #key: key,
        #handler: handler
    )$;
    let addr = http::server_addr(server);
    let silent = sys::tcp::connect(addr)$;
    let ca = sys::fs::read_all_bin("{cd}/ca.pem")$;
    let client = http::client(#ca_cert: ca, server)$;
    let resp = http::request(client, silent ~ "https://[addr]/")$;
    resp.body
}}"#) }, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello GET")
}, timeout: 5; FuseExpect::Jit);

// A #ca_cert holding no certificate is refused, never a client that
// trusts nothing (x-security-07).
run!(http_client_refuses_a_ca_without_certificate, r#"{
    let c = http::client(#ca_cert: buffer::from_string("this is not a certificate"), null);
    select c { error as e => "[e]", _ => "built" }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s))
        if &**s == "error:`HTTPError(\"invalid ca_cert PEM: no certificate in it\")")
}; FuseExpect::Jit);

run!(http_status_round_trip, r#"{
    let handler = |req: http::Request| {
        body: "created [req.method]",
        headers: [],
        status: u16:201
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
        status: u16:1000
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
        status: u16:200
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let resp = http::request(client, "http://[addr]/")$;
    resp.status
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::U16(500)))
}; FuseExpect::Jit);

// A request the server holds when it sleeps is answered 500.
run!(http_request_held_at_the_sleep_is_500, r#"{
    let up = true;
    up <- sys::time::timer(duration:300.ms, false) ~ false;
    let handler = |req: http::Request| {
        let t = sys::time::timer(duration:10.s, false);
        t ~ {body: "late [req.path]", headers: [], status: u16:200}
    };
    let addr: string = never();
    addr <- select up {
        true => http::server_addr(http::serve(#addr: "127.0.0.1:0", #handler: handler)$),
        false => never()
    };
    let client = http::default_client(null)$;
    let r = http::request(#timeout: duration:5.s, client, "http://[addr]/");
    select r { error as e => "[e]", r => "[r.status] [r.body]" }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "500 Internal Server Error")
}; FuseExpect::Jit);

// A handler's content-length is replaced by the body's own.
run!(http_handler_content_length_is_the_bodys, r#"{
    let handler = |req: http::Request| {
        body: "hello [req.path]",
        headers: [("Content-Length", "1"), ("x-kept", "yes")],
        status: u16:200
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let r = http::request(#timeout: duration:5.s, client, "http://[addr]/world");
    select r {
        error as e => "[e]",
        r => {
            let named = |n| array::map(array::filter(r.headers, |(k, _)| k == n), |(_, v)| v);
            "[r.body] [named("content-length")] [named("x-kept")]"
        }
    }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == r#"hello /world ["12"] ["yes"]"#)
}; FuseExpect::Jit);

// A request whose handler raises is answered 500 and does not hold up
// the next.
run!(http_raising_handler_then_good, r#"{
    let handler = |req: http::Request| {
        let text: Result<string, `Empty> = select buffer::to_string_lossy(req.raw) {
            "" => error(`Empty),
            s => s
        };
        { body: "hi [text?]", headers: [], status: u16:200 }
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
// client gives up, its connection is let go and the next request is
// served on the one connection the server allows.
// XCR claude for claude: [test-gap/medium] This fixture cannot fail if the abandoned
// request is never released. Under serve's default connection limit, r2 gets a
// connection of its own and is answered while r1 is still pending: in the probe, r2 is
// answered 0.4 s in while r1 has 4.5 s left on its timeout. A serve that kept the
// instance and permit of every request whose client went away would pass. Give the
// server `#max_connections: 1`, so r2 can only be served once r1's connection is let
// go; that version passes today. probe:
// design/review-2026-10-26/repro/tests-lib-b1-http-next-not-held.sh (tests-lib-b1-01)
// 2026-10-26 claude: The server now allows one connection (#max_connections: 1) and r2
// carries a 5 s timeout whose error the result prints, so r2 is served only once r1's
// abandoned connection and permit are let go. With serve_loop forgetting the permit
// every mode fails (checked). Pin: lib_tests::http::http_abandoned_request_then_good.
run!(http_abandoned_request_then_good, r#"{
    let handler = |req: http::Request| {
        let text = select buffer::to_string_lossy(req.raw) { "" => never(), s => s };
        { body: "hi [text]", headers: [], status: u16:200 }
    };
    let server = http::serve(#addr: "127.0.0.1:0", #max_connections: 1, #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let r1 = http::request(#timeout: duration:200.ms, client, "http://[addr]/");
    let r2 = http::request(
        #timeout: duration:5.s,
        #method: `POST,
        #body: "bob",
        r1 ~ client,
        "http://[addr]/"
    );
    select r2 { error as e => "[e]", r => "[r.status] [r.body]" }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "200 hi bob")
}; FuseExpect::Jit);

// A request whose instance raises is answered 500, and a concurrent one
// whose instance answers gets its own page.
run!(http_raises_answer_their_own_requests, r#"{
    let handler = |req: http::Request| {
        let late = sys::time::after_idle(duration:300.ms, req.path);
        let p = select req.path { "/a" => late, p => p };
        let e: Result<string, `Bad(string)> = select p { "/a" => error(`Bad(p)), p => p };
        { body: e?, headers: [], status: u16:200 }
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let a = http::request(client, "http://[addr]/a")$;
    let b = http::request(sys::time::after_idle(duration:100.ms, client), "http://[addr]/b")$;
    "[a.status] [b.status] [b.body]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "500 200 /b")
}; FuseExpect::Jit);

// Concurrent requests each get their own instance's answer.
run!(http_concurrent_requests_pair, r#"{
    let handler = |req: http::Request| {
        let page = sys::time::after_idle(duration:50.ms, "page [req.path]");
        { body: page, headers: [], status: u16:200 }
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let a = http::request(client, "http://[addr]/a")$;
    let b = http::request(client, "http://[addr]/b")$;
    let c = http::request(client, "http://[addr]/c")$;
    "[a.body] [b.body] [c.body]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "page /a page /b page /c")
}; FuseExpect::Jit);

// A fire the request did not cause (a shared counter's write) answers
// nothing: each sequential request gets its own page.
run!(http_sequential_requests_pair, r#"{
    let hits = 0;
    let handler = |req: http::Request| {
        hits <- req ~ hits + 1;
        let page = sys::time::after_idle(duration:50.ms, "page [req.path]");
        { body: page, headers: [("x-hits", "[hits]")], status: u16:200 }
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let a = http::request(client, "http://[addr]/a")$;
    let b = http::request(a ~ client, "http://[addr]/b")$;
    let c = http::request(b ~ client, "http://[addr]/c")$;
    "[a.body] [b.body] [c.body]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "page /a page /b page /c")
}; FuseExpect::Jit);

// A reply that waits on an async value is sent when the value arrives.
run!(http_async_handler_answers, r#"{
    let handler = |req: http::Request| {
        let page = sys::time::after_idle(duration:50.ms, "p [req.path]");
        { body: "[req.method] [page]", headers: [], status: u16:200 }
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
        status: u16:200
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
}; FuseExpect::None);

// A body over #max_body is answered 413 without reaching the handler.
run!(http_max_body, r#"{
    let handler = |req: http::Request| {
        body: "[req.path]",
        headers: [],
        status: u16:200
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
        body: "[req.raw == bytes:/+4=]",
        headers: [],
        status: u16:200
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let r = http::request_bin(#method: `POST, #body: bytes:/+4=, client, "http://[addr]/")$;
    r.body
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bytes(b)) if &***b == b"true")
}; FuseExpect::Jit);

// rest::post sends JSON headers, the bearer and the body.
run!(http_rest_post, r#"{
    let handler = |req: http::Request| {
        let has = |name: string, value: string|
            array::len(array::filter(req.headers, |h: (string, string)| (h.0 == name) && (h.1 == value))) == 1;
        {
            body: "[req.method] [has("content-type", "application/json")] [has("authorization", "Bearer t")] [buffer::to_string_lossy(req.raw)]",
            headers: [],
            status: u16:200
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

// A cert that has not arrived is not a missing one: no server runs, plain
// or otherwise, until every argument holds a value.
run!(http_serve_waits_for_its_cert, r#"{
    let cert: [bytes, null] = never();
    let server = http::serve(
        #addr: "127.0.0.1:0",
        #cert: cert,
        #key: null,
        #handler: |req: http::Request| {body: "plain", headers: [], status: u16:200}
    );
    any(server ~ "started", sys::time::timer(duration:200.ms, false) ~ "waiting")
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "waiting")
}; FuseExpect::None);

/// A server that slept and wakes with no address gives no handle: the old
/// one, aborted at the sleep, does not stand as its value.
#[tokio::test(flavor = "multi_thread")]
async fn http_serve_forgets_its_server_asleep() -> Result<()> {
    const CODE: &str = r#"{
    let n = 0;
    n <- sys::time::timer(duration:300.ms, false) ~ 1;
    n <- sys::time::timer(duration:600.ms, false) ~ 2;
    let addr = select n { 0 => "127.0.0.1:0", _ => never() };
    let handler = |req: http::Request| {body: "hello", headers: [], status: u16:200};
    let v = select n % 2 == 0 {
        true => http::server_addr(http::serve(#addr: addr, #handler: handler)$),
        false => "asleep"
    };
    let client = http::default_client(null)$;
    select v {
        "asleep" => "-",
        a => {
            let resp = http::request(#timeout: duration:200.ms, client, "http://[a]/");
            select resp { error as _ => "error", rs => rs.body }
        }
    }
}"#;
    for mode in Mode::ALL {
        let quiet = Collect::Quiet(Duration::from_millis(1000));
        let got =
            collect(CODE, crate::TEST_REGISTER, mode, quiet, Duration::from_secs(10))
                .await?;
        let want: Vec<Value> =
            ["hello", "-"].into_iter().map(|s| Value::String(s.into())).collect();
        assert_eq!(got, want, "{mode:?}");
    }
    Ok(())
}

/// A connection that never sends a request gives its permit back at the
/// header read timeout: with one permit, a request after it is served.
#[tokio::test]
async fn http_silent_socket_releases_its_permit() -> Result<()> {
    const CODE: &str = r#"{
    let handler = |req: http::Request| {body: "served", headers: [], status: u16:200};
    let server = http::serve(
        #addr: "127.0.0.1:0",
        #max_connections: 1,
        #header_timeout: duration:1.s,
        #handler: handler
    )$;
    let addr = http::server_addr(server);
    let silent = sys::tcp::connect(addr)$;
    let client = http::default_client(null)$;
    let later = sys::time::timer(duration:2.s, false);
    let resp = http::request(#timeout: duration:10.s, client, (later ~ silent) ~ "http://[addr]/");
    select resp { error as e => "[e]", r => r.body }
}"#;
    // the modes wait out the timeout together
    let run = |mode| {
        let first = Collect::First(1);
        collect(CODE, crate::TEST_REGISTER, mode, first, Duration::from_secs(30))
    };
    let [interp, jit, par, jit_par] = Mode::ALL;
    let (a, b, c, d) = tokio::join!(run(interp), run(jit), run(par), run(jit_par));
    for got in [a?, b?, c?, d?] {
        assert_eq!(got, [Value::String("served".into())]);
    }
    Ok(())
}

// A header the caller gives replaces rest's default of that name, whatever
// its case, rather than going out beside it.
run!(http_rest_caller_header_replaces_default, r#"{
    let srv = http::serve(#addr: "127.0.0.1:0", #handler: |req: http::Request| {
        body: str::join(#sep: "|", array::map(array::filter(req.headers, |(k, _)| (k == "content-type") || (k == "accept")), |(k, v)| "[k]=[v]")),
        headers: [],
        status: u16:200
    })$;
    let addr = http::server_addr(srv);
    let client = http::default_client(addr)$;
    let r = http::rest::post(#headers: [("Content-Type", "application/merge-patch+json")], #body: "{}", client, "http://[addr]/x");
    (r$).body
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "accept=application/json|content-type=application/merge-patch+json")
}; FuseExpect::Jit);

// A request's HTTPError names its cause, which reqwest's own message
// leaves out (http-sqlite-db1-03).
run!(http_error_names_its_cause, r#"{
    let c = http::default_client(null)$;
    let msg = |r: Any| select r { error as e => "[e]", _ => "answered" };
    let refused = msg(http::get(c, "http://127.0.0.1:1/x"));
    let relative = msg(http::get(c, "notaurl"));
    (str::contains(#part: "refused", refused), str::contains(#part: "relative URL without a base", relative))
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[true, true]")
}; FuseExpect::Jit);

// A raising handler's 500 says nothing of the error: no payload, no source
// position, no program text.
run!(http_500_body_is_generic, r#"{
    let handler = |req: http::Request| {
        let e: Result<string, `Internal(string)> = error(`Internal(req.path));
        { body: e?, headers: [], status: u16:200 }
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let a = http::request(client, "http://[addr]/raise")$;
    (a.status, a.body)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == r#"[u16:500, "Internal Server Error"]"#)
}; FuseExpect::Jit);

// A response body over the client's #max_body is an error, whether the
// server declares its length or not.
run!(http_client_max_body, r#"{
    let handler = |req: http::Request| { body: "hello", headers: [], status: u16:200 };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let small = http::request(#max_body: 4, client, "http://[addr]/");
    let fits = http::request(#max_body: 5, client, "http://[addr]/")$;
    (fits.body, select small { error as e => "[e]", _ => "read" })
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a.len() == 2
        && a[0] == Value::String("hello".into())
        && matches!(&a[1], Value::String(s) if s.contains("body over 4 bytes")))
}; FuseExpect::Jit);
