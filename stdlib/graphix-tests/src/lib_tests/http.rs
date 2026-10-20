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

// A client that connects and never speaks holds only its own handshake:
// a request beside it answers at once, well inside the handshake timeout.
run!(https_beside_a_silent_client, { let cd = crate::lib_tests::cert_dir(); format!(r#"{{
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
    let silent = sys::tcp::connect(addr)$;
    let ca = sys::fs::read_all_bin("{cd}/ca.pem")$;
    let client = http::client(#ca_cert: ca, server)$;
    let resp = http::request(client, silent ~ "https://[addr]/")$;
    resp.body
}}"#) }, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello GET")
}, timeout: 5; FuseExpect::Jit);

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

// Concurrent requests that raise are each answered with their own error.
// CR claude for claude: [test-gap/low] The scenario in the comment above does not
// happen any more. Request instances run concurrently, each with its own ErrorRelay, so
// /b raises at about 100 ms and /a at about 300 ms, never in one cycle.
// Receipts::take's one-delivery-a-cycle guard, which this fixture was added to pin, is
// exercised by nothing: removing it would pass. Reword the comment to what is checked
// (concurrent raises answered with their own errors), and pin the guard where two
// deliveries can reach one relay in one cycle, or delete it if per-request relays make
// it unreachable. probe:
// design/review-2026-10-19/repro/tests-lib-b1-http-raise-not-same-cycle.gx
// (tests-lib-b1-04)
// 2026-10-19 claude: Done: the comment above now says what the fixture checks
// (concurrent raises answered with their own errors). Open: the guard. Receipts::take's
// one-delivery-a-cycle condition (`self.last_cycle != Some(cycle)`) lives in
// graphix-compiler/src/node/error.rs and belongs to every catch, not to the http relay;
// replacing it with `true` leaves lang::errors, lang::seq*, lib_tests::errors and
// lib_tests::http all green, so nothing in the tree reaches it. Whether two deliveries
// can reach one catch's take in one cycle (and so whether the guard is dead) is a
// compiler question; a pin, or the guard's removal, belongs with whoever owns
// node/error.rs.
run!(http_raises_answer_their_own_requests, r#"{
    let handler = |req: http::Request| {
        let late = sys::time::after_idle(duration:300.ms, req.path);
        let p = select req.path { "/a" => late, p => p };
        let e: Result<string, `Bad(string)> = error(`Bad(p));
        { body: e?, headers: [], status: u16:200, url: "" }
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let a = http::request(client, "http://[addr]/a")$;
    let b = http::request(sys::time::after_idle(duration:100.ms, client), "http://[addr]/b")$;
    let both = (a.body, b.body);
    select (str::contains(#part: "/a", both.0), str::contains(#part: "/b", both.1)) {
        (true, true) => "own errors",
        _ => "crossed: [both]"
    }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "own errors")
}; FuseExpect::Jit);

// Concurrent requests each get their own instance's answer.
run!(http_concurrent_requests_pair, r#"{
    let handler = |req: http::Request| {
        let page = sys::time::after_idle(duration:50.ms, "page [req.path]");
        { body: page, headers: [], status: u16:200, url: "" }
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
        { body: page, headers: [("x-hits", "[hits]")], status: u16:200, url: "" }
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
}; FuseExpect::None);

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

// A cert that has not arrived is not a missing one: no server runs, plain
// or otherwise, until every argument holds a value.
run!(http_serve_waits_for_its_cert, r#"{
    let cert: [bytes, null] = never();
    let server = http::serve(
        #addr: "127.0.0.1:0",
        #cert: cert,
        #key: null,
        #handler: |req: http::Request| {body: "plain", headers: [], status: u16:200, url: ""}
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
    let handler = |req: http::Request| {body: "hello", headers: [], status: u16:200, url: ""};
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
#[cfg_attr(not(feature = "slow-tests"), ignore = "slow-tests")]
async fn http_silent_socket_releases_its_permit() -> Result<()> {
    const CODE: &str = r#"{
    let handler = |req: http::Request| {body: "served", headers: [], status: u16:200, url: ""};
    let server = http::serve(#addr: "127.0.0.1:0", #max_connections: 1, #handler: handler)$;
    let addr = http::server_addr(server);
    let silent = sys::tcp::connect(addr)$;
    let client = http::default_client(null)$;
    let later = sys::time::timer(duration:32.s, false);
    let resp = http::request(#timeout: duration:10.s, client, (later ~ silent) ~ "http://[addr]/");
    select resp { error as e => "[e]", r => r.body }
}"#;
    // the modes wait out the timeout together
    let run = |mode| {
        let first = Collect::First(1);
        collect(CODE, crate::TEST_REGISTER, mode, first, Duration::from_secs(120))
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
        status: u16:200,
        url: ""
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

// A raising handler's 500 body is the error as data, never its kind
// markers (x-rerun-2-02).
run!(http_500_body_carries_no_markers, r#"{
    let handler = |req: http::Request| {
        let e: Result<string, `Internal(string)> = error(`Internal(req.path));
        { body: e?, headers: [], status: u16:200, url: "" }
    };
    let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
    let addr = http::server_addr(server);
    let client = http::default_client(server)$;
    let a = http::request(client, "http://[addr]/raise")$;
    (a.status, str::contains(#part: "abstract:", a.body), str::contains(#part: "\[\"Internal\", \"/raise\"\]", a.body))
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[u16:500, false, true]")
}; FuseExpect::Jit);
