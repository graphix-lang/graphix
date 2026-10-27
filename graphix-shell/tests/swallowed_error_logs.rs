//! `$` and a handler-less `?` swallow an error, and say so: the
//! node-walk logs the site and the error, and a fused kernel logs the
//! same line through its helper. The logger's module path tells the
//! two apart, so each engine is proven to have produced its own. A
//! collection init count past the element limit bottoms, and says so,
//! in both engines. A dropped write through a reference names its
//! writer, and a task the runtime cancels itself logs nothing.

mod common;
use common::graphix;

/// A runtime index (rand cannot fold) that is always out of bounds,
/// swallowed once by `$` and once by a `?` no catch covers.
const PROGRAM: &str = r#"
let a = [1, 2, 3];
let i = rand::rand(#start: 10, #end: 20, #clock: 0);
let x = a[i]$;
let y = a[i]?;
sys::exit(sys::time::after_idle(duration:200.ms, 0))
"#;

/// A count past `MAX_ARRAY_INIT_LEN`; `#[native]` proves the kernel ran it.
const OVERSIZE: &str = r#"
let n = 20000000;
let a = #[native] array::init(n, |i| i);
sys::exit(sys::time::after_idle(duration:200.ms, 0))
"#;

/// Run `program`; returns (stderr, log text).
fn run(program: &str, no_fusion: bool, label: &str) -> (String, String) {
    let dir = tempfile::tempdir().expect("tempdir");
    let args: &[&str] = match no_fusion {
        true => &["--no-cache", "--no-fusion"],
        false => &["--no-cache"],
    };
    let ran = graphix(dir.path(), program, args, &[("RUST_LOG", "warn")]);
    assert!(ran.status.success(), "{label}: graphix failed: {}", ran.stderr);
    (ran.stderr, ran.log)
}

fn assert_diagnostics(label: &str, module: &str, stderr: &str, log: &str) {
    let ignored = format!("WARN [{module}] ignored error in");
    let unhandled = format!("ERROR [{module}] unhandled error in");
    assert!(
        log.contains(&ignored) && log.contains("line: 4, column: 9"),
        "{label}: `$` did not log its swallowed error from {module}:\n{log}"
    );
    assert!(
        log.contains(&unhandled) && log.contains("line: 5, column: 9"),
        "{label}: handler-less `?` did not log its error from {module}:\n{log}"
    );
    assert!(
        stderr.contains("unhandled error in"),
        "{label}: handler-less `?` did not report on stderr:\n{stderr}"
    );
}

#[test]
fn fused_swallowed_errors_are_logged() {
    let (stderr, log) = run(PROGRAM, false, "jit");
    assert_diagnostics("jit", "graphix_compiler::fusion::emit_helpers", &stderr, &log);
}

#[test]
fn node_walk_swallowed_errors_are_logged() {
    let (stderr, log) = run(PROGRAM, true, "interp");
    assert_diagnostics("interp", "graphix_compiler::node::error", &stderr, &log);
}

#[test]
fn init_oversize_is_logged_by_both_engines() {
    for (no_fusion, label) in [(false, "oversize-jit"), (true, "oversize-interp")] {
        let (_, log) = run(OVERSIZE, no_fusion, label);
        assert!(
            log.contains("collection init size 20000000 exceeds"),
            "{label}: the oversize count was not logged:\n{log}"
        );
    }
}

/// A debounce re-arms its timer at every input; the cancelled timer task
/// is the runtime's own doing, never a logged failure.
#[test]
fn cancelled_timers_are_not_logged() {
    const DEBOUNCE: &str = r#"
let t = sys::time::timer(duration:10.ms, 20);
let d = sys::time::after_idle(duration:50.ms, t);
sys::exit(sys::time::after_idle(duration:300.ms, d ~ 0));
d
"#;
    let (_, log) = run(DEBOUNCE, false, "debounce");
    assert!(!log.contains("without replying"), "{log}");
}

/// A write through a reference past its root's end is dropped, and the
/// log names where it was written.
#[test]
fn a_dropped_write_names_its_writer() {
    const PROGRAM: &str = r#"
let a = [1, 2, 3];
let r = &mut a[7];
*r <- 5;
sys::exit(sys::time::after_idle(duration:200.ms, 0))
"#;
    for (no_fusion, label) in [(false, "dropped-jit"), (true, "dropped-interp")] {
        let (_, log) = run(PROGRAM, no_fusion, label);
        assert!(
            log.contains("line: 4, column: 1 was dropped: index 7"),
            "{label}: {log}"
        );
    }
}

/// An rpc or http handler's raise is logged as Graphix prints the error,
/// with none of its kind markers.
#[test]
fn a_handler_raise_is_logged_as_graphix_prints_it() {
    const RPC: &str = r#"
let p = "/local/raise";
sys::net::rpc(#path: p, #doc: "d", #spec: null, #f: |a: null| -> i64 {
    let x: i64 = error(`Boom("bad"))?;
    x
});
let r: Result<i64, [`RpcError(string), `InvalidCast(string)]> =
    sys::net::call(sys::time::after_idle(duration:100.ms, p), null);
sys::exit(r ~ 0)
"#;
    const HTTP: &str = r#"
let handler = |req: http::Request| {
    let b: Result<string, `Boom(string)> = error(`Boom("bad"));
    { body: req.path ~ b?, headers: [], status: u16:200 }
};
let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
let client = http::default_client(server)$;
let resp = http::request(client, "http://[http::server_addr(server)]/")$;
sys::exit(resp ~ 0)
"#;
    for (program, label) in [(RPC, "rpc"), (HTTP, "http")] {
        let (_, log) = run(program, false, label);
        let want = format!(
            "an {label} handler raised: error:{{cause: null, error: `Boom(\"bad\")"
        );
        assert!(log.contains(&want), "{label}: {log}");
    }
}

/// A rand range holding no value (an empty one, a float one included, or
/// one with a NaN bound) produces nothing and logs the range (x-panics-02).
#[test]
fn an_empty_rand_range_is_logged() {
    const PROGRAM: &str = r#"
let a = rand::rand(#start: 5, #end: 5, #clock: 0);
let b = rand::rand(#start: 2.5, #end: 2.5, #clock: 0);
let c = rand::rand(#start: 1.0, #end: 0.0 / 0.0, #clock: 0);
sys::exit(sys::time::after_idle(duration:200.ms, 0))
"#;
    let (_, log) = run(PROGRAM, false, "rand");
    for range in ["[5, 5)", "[2.5, 2.5)", "[1, NaN)"] {
        assert!(log.contains(&format!("rand: no value in {range}")), "{range}: {log}");
    }
}
