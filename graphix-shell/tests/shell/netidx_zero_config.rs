//! A `netidx:` script with no netidx config file falls back to the
//! machine-local resolver, whose lookup blocks on a runtime of its own,
//! and so must run off the shell's.

use std::{
    io::Read,
    net::TcpListener,
    process::{Command, Stdio},
    thread,
    time::{Duration, Instant},
};

#[test]
fn netidx_script_loads_without_a_config_file() {
    let home = tempfile::tempdir().expect("tempdir");
    // a bound port reads as a running local resolver: no daemon starts
    let resolver = TcpListener::bind("127.0.0.1:0").expect("bind");
    let port = resolver.local_addr().expect("local addr").port();
    let mut child = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .args(["--no-cache", "--resolve-timeout", "1", "netidx:/graphix-test/none"])
        .env_remove("NETIDX_CFG")
        .env("HOME", home.path())
        .env("XDG_CONFIG_HOME", home.path())
        .env("NETIDX_LOCAL_ONLY_RESOLVER_PORT", port.to_string())
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .expect("run graphix");
    // CR claude for claude: [test-gap/low] A 5-second deadline of the test's own ends
    // the run. The child is then killed, and the only check is that stderr holds no
    // panic. A child slowed past 5 s by the suite's load is killed before it reaches
    // the lookup, and the test passes having tested nothing. design/testing.md says a
    // test takes no deadline of its own for what must come. Wait for the exit instead
    // (`--resolve-timeout 1` ends the run in about a second; make `common`'s
    // `within_deadline` pub to bound a hang) and assert stderr names the resolve
    // timeout. Then the run is known to have reached the local resolver. Found by
    // reading; the run ends by itself in 1.1 s here. (tests-shell-compiler-b-09)
    let deadline = Instant::now() + Duration::from_secs(5);
    while child.try_wait().expect("wait").is_none() && Instant::now() < deadline {
        thread::sleep(Duration::from_millis(50));
    }
    let _ = child.kill();
    let _ = child.wait();
    let mut stderr = String::new();
    child.stderr.take().expect("stderr").read_to_string(&mut stderr).expect("read");
    assert!(!stderr.contains("panicked"), "{stderr}");
}
