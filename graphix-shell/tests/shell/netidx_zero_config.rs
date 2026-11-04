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
