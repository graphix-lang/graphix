//! The end of the process waits for the runtime's teardown, bounded: what
//! the runtime drops as it stops reaches the disk, and a stdin read that
//! never ends does not hold the exit past its bound.

mod common;
use common::graphix;
use graphix_shell::TEARDOWN_BOUND;
use std::{
    fs,
    io::Read,
    process::{Command, Stdio},
    thread,
    time::{Duration, Instant},
};

/// A db insert followed by `sys::exit` is on disk once the process has
/// ended: the database's last handle drops as the runtime stops and its
/// flush is part of the exit, not a race with it.
#[test]
fn a_db_write_before_sys_exit_reaches_the_disk() {
    const TRIES: usize = 8;
    let root = tempfile::tempdir().expect("tempdir");
    fs::create_dir_all(root.path().join("w")).expect("writer dir");
    fs::create_dir_all(root.path().join("r")).expect("reader dir");
    for i in 0..TRIES {
        // a database of its own, so a run reads what only its own write
        // could have put there
        let open = format!(
            "let db = db::open(\"{}\")$;\n\
             let t: db::Tree<string, i64> = db::tree(db, \"t\")$;\n",
            root.path().join(format!("db{i}")).display()
        );
        let writer =
            format!("{open}let r = db::insert(t, \"k\", 42)$;\nsys::exit(r ~ 0);\nr\n");
        let reader = format!(
            "{open}let r = db::get(t, \"k\")$;\n\
             sys::exit(sys::time::after_idle(duration:200.ms, r ~ 0));\n\
             r\n"
        );
        let ran = graphix(&root.path().join("w"), &writer, &[], &[]);
        assert!(ran.status.success(), "write {i} failed:\n{}", ran.stderr);
        let ran = graphix(&root.path().join("r"), &reader, &[], &[]);
        assert!(ran.status.success(), "read {i} failed:\n{}", ran.stderr);
        assert!(
            ran.stdout.trim() == "42",
            "run {i} lost its write: read [{}] in a loop of {TRIES}\n{}",
            ran.stdout.trim(),
            ran.stderr
        );
    }
}

/// A stdin read the program never finishes must not hold the exit: the
/// bound ends the process, not the read. The child's stdin is a pipe that
/// stays open and is never written to, so its read never answers.
#[test]
fn a_stdin_read_that_never_ends_does_not_hold_the_exit() {
    fn drain<R: Read + Send + 'static>(r: Option<R>) -> thread::JoinHandle<Vec<u8>> {
        thread::spawn(move || {
            let mut buf = vec![];
            if let Some(mut r) = r {
                let _ = r.read_to_end(&mut buf);
            }
            buf
        })
    }
    let program = "use sys::io::Lines;\n\
                   let l = Lines::lines(sys::io::stdin(null));\n\
                   sys::exit(sys::time::timer(duration:500.ms, false) ~ 0);\n\
                   l\n";
    let dir = tempfile::tempdir().expect("tempdir");
    let path = dir.path().join("main.gx");
    fs::write(&path, program).expect("write the program");
    let mut child = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-netidx")
        .arg("--no-cache")
        .env("XDG_CACHE_HOME", dir.path().join("cache"))
        .arg(&path)
        // taken by the child and never written to: its stdin read blocks
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn graphix");
    // read on while it runs: a full pipe would look like a held exit
    let stdout = drain(child.stdout.take());
    let stderr = drain(child.stderr.take());
    let start = Instant::now();
    // a run, its bound, and the time a cold compile takes on a busy machine
    let deadline = start + TEARDOWN_BOUND + Duration::from_secs(30);
    let status = loop {
        match child.try_wait().expect("wait for graphix") {
            Some(status) => break status,
            None if Instant::now() > deadline => {
                let pid = child.id();
                let _ = child.kill();
                let _ = child.wait();
                panic!("graphix (pid {pid}) was held by its own stdin read");
            }
            None => thread::sleep(Duration::from_millis(20)),
        }
    };
    let err = String::from_utf8_lossy(&stderr.join().expect("drain stderr")).into_owned();
    let bytes = stdout.join().expect("drain stdout");
    let out = String::from_utf8_lossy(&bytes);
    let out = out.trim();
    let elapsed = start.elapsed();
    assert!(
        status.success(),
        "the program did not exit as asked: {status:?}\nstdout: {out}\nstderr: {err}"
    );
    assert!(
        elapsed <= TEARDOWN_BOUND + Duration::from_secs(20),
        "the exit took {elapsed:?}, past the {TEARDOWN_BOUND:?} it is bounded by"
    );
}
