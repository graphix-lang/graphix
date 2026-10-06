//! A full JIT arena retires its module and the link reinstalls in a
//! fresh one: with an arena that holds one link's regions but not two,
//! the program still fuses and runs to its value, and the rotation says
//! so.

use std::{
    fs,
    process::{Command, Stdio},
};

/// Regions for two links.
const REGIONS: usize = 512;

fn program() -> String {
    let mut s = String::from("let x = sys::time::after_idle(duration:1.ms, 3);\n");
    for i in 0..REGIONS {
        s.push_str(&format!("let r{i} = #[native] (x * {i} + 1);\n"));
    }
    let sum = (0..REGIONS).map(|i| format!("r{i}")).collect::<Vec<_>>().join(" + ");
    let expect: usize = (0..REGIONS).map(|i| 3 * i + 1).sum();
    s.push_str(&format!("let total = {sum};\n"));
    s.push_str(&format!("sys::exit(select total {{ {expect} => 0, _ => 1 }})\n"));
    s
}

#[test]
fn exhausted_arena_rotates() {
    let dir = std::env::temp_dir().join(format!("gx-arena-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("arena.gx");
    fs::write(&path, program()).expect("write program");
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-netidx")
        .arg("--no-cache")
        .arg("--log-dir")
        .arg(&dir)
        .arg(&path)
        .env("RUST_LOG", "warn")
        .env("GRAPHIX_JIT_ARENA", "196608")
        .stdin(Stdio::null())
        .output()
        .expect("run graphix");
    let log = fs::read_dir(&dir)
        .expect("read log dir")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().is_some_and(|x| x == "log"))
        .map(|e| fs::read_to_string(e.path()).expect("read log"))
        .collect::<String>();
    let _ = fs::remove_dir_all(&dir);
    assert!(
        out.status.success(),
        "the program did not compute its total: {}\n{log}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        log.contains("JIT code arena exhausted: retired generation"),
        "a 192KB arena never rotated:\n{log}"
    );
    // CR claude for claude: [test-gap] No code writes 'rebuilt without fusion' any more,
    // so this assertion cannot fail; a link that outgrows a fresh arena now panics,
    // which the status assertion above catches. The pin also covers only the cold link.
    // The warm start's rotation has none, although Jit::load_wrapped takes a page of
    // arena per restored region and rotates where the cold run does not
    // (design/review-2026-10-05/repro/f-jit-05.sh). Replace this with a second, warm
    // run of the same program (no --no-cache, a private XDG_CACHE_HOME, an arena the
    // warm start overflows) that must still compute its total. (f-jit-13)
    assert!(
        !log.contains("rebuilt without fusion"),
        "a link outgrew a fresh arena:\n{log}"
    );
}
