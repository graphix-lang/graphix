mod args;
mod arith;
mod array;
mod bitwise;
mod bottom;
mod buffer;
mod callable;
mod core;
mod db;
mod dirs;
mod expr_types;
mod fs;
mod hbs;
mod http;
mod interrupt;
mod json;
mod leaks;
mod lift;
mod list;
mod lsp_fusion;
mod map;
mod math;
mod module_stmt;
mod native;
mod neg;
mod net;
mod pack;
mod packed_ast;
mod process;
mod recheck;
mod sqlite;
#[path = "str.rs"]
mod str_tests;
mod sys;
mod tcp;
mod tls;
mod toml;
mod typecheck;
mod wake;
mod xls;

/// The test certificates, written once to a directory of their own so a
/// test binary run away from its source tree has them; escaped for a
/// Graphix string.
fn cert_dir() -> String {
    // CR claude for claude: [risk/low] A TempDir in a static is never dropped (Rust
    // runs no destructors for statics), so every run of the test binary that touches
    // tls leaves a directory holding ca.pem, server.pem and server.key in the temp dir,
    // and they pile up across runs. Fix: write the certs to one fixed directory under
    // std::env::temp_dir() (a name of this crate's own), created if missing with
    // write-then-rename so concurrent binaries never read a partial file. That way runs
    // reuse it instead of leaking a new one each time. (tests-lib-b2-13)
    static DIR: std::sync::LazyLock<tempfile::TempDir> = std::sync::LazyLock::new(|| {
        let dir = tempfile::tempdir().expect("a directory for the test certs");
        for (name, pem) in [
            ("ca.pem", &include_bytes!("../../certs/ca.pem")[..]),
            ("server.pem", &include_bytes!("../../certs/server.pem")[..]),
            ("server.key", &include_bytes!("../../certs/server.key")[..]),
        ] {
            std::fs::write(dir.path().join(name), pem).expect("write a test cert");
        }
        dir
    });
    let dir = DIR.path().display().to_string().replace('\\', "/");
    graphix_package_core::testing::escape_path(std::path::Path::new(&dir).display())
        .to_string()
}
