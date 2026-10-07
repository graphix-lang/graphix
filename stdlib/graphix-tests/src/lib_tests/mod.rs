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

/// The test certificates' directory, escaped for a Graphix string.
fn cert_dir() -> String {
    let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/certs").replace('\\', "/");
    graphix_package_core::testing::escape_path(std::path::Path::new(&dir).display())
        .to_string()
}
