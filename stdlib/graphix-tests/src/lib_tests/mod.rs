use netidx::subscriber::Value;

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

/// The field `name` of a struct value, an array of `[name, value]` pairs.
fn struct_field<'a>(v: &'a Value, name: &str) -> Option<&'a Value> {
    let Value::Array(fields) = v else { return None };
    fields.iter().find_map(|f| match f {
        Value::Array(kv) => match &kv[..] {
            [Value::String(k), v] if &**k == name => Some(v),
            _ => None,
        },
        _ => None,
    })
}

/// The test certificates, in one fixed directory under the temp dir that
/// every run reuses, so a test binary run away from its source tree has
/// them; escaped for a Graphix string. Each file is written whole and
/// renamed into place, so a concurrent binary never reads a partial one.
fn cert_dir() -> String {
    static DIR: std::sync::LazyLock<std::path::PathBuf> =
        std::sync::LazyLock::new(|| {
            let dir = std::env::temp_dir().join("graphix-tests-certs");
            std::fs::create_dir_all(&dir).expect("a directory for the test certs");
            for (name, pem) in [
                ("ca.pem", &include_bytes!("../../certs/ca.pem")[..]),
                ("server.pem", &include_bytes!("../../certs/server.pem")[..]),
                ("server.key", &include_bytes!("../../certs/server.key")[..]),
            ] {
                let path = dir.join(name);
                if std::fs::read(&path).is_ok_and(|have| have == pem) {
                    continue;
                }
                let tmp = dir.join(format!("{name}.{}", std::process::id()));
                std::fs::write(&tmp, pem).expect("write a test cert");
                std::fs::rename(&tmp, &path).expect("move a test cert into place");
            }
            dir
        });
    let dir = DIR.display().to_string().replace('\\', "/");
    graphix_package_core::testing::escape_path(std::path::Path::new(&dir).display())
        .to_string()
}
