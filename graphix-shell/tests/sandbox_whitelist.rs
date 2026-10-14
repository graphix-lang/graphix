//! A dynamic module's whitelist keeps the modules enclosing what it lists,
//! so a nested module, a value two levels down and a granted trait below
//! the root are reachable by their qualified paths.

use std::{
    fs,
    process::{Command, Stdio},
};

const MAIN: &str = r#"mod a;
let nested = mod foo dynamic {
    sandbox whitelist [core, sys::fs::tempdir, a::b::f];
    sig { val y: i64 };
    source "let t = sys::fs::tempdir::create; let y = super::a::b::f(1)"
};
let traits = mod bar dynamic {
    sandbox whitelist [core, a::b];
    sig {
        use super::a::b::Show;
        type Counter;
        impl Show for Counter;
        val make: fn(x: i64) -> Counter
    };
    source "type Counter = Abstract<i64>; impl Show for Counter { let show = |c| \"Counter(\[c.0\])\" }; let make = |x| Counter(x)"
};
sys::exit(sys::time::after_idle(duration:300.ms, 0));
select nested {
    error as e => println("[e]"),
    null as _ => select traits {
        error as e => println("[e]"),
        null as _ => println("[foo::y] [a::b::Show::show(bar::make(7))]")
    }
}
"#;

#[test]
fn whitelist_keeps_enclosing_modules() {
    let dir =
        std::env::temp_dir().join(format!("gx-sandbox-whitelist-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(dir.join("a")).expect("tempdir");
    fs::write(dir.join("main.gx"), MAIN).expect("write program");
    fs::write(dir.join("a/mod.gx"), "mod b").expect("write a");
    fs::write(
        dir.join("a/b.gx"),
        "trait Show { val show: fn(self) -> string };\nlet f = |x: i64| x\n",
    )
    .expect("write b");
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .args(["--no-netidx", "--no-cache"])
        .arg(dir.join("main.gx"))
        .stdin(Stdio::null())
        .output()
        .expect("run graphix");
    let _ = fs::remove_dir_all(&dir);
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("1 Counter(7)"),
        "stdout {stdout} stderr {}",
        String::from_utf8_lossy(&out.stderr)
    );
}
