//! A dynamic module's whitelist keeps the modules enclosing what it lists,
//! so a nested module, a value two levels down and a granted trait below
//! the root are reachable by their qualified paths.

mod common;
use common::graphix;
use std::fs;

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
    let dir = tempfile::tempdir().expect("tempdir");
    let a = dir.path().join("a");
    fs::create_dir_all(&a).expect("tempdir");
    fs::write(a.join("mod.gx"), "mod b").expect("write a");
    fs::write(
        a.join("b.gx"),
        "trait Show { val show: fn(self) -> string };\nlet f = |x: i64| x\n",
    )
    .expect("write b");
    let ran = graphix(dir.path(), MAIN, &["--no-cache"], &[]);
    assert!(
        ran.stdout.contains("1 Counter(7)"),
        "stdout {} stderr {}",
        ran.stdout,
        ran.stderr
    );
}
