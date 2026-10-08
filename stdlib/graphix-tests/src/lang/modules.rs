// Tests for dynamic modules

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, Mode, eval, refusal, refused},
};
use netidx::publisher::Value;

const DYNAMIC_MODULE0: &str = r#"
{
    let source = "
        let add = |x| x + 1;
        let sub = |x| x - 1;
        let cfg = \[1, 2, 3, 4, 5\];
        let hidden = 42
    ";
    sys::net::publish("/local/foo", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig {
            val add: fn(x: i64) -> i64;
            val sub: fn(x: i64) -> i64;
            val cfg: Array<i64>
        };
        source sys::net::subscribe("/local/foo")?
    };
    select status {
        error as e => never(dbg(e)),
        null as _ => foo::add(foo::cfg[0]?)
    }
}
"#;

run!(dynamic_module0, DYNAMIC_MODULE0, |v: Result<&Value>| match v {
    Ok(Value::I64(2)) => true,
    _ => false,
}; FuseExpect::Jit);

const DYNAMIC_MODULE1: &str = r#"
{
    let source = "
        let add = |x| x + 1.;
        let sub = |x| x - 1;
        let cfg = \[1, 2, 3, 4, 5\];
        let hidden = 42
    ";
    sys::net::publish("/local/foo", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig {
            val add: fn(x: i64) -> i64;
            val sub: fn(x: i64) -> i64;
            val cfg: Array<i64>
        };
        source sys::net::subscribe("/local/foo")?
    };
    select status {
        error as e => dbg(e),
        null as _ => foo::add(foo::cfg[0]?)
    }
}
"#;

run!(dynamic_module1, DYNAMIC_MODULE1, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
}; FuseExpect::Jit);

const DYNAMIC_MODULE2: &str = r#"
{
    let source = "let add = 'a: Number |x: 'a| -> 'a x + x";
    sys::net::publish("/local/foo", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig {
            val add: fn<'a: Number + Singleton>(x: 'a) -> 'a
        };
        source sys::net::subscribe("/local/foo")?
    };
    select status {
        error as e => dbg(e),
        null as _ => foo::add(2)
    }
}
"#;

run!(dynamic_module2, DYNAMIC_MODULE2, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
}; FuseExpect::Jit);

const DYNAMIC_MODULE3: &str = r#"
{
    let source = "
        let foo = never();
        let bar = never();
        select foo { x => bar <- dbg(x) }
    ";
    sys::net::publish("/local/test", source)?;
    let status = mod test dynamic {
        sandbox whitelist [core];
        sig {
            val foo: string;
            val bar: string
        };
        source sys::net::subscribe("/local/test")?
    };
    select status {
        error as e => dbg(e),
        null as _ => {
            test::foo <- dbg("hello world");
            test::bar
        }
    }
}
"#;

run!(dynamic_module3, DYNAMIC_MODULE3, |v: Result<&Value>| match v {
    Ok(Value::String(s)) if s == "hello world" => true,
    _ => false,
}; FuseExpect::Jit);

const DYNAMIC_MODULE4: &str = r#"
{
    let source = "
        let foo = never();
        let bar = never();
        select foo { x => bar <- dbg(x) }
    ";
    sys::net::publish("/local/test", source)?;
    let status = mod test dynamic {
        sandbox whitelist [core];
        sig {
            val foo: string;
            val bar: string;
            val baz: string
        };
        source sys::net::subscribe("/local/test")?
    };
    select status {
        error as e => dbg(e),
        null as _ => {
            test::foo <- dbg("hello world");
            test::bar
        }
    }
}
"#;

run!(dynamic_module4, DYNAMIC_MODULE4, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
}; FuseExpect::Jit);

const DYNAMIC_MODULE5: &str = r#"
{
    let source = "
        let foo = never();
        let bar = never();
        select foo { x => bar <- dbg(x) };
        let probe: string = sys::net::subscribe(\"/local/test\")$
    ";
    sys::net::publish("/local/test", source)?;
    let status = mod test dynamic {
        sandbox whitelist [core];
        sig {
            val foo: string;
            val bar: string
        };
        source sys::net::subscribe("/local/test")?
    };
    select status {
        error as e => dbg(e),
        null as _ => {
            test::foo <- dbg("hello world");
            test::bar
        }
    }
}
"#;

run!(dynamic_module5, DYNAMIC_MODULE5, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
}; FuseExpect::Jit);

const DYNAMIC_MODULE6: &str = r#"
{
    let source = "
        let foo = never();
        let bar = never(); select foo { x => bar <- dbg(x) };
        let probe: string = sys::net::subscribe(\"/local/test\")$
    ";
    sys::net::publish("/local/test", source)?;
    let status = mod test dynamic {
        sandbox blacklist [sys::net::publish];
        sig {
            val foo: string;
            val bar: string
        };
        source sys::net::subscribe("/local/test")?
    };
    select status {
        error as e => dbg(e),
        null as _ => {
            test::foo <- dbg("hello world");
            test::bar
        }
    }
}
"#;

run!(dynamic_module6, DYNAMIC_MODULE6, |v: Result<&Value>| match v {
    Ok(Value::String(s)) if s == "hello world" => true,
    _ => false,
}; FuseExpect::Jit);

const DYNAMIC_MODULE7: &str = r#"
{
    let source = "
        let foo = never();
        let bar = never();
        select foo { x => bar <- dbg(x) };
        sys::net::publish(\"/local/test\", 42)
    ";
    sys::net::publish("/local/test", source)?;
    let status = mod test dynamic {
        sandbox blacklist [sys::net::publish];
        sig {
            val foo: string;
            val bar: string
        };
        source sys::net::subscribe("/local/test")?
    };
    select status {
        error as e => dbg(e),
        null as _ => {
            test::foo <- dbg("hello world");
            test::bar
        }
    }
}
"#;

run!(dynamic_module7, DYNAMIC_MODULE7, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
}; FuseExpect::Jit);

const DYNAMIC_MODULE8: &str = r#"
{
    let source = "
        let foo = never();
        let bar = never();
        select foo { x => bar <- dbg(x) };
        let probe: string = sys::net::subscribe(\"/local/test\")$
    ";
    sys::net::publish("/local/test", source)?;
    let status = mod test dynamic {
        sandbox whitelist [core, sys::net::subscribe];
        sig {
            val foo: string;
            val bar: string
        };
        source sys::net::subscribe("/local/test")?
    };
    select status {
        error as e => dbg(e),
        null as _ => {
            test::foo <- dbg("hello world");
            test::bar
        }
    }
}
"#;

run!(dynamic_module8, DYNAMIC_MODULE8, |v: Result<&Value>| match v {
    Ok(Value::String(s)) if s == "hello world" => true,
    _ => false,
}; FuseExpect::Jit);

// A module declared in a seq body is a scope of its own: the seq's
// rewrite and its catch refusal do not reach into its file.
run!(
    seq_module_is_its_own_scope,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(29))),
    "/test.gx" => r#"
let x = 100;
let t = 1;
let a = seqq t { let f = |y| { mod m2; m2::z + y }; f(1) };
let b = seq t { let v = select t { _ => { mod m3; m3::z } }; v };
let result = a + b
"#,
    "/test/m2.gx" => r#"
let x = 7;
let z = x * 2
"#,
    "/test/m3.gx" => r#"
let z = {
  catch(e) println("[e]");
  14
}
"#
    ; FuseExpect::Jit);

// A module's check decides no type the code around it left open: its
// siblings check concurrently and see each other through interfaces.
run!(
    module_check_decides_no_outer_type,
    refused("left open"),
    "/test.gx" => r#"
let x = never();
mod m;
let result = m::y
"#,
    "/test/m.gxi" => r#"
val y: i64;
"#,
    "/test/m.gx" => r#"
let y = super::x + 1
"#
    ; FuseExpect::None);

// Two siblings that would decide the same outer type are both refused,
// and the first in order is the one reported, whatever ran first.
run!(
    module_check_refusal_is_the_first_in_order,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("left open")
        && format!("{e:#}").contains("::a")),
    "/test.gx" => r#"
let x = never();
mod a;
mod b;
let result = a::y + b::y
"#,
    "/test/a.gxi" => r#"
val y: i64;
"#,
    "/test/a.gx" => r#"
let y = super::x + 1
"#,
    "/test/b.gxi" => r#"
val y: i64;
"#,
    "/test/b.gx" => r#"
let y = super::x + 2
"#
    ; FuseExpect::None);

// Annotated, the outer binding's type is the module's to read.
run!(
    module_check_reads_an_annotated_outer_type,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(2))),
    "/test.gx" => r#"
let x: i64 = never();
x <- 1;
mod m;
let result = m::y
"#,
    "/test/m.gxi" => r#"
val y: i64;
"#,
    "/test/m.gx" => r#"
let y = super::x + 1
"#
);

// A gxi signature spells a type through a `use … as` alias.
// Resolution is a pure function of (module, name): a name spelled at
// the def site resolves the same from any deferred consumer.
run!(
    sig_type_through_use_alias,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(21))),
    "/test.gx" => r#"
mod a;
mod m;
let result = m::wrap(a::mk(20)) + 1
"#,
    "/test/a.gxi" => r#"
type T = i64;
val mk: fn(x: i64) -> T;
"#,
    "/test/a.gx" => r#"
let mk = |x: i64| -> T x
"#,
    "/test/m.gxi" => r#"
use super::a::T as U;
val wrap: fn(x: U) -> U;
"#,
    "/test/m.gx" => r#"
let wrap = |x: U| -> U x
"#
    ; FuseExpect::Jit);

// A module-private type annotating a public lambda's body.
run!(
    private_type_in_public_body,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(21))),
    "/test.gx" => r#"
mod m;
let result = m::f(20)
"#,
    "/test/m.gxi" => r#"
val f: fn(x: i64) -> i64;
"#,
    "/test/m.gx" => r#"
type P = i64;
let f = |x: i64| -> i64 {
    let y: P = x;
    y + 1
}
"#
    ; FuseExpect::Jit);

// A use-imported bare type name annotating a binding inside a public
// lambda's body.
run!(
    imported_type_in_body,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(21))),
    "/test.gx" => r#"
mod a;
mod m;
let result = m::f(21)
"#,
    "/test/a.gxi" => r#"
type T = i64;
"#,
    "/test/a.gx" => r#"
let unused = 0
"#,
    "/test/m.gxi" => r#"
use super::a::T;
val f: fn(x: i64) -> i64;
"#,
    "/test/m.gx" => r#"
let f = |x: i64| -> i64 {
    let y: T = x;
    y
}
"#
    ; FuseExpect::Jit);

// `use` and static `mod` are declarations: value position (a `let` RHS,
// a call arg, a block's value slot) is rejected at typecheck. A dynamic
// module stays a real `[error, null]` value.
#[tokio::test]
async fn use_in_value_position_is_compile_error() {
    for src in [
        "{let tag = use array::iter; tag}",
        "{i64:1; use array::iter}",
        "array::len(use array::iter)",
    ] {
        let e = refusal(src, crate::TEST_REGISTER).await.unwrap();
        assert!(e.contains("a use declaration is not an expression"), "{src}: {e}");
    }
}

#[tokio::test]
async fn use_value_soundness_witness_rejected() {
    let src = r#"{let a = {let a = [true]; let tag = use array::*; {catch(e) tag <- e.0; any(a[i64:5]?, i64:0)}; select tag {"" => never(""), t => t}}; select a {[init.., x] => x * i64:100, _ => i64:0}}"#;
    let e = refusal(src, crate::TEST_REGISTER).await.unwrap();
    assert!(e.contains("a use declaration is not an expression"), "{e}");
}

// `type`/`trait`/`impl` are statement-position-only declarations too.
#[tokio::test]
async fn declaration_in_value_position_is_compile_error() {
    for src in [
        "{let t = type M = i64; t}",
        "{i64:1; type M = i64}",
        "select i64:0 {_ => type M = i64}",
        "{let t = trait Sh { val sh: fn(self) -> i64 }; t}",
        "{trait Sh { val sh: fn(self) -> i64 }; type C = Abstract<i64>; \
         let x = impl Sh for C { let sh = |c| c.0 }; x}",
    ] {
        let r = eval(src, crate::TEST_REGISTER).await;
        let msg = match &r {
            Err(e) => format!("{e:?}"),
            Ok((v, _)) => {
                panic!("declaration in value position must be rejected: {src} => {v:?}")
            }
        };
        assert!(msg.contains("not an expression"), "wrong refusal for {src}: {msg}");
    }
}

#[tokio::test]
async fn bottom_connect_target_witness_rejected() {
    // A typedef in value position, and a value-position connect into a
    // ⊥-initialized binding whose site cell is already bound.
    for (src, refused) in [
        (
            r#"{let outer = never(); catch(e) outer <- i64:1; let g = || {let inner = type M = [`M(Map<string, i64>), `N]; catch(e) inner <- array::filter(["a", "b"], |s| str::len(s) > i64:5); error(`A)?; inner}; let v = g(); error(`B)?; v - outer}"#,
            "a type definition is not an expression",
        ),
        (
            r#"{let dummy = i64:0; let g = || {let inner = dummy <- i64:1; catch(e) inner <- array::filter(["a", "b"], |s| str::len(s) > i64:5); error(`A)?; inner}; let v = g(); v - i64:1}"#,
            "cannot compute",
        ),
    ] {
        let e = refusal(src, crate::TEST_REGISTER).await.unwrap();
        assert!(e.contains(refused), "{src}: {e}");
    }
}

// A module-private type as a union member in a body annotation, reached
// through a nested lambda's connect: the instance body typechecks under
// the defining module's env.
run!(
    private_type_union_member,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(21))),
    "/test.gx" => r#"
mod m;
let result = m::f(20)
"#,
    "/test/m.gxi" => r#"
val f: fn(x: i64) -> i64;
"#,
    "/test/m.gx" => r#"
type P = { title: string, ok: bool };
let f = |x: i64| -> i64 {
    let toast: [P, null] = null;
    let fail = |t: string| -> null {
        toast <- { title: t, ok: false };
        null
    };
    fail("t");
    select toast {
        null as _ => x + 1,
        _ => x
    }
}
"#
    ; FuseExpect::Jit);

// A `mod` declared inside a field access's source, a labeled default
// and a map key resolves like one in a lambda body.
run!(
    mod_in_every_expression_position,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(7))),
    "/test.gx" => r#"
let a = ({mod helper; {x: helper::x}}).x;
let f = |#x = {mod helper2; helper2::x}, y| x + y;
let m = {{mod helper3; helper3::x} => 1};
let result = a + f(1) + map::len(m) + map::get_or(m, 2, 3)
"#,
    "/test/helper.gx" => "let x = 2",
    "/test/helper2.gx" => "let x = 2",
    "/test/helper3.gx" => "let x = 2"
    ; FuseExpect::Jit);

// A module inside a module of its own name is another module, not an
// import cycle (a real cycle: graphix-shell/tests/import_cycle.rs).
run!(
    nested_module_of_its_own_name,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(3))),
    "/test.gx" => "mod a;\nlet result = a::x",
    "/test/a.gx" => "mod b;\nlet x = b::y",
    "/test/a/b.gx" => "mod a;\nlet y = a::z",
    "/test/a/b/a.gx" => "let z = 3"
    ; FuseExpect::Jit);

// The same source imported on two branches is not a cycle.
run!(
    sibling_imports_of_one_source,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(2))),
    "/test.gx" => r#"
mod x;
mod y;
let result = x::a + y::b
"#,
    "/test/x.gx" => "mod shared;\nlet a = shared::v",
    "/test/y.gx" => "mod shared;\nlet b = shared::v",
    "/test/x/shared.gx" => "let v = 1",
    "/test/y/shared.gx" => "let v = 1"
    ; FuseExpect::Jit);

// A blacklisted package takes its submodules with it.
const DYNAMIC_MODULE_BLACKLIST_ROOT: &str = r#"
{
    let source = "
        let foo = never();
        let bar = never();
        select foo { x => bar <- dbg(x) };
        sys::net::publish(\"/local/test\", 42)
    ";
    sys::net::publish("/local/test", source)?;
    let status = mod test dynamic {
        sandbox blacklist [sys];
        sig {
            val foo: string;
            val bar: string
        };
        source sys::net::subscribe("/local/test")?
    };
    select status {
        error as e => dbg(e),
        null as _ => {
            test::foo <- dbg("hello world");
            test::bar
        }
    }
}
"#;

run!(dynamic_module_blacklist_root, DYNAMIC_MODULE_BLACKLIST_ROOT, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
}; FuseExpect::Jit);

// A loaded body's deferred import must name something, as a file's must.
const DYNAMIC_MODULE_MISSING_IMPORT: &str = r#"
{
    let source = "use core::no_such_thing; let x = 42";
    sys::net::publish("/local/imp", source)?;
    let status = mod imp dynamic {
        sandbox whitelist [core];
        sig { val x: i64 };
        source sys::net::subscribe("/local/imp")?
    };
    select status {
        error as e => dbg(e),
        null as _ => imp::x
    }
}
"#;

run!(dynamic_module_missing_import, DYNAMIC_MODULE_MISSING_IMPORT, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
}; FuseExpect::Jit);

// Sleep is pause: a loaded body whose arm sleeps resumes at the wake
// even though its source does not fire again.
const DYNAMIC_MODULE_SLEEP: &str = r#"
{
    let src = "let x = 0; x <- sys::time::timer(duration:2.ms, true) ~ x + 1";
    let n = 0;
    n <- sys::time::timer(duration:40.ms, true) ~ n + 1;
    let x = select n % 2 {
        0 => {
            let status = mod foo dynamic {
                sandbox whitelist [core, sys::time];
                sig { val x: i64 };
                source src
            };
            select status { error as e => never(dbg(e)), null as _ => foo::x }
        },
        _ => never()
    };
    filter(x, |v| v >= 40)
}
"#;

run!(dynamic_module_sleep_is_pause, DYNAMIC_MODULE_SLEEP, |v: Result<&Value>| match v {
    Ok(Value::I64(n)) => *n >= 40,
    _ => false,
}; FuseExpect::Jit);

async fn compile_error(
    files: &[(&str, &str)],
    text: &'static str,
) -> Result<anyhow::Error> {
    let tbl = ahash::AHashMap::from_iter(files.iter().map(|(p, t)| {
        (
            netidx_core::path::Path::from(arcstr::ArcStr::from(*p)),
            graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(*t)),
        )
    }));
    let (tx, _rx) = tokio::sync::mpsc::channel(10);
    let resolver = graphix_compiler::expr::VfsResolver::new(tbl);
    let ctx = graphix_package_core::testing::init_with_setup(
        tx,
        &crate::TEST_REGISTER,
        vec![resolver],
        |_| {},
    )
    .await?;
    let e = ctx.rt.compile(arcstr::ArcStr::from(text)).await.err();
    ctx.shutdown().await;
    e.ok_or_else(|| anyhow::anyhow!("{text} compiled"))
}

// Refusals of a statement in the wrong place carry their site.
#[tokio::test(flavor = "current_thread")]
async fn misplaced_statements_are_placed() -> Result<()> {
    use graphix_compiler::expr::ErrorSite;
    let m = [("/m.gx", "let k = 1")];
    for text in ["let x = (use array::map)", "mod m; mod m", "let x = (type T = i64)"] {
        let e = compile_error(&m, text).await?;
        assert!(e.downcast_ref::<ErrorSite>().is_some(), "{text}: {e:#}");
    }
    Ok(())
}

// An error in a module's body is framed by the module's own file.
#[tokio::test(flavor = "current_thread")]
async fn module_errors_are_framed_by_the_module() -> Result<()> {
    for inner in ["let x: i64 = \"s\"", "let x = no_such_name"] {
        let files =
            [("/test.gx", "mod inner; let result = 0"), ("/test/inner.gx", inner)];
        let e = compile_error(&files, "{ mod test; test::result }").await?;
        assert!(format!("{e:?}").contains("in module inner"), "{inner}: {e:?}");
    }
    Ok(())
}

// A source that fails to compile leaves nothing behind: the next one
// compiles the same names.
const DYNAMIC_MODULE_AFTER_A_FAILURE: &str = r#"
{
    let source = "let f = |x| x; let h = missing";
    source <- sys::time::after_idle(duration:10.ms, "let f = |x| x + 1; let h = f(41)");
    let status = mod m dynamic {
        sandbox whitelist [core];
        sig { val h: i64 };
        source source
    };
    select status {
        error as _ => never(),
        null as _ => m::h
    }
}
"#;

run!(dynamic_module_after_a_failure, DYNAMIC_MODULE_AFTER_A_FAILURE, |v: Result<
    &Value,
>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
});

// A module's check that would unify two cells the code around it left
// open is refused, as a write to one is.
run!(
    module_check_unifies_outer_cells_refused,
    graphix_package_core::testing::refused("the check decides a type the code around the module left open"),
    "/test.gx" => r#"
        let x = [];
        let y = [];
        mod m;
        y <- ["s"];
        let result = array::map(x, |v| v + 1)
    "#,
    "/test/m.gxi" => "",
    "/test/m.gx" => "super::x <- super::y";
    FuseExpect::None
);

// A module that only reads a binding the code around it settled is not
// deciding it, though the binding is not in normal form.
run!(
    module_reads_an_outer_binding,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(5))),
    "/test.gx" => r#"
        let v = never();
        v <- [`A, `B][0];
        mod inner;
        let result = inner::f(5)
    "#,
    "/test/inner.gxi" => "val f: fn(x: i64) -> i64",
    "/test/inner.gx" => r#"
        use super::v;
        let f = |x: i64| -> i64 select v { _ => x }
    "#;
    FuseExpect::Jit
);

// An interface's labeled arguments pair with the implementation's by
// label, whatever order each writes them in.
run!(
    interface_labels_pair_by_name,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(1))),
    "/test.gx" => r#"
        mod m;
        let result = m::f(#a: 1, #b: "x")
    "#,
    "/test/m.gxi" => "val f: fn(#a: i64, #b: string) -> i64",
    "/test/m.gx" => "let f = |#b: string, #a: i64| -> i64 a"
);

// An implementation's bounds on cells its inferred parameters hold are
// checked against the interface: declared, they pass; omitted, refused.
run!(
    interface_declares_inferred_bounds,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(2))),
    "/test.gx" => r#"
        mod m;
        let result = m::f([1])[0]$
    "#,
    "/test/m.gxi" => "val f: fn<'a: Number + Singleton>(x: Array<'a>) -> Array<'a>",
    "/test/m.gx" => "let f = |x| { let y = x[0]$; [y + y] }"
);

run!(
    interface_omits_inferred_bounds,
    graphix_package_core::testing::refused("the implementation requires 'a: Number"),
    "/test.gx" => r#"
        mod m;
        let result = m::f([1])
    "#,
    "/test/m.gxi" => "val f: fn(x: Array<'a>) -> Array<'a>",
    "/test/m.gx" => "let f = |x| { let y = x[0]$; [y + y] }";
    FuseExpect::None
);

/// A dynamic module's load failed with `phrase` in its error.
fn load_refused(phrase: &'static str) -> impl Fn(Result<&Value>) -> bool {
    move |v| matches!(v, Ok(Value::Error(e)) if format!("{e}").contains(phrase))
}

// The sandbox follows a definition into every later compile of its body.
const LOADED_BUILTIN_IN_A_LAMBDA: &str = r#"
{
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { val n: i64 };
        source """
            let outer = |s: string| -> i64 {
                let len = |s: string| -> i64 'str_len;
                len(s)
            };
            let n = outer("abc")
        """
    };
    select status { error as e => e, null as _ => never() }
}
"#;

run!(loaded_builtin_in_a_lambda, LOADED_BUILTIN_IN_A_LAMBDA,
    load_refused("defining builtins is not allowed"); FuseExpect::Jit);

// A loaded body's top-level cells are monomorphic: a function writing one
// is not generalized over it.
const LOADED_BODY_IS_TOP_LEVEL: &str = r#"
{
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { val w: i64 };
        source """
            let z = never();
            let f = |x| { z <- x; x };
            let a = f("s");
            let w = f(1)
        """
    };
    select status { error as e => e, null as _ => never() }
}
"#;

run!(loaded_body_is_top_level, LOADED_BODY_IS_TOP_LEVEL,
    load_refused("type mismatch"); FuseExpect::Jit);

// A loaded body settles what its check deferred before it elaborates.
const LOADED_BODY_SETTLES: &str = r#"
{
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { val a: i64; val w: i64 };
        source """
            let a = 1;
            let w = { catch(e: Error<`A>) println("[e]"); error(`B)?; 1 }
        """
    };
    select status { error as e => e, null as _ => never() }
}
"#;

run!(loaded_body_settles, LOADED_BODY_SETTLES,
    load_refused("does not contain"); FuseExpect::Jit);

// The interface exports the last binding of a name.
const EXPORT_IS_THE_LAST_BINDING: &str = r#"
{
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { val f: fn(x: i64) -> i64 };
        source """
            let f = |x: i64| -> i64 x + 1;
            let g = |x: i64| -> i64 x + 100;
            let f = g
        """
    };
    select status { error as e => never(dbg(e)), null as _ => foo::f(1) }
}
"#;

run!(export_is_the_last_binding, EXPORT_IS_THE_LAST_BINDING, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(101)))
}; FuseExpect::None);

// An implementation fulfils a declared impl only at the declared head.
const IMPL_NARROWER_THAN_DECLARED: &str = r#"
{
    trait Val { val v: fn(self) -> i64 };
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig {
            use super::Val;
            type Box<'a>;
            impl<'a> Val for Box<'a>;
            val make: fn(x: 'a) -> Box<'a>
        };
        source """
            type Box<'a> = Abstract<'a>;
            impl Val for Box<i64> { let v = |b| b.0 };
            let make = |x| Box(x)
        """
    };
    select status { error as e => e, null as _ => never() }
}
"#;

run!(impl_narrower_than_declared, IMPL_NARROWER_THAN_DECLARED,
    load_refused("does not implement the declared impl"); FuseExpect::Jit);

// A reload may not change a hidden type's representation: a value of
// the old one may still be held.
const RELOAD_CHANGES_A_REPRESENTATION: &str = r#"
{
    let source = """type C = Abstract<string>; let make = |x: i64| C("v")""";
    sys::net::publish("/local/reprchange", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { type C; val make: fn(x: i64) -> C };
        source sys::net::subscribe("/local/reprchange")?
    };
    source <- once(status) ~ """type C = Abstract<i64>; let make = |x: i64| C(x)""";
    select status { error as e => e, null as _ => never() }
}
"#;

run!(reload_changes_a_representation, RELOAD_CHANGES_A_REPRESENTATION,
    load_refused("representation cannot change"); FuseExpect::Jit);

// `use super::*` from a block-level module takes a name shadowed across
// the block chain innermost-first, as `use super::x` does.
run!(
    glob_super_takes_the_innermost,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(2))),
    "/test.gx" => r#"
        let x = 1;
        let result = {
            let x = 2;
            mod inner;
            inner::y
        }
    "#,
    "/test/inner.gx" => r#"
        use super::*;
        let y = x
    "#
);

// A use statement's items resolve against the scope as it stood before
// it, whatever their order: a sibling's rename never reroutes them.
run!(
    use_items_resolve_together,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) => a[..] == [Value::I64(1), Value::I64(1)],
        _ => false,
    },
    "/test.gx" => r#"
        mod a;
        let b = { use a::{b, c as a}; b };
        let z = { use a::{z, c as a}; z };
        let result = (b, z)
    "#,
    "/test/a.gx" => r#"
        mod c;
        let b = 1;
        let z = 1
    "#,
    "/test/a/c.gx" => r#"
        let b = 2;
        let z = 2
    "#
);

// An interface's use is registered by the signature and again where it
// is spliced into the body: one site, one import.
run!(
    interface_use_renaming_its_root,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(1))),
    "/test.gx" => r#"
        mod m;
        let result = m::v
    "#,
    "/test/m.gxi" => r#"
        use sys::net as sys;
        val v: i64
    "#,
    "/test/m.gx" => r#"
        let v = 1
    "#
);

// A loaded body's primes are its own: two modules loading in one cycle in
// forked branches requeue nothing, and nothing later in the cycle fires
// from a prime.
async fn loads_leave_no_primes(mode: Mode) -> Result<()> {
    use super::dense_deltas::run_delta;
    let (values, _) = run_delta(
        r#"{
            let y = 1;
            let k = 0;
            k <- select k { x if x < 3 => x + 1, _ => never() };
            let src = never();
            src <- select k { 2 => "let v = super::y + 1", _ => never() };
            let r = #[parallel] (
                mod a dynamic { sandbox unrestricted; sig { val v: i64 }; source src },
                mod b dynamic { sandbox unrestricted; sig { val v: i64 }; source src }
            );
            let n = 0;
            n <- y ~ n + 1;
            n
        }"#,
        mode,
    )
    .await?;
    assert_eq!(values, vec![Value::I64(0), Value::I64(1)]);
    Ok(())
}

modes!(loads_leave_no_primes);

// Sibling modules checked in parallel each implementing one trait
// undeclared: both impls survive the join.
run!(
sibling_modules_implement_one_trait,
|v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "a1 b2"),
"/test.gx" => r#"
trait Show { val show: fn(self) -> string };
mod a;
mod b;
let result = "[Show::show(a::v)] [Show::show(b::v)]"
"#,
"/test/a.gxi" => r#"
type A = Abstract<i64>;
val v: A;
"#,
"/test/a.gx" => r#"
type A = Abstract<i64>;
let v = A(1);
impl super::Show for A { let show = |a| "a[a.0]" }
"#,
"/test/b.gxi" => r#"
type B = Abstract<i64>;
val v: B;
"#,
"/test/b.gx" => r#"
type B = Abstract<i64>;
let v = B(2);
impl super::Show for B { let show = |b| "b[b.0]" }
"#
);
