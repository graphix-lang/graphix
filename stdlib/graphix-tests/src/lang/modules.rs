// Tests for dynamic modules

use anyhow::Result;
use graphix_package_core::{run, testing::eval};
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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

const DYNAMIC_MODULE2: &str = r#"
{
    let source = "let add = 'a: Number |x: 'a| -> 'a x + x";
    sys::net::publish("/local/foo", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig {
            val add: fn<'a: Number>(x: 'a) -> 'a
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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

// Resolution is a pure function of (module, name): a name spelled at
// the def site resolves the same from any deferred consumer.

// A gxi signature spells a type through a `use … as` alias.
run!(
    finding1_sig_alias,
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
    ; graphix_package_core::testing::FuseExpect::Jit);

// A module-private type annotating a public lambda's body.
run!(
    finding1_private_type_in_body,
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
    ; graphix_package_core::testing::FuseExpect::Jit);

// A use-imported bare type name annotating a binding inside a public
// lambda's body.
run!(
    finding1_imported_body_annotation,
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
    ; graphix_package_core::testing::FuseExpect::Jit);

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
        let r = eval(src, crate::TEST_REGISTER).await;
        assert!(
            r.is_err(),
            "`use` in value position must be rejected: {src} => {:?}",
            r.map(|(v, _)| v)
        );
    }
}

#[tokio::test]
async fn use_value_soundness_witness_rejected() {
    let src = r#"{let a = {let a = [true]; let tag = use array::*; {catch(e) tag <- e.0; any(a[i64:5]?, i64:0)}; select tag {"" => never(""), t => t}}; select a {[init.., x] => x * i64:100, _ => i64:0}}"#;
    let r = eval(src, crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "the aieka use-in-value witness must be rejected, got {:?}",
        r.map(|(v, _)| v)
    );
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
    for src in [
        r#"{let outer = never(); catch(e) outer <- i64:1; let g = || {let inner = type M = [`M(Map<string, i64>), `N]; catch(e) inner <- array::filter(["a", "b"], |s| str::len(s) > i64:5); error(`A)?; inner}; let v = g(); error(`B)?; v - outer}"#,
        r#"{let dummy = i64:0; let g = || {let inner = dummy <- i64:1; catch(e) inner <- array::filter(["a", "b"], |s| str::len(s) > i64:5); error(`A)?; inner}; let v = g(); v - i64:1}"#,
    ] {
        let r = eval(src, crate::TEST_REGISTER).await;
        assert!(
            r.is_err(),
            "the ⊥ connect-target witness must be rejected: {src} => {:?}",
            r.map(|(v, _)| v)
        );
    }
}

// A module-private type as a union member in a body annotation, reached
// through a nested lambda's connect: the instance body typechecks under
// the defining module's env.
run!(
    finding1_private_type_union_member,
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
    ; graphix_package_core::testing::FuseExpect::Jit);
