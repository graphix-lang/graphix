// Tests for dynamic modules

use anyhow::Result;
use graphix_package_core::run;
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
        sys::net::subscribe(\"/local/test\")$
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
        sys::net::subscribe(\"/local/test\")$
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
        sys::net::subscribe(\"/local/test\")$
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

// ── Finding-1 regression fixtures (design/module_system.md P3) ──
//
// The admin-TUI campaign's finding 1 (2026-08-21): under the old
// open-style resolver, a name spelled at the DEF site could be
// unresolvable when a deferred consumer (per-callsite instance
// elaboration, TypeRef touch) re-resolved it from a different
// scope/time — the ambient environment could not be reconstructed.
// The namespace table makes resolution a pure function of
// (module, name); these pin the three faces green.

// Face 1: a gxi signature spells a type through a `use … as` alias;
// the consumer resolves it at instance/callsite touch.
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

// Face 2: a module-PRIVATE type (not in the gxi) annotating a public
// lambda's body — resolved at per-callsite instance elaboration.
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

// Face 3: a use-imported bare type name annotating a binding inside
// a public lambda's body.
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
