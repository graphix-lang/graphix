// Modules: static, dynamic, interfaces and `use`.

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, Mode, compile_error, eval, is, prints, refusal, refused},
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

run!(dynamic_module0, DYNAMIC_MODULE0, is(Value::I64(2)); FuseExpect::None);

// A source that is no program reports through the status, and the last
// good load runs on: a write to its val still reaches it.
const DYNAMIC_MODULE_SOURCE_ERROR: &str = r#"
{
    let src: [string, Error<`E>] = "let x = 0; let y = x * 10";
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { val x: i64; val y: i64 };
        source src
    };
    src <- once(status) ~ error(`E);
    foo::x <- select status { error as _ => 5, _ => never() };
    select foo::y { 50 => "kept", _ => never() }
}
"#;

run!(dynamic_module_source_error_keeps_the_load, DYNAMIC_MODULE_SOURCE_ERROR, is(Value::String("kept".into())); FuseExpect::Jit);

// A source string that is no program is the same case: the new source is
// compiled before the running body is torn down, so a typo reports through
// the status and the last good load still computes.
const DYNAMIC_MODULE_BAD_SOURCE_KEEPS_THE_LOAD: &str = r#"
{
    let src: [string, Error<`E>] = "let x = 0; let y = x * 10";
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { val x: i64; val y: i64 };
        source src
    };
    src <- once(status) ~ "let x = ;";
    foo::x <- select status { error as _ => 5, _ => never() };
    select foo::y { 50 => "kept", _ => never() }
}
"#;

run!(dynamic_module_bad_source_keeps_the_load, DYNAMIC_MODULE_BAD_SOURCE_KEEPS_THE_LOAD, is(Value::String("kept".into())); FuseExpect::Jit);

// A refused reload is not the end of the module: the good source that
// follows it loads and runs.
const DYNAMIC_MODULE_GOOD_SOURCE_AFTER_A_BAD_ONE: &str = r#"
{
    let step = 0;
    step <- select step { k if k < 6 => (k ~ step) + 1, _ => never() };
    let src: [string, Error<`E>] = "let x = 0; let y = x * 10";
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { val x: i64; val y: i64 };
        source src
    };
    src <- select step {
        1 => step ~ "let x = ;",
        3 => step ~ "let x = 0; let y = x * 100",
        _ => never()
    };
    foo::x <- select step { 5 => step ~ 5, _ => never() };
    select step { 6 => foo::y, _ => never() }
}
"#;

run!(dynamic_module_good_source_after_a_bad_one, DYNAMIC_MODULE_GOOD_SOURCE_AFTER_A_BAD_ONE, is(Value::I64(500)); FuseExpect::Jit);

// A reload refused for a typo leaves what the running body registered in
// the interface's place: the good source that follows narrows the
// declared Cast with its own method, as a reload with no typo does.
run!(
    dynamic_module_bad_source_then_a_cast_reload_narrows,
    r#"{
    let step = 0;
    step <- select step { k if k < 7 => (k ~ step) + 1, _ => never() };
    let first = "type C = Abstract<i64>; impl Cast for C {}; let make = |x: i64| C(x)";
    let typo = "type C = Abstract<i64>; impl Cast for C { let to_value = |c: C| ; }; let make = |x: i64| C(x)";
    let good = "type C = Abstract<i64>; impl Cast for C { let to_value = |c: C| c.0 * 10 }; let make = |x: i64| C(x)";
    let source: [string, Error<`E>] = first;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { type C; impl Cast for C; val make: fn(x: i64) -> C };
        source source
    };
    source <- select step {
        1 => step ~ typo,
        3 => step ~ good,
        _ => never()
    };
    let w = select step { 5 => json::write_str(foo::make(3))$, _ => never() };
    select w { "30" => "reloaded", _ => never() }
}"#,
    is(Value::String("reloaded".into()));
    FuseExpect::Jit
);

// A body that declares a trait is the same case twice over: the refused
// load leaves the running one dispatching through it, and the load that
// follows declares the trait -- whose identity is its scope and name --
// again over the body it replaces.
run!(
    dynamic_module_a_trait_reloads_with_the_source,
    r#"{
    let step = 0;
    step <- select step { k if k < 7 => (k ~ step) + 1, _ => never() };
    let first = "trait T { val d: fn(self) -> i64 }; type C = Abstract<i64>; impl T for C { let d = |c: C| c.0 + 1 }; let make = |x: i64| T::d(C(x))";
    let good = "trait T { val d: fn(self) -> i64 }; type C = Abstract<i64>; impl T for C { let d = |c: C| c.0 + 2 }; let make = |x: i64| T::d(C(x))";
    let source: [string, Error<`E>] = first;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { val make: fn(x: i64) -> i64 };
        source source
    };
    source <- select step {
        1 => step ~ "let make = ;",
        3 => step ~ good,
        _ => never()
    };
    let seen: Array<i64> = [];
    seen <- select step {
        2 => step ~ array::push(seen, foo::make(5)),
        5 => step ~ array::push(seen, foo::make(5)),
        _ => never()
    };
    select step { 6 => seen, _ => never() }
}"#,
    prints("[i64:6, i64:7]");
    FuseExpect::Jit
);

// A reload whose Cast impl writes a method the first load left to the
// representation writes through it: the interface's declaration is
// narrowed afresh at each load.
run!(
    dynamic_cast_reload_gains_a_method,
    r#"{
    let source = "type C = Abstract<i64>; impl Cast for C {}; let make = |x: i64| C(x)";
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { type C; impl Cast for C; val make: fn(x: i64) -> C };
        source source
    };
    source <- once(status) ~ "type C = Abstract<i64>; impl Cast for C { let to_value = |c: C| c.0 * 10 }; let make = |x: i64| C(x)";
    let w = select status { error as e => never(dbg(e)), null as _ => json::write_str(foo::make(3))$ };
    select w { "30" => "reloaded", _ => never() }
}"#,
    is(Value::String("reloaded".into()));
    FuseExpect::Jit
);

// A statement reading a dynamic module's val runs after the statement
// holding the module, which publishes it at each load.
run!(
    a_dynamic_module_publishes_before_its_readers,
    r#"{
  let n = 0;
  n <- select n < 12 { true => n + 1, false => never() };
  let src = "let v = 5";
  src <- select n { 2 => "let v = 7", _ => never() };
  let status = mod foo dynamic {
    sandbox whitelist [core];
    sig { val v: i64 };
    source src
  };
  let a = foo::v;
  let seen: Array<i64> = [];
  seen <- a ~ array::push(seen, a);
  select n { 12 => (status, seen), _ => never() }
}"#,
    prints("[null, [i64:5, i64:7]]");
    FuseExpect::Jit
);

// A trait is declared once: in a function body it is refused, as an
// `impl` is.
run!(
    trait_in_a_function_body_refused,
    refused("trait L is in a function body"),
    "/test.gx" => r#"
        let mk = |x: i64| { trait L { val l: fn(self) -> i64 = |s| 7 }; x };
        let result = mk(1)
    "#
);

// A block's value is its last statement, which a let does not have.
run!(
    block_ending_in_a_let_refused,
    refused("a let has none"),
    "/test.gx" => r#"
        let f = |x: i64| { let a = x; let b = a + 1 };
        let result = f(1)
    "#
);

// A seq step holds no exemption for a block nested in it.
run!(
    block_ending_in_a_let_in_a_seq_step_refused,
    refused("a let has none"),
    "/test.gx" => r#"
        let t = 1;
        let result = seq t { let a = (1, { let c = 2; let b = c + 1 }); a }
    "#
);

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

run!(dynamic_module1, DYNAMIC_MODULE1, load_refused("signature has i64, implementation has f64"); FuseExpect::Jit);

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

run!(dynamic_module2, DYNAMIC_MODULE2, is(Value::I64(4)); FuseExpect::Jit);

// A dynamic module's source can declare no module, so a sig's `mod` is
// refused when the program compiles, never at each load.
run!(
    dynamic_module_sig_mod_is_refused,
    r#"
{
    let status = mod foo dynamic {
        sandbox unrestricted;
        sig { mod m; val x: i64 };
        source "let x = 1"
    };
    status
}
"#,
    refused("a dynamic module has no sub-modules")
);

/// A dynamic module `test` loaded under `sandbox`, its sig `foo` and `bar`
/// with `sig` after them, its body the relay `foo` to `bar` with `body`
/// after it: `test::bar` once the program writes `test::foo`, or the
/// load's error.
fn sandboxed(sandbox: &str, sig: &str, body: &str) -> String {
    format!(
        r#"{{
    let source = "
        let foo = never();
        let bar = never();
        select foo {{ x => bar <- dbg(x) }}{body}
    ";
    sys::net::publish("/local/test", source)?;
    let status = mod test dynamic {{
        sandbox {sandbox};
        sig {{
            val foo: string;
            val bar: string{sig}
        }};
        source sys::net::subscribe("/local/test")?
    }};
    select status {{
        error as e => dbg(e),
        null as _ => {{
            test::foo <- dbg("hello world");
            test::bar
        }}
    }}
}}"#
    )
}

/// A body line that subscribes, which `sys::net::subscribe` grants.
const SUBSCRIBES: &str = r#";
        let probe: string = sys::net::subscribe(\"/local/test\")$"#;

/// A body line that publishes, which `sys::net::publish` grants.
const PUBLISHES: &str = r#";
        sys::net::publish(\"/local/test\", 42)"#;

run!(
    whitelisted_core_loads,
    sandboxed("whitelist [core]", "", ""),
    prints(r#""hello world""#);
    FuseExpect::Jit
);

run!(
    a_missing_implementation_is_refused,
    sandboxed("whitelist [core]", ";\n            val baz: string", ""),
    load_refused("val baz: string is missing an implementation");
    FuseExpect::Jit
);

run!(
    an_unlisted_package_is_refused,
    sandboxed("whitelist [core]", "", SUBSCRIBES),
    load_refused("no module `net` in `sys`");
    FuseExpect::Jit
);

run!(
    a_blacklist_keeps_the_rest,
    sandboxed("blacklist [sys::net::publish]", "", SUBSCRIBES),
    prints(r#""hello world""#);
    FuseExpect::Jit
);

run!(
    a_blacklisted_function_is_refused,
    sandboxed("blacklist [sys::net::publish]", "", PUBLISHES),
    load_refused("sys::net::publish not defined");
    FuseExpect::Jit
);

run!(
    a_whitelisted_function_loads,
    sandboxed("whitelist [core, sys::net::subscribe]", "", SUBSCRIBES),
    prints(r#""hello world""#);
    FuseExpect::Jit
);

// A blacklisted package takes its submodules with it.
run!(
    a_blacklisted_root_is_refused,
    sandboxed("blacklist [sys]", "", PUBLISHES),
    load_refused("no module `net` in `sys`");
    FuseExpect::Jit
);

/// A module `m` loaded under `sandbox` from `body`, reading the loader's
/// `y`: its status.
fn reads_the_loader(sandbox: &str, body: &str) -> String {
    format!(
        r#"{{
    let y = 41;
    mod m dynamic {{ sandbox {sandbox}; sig {{ val v: i64 }}; source "{body}" }}
}}"#
    )
}

// A whitelist sandbox keeps a loaded body from the loader's own
// bindings, however the body names them; a blacklist only removes what
// it lists.
run!(
    a_whitelist_hides_the_loaders_super,
    reads_the_loader("whitelist [core]", "let v = super::y + 1"),
    load_refused("super::y not defined");
    FuseExpect::None
);

run!(
    a_whitelist_hides_the_loaders_package,
    reads_the_loader("whitelist [core]", "let v = package::y + 1"),
    load_refused("package::y not defined");
    FuseExpect::None
);

run!(
    a_whitelist_hides_the_loaders_glob,
    reads_the_loader("whitelist [core]", "use super::*; let v = y + 1"),
    load_refused("y not defined");
    FuseExpect::None
);

run!(
    a_blacklist_shows_the_loaders_super,
    reads_the_loader("blacklist [sys]", "let v = super::y + 1"),
    is(Value::Null);
    FuseExpect::None
);

// A module declared in a seq body is a scope of its own: the seq's
// rewrite and its catch refusal do not reach into its file.
run!(
    seq_module_is_its_own_scope,
    is(Value::I64(29)),
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

// A module's abstract type is its path without the components a body
// mints: the module loaded at the top level and in a function body
// declares one type.
run!(
    module_abstract_type_is_one_in_a_body_and_at_the_top,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:4, i64:4]"),
    "/test.gx" => r#"
mod m;
let g = |v: Any| -> i64 {
  mod m;
  select v { m::T as t => m::get(t), _ => -1 }
};
let v: Any = m::mk(4);
let result = (g(v), select v { m::T as t => m::get(t), _ => -1 })
"#,
    "/test/m.gx" => r#"
type T = Abstract<i64>;
let mk = |x: i64| -> T T(x);
let get = |t: T| -> i64 t.0
"#
    ; FuseExpect::Jit);

// A function that declares an abstract type, called by two statements
// that elaborate in parallel tasks, registers the type once.
run!(
    body_abstract_type_from_two_statements,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:1, i64:2]"),
    "/test.gx" => r#"
let mk = |x: i64| { type B = Abstract<i64>; B(x).0 };
let a = mk(1);
let b = mk(2);
let result = [a, b]
"#
    ; FuseExpect::Jit);

// A module's check decides no type the code around it left open: its
// siblings check concurrently and see each other through interfaces.
run!(
    module_check_decides_no_outer_type,
    refused("left open: annotate `x` (line 2, column 5)"),
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
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("::a")
        && format!("{e:#}").ends_with("left open: annotate `x` (line 2, column 5)")),
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

// The refusal names only the bindings around the module, never a
// sibling's local whose type the same write holds.
run!(
    module_check_refusal_names_no_sibling_local,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("::n")
        && format!("{e:#}").ends_with("left open: annotate `x` (line 2, column 5)")),
    "/test.gx" => r#"
let x = never();
mod m;
mod n;
x <- 1;
let result = (m::y, n::z)
"#,
    "/test/m.gxi" => r#"
val y: i64;
"#,
    "/test/m.gx" => r#"
let y = { let v = super::x; 1 }
"#,
    "/test/n.gxi" => r#"
val z: string;
"#,
    "/test/n.gx" => r#"
let z = { super::x <- "a"; "b" }
"#
    ; FuseExpect::None);

// A binding the refusal names in another file than the module
// statement's is named with its file.
run!(
    module_check_refusal_names_the_file,
    |v: Result<&Value>| matches!(v, Err(e)
        if format!("{e:#}").ends_with("annotate `x` (line 2, column 5 in module /test)")),
    "/test.gx" => r#"
let x = never();
mod p;
let result = p::q::y
"#,
    "/test/p.gxi" => r#"
mod q;
"#,
    "/test/p.gx" => r#"
mod q;
"#,
    "/test/p/q.gxi" => r#"
val y: i64;
"#,
    "/test/p/q.gx" => r#"
let y = super::super::x + 1
"#
    ; FuseExpect::None);

// Annotated, the outer binding's type is the module's to read.
run!(
    module_check_reads_an_annotated_outer_type,
    is(Value::I64(2)),
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

// A module with an interface declared in a function body checks its body
// against the interface in each instance.
run!(
    interfaced_module_in_a_lambda_body,
    is(Value::I64(33)),
    "/test.gx" => r#"
let h = |k: i64| -> i64 {
    mod m;
    m::f(m::y) + k
};
let result = h(1) + h(20)
"#,
    "/test/m.gxi" => r#"
val y: i64;
val f: fn(x: i64) -> i64
"#,
    "/test/m.gx" => r#"
let y = 2;
let f = |x: i64| -> i64 x * 3
"#
);

// A gxi signature spells a type through a `use … as` alias.
// Resolution is a pure function of (module, name): a name spelled at
// the def site resolves the same from any deferred consumer.
run!(
    sig_type_through_use_alias,
    is(Value::I64(21)),
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
    is(Value::I64(21)),
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
    is(Value::I64(21)),
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
    is(Value::I64(21)),
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
    is(Value::I64(7)),
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
// import cycle (graphix-shell/tests/import_cycle.rs: two files naming
// each other are not one either; a directory linking into itself is).
run!(
    nested_module_of_its_own_name,
    is(Value::I64(3)),
    "/test.gx" => "mod a;\nlet result = a::x",
    "/test/a.gx" => "mod b;\nlet x = b::y",
    "/test/a/b.gx" => "mod a;\nlet y = a::z",
    "/test/a/b/a.gx" => "let z = 3"
    ; FuseExpect::None);

// The same source imported on two branches is not a cycle.
run!(
    sibling_imports_of_one_source,
    is(Value::I64(2)),
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

run!(dynamic_module_missing_import, DYNAMIC_MODULE_MISSING_IMPORT, load_refused("no `no_such_thing` in `core`"); FuseExpect::Jit);

// Sleep is pause: a loaded body whose arm sleeps resumes at the wake
// even though its source does not fire again.
const DYNAMIC_MODULE_SLEEP: &str = r#"
{
    let src = "let x = 0; x <- select x { v if v < 100 => v + 1, _ => never() }";
    let step = 0;
    step <- select step { s if s < 30 => s + 1, _ => never() };
    let x = select step < 10 || step >= 20 {
        true => {
            let status = mod foo dynamic {
                sandbox whitelist [core];
                sig { val x: i64 };
                source src
            };
            select status { error as e => never(dbg(e)), null as _ => foo::x }
        },
        false => never()
    };
    select step >= 20 { true => step ~ x, false => never() }
}
"#;

run!(dynamic_module_sleep_is_pause, DYNAMIC_MODULE_SLEEP, is(Value::I64(10)); FuseExpect::Jit);

// Refusals of a statement in the wrong place carry their site.
#[tokio::test(flavor = "current_thread")]
async fn misplaced_statements_are_placed() -> Result<()> {
    use graphix_compiler::expr::ErrorSite;
    let m = [("/m.gx", "let k = 1")];
    for text in ["let x = (use array::map)", "mod m; mod m", "let x = (type T = i64)"] {
        let e = compile_error(&m, text, crate::TEST_REGISTER).await?;
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
        let e = compile_error(&files, "{ mod test; test::result }", crate::TEST_REGISTER)
            .await?;
        assert!(format!("{e:?}").contains("in module /test/inner"), "{inner}: {e:?}");
    }
    Ok(())
}

// A source that fails to compile leaves nothing behind: the next one
// defines the same hidden type with another representation.
const DYNAMIC_MODULE_AFTER_A_FAILURE: &str = r#"
{
    let source = """type C = Abstract<string>; let make = |x: i64| C("v"); let h = missing""";
    source <- sys::time::after_idle(
        duration:10.ms,
        "type C = Abstract<i64>; let make = |x: i64| C(x); let h = make(41).0 + 1"
    );
    let status = mod m dynamic {
        sandbox whitelist [core];
        sig { type C; val make: fn(x: i64) -> C; val h: i64 };
        source source
    };
    select status {
        error as _ => never(),
        null as _ => m::h
    }
}
"#;

run!(dynamic_module_after_a_failure, DYNAMIC_MODULE_AFTER_A_FAILURE, is(Value::I64(42)));

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
    is(Value::I64(5)),
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
    is(Value::I64(1)),
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
    is(Value::I64(2)),
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

// A source that is no program reports, and the loaded body runs on: a
// write to a val in that cycle reaches it.
const SOURCE_ERROR_KEEPS_THE_BODY_RUNNING: &str = r#"
{
    let src: [string, Error<`E>] = "let x = 0; let y = x * 10";
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { val x: i64; val y: i64 };
        source src
    };
    let go = once(status);
    src <- go ~ error(`E);
    foo::x <- go ~ 5;
    select status { error as _ => foo::x + foo::y, null as _ => never() }
}
"#;

run!(source_error_keeps_the_body_running, SOURCE_ERROR_KEEPS_THE_BODY_RUNNING,
    is(Value::I64(55)); FuseExpect::Jit);

// A value bound after its `mod` is not visible to the module.
run!(
    an_import_of_a_later_binding_is_refused,
    refused("top not defined"),
    "/test.gx" => "mod a; let top = 1; let result = a::x",
    "/test/a.gx" => "use super::top;\nlet x = top"
    ; FuseExpect::None
);

// Two explicit imports of one name are refused.
run!(
    two_imports_of_one_name_are_refused,
    "{ use array::len; use str::len; len([1]) }",
    refused("`len` is already imported here (from `array`); rename one");
    FuseExpect::None
);

// An import colliding with a declaration of its scope is refused.
run!(
    an_import_colliding_with_a_declaration_is_refused,
    "{ let len = 1; use array::len; len }",
    refused("`len` is already defined in this scope; use `as` to rename");
    FuseExpect::None
);

// A `pub use` is refused.
run!(
    pub_use_is_refused,
    "{ pub use array::len; 1 }",
    refused("re-exports (`pub use`) are not yet supported");
    FuseExpect::None
);

// `super` above the root is refused.
run!(
    super_above_the_root_is_refused,
    "{ use super::super::super::x; 1 }",
    refused("`super` goes above the package root");
    FuseExpect::None
);

// An interface's `use` is a private import, never a re-export.
run!(
    an_interface_use_is_not_a_reexport,
    refused("undefined type m::U"),
    "/test.gx" => "mod ta; mod m; let q: m::U = 1; let result = m::f(q)",
    "/test/ta.gxi" => "type T = i64;",
    "/test/ta.gx" => "let unused = 0",
    "/test/m.gxi" => "use super::ta::T as U;\nval f: fn(x: U) -> U;",
    "/test/m.gx" => "let f = |x: U| -> U x"
    ; FuseExpect::None
);

// A path root consults modules only: a value named like a package
// leaves the package's paths alone.
run!(
    a_value_named_like_a_package_leaves_its_paths,
    "{ let array = 1; array::len([1, 2]) + array }",
    is(Value::I64(3))
);

// Two globs that reach one binding name it once; two that reach
// different bindings are ambiguous.
run!(
    globs_reaching_one_binding_agree,
    is(Value::I64(1)),
    "/test.gx" => r#"
        let x = 1;
        let result = {
            mod inner;
            inner::y
        }
    "#,
    "/test/inner.gx" => r#"
        use super::*;
        use super::super::test::*;
        let y = x
    "#
    ; FuseExpect::None
);

run!(
    globs_reaching_two_bindings_are_ambiguous,
    refused("`x` is ambiguous: both a block in `/test` and `/test` provide it"),
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
        use super::super::test::*;
        let y = x
    "#
    ; FuseExpect::None
);

// `use super::*` from a block-level module takes a name shadowed across
// the block chain innermost-first, as `use super::x` does.
run!(
    glob_super_takes_the_innermost,
    is(Value::I64(2)),
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
    "#; FuseExpect::None
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
    is(Value::I64(1)),
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
    "#; FuseExpect::None
);

// A loaded body's primes are its own: two modules loading in one cycle
// requeue nothing, and nothing later in the cycle fires from a prime.
async fn loads_leave_no_primes(mode: Mode) -> Result<()> {
    use super::dense_deltas::run_delta;
    let (values, _) = run_delta(
        r#"{
            let y = 1;
            let k = 0;
            k <- select k { x if x < 3 => x + 1, _ => never() };
            let src = never();
            src <- select k { 2 => "let v = super::y + 1", _ => never() };
            let r = (
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
let v = A(1);
impl super::Show for A { let show = |a| "a[a.0]" }
"#,
"/test/b.gxi" => r#"
type B = Abstract<i64>;
val v: B;
"#,
"/test/b.gx" => r#"
let v = B(2);
impl super::Show for B { let show = |b| "b[b.0]" }
"#
);

// A dynamic module compiles after the catch around it was checked: its
// raise must fit the type the catch's handler was checked at.
run!(
    dynamic_raise_fits_the_checked_catch,
    r##"{
    let r = {
        catch(e: Error<ErrChain<i64>>) never(e);
        mod foo dynamic {
            sandbox unrestricted;
            sig { val v: i64 };
            source "let v = 1; let w = error(\"hello\")?"
        }
    };
    r
}"##,
    |v: Result<&Value>| matches!(v, Ok(v @ Value::Error(_))
        if format!("{v}").contains("handler was checked to take Error<ErrChain<i64>>"));
    FuseExpect::None
);

// A declared impl taking a trait default from a sibling module resolves
// statically though the two modules check in one run of tasks.
run!(
    sibling_trait_default_resolves_statically,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "dflt"),
    "/test.gx" => r#"
        mod a;
        mod b;
        let result = #[native] a::Show::show(b::mk(1))
    "#,
    "/test/a.gxi" => r#"trait Show { val show: fn(self) -> string = |s| "dflt" };"#,
    "/test/a.gx" => "",
    "/test/b.gxi" => "use super::a::Show; type T; val mk: fn(x: i64) -> T; impl Show for T;",
    "/test/b.gx" => "use super::a::Show; type T = Abstract<i64>; let mk = |x| T(x); impl Show for T"
; FuseExpect::Jit);

// An exported val is one variable: the module's own write and an outside
// write in one cycle both land, the second a cycle later, as two writes to
// any variable do.
run!(
    exported_val_takes_both_writes,
    prints("[i64:0, i64:1, i64:100]"),
    "/test.gx" => r#"
        mod m;
        let go = 1;
        m::t <- go ~ 5;
        m::x <- m::t ~ 100;
        let result = array::group(m::x, |n, _| n == 3)
    "#,
    "/test/m.gxi" => "val x: i64; val t: i64;",
    "/test/m.gx" => "let t = never(); let x = 0; x <- t ~ 1"
; FuseExpect::None);

// A catch that saw no raise at its check takes none later: the refusal
// names the annotation that would take it.
run!(
    dynamic_raise_names_the_catch_annotation,
    r##"{
    let r = {
        catch(e) never(e);
        mod foo dynamic {
            sandbox unrestricted;
            sig { val v: i64 };
            source "let v = 1; let w = error(\"hello\")?"
        }
    };
    r
}"##,
    |v: Result<&Value>| matches!(v, Ok(v @ Value::Error(_))
        if format!("{v}").contains("saw no error when it was checked")
            && format!("{v}").contains("`catch(e: Error<ErrChain<string>>)`"));
    FuseExpect::None
);

/// The seq lowerings a program leaves once `code` settles on `last`.
async fn lowerings_kept(code: &str, last: Value) -> Result<usize> {
    use graphix_package_core::testing::{
        compile_result, fixture_runtime, result_source, updates_until_quiet,
    };
    use std::time::Duration;
    let (ctx, mut rx) = fixture_runtime(
        [("/test.gx", result_source(code))],
        &crate::TEST_REGISTER,
        Mode::Interp,
        |_| (),
    )
    .await?;
    let res = compile_result(&ctx).await?;
    let deadline = tokio::time::Instant::now() + Duration::from_secs(20);
    let values = updates_until_quiet(
        &ctx.rt,
        &mut rx,
        res.exprs[0].id,
        Duration::from_millis(500),
        deadline,
    )
    .await?;
    assert_eq!(values.last(), Some(&last), "{values:?}");
    let kept = ctx.rt.env_stats().await?.lowerings_len;
    ctx.shutdown().await;
    Ok(kept)
}

/// A dynamic module reloaded with a seq in its body keeps no lowering of
/// the bodies it replaced.
#[tokio::test(flavor = "current_thread")]
async fn reloads_keep_no_old_lowerings() -> Result<()> {
    async fn kept(reloads: i64) -> Result<usize> {
        let code = format!(
            r#"{{
                let k = 0;
                k <- select k {{ x if x < {reloads} => x + 1, _ => never() }};
                let status = mod foo dynamic {{
                    sandbox whitelist [core];
                    sig {{ val v: i64 }};
                    source "let v = seq {{ let a = [k]; a + 1 }}"
                }};
                select status {{ error as e => never(dbg(e)), null as _ => foo::v }}
            }}"#
        );
        lowerings_kept(&code, Value::I64(reloads + 1)).await
    }
    assert_eq!(kept(2).await?, kept(30).await?);
    Ok(())
}

/// An embedder that compiles and drops programs holding seqs keeps no
/// lowering of the programs it dropped.
#[tokio::test(flavor = "current_thread")]
async fn dropped_compiles_keep_no_lowerings() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    async fn kept(n: usize) -> Result<usize> {
        let (tx, _rx) = tokio::sync::mpsc::channel(1000);
        let ctx = init(tx, crate::TEST_REGISTER).await?;
        for _ in 0..n {
            let code = "(|x: i64| seq { let a = x; a + 1 })(1)";
            drop(ctx.rt.compile(ArcStr::from(code)).await?);
        }
        let kept = ctx.rt.env_stats().await?.lowerings_len;
        ctx.shutdown().await;
        Ok(kept)
    }
    assert_eq!(kept(2).await?, kept(30).await?);
    Ok(())
}

/// Instances of a function whose seqs sit in a try body and in a lambda
/// literal share one lowering of each.
#[tokio::test(flavor = "current_thread")]
async fn instances_share_their_lowerings() -> Result<()> {
    async fn kept(n: i64) -> Result<usize> {
        let code = format!(
            r#"{{
                let f = |x: i64| {{
                    let g = |y: i64| seq {{ let a = y; a + 1 }};
                    seq {{
                        try {{ let b = [g(x)][0]?; seq {{ let c = b; c }} }} with(e) {{ 0 }}
                    }}
                }};
                array::fold(array::init({n}, f), 0, |acc, v| acc + v)
            }}"#
        );
        lowerings_kept(&code, Value::I64(n * (n + 1) / 2)).await
    }
    assert_eq!(kept(2).await?, kept(30).await?);
    Ok(())
}

// A reload may not change a hidden representation through a private
// typedef either: the old and the new `R` are compared by what they mean.
const RELOAD_CHANGES_A_REPRESENTATION_THROUGH_A_TYPEDEF: &str = r#"
{
    let source = """type R = {x: string}; type C = Abstract<R>; let make = |x: i64| C({x: "v"})""";
    sys::net::publish("/local/reprchange_td", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { type C; val make: fn(x: i64) -> C };
        source sys::net::subscribe("/local/reprchange_td")?
    };
    source <- once(status) ~ """type R = {x: i64}; type C = Abstract<R>; let make = |x: i64| C({x: x})""";
    select status { error as e => e, null as _ => never() }
}
"#;

run!(reload_changes_a_representation_through_a_typedef,
    RELOAD_CHANGES_A_REPRESENTATION_THROUGH_A_TYPEDEF,
    load_refused("representation cannot change"); FuseExpect::Jit);

// Of several modules that fail to resolve, the first in program order is
// the one reported.
run!(
    the_first_unresolved_module_is_reported,
    refused("nosuch_a could not be found"),
    "/test.gx" => "mod nosuch_a; mod nosuch_b; mod nosuch_c; let result = 0"
; FuseExpect::None);

// A module body's last statement is no value, but a catch there still
// covers nothing.
run!(
    module_ending_in_catch_refused,
    refused("it would cover nothing"),
    "/test.gx" => r#"
        mod m;
        m::x
    "#,
    "/test/m.gx" => r#"
        let x = 1;
        catch(e) println("caught")
    "#; FuseExpect::None);

// A destructuring let's sibling first written from outside its module
// leaves the siblings' group: each write of `a` fires `a` alone, and an
// arm reading both never catches `b` up.
run!(
    sibling_written_from_outside_its_module_fires_alone,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string().ends_with(", i64:1]")),
    "/test.gx" => r#"
        mod m;
        let n = 0;
        n <- select n { k if k < 6 => (k ~ n) + 1, _ => never() };
        m::a <- n ~ m::a + 10;
        let sel = select n { 2 => false, 3 => false, _ => true };
        let out = select sel { true => (m::a, count(m::b)), false => (-1, -1) };
        let result = select n { 6 => out, _ => never() }
    "#,
    "/test/m.gx" => r#"
        let (a, b) = (1, 2)
    "#; FuseExpect::Jit);

// A sibling a later REPL input first writes, while the arm reading both
// siblings sleeps, leaves the group in the live select too: the wake
// catches `a` up and not `b`.
#[tokio::test(flavor = "current_thread")]
async fn sibling_first_written_by_a_later_input_fires_alone() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::{data, init};
    use graphix_rt::GXEvent;
    use tokio::{sync::mpsc, time};
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let probe = ctx
        .rt
        .compile(ArcStr::from(
            "let (a, b) = (1, 2);
             let gate = true;
             select gate { true => (a, count(b)), false => (-1, -1) }",
        ))
        .await?;
    let id = probe.exprs.last().expect("the select").id;
    let mut held = vec![];
    let deadline = time::Instant::now() + time::Duration::from_secs(10);
    let mut next = async || -> Result<String> {
        loop {
            let batch = time::timeout_at(deadline, rx.recv()).await;
            let Ok(Some(batch)) = batch else { anyhow::bail!("the probe went quiet") };
            for ev in batch.iter() {
                if let GXEvent::Updated(eid, v) = ev
                    && *eid == id
                {
                    return Ok(data(v.clone()).to_string());
                }
            }
        }
    };
    assert_eq!(next().await?, "[i64:1, i64:1]");
    held.push(ctx.rt.compile(ArcStr::from("gate <- false")).await?);
    assert_eq!(next().await?, "[i64:-1, i64:-1]");
    held.push(ctx.rt.compile(ArcStr::from("a <- 5")).await?);
    time::sleep(time::Duration::from_millis(200)).await;
    held.push(ctx.rt.compile(ArcStr::from("gate <- true")).await?);
    assert_eq!(next().await?, "[i64:5, i64:1]");
    Ok(())
}

// A module with an interface raises to an ascribed catch outside it: the
// ascription covers the raise, so the module's check writes nothing.
run!(
    module_raises_to_an_ascribed_outer_catch,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:2, i64:0]"),
    "/test.gx" => r#"
        let n = 0;
        catch(e: Error<ErrChain<`E>>) n <- e ~ n + 1;
        mod m;
        let result = (m::y, n)
    "#,
    "/test/m.gxi" => r#"
        val y: i64;
    "#,
    "/test/m.gx" => r#"
        let f = |x: i64| -> i64 select x { 1 => error(`E)?, x => x };
        let y = f(2)
    "#; FuseExpect::Jit);

// An interface typedef naming a trait-typed parameter serves its
// implementation.
run!(
    interface_typedef_with_a_trait_parameter,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "3"),
    "/test.gx" => r#"
        mod m;
        let result = m::apply(|x| "[x]", 3)
    "#,
    "/test/m.gxi" => r#"
        type F = fn(x: Display) -> string;
        val apply: fn(f: F, x: i64) -> string
    "#,
    "/test/m.gx" => r#"
        let apply = |f: F, x: i64| -> string f(x)
    "#; FuseExpect::None);

// A predicate bound on a typedef parameter reaches the open members of a
// union argument on the interface's side as on the implementation's.
run!(
    interface_typedef_bound_reaches_open_members,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(2))),
    "/test.gx" => r#"
        mod m;
        let result = m::f([1, 2])
    "#,
    "/test/m.gxi" => r#"
        type T<'a: Discernible> = Array<'a>;
        val f: fn(x: T<[i64, 'b]>) -> i64
    "#,
    "/test/m.gx" => r#"
        let f = |x: T<[i64, 'b]>| -> i64 array::len(x)
    "#; FuseExpect::Jit);

// The same with the trait-typed parameter inside an abstract's
// representation.
run!(
    interface_abstract_typedef_with_a_trait_parameter,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "3"),
    "/test.gx" => r#"
        mod m;
        let result = m::apply(3)
    "#,
    "/test/m.gxi" => r#"
        type F = Abstract<fn(x: Display) -> string>;
        val apply: fn(x: i64) -> string
    "#,
    "/test/m.gx" => r#"
        let apply = |x: i64| -> string {
            let f = F(|y| "[y]");
            let g = f.0;
            g(x)
        }
    "#; FuseExpect::None);

// A dynamic module whose source goes bottom keeps its loaded body
// running: a write to an interface val reaches the body that cycle.
run!(dynamic_module_runs_on_under_a_bottom_source, r#"{
  let step = 0;
  step <- select step { k if k < 3 => (k ~ step) + 1, _ => never() };
  let src0: [string, Error<`E>] = "let x = 0; let y = x * 10";
  let status = mod foo dynamic {
    sandbox whitelist [core];
    sig { val x: i64; val y: i64 };
    source src0$
  };
  src0 <- select step { 1 => step ~ error(`E), _ => never() };
  foo::x <- select step { 1 => step ~ 5, _ => never() };
  select step { 3 => foo::y, _ => never() }
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(50))); FuseExpect::Jit);

// Two dynamic modules named alike in two blocks are two modules: each one's
// abstract type is its own, keyed by where its `mod` statement is written.
const TWO_BLOCK_MODULES: &str = r#"
{
  let a = {
    let s = mod m dynamic {
      sandbox unrestricted;
      sig { type T; val mk: fn(x: i64) -> T; val get: fn(t: T) -> i64 };
      source "type T = Abstract<i64>; let mk = |x: i64| -> T T(x); let get = |t: T| -> i64 t.0"
    };
    select s { error as e => never(e), null as _ => m::get(m::mk(41)) }
  };
  let b = {
    let s = mod m dynamic {
      sandbox unrestricted;
      sig { type T; val mk: fn(x: string) -> T; val get: fn(t: T) -> string };
      source "type T = Abstract<string>; let mk = |x: string| -> T T(x); let get = |t: T| -> string t.0"
    };
    select s { error as e => never(e), null as _ => m::get(m::mk("hello")) }
  };
  a + str::len(b)
}
"#;
run!(two_block_modules, TWO_BLOCK_MODULES, is(Value::I64(46)); FuseExpect::Jit);

const TWO_BLOCK_MODULES_CROSSED: &str = r#"
{
  let a = {
    let s = mod m dynamic {
      sandbox unrestricted;
      sig { type T; val mk: fn(x: i64) -> T; val get: fn(t: T) -> i64 };
      source "type T = Abstract<i64>; let mk = |x: i64| -> T T(x); let get = |t: T| -> i64 t.0"
    };
    select s { error as e => never(e), null as _ => m::mk(41) }
  };
  let s = mod m dynamic {
    sandbox unrestricted;
    sig { type T; val mk: fn(x: i64) -> T; val get: fn(t: T) -> i64 };
    source "type T = Abstract<i64>; let mk = |x: i64| -> T T(x); let get = |t: T| -> i64 t.0 + 1"
  };
  select s { error as e => never(e), null as _ => m::get(a) }
}
"#;
run!(two_block_modules_crossed, TWO_BLOCK_MODULES_CROSSED, refused("type mismatch T does not contain T"); FuseExpect::None);
