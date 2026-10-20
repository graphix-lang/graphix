use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, refused},
};
use netidx_value::Value;

// A callee first dispatched late, under an arm, prints its constant at
// that dispatch, once. The arm node-walks: its callee's print does not
// follow the arm's entry, which a kernel could not re-fire at a re-entry.
#[tokio::test]
async fn late_callee_dispatch_primes_constants() -> Result<()> {
    use arcstr::literal;
    use graphix_package_core::{
        PrintSink,
        testing::{Mode, fixture_runtime, updates_until_quiet},
    };
    use std::time::Duration;
    use tokio::time::Instant;

    for mode in Mode::ALL {
        let sink = PrintSink::default();
        let (ctx, mut rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |ctx| {
            ctx.libstate.set(sink.clone());
        })
        .await?;
        let result = ctx
            .rt
            .compile(literal!(
                r#"{
                    let k = 0;
                    k <- select k < 6 { true => k + 1, false => never() };
                    let g = |n: i64| -> i64 { println("first"); n };
                    let f = |s: i64| -> i64 select s { 0 => 0, n => g(n) };
                    f(select k < 3 { true => 0, false => k })
                }"#
            ))
            .await?;
        let values = updates_until_quiet(
            &ctx.rt,
            &mut rx,
            result.exprs[0].id,
            Duration::from_millis(100),
            Instant::now() + Duration::from_secs(5),
        )
        .await?;
        assert_eq!(values, [0, 0, 0, 3, 4, 5, 6].map(Value::I64), "{mode:?}");
        assert_eq!(sink.take(), "first\n", "{mode:?}");
        ctx.shutdown().await;
    }
    Ok(())
}

#[tokio::test]
async fn deleted_kernel_releases_activation_budget() -> Result<()> {
    use arcstr::literal;
    use graphix_package_core::testing::{Mode, fixture_runtime, next_update};
    use std::time::Duration;
    use tokio::time::Instant;

    for mode in Mode::ALL {
        let (ctx, mut rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |ctx| {
            ctx.control.set_activation_budget(300);
        })
        .await?;
        let definitions = ctx
            .rt
            .compile(literal!(
                r#"
            let g = |x: i64| x + 1;
            let rec f = |n: i64| -> i64 select n < 1 {
                true => 0,
                false => f(n - 1) + g(n)
            }
        "#
            ))
            .await?;
        let base = ctx.rt.control().live_activations();
        for _ in 0..8 {
            let result = ctx.rt.compile(literal!("#[native] f(50)")).await?;
            let id = result.exprs.last().unwrap().id;
            let value =
                next_update(&mut rx, id, Instant::now() + Duration::from_secs(5)).await?;
            assert_eq!(value, Value::I64(1325), "{mode:?}");
            if !mode.node_walk() {
                assert!(ctx.rt.control().live_self_blocks() >= 50, "vacuous kernel test");
            }
            drop(result);
            let (blocks, activations) = ctx
                .rt
                .with_ctx(|ctx| {
                    (ctx.control.live_self_blocks(), ctx.control.live_activations())
                })
                .await?;
            assert_eq!(blocks, 0, "{mode:?}");
            assert_eq!(activations, base, "{mode:?}");
        }
        drop(definitions);
        ctx.shutdown().await;
    }
    Ok(())
}

run!(outlined_loop_stack_space, {
    use std::fmt::Write;
    let mut src = poolshark::local::LPooled::<String>::take();
    src.push_str("{ let f = |a0: Array<i64>| {\n");
    for i in 1..=500 {
        writeln!(src, "let a{i} = array::map(a{}, |x| x + 1);", i - 1).unwrap();
    }
    src.push_str("a500 }; #[native] f([0]) }");
    arcstr::ArcStr::from(src.as_str())
}, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if **a == [Value::I64(500)]); FuseExpect::Jit);

run!(inferred_guard_masks_unmatched_bind, r#"
{
    let errors = 0;
    catch(e) errors <- e ~ errors + 1;
    let f = |v: [i64, null]| select v {
        null as _ => 0,
        x if (100 /? x)? > 1 => 1,
        _ => 2
    };
    let n: [i64, null] = null;
    let selected = #[native] f(n);
    let variant = |v: [`A(i64), `B(i64)]| select v {
        `A(_) => 0,
        x if cast<i64>(x)? > 1 => 1,
        _ => 2
    };
    let selected_variant = #[native] variant(`A(0));
    sys::time::after_idle(duration:10.ms, (selected, selected_variant, errors))
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:0, i64:0, i64:0]"); FuseExpect::Jit);

run!(decimal_guard_bottom_is_undecided, r#"
{
    let g = |v: decimal| select v {
        x if x * x > decimal:1.0 => 1,
        _ => 2
    };
    any(#[native] g(decimal:79228162514264337593543950335.0), 7)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(7))); FuseExpect::Jit);

run!(variant_payload_literal_tags, r#"
{
    let count = |v: [`Count([i64, null]), `Off]| select v {
        `Count(0) => 0,
        `Count(_) => 1,
        `Off => 2
    };
    let flag = |v: `Flag([bool, string])| select v {
        `Flag(false) => 3,
        `Flag(_) => 4
    };
    let nested = |v: `Outer(`Inner([i32, null]))| select v {
        `Outer(`Inner(i32:0)) => 5,
        `Outer(`Inner(_)) => 6
    };
    #[native] (count(`Count(null)), count(`Count(0)), count(`Count(5)),
        flag(`Flag("hello")), flag(`Flag(false)),
        nested(`Outer(`Inner(null))), nested(`Outer(`Inner(i32:0))))
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:1, i64:0, i64:1, i64:4, i64:3, i64:6, i64:5]"); FuseExpect::Jit);

run!(select_null_literal, r#"
{
    let f = |v: [i64, null]| select v { null => 0, n => n + 1 };
    let g = |v: [i64, string, null]| select v { null => 2, _ => 3 };
    #[native] (f(null), f(5), g(null), g("present"))
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:0, i64:6, i64:2, i64:3]"); FuseExpect::Jit);

run!(select_named_numeric_predicates, r#"
{
    let f = |v: [i64, f64]| select v { Int as _ => 0, _ => 1 };
    let g = |v: [i64, f64, decimal, string]| select v { Number as _ => 2, _ => 3 };
    #[native] (f(7), f(2.5), g(7), g(2.5), g(decimal:1.5), g("text"))
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:0, i64:1, i64:2, i64:2, i64:2, i64:3]"); FuseExpect::Jit);

run!(select_variant_composite_leaves, r#"
{
    let f = |m: [`Idle, `Busy(i64)], n: i64| select (m, n) {
        (`Idle, 0) => 1,
        (`Busy(x), y) => x + y,
        (_, _) => 0
    };
    let g = |m: [`Idle, `Busy(i64)]| select {mode: m, step: 0} {
        {mode: `Busy(x), step: _} => x,
        _ => 0
    };
    #[native] (f(`Idle, 0), f(`Idle, 1), f(`Busy(5), 7), g(`Busy(9)), g(`Idle))
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:1, i64:0, i64:12, i64:9, i64:0]"); FuseExpect::Jit);

run!(select_variant_composite_whole_bind, r#"
{
    let f = |m: [`No, `Some(string)]| select (m, 0) {
        (v@ `Some(s), _) => (select v { `Some(t) => t }) == s,
        _ => false
    };
    #[native] (f(`Some("payload")), f(`No))
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if **a == [Value::Bool(true), Value::Bool(false)]); FuseExpect::Jit);

// A chain of distinct functions nests kernel frames as deep as the
// program is long; each call checks the stack as a self-call does.
run!(cross_kernel_chain_stack_space, {
    use std::fmt::Write;
    const N: usize = 500;
    let mut src = poolshark::local::LPooled::<String>::take();
    src.push_str("{\n");
    for i in 0..N {
        writeln!(src, "let v{i} = {i};").unwrap();
    }
    writeln!(src, "let f{N} = |x: i64| -> i64 x;").unwrap();
    for i in (0..N).rev() {
        writeln!(src, "let f{i} = |x: i64| -> i64 f{}(x) + v{i};", i + 1).unwrap();
    }
    src.push_str("#[native] f0(1) }");
    arcstr::ArcStr::from(src.as_str())
}, |v: Result<&Value>| matches!(v, Ok(Value::I64(124751))); FuseExpect::Jit);

// A value-shaped production, bottom included, entering a composite
// slot: an argument, a composite return, a tail loop's rebind and a
// fold's accumulator.
run!(value_into_composite_slot, r#"
{
    let f = |a: Array<[i64, string]>| -> i64 array::len(a);
    let g = |k: i64| f(select k { 0 => [1], 1 => ["x", "y"], _ => never() });
    let h = |k: i64| -> Array<[i64, string]> select k { 0 => [1], 1 => ["x", "y"], _ => never() };
    let rec t = |a: Array<[i64, string]>, k: i64| -> i64 select k {
        0 => array::len(a),
        k => t(select 6 / (k - 3) { 1 => [1], _ => ["x", "y"] }, k - 1)
    };
    let fl = |k: i64| {
        let init: Array<[i64, string]> = [];
        array::len(array::fold([k, k], init, |acc, x| select x { 0 => [1], 1 => ["x", "y"], _ => never() }))
    };
    (
        any(#[native] g(2), 7),
        any(#[native] array::len(h(2)), 7),
        #[native] t([], 4),
        any(#[native] fl(2), 7),
        #[native] (g(0), g(1), array::len(h(1)), t([], 2), fl(1))
    )
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:7, i64:7, i64:2, i64:7, [i64:1, i64:2, i64:2, i64:2, i64:2]]"); FuseExpect::Jit);

// A kernel evaluates a call's arguments in the order they are written,
// as the node-walk does, so their raises reach the handler in that order.
run!(labeled_args_raise_in_written_order, r#"
{
    let f = |#a: i64, #b: i64, c: i64| a + b + c;
    let log: Array<string> = [];
    let n = 1;
    let r = {
        catch(e) log <- e ~ array::push(log, select "[e]" {
            s if str::contains(#part: "remainder", s) => "b",
            _ => "a"
        });
        #[native] f(#b: (n %? 0)?, #a: (n /? 0)?, n)
    };
    sys::time::after_idle(duration:10.ms, log)
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == r#"["b", "a"]"#); FuseExpect::Jit);

// An argument that raises through a call with no static target is not
// fed to a kernel ahead of an earlier argument that raises in it.
run!(fed_arg_raising_through_a_dynamic_call, r#"
{
    let f = |a: i64, b: i64| a + b;
    let n = 1;
    let g = select n {
        0 => |x: i64| -> i64 (x %? 0)?,
        _ => |x: i64| -> i64 (x %? 0)?
    };
    let log: Array<string> = [];
    let r = {
        catch(e) log <- e ~ array::push(log, select "[e]" {
            s if str::contains(#part: "remainder", s) => "fed",
            _ => "fused"
        });
        f((n /? 0)?, count(g(n)))
    };
    sys::time::after_idle(duration:10.ms, log)
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == r#"["fused", "fed"]"#); FuseExpect::Jit);

// Select shapes that fuse: a literal of any type or a bind-free
// or-pattern at an element or payload position (t*), a composite pattern
// over an option of a composite (o*), an or-arm whose shared name binds
// at different kinds (w*), a type test of the scrutinee's own type (y*).
run!(select_leaves_options_or_kinds_and_own_type_tests, r#"
{
  let t0 = |a: (string, i64)| -> i64 #[native] select a { ("a", n) => n, _ => 0 };
  let t1 = |a: Array<string>| -> i64 #[native] select a { ["a", ..] => 1, _ => 0 };
  let t2 = |a: Array<[i64, null]>| -> i64 #[native] select a { [1, ..] => 5, [null, ..] => 6, _ => 0 };
  let t3 = |v: (bool, [`A, `B])| -> i64 #[native] select v { (true, `A | `B) => 1, (false, _) => 2 };
  let t4 = |a: Array<i64>| #[native] select a { [x, 1 | 2] => x, _ => 0 };
  let t5 = |v: [`A(i64), `B]| #[native] select v { `A(1 | 2) => "y", _ => "n" };
  let t6 = |s: [string, null], n: i64| #[native] select (s, n) { (null, _) => 0, (_, m) => m };
  let t7 = |s: [i64, null], n: i64| #[native] select (s, n) { (0, _) => 0, (_, m) => m };
  let t8 = |v: [`A([i64, null]), `B]| #[native] select v { `A(null) => "null", `A(_) => "some", `B => "b" };
  let o0 = |a: [Array<string>, null]| #[native] select a {
      null as _ => -1,
      [s, rest..] => str::len(s) + array::len(rest),
      [] => 0
  };
  let o1 = |t: [(i64, i64), null]| #[native] select t { null as _ => 0, (x, y) => x + y };
  let o2 = |v: [{x: i64, y: i64}, null]| #[native] select v { null as _ => -1, {x, y} => x + y };
  let o3 = |v: [(i64, i64), null]| #[native] select v { (i64, i64) as (x, y) => x * y, null as _ => -2 };
  let w0 = |v: [`A(i64), `B(f64), `C]| #[native] select v { `A(x) | `B(x) => "[x]", `C => "c" };
  let w1 = |v: [`A(i64), `B(string), `C]| #[native] select v { `A(x) | `B(x) => "[x]", `C => "c" };
  let y0 = |a: Array<i64>| #[native] select a { Array<i64> as [h, ..] => h, _ => 0 };
  let y1 = |t: (i64, string)| #[native] select t { (i64, string) as (n, _) => n };
  let y2 = |s: string| #[native] select s { string as x => str::len(x) };
  (
    (t0(("a", 2)), t0(("b", 2)), t1(["a"]), t1(["b"]), t2([1]), t2([null]), t2([2])),
    (t3((true, `B)), t3((false, `A)), t4([3, 2]), t4([3, 4]), t5(`A(2)), t5(`A(3)), t5(`B)),
    (t6(null, 2), t6("x", 3), t7(0, 4), t7(null, 4), t7(5, 6), t8(`A(null)), t8(`A(1)), t8(`B)),
    (o0(null), o0(["ab", "c"]), o0([]), o1(null), o1((1, 2)), o2({x: 3, y: 4}), o2(null), o3((2, 5)), o3(null)),
    (w0(`A(1)), w0(`B(2.5)), w1(`B("s")), w1(`A(4)), w0(`C)),
    (y0([7]), y0([]), y1((2, "x")), y2("abc"))
  )
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == r#"[[i64:2, i64:0, i64:1, i64:0, i64:5, i64:6, i64:0], [i64:1, i64:2, i64:3, i64:0, "y", "n", "n"], [i64:0, i64:3, i64:0, i64:4, i64:6, "null", "some", "b"], [i64:-1, i64:3, i64:0, i64:0, i64:3, i64:7, i64:-1, i64:10, i64:-2], ["1", "2.5", "s", "4", "c"], [i64:7, i64:0, i64:2, i64:3]]"#); FuseExpect::Jit);

// A narrow MIN / -1 in a kernel is bottom at its own width's minimum, as
// in the node-walk; a check against i64::MIN alone would trap.
run!(narrow_min_div_minus_one_is_bottom, r#"
{
    let d8 = |a: i8, b: i8| a / b;
    let d16 = |a: i16, b: i16| a / b;
    let d32 = |a: i32, b: i32| a / b;
    (
        any(#[native] d8(i8:-128, i8:-1), i8:7),
        any(#[native] d16(i16:-32768, i16:-1), i16:7),
        any(#[native] d32(i32:-2147483648, i32:-1), i32:7),
        #[native] d8(i8:-128, i8:2)
    )
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i8:7, i16:7, i32:7, i8:-64]"); FuseExpect::Jit);

// A collection callback whose formal destructures past a flat tuple has
// no fused loop, and a #[native] says which formal, not an internal name.
run!(destructured_callback_formal_is_named, r#"
{
    let rows = [{a: 1, b: 2}, {a: 3, b: 4}];
    #[native] array::map(rows, |{a, b}| a + b)
}
"#, refused("parameter 1 destructures past a flat tuple"); FuseExpect::None; jit_only);

// A native tail loop polls the interrupt every 64 iterations, its first
// included: a long one still runs to its answer.
run!(native_tail_loop_polls_by_count, r#"
{
    let rec sum = |n: i64, acc: i64| -> i64 select n { 0 => acc, n => sum(n - 1, acc + n) };
    #[native] sum(100000, 0)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(5000050000))); FuseExpect::Jit);

// Checked integer arithmetic fuses inline and fails as the node-walk
// does, at every width.
run!(checked_int_arith_inline, r#"
{
    let ck = |a: i8, b: i8| (a +? b, a -? b, a *? b, a /? b, a %? b);
    let ck16 = |a: u16, b: u16| (a +? b, a -? b, a *? b, a /? b, a %? b);
    let ck64 = |a: i64, b: i64| (a +? b, a -? b, a *? b, a /? b, a %? b);
    let cku64 = |a: u64, b: u64| (a +? b, a -? b, a *? b, a /? b, a %? b);
    (
        #[native] ck(i8:100, i8:27), #[native] ck(i8:-128, i8:-1),
        #[native] ck16(u16:65535, u16:2), #[native] ck64(9223372036854775807, 1),
        #[native] ck64(3037000500, 3037000500), #[native] cku64(u64:1, u64:2),
        #[native] cku64(u64:9, u64:0)
    )
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == CHECKED_INT_ARITH); FuseExpect::Jit);

const CHECKED_INT_ARITH: &str = r#"[[i8:127, i8:73, error:["ArithError", "attempt to multiply with overflow"], i8:3, i8:19], [error:["ArithError", "attempt to add with overflow"], i8:-127, error:["ArithError", "attempt to multiply with overflow"], error:["ArithError", "attempt to divide with overflow"], i8:0], [error:["ArithError", "attempt to add with overflow"], u16:65533, error:["ArithError", "attempt to multiply with overflow"], u16:32767, u16:1], [error:["ArithError", "attempt to add with overflow"], i64:9223372036854775806, i64:9223372036854775807, i64:9223372036854775807, i64:0], [i64:6074001000, i64:0, error:["ArithError", "attempt to multiply with overflow"], i64:1, i64:0], [u64:3, error:["ArithError", "attempt to subtract with overflow"], u64:2, u64:0, u64:1], [u64:9, u64:9, u64:0, error:["ArithError", "attempt to divide by zero"], error:["ArithError", "attempt to calculate the remainder with a divisor of zero"]]]"#;
