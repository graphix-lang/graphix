use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx_value::Value;

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
