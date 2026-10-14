use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx_value::Value;

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
