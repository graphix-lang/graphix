use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx_value::Value;

const TEST_REGISTER: &[&dyn graphix_package::Package<graphix_rt::NoExt>] =
    graphix_package::package_refs!();

run!(queuefn_banks_idle_permits, r#"{
    let phase = 0;
    phase <- select phase { n if n < 6 => n + 1, _ => never() };
    let trigger = select phase { n if n < 6 => n, _ => never() };
    let depth = 0;
    let q = queuefn(#trigger: trigger, #count: &mut depth, |x: i64| -> i64 x);
    let call = 0;
    call <- select (phase, call) { (6, n) if n < 7 => n + 1, _ => never() };
    let r = q(select call { 0 => never(), n => n });
    let answers: Array<i64> = [];
    answers <- r ~ array::push(answers, r);
    sys::time::after_idle(duration:10.ms, (answers, depth, phase))
}"#, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) if a.len() == 3 => match (&a[0], &a[1], &a[2]) {
        (Value::Array(got), Value::I64(1), Value::I64(6)) =>
            got.len() == 6 && got.iter().zip(1..=6).all(|(v, i)| *v == Value::I64(i)),
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit);
