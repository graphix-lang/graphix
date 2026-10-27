// Fixtures over builtins only a test registers: Rust that panics, a
// function value retyped past the checker, and a builtin's quantifier no
// argument can carry a value of; and `test_note`, which other fixtures
// register to watch what runs.

use super::dense_deltas::run_delta_with;
use anyhow::Result;
use graphix_compiler::{Effect, ExecCtx, Rt, UserEvent};
use graphix_package_core::{
    CachedArgs, CachedVals, EvalCached, fast_builtin,
    testing::{Mode, TestState},
};
use netidx_value::Value;
use parking_lot::Mutex;
use std::{
    sync::atomic::{AtomicI64, Ordering},
    time::Duration,
};

fn fc_panic_at_13(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::I64(13) => panic!("thirteen"),
        v => Some(v.clone()),
    }
}

fast_builtin!(PanicAt13, PanicAt13Ev, "test_panic_at_13", fc_panic_at_13);

/// The second argument of each `test_panic_at_13_keyed` call that
/// panicked, so a fixture counts its own.
static PANICKED: Mutex<Vec<i64>> = Mutex::new(Vec::new());

fn fc_panic_at_13_keyed(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::I64(13), Value::I64(key)) => {
            PANICKED.lock().push(*key);
            panic!("thirteen")
        }
        (v, _) => Some(v.clone()),
    }
}

fast_builtin!(
    PanicAt13Keyed,
    PanicAt13KeyedEv,
    "test_panic_at_13_keyed",
    fc_panic_at_13_keyed
);

/// The lines `test_note` took, shared with the test through the
/// context's libstate.
#[derive(Debug, Clone, Default)]
pub(super) struct Notes(triomphe::Arc<parking_lot::Mutex<Vec<String>>>);

impl Notes {
    /// Register `test_note` on `ctx`, writing here.
    pub(super) fn install(&self, ctx: &mut TestState) {
        ctx.libstate.set(self.clone());
        ctx.register_builtin::<CachedArgs<Note>>().unwrap();
    }

    pub(super) fn take(&self) -> Vec<String> {
        std::mem::take(&mut *self.0.lock())
    }
}

/// `test_note(s)` records `s` and returns it. It declares itself
/// stateless, which it is to the program, so a `#[pure]` body can hold
/// it: a test sees how often that body ran.
#[derive(Debug, Default)]
pub(super) struct Note;

graphix_package_core::unit_image_state!(Note);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for Note {
    const NAME: &str = "test_note";
    const EFFECT: Effect = Effect::Stateless(None);

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        let v = from.value(0)?;
        if let (Some(notes), Value::String(s)) = (ctx.libstate.get::<Notes>(), v) {
            notes.0.lock().push(s.to_string());
        }
        Some(v.clone())
    }
}

fn fc_identity(args: &[Value]) -> Option<Value> {
    Some(args[0].clone())
}

fast_builtin!(Identity, IdentityEv, "test_identity", fc_identity);

fn fc_last(args: &[Value]) -> Option<Value> {
    args.last().cloned()
}

fast_builtin!(Last, LastEv, "test_last", fc_last);

async fn run(code: &str, mode: Mode) -> Result<Vec<Value>> {
    let setup = |ctx: &mut TestState| {
        ctx.register_builtin::<PanicAt13>().unwrap();
        ctx.register_builtin::<PanicAt13Keyed>().unwrap();
        ctx.register_builtin::<Identity>().unwrap();
        ctx.register_builtin::<Last>().unwrap();
    };
    Ok(run_delta_with(code, mode, setup, Duration::from_millis(700)).await?.values)
}

/// Each program panics at n = 13 inside `#[native]`: the runtime dies
/// as the node-walk's unwind kills it, and the process lives.
async fn rust_panics_in_a_kernel(mode: Mode) -> Result<()> {
    let head = r#"
        let boom = |x: i64| -> i64 'test_panic_at_13;
        type K = Abstract<i64>;
        impl Eq for K { let eq = |a, b| boom(a.0) == boom(b.0) };
        impl Ord for K {
            let cmp = |a, b| select boom(a.0) < boom(b.0) { true => `Less, false => `Greater }
        };
        impl Display for K { let fmt = |k| "[boom(k.0)]" };
        let n = 10;
        n <- select n { k if k < 20 => k + 1, _ => never() };"#;
    for body in [
        "#[native] (boom(n) + 1)",
        "#[native] ([K(n)] == [K(1)])",
        "#[native] map::len({K(n) => 1, K(1) => 2})",
        "#[native] str::len(\"[K(n)]\")",
    ] {
        let r = run(&format!("{{ {head} {body} }}"), mode).await;
        match r {
            Err(e) if e.to_string().contains("the runtime died") => (),
            r => panic!("{body}: {r:?}"),
        }
    }
    Ok(())
}

/// A kernel loop or self-call goes no further once a value hook it ran
/// panicked: the hook's placeholder answer ("not equal") would loop on
/// to the bound, and the run leaves at the next poll instead.
async fn a_panic_stops_a_kernel_loop(mode: Mode) -> Result<()> {
    static KEYS: AtomicI64 = AtomicI64::new(0);
    for call in ["go(i + 1)", "go(i + 1) + 0"] {
        let key = KEYS.fetch_add(1, Ordering::Relaxed);
        let code = format!(
            r#"{{
                let boom = |x: i64, k: i64| -> i64 'test_panic_at_13_keyed;
                type K = Abstract<i64>;
                impl Eq for K {{ let eq = |a, b| boom(a.0, {key}) == boom(b.0, {key}) }};
                let rec go = |i: i64| -> i64 select i >= 1000 {{
                    true => i,
                    false => select K(i) == K(13) {{ true => i, false => {call} }}
                }};
                let n = 10;
                #[native] go(n)
            }}"#
        );
        match run(&code, mode).await {
            Err(e) if e.to_string().contains("the runtime died") => (),
            r => panic!("{call}: {r:?}"),
        }
        // 990 at the bound; a loop polls every few dozen iterations
        let panics = PANICKED.lock().iter().filter(|k| **k == key).count();
        assert!((1..100).contains(&panics), "{call}: {panics} panics");
    }
    Ok(())
}

/// A function value whose type the checker was told wrong reaches a
/// dynamic call site: the site refuses the instance and never runs it.
async fn mistyped_function_value_is_refused(mode: Mode) -> Result<()> {
    let code = |f: &str| {
        format!(
            r#"{{
                let launder = |f: Any| -> fn(x: f64) -> f64 'test_identity;
                let g = launder({f});
                g(1.5)
            }}"#
        )
    };
    let good = run(&code("|x: f64| x + 1.0"), mode).await?;
    assert_eq!(good, [Value::F64(2.5)]);
    let bad = run(&code("|x: i64| x + 1"), mode).await?;
    assert_eq!(bad, []);
    Ok(())
}

/// A builtin's `'a: Concrete` that only a handler the call does not pass
/// holds is never read: the call is accepted with the handler omitted or
/// null, and refused where a value of `'a` can arrive and nothing settles
/// it (a handler whose argument is left open) or the result holds it.
async fn unread_quantifier_settles(mode: Mode) -> Result<()> {
    let code = |ret: &str, call: &str| {
        format!(
            r#"{{
                let f = 'a: Concrete |#h: [fn(v: 'a) -> i64, null] = null, x: i64|
                    -> {ret} 'test_last;
                {call}
            }}"#
        )
    };
    assert_eq!(run(&code("i64", "f(3)"), mode).await?, [Value::I64(3)]);
    assert_eq!(run(&code("i64", "f(#h: null, 3)"), mode).await?, [Value::I64(3)]);
    for (ret, call) in [
        ("i64", "f(#h: |v| 0, 3)"),
        ("['a, i64]", "f(3)"),
        (
            "i64",
            "{ let g = 'b: Concrete |#h: [fn(v: 'b) -> i64, null] = null, x: i64| x; g(3) }",
        ),
    ] {
        match run(&code(ret, call), mode).await {
            Err(e) if format!("{e:#}").contains("must be fully known") => (),
            r => panic!("{ret} {call}: {r:?}"),
        }
    }
    Ok(())
}

modes!(
    rust_panics_in_a_kernel,
    a_panic_stops_a_kernel_loop,
    mistyped_function_value_is_refused,
    unread_quantifier_settles
);
