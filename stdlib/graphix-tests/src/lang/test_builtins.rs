// Fixtures over builtins only a test registers: Rust that panics, a
// function value retyped past the checker, a builtin's quantifier no
// argument can carry a value of, and an effect queued at every walk; and
// `test_note`, which other fixtures register to watch what runs.

use super::dense_deltas::run_delta_with;
use anyhow::Result;
use compact_str::{CompactString, format_compact};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, Effect, ExecCtx, Node, PruneMode, Rt, Scope,
    TagValue, expr::ExprId, image::ImageBuf, typ::FnType,
};
use graphix_package_core::{
    CachedArgs, CachedArgsAsync, CachedVals, EvalCached, EvalCachedAsync, fast_builtin,
    testing::{Mode, TestState},
};
use netidx_core::pack::PackError;
use netidx_value::Value;
use parking_lot::Mutex;
use std::{
    process::Command,
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

impl<R: Rt> EvalCached<R> for Note {
    const NAME: &str = "test_note";
    const EFFECT: Effect = Effect::Stateless(None);

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R>, from: &CachedVals) -> Option<Value> {
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

/// `test_unmarked_effect(x)` is `x`, and queues a runtime call at every
/// walk whether or not anything it reads was delivered: the shape of a
/// node whose input forgot to mark its boundary.
#[derive(Debug)]
struct UnmarkedEffect {
    id: BindId,
    out: TagValue,
}

impl UnmarkedEffect {
    fn new() -> Box<Self> {
        Box::new(Self { id: BindId::new(), out: TagValue::phantom() })
    }
}

impl<R: Rt> BuiltIn<R> for UnmarkedEffect {
    const NAME: &str = "test_unmarked_effect";
    const EFFECT: Effect = Effect::Sync;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Self::new())
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        _buf: &mut &[u8],
    ) -> std::result::Result<Box<dyn Apply<R>>, PackError> {
        Ok(Self::new())
    }
}

impl<R: Rt> Apply<R> for UnmarkedEffect {
    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let tv = from[0].update(ctx).clone();
        ctx.rt.cancel_timer(self.id);
        self.out.set(tv)
    }

    fn image_encode(&self, _buf: &mut ImageBuf) -> std::result::Result<(), PackError> {
        Ok(())
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {}
}

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

#[test]
fn mistyped_function_value_reports_bind_context() {
    let out = Command::new(std::env::current_exe().unwrap())
        .args(["mistyped_function_value_is_refused::", "--nocapture"])
        .env("RUST_LOG", "warn")
        .env("GRAPHIX_ELAB_AUDIT", "1")
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "{stderr}");
    let context = "a run-time bind at g(1.5) did not type";
    assert_eq!(stderr.matches(context).count(), 4, "{stderr}");
    let refusals = stderr.lines().filter(|line| {
        line.starts_with("ELAB-AUDIT ")
            && line.contains("an instance at fn(x: f64) -> f64")
            && line.contains("of a definition typed fn(x: i64) -> i64")
    });
    assert_eq!(refusals.count(), 4, "{stderr}");
}

/// A builtin's `'a: Concrete` that only a handler the call does not pass
/// holds is never read: the call is accepted with the handler omitted or
/// null, and refused where a value of `'a` can arrive and nothing settles
/// it (a handler whose argument is left open) or the result holds it.
async fn unread_quantifier_settles(mode: Mode) -> Result<()> {
    let code = |ret: &str, call: &str| {
        format_compact!(
            r#"{{
                let f = 'a: Concrete |#h: [fn(v: 'a) -> i64, null] = null, x: i64|
                    -> {ret} 'test_last;
                {call}
            }}"#
        )
    };
    assert_eq!(run(&code("i64", "f(3)"), mode).await?, [Value::I64(3)]);
    assert_eq!(run(&code("i64", "f(#h: null, 3)"), mode).await?, [Value::I64(3)]);
    assert_eq!(run(&code("i64", "((f))(3)"), mode).await?, [Value::I64(3)]);
    for (ret, call) in [
        ("i64", "f(#h: |v| 0, 3)"),
        ("['a, i64]", "f(3)"),
        (
            "i64",
            "{ let g = 'b: Concrete |#h: [fn(v: 'b) -> i64, null] = null, x: i64| x; g(3) }",
        ),
    ] {
        match run(&code(ret, call), mode).await {
            Err(e) if format_compact!("{e:#}").contains("must be fully known") => (),
            r => panic!("{ret} {call}: {r:?}"),
        }
    }
    Ok(())
}

async fn unread_forwarded_quantifier_message(mode: Mode) -> Result<()> {
    let code = |call: &str| {
        format_compact!(
            r#"{{
        let f = 'a: Concrete |#h: [fn(v: 'a) -> i64, null] = null, x: i64|
            -> i64 'test_last;
        let g = |#h: [fn(v: 'b) -> i64, null] = null, x: i64| f(#h: h, x);
        {call}
    }}"#
        )
    };
    for call in ["g(3)", "g(#h: null, 3)"] {
        let e = run(&code(call), mode).await.unwrap_err();
        let message = format_compact!("{e:#}");
        assert!(message.contains("typed handler or a typed null"), "{message}");
        assert!(!message.contains("operation reads it"), "{message}");
    }
    for call in [
        "g(#h: |v: i64| v, 3)",
        "{ let h: [fn(v: i64) -> i64, null] = null; g(#h: h, 3) }",
    ] {
        assert_eq!(run(&code(call), mode).await?, [Value::I64(3)]);
    }
    Ok(())
}

async fn read_quantifier_keeps_type_directed_message(mode: Mode) -> Result<()> {
    for (ret, call) in [("'a", "f(3)"), ("i64", "f(#h: |v| 3, 3)")] {
        let code = format_compact!(
            r#"{{
            let f = 'a: Concrete |#h: [fn(v: 'a) -> i64, null] = null, x: i64|
                -> {ret} 'test_last;
            {call}
        }}"#
        );
        let e = run(&code, mode).await.unwrap_err();
        let message = format_compact!("{e:#}");
        assert!(
            message.contains("a type-directed operation reads it: annotate it"),
            "{message}"
        );
        assert!(!message.contains("typed handler"), "{message}");
    }
    Ok(())
}

// The prune audit walks a boundary nothing marked and refuses what that
// walk queues for the runtime, as it refuses a publication: a statement
// whose effect a missed mark would drop under real pruning.
#[tokio::test(flavor = "multi_thread")]
async fn prune_audit_refuses_a_queued_effect() -> Result<()> {
    let code = "{
  let n = 0;
  select n { x if x < 3 => n <- (x ~ n) + 1, _ => never() };
  let leak = |x: i64| -> i64 'test_unmarked_effect;
  leak(1);
  n
}";
    let run = |prune: PruneMode| {
        let setup = move |ctx: &mut TestState| {
            ctx.register_builtin::<UnmarkedEffect>().unwrap();
            ctx.control.set_prune_mode(prune);
        };
        run_delta_with(code, Mode::Interp, setup, Duration::from_millis(300))
    };
    let walked = run(PruneMode::Off).await?.values;
    assert_eq!(walked, (0..=3).map(Value::I64).collect::<Vec<_>>());
    match run(PruneMode::Audit).await {
        Err(e) if e.to_string().contains("the runtime died") => Ok(()),
        r => panic!("the audit passed a queued effect: {:?}", r.map(|d| d.values)),
    }
}

/// Where each `test_slow_log` call started and ended, by the key its
/// fixture passes.
static SLOW_LOG: Mutex<Vec<(i64, CompactString)>> = Mutex::new(Vec::new());
static SLOW_KEYS: AtomicI64 = AtomicI64::new(0);

/// `test_slow_log(key, k, ms)`: answers `k` after `ms`, and may not be
/// dropped once started, as a write or a db op may not.
#[derive(Debug, Default)]
struct SlowLogEv;

graphix_package_core::unit_image_state!(SlowLogEv);

impl EvalCachedAsync for SlowLogEv {
    const NAME: &str = "test_slow_log";
    const CANCEL_SAFE: bool = false;
    type Args = (i64, i64, i64);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get(0)?, cached.get(1)?, cached.get(2)?))
    }

    fn eval((key, k, ms): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            SLOW_LOG.lock().push((key, format_compact!("start {k}")));
            tokio::time::sleep(Duration::from_millis(ms as u64)).await;
            SLOW_LOG.lock().push((key, format_compact!("end {k}")));
            Value::I64(k)
        }
    }
}

/// A call that may not be dropped runs on when its arm sleeps, unanswered,
/// and a one-at-a-time site's next call, issued at the wake, waits for it.
async fn calls_in_order_across_a_sleep(mode: Mode) -> Result<()> {
    let key = SLOW_KEYS.fetch_add(1, Ordering::Relaxed);
    let code = format!(
        "{{
  let step = 0;
  step <- select step {{ s if s < 3 => s + 1, _ => never() }};
  let ev = select step {{ 0 => `Call(1), 3 => `Call(2), _ => `Idle }};
  let slow = |key: i64, k: i64, ms: i64| -> i64 'test_slow_log;
  select ev {{
    `Call(k) => slow({key}, k, select k {{ 1 => 300, _ => 10 }}),
    `Idle => never()
  }}
}}"
    );
    let setup = |ctx: &mut TestState| {
        ctx.register_builtin::<CachedArgsAsync<SlowLogEv>>().unwrap();
    };
    run_delta_with(&code, mode, setup, Duration::from_millis(700)).await?;
    let log: Vec<CompactString> = SLOW_LOG
        .lock()
        .iter()
        .filter(|(k, _)| *k == key)
        .map(|(_, e)| e.clone())
        .collect();
    assert_eq!(log, ["start 1", "end 1", "start 2", "end 2"]);
    Ok(())
}

modes!(
    calls_in_order_across_a_sleep,
    rust_panics_in_a_kernel,
    a_panic_stops_a_kernel_loop,
    mistyped_function_value_is_refused,
    unread_quantifier_settles,
    unread_forwarded_quantifier_message,
    read_quantifier_keeps_type_directed_message
);
