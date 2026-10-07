//! The embedder-callable path (`GXHandle::compile_callable`): a callee
//! instance born lazily at its first dispatch must resolve `*st <- v`
//! from the standing store, like `Deref`'s read side; and a handler's
//! arm woken by a state change reads the event that changed it stale.

use anyhow::{Context, Result};
use graphix_compiler::expr::VfsEntry;
use graphix_package_core::testing::{
    Events, Mode, TestCtx, compile_named_callable, compile_result, find_bind_id,
    fixture_runtime,
};
use graphix_rt::{Callable, CompRes, GXEvent, NoExt, Ref};
use netidx::{protocol::valarray::ValArray, publisher::Value};

/// A program's runtime with its `result` compiled and the callables it
/// handed out kept alive.
struct Fixture {
    ctx: TestCtx,
    rx: Events,
    res: CompRes<NoExt>,
    held: Vec<(&'static str, Ref<NoExt>, Callable<NoExt>)>,
    last: Option<Value>,
}

impl Fixture {
    async fn new(src: &'static str, mode: Mode) -> Result<Self> {
        let (ctx, rx) = fixture_runtime(
            [("/test.gx", VfsEntry::from(arcstr::ArcStr::from(src)))],
            crate::TEST_REGISTER,
            mode,
            |_| {},
        )
        .await?;
        let res = compile_result(&ctx).await?;
        Ok(Self { ctx, rx, res, held: vec![], last: None })
    }

    /// Call the lambda bound to `name` with `args`.
    async fn call<const N: usize>(
        &mut self,
        name: &'static str,
        args: [Value; N],
    ) -> Result<()> {
        let i = match self.held.iter().position(|(n, _, _)| *n == name) {
            Some(i) => i,
            None => {
                let (r, cb) =
                    compile_named_callable(&self.ctx.rt, &self.res.env, name).await?;
                self.held.push((name, r, cb));
                self.held.len() - 1
            }
        };
        self.held[i].2.call(ValArray::from(args)).await
    }

    /// The value bound to `name`.
    async fn value_of(&self, name: &str) -> Result<Value> {
        let bid = find_bind_id(&self.res.env, name)?;
        self.ctx.rt.compile_ref(bid).await?.last.clone().context("no value")
    }

    /// Run the runtime until it is idle; the result's latest value.
    async fn settle(&mut self) -> Result<Option<Value>> {
        self.ctx.rt.wait_idle().await?;
        let id = self.res.exprs[0].id;
        while let Ok(mut batch) = self.rx.try_recv() {
            for e in batch.drain(..) {
                if let GXEvent::Updated(eid, v) = e
                    && eid == id
                {
                    self.last = Some(v);
                }
            }
        }
        Ok(self.last.clone())
    }

    /// Cycles that run nothing of the program.
    async fn idle_cycles(&self, n: usize) -> Result<()> {
        for _ in 0..n {
            self.ctx.rt.compile(arcstr::literal!("i64:0")).await?;
        }
        Ok(())
    }
}

const PROG: &str = r#"
type St = { value: string, cursor: i64 };
let ed: St = { value: "", cursor: 0 };
let poke = |st: &mut St, tag: string| -> null select tag {
  "" => null,
  t => {
    let s = t ~ *st;
    *st <- { value: "[s.value][t]", cursor: s.cursor + 1 };
    null
  }
};
let handle = |tag: string| -> null poke(&mut ed, tag);
let result = ed.value
"#;

async fn handler_writes_through_ref_param(mode: Mode) -> Result<()> {
    let mut f = Fixture::new(PROG, mode).await?;
    f.settle().await?;
    // Cycles between the callable's init and its first dispatch: the
    // reference value delivered at init must survive to the instance.
    f.idle_cycles(3).await?;
    f.call("test::handle", ["x".into()]).await?;
    assert_eq!(f.settle().await?, Some(Value::from("x")));
    Ok(())
}

/// A handler whose interior select routes by a state variable: flipping
/// the state wakes an arm for the first time, and the handler's standing
/// params must deliver stale, never re-fire a consumed event.
const PHANTOM: &str = r#"
let active: [`A, `B, null] = null;
let submitted = 0;
let fire = |t: Any| -> null {
  submitted <- t ~ (submitted + 1);
  null
};
let set_active = |b: bool| -> null {
  active <- select b {
    true => `B,
    false => `A
  };
  null
};
let handle = |e: string| -> i64 select active {
  null as _ => 0,
  `A => e ~ 1,
  `B => {
    fire(e);
    2
  }
};
let result = submitted
"#;

async fn arm_wake_delivers_standing_args_stale(mode: Mode) -> Result<()> {
    let mut f = Fixture::new(PHANTOM, mode).await?;
    // route to `A, deliver one real event, then flip to `B with no new
    // event: the flip must not fire the `B arm's callee with the standing "x"
    f.call("test::set_active", [Value::Bool(false)]).await?;
    f.call("test::handle", ["x".into()]).await?;
    f.call("test::set_active", [Value::Bool(true)]).await?;
    assert_eq!(f.settle().await?, Some(Value::I64(0)), "a phantom fire at the wake");
    f.call("test::handle", ["y".into()]).await?;
    assert_eq!(f.settle().await?, Some(Value::I64(1)));
    Ok(())
}

/// A callable's body flips its own routing state from a key it consumed:
/// the newly selected arm's callee must read the standing key stale,
/// never dispatch on it.
const FLIP: &str = r#"
type Key = { code: [`Enter, `Other], kind: [`Press, `Release] };
type Event = [`Key(Key), `Mouse];
let screen = 0;
let fired = 0;
let req: [i64, null] = null;
let land = {
  handle: |e: Event| -> [`Stop, `Continue] select e {
    `Key(k) => select k.code {
      kk@ `Enter => { req <- kk ~ 1; `Stop },
      `Other => `Continue
    },
    `Mouse => `Continue
  }
};
select req {
  null as _ => never(),
  _ => screen <- 1
};
let connect_keys = |k: Key| -> [`Stop, `Continue] select k.code {
  kk@ `Enter => { fired <- (kk ~ fired) + 1; `Stop },
  `Other => `Continue
};
let handle = |e: Event| -> [`Stop, `Continue] select e {
  ev@ `Key(k) => select k.kind {
    `Press => select screen {
      0 => land.handle(ev),
      _ => connect_keys(k)
    },
    `Release => `Continue
  },
  `Mouse => `Continue
};
let result = (screen, fired)
"#;

fn key_event(code: &'static str) -> Value {
    let field = |k: &'static str, v: &'static str| {
        Value::Array(ValArray::from([k.into(), v.into()]))
    };
    let key = Value::Array(ValArray::from([field("code", code), field("kind", "Press")]));
    Value::Array(ValArray::from(["Key".into(), key]))
}

async fn callable_body_flip_reads_standing_key_stale(mode: Mode) -> Result<()> {
    let mut f = Fixture::new(FLIP, mode).await?;
    f.call("test::handle", [key_event("Enter")]).await?;
    // the request flips the screen with no further key: (screen, fired)
    let want = Value::Array(ValArray::from([Value::I64(1), Value::I64(0)]));
    assert_eq!(f.settle().await?, Some(want));
    Ok(())
}

/// The screen dispatcher shape of a TUI: the handler routes the key to
/// one of two callees by a screen variable that the callee itself
/// moves. The key that moved the screen must not be delivered again to
/// the callee the move woke: it reads its formal stale, a select arm
/// that binds that formal binds the standing key stale, and `kk ~ ..`
/// stays quiet.
const DISPATCH: &str = r#"
let screen: [`Landing, `Panels] = `Landing;
let opened = 0;
let pan_handle = |e: string| -> i64 select e {
  kk if kk == "enter" => { opened <- kk ~ opened + 1; 1 },
  _ => 0
};
let land_handle = |e: string| -> i64 select e {
  kk if kk == "enter" => { screen <- kk ~ `Panels; 1 },
  _ => 0
};
let handle = |e: string| -> i64 select e {
  "" => 0,
  ev => select screen {
    `Landing => land_handle(ev),
    `Panels => pan_handle(ev)
  }
};
let result = opened
"#;

/// `handle` with each of `first`, which move the screen: the count must
/// not move. `second` is the one legitimate count.
async fn woken_callee_counts_once(
    src: &'static str,
    mode: Mode,
    first: &[&'static str],
    second: &'static str,
) -> Result<()> {
    let mut f = Fixture::new(src, mode).await?;
    for k in first {
        f.call("test::handle", [(*k).into()]).await?;
        assert_eq!(
            f.settle().await?,
            Some(Value::I64(0)),
            "counted the key that woke it"
        );
    }
    f.call("test::handle", [second.into()]).await?;
    assert_eq!(f.settle().await?, Some(Value::I64(1)));
    Ok(())
}

async fn arm_wake_does_not_redeliver_the_key_to_the_woken_callee(
    mode: Mode,
) -> Result<()> {
    woken_callee_counts_once(DISPATCH, mode, &["enter"], "enter").await
}

/// The dispatcher hands one callee the pattern bind `ev` and the other
/// the formal `e` itself. `ev` is a facet of `e`, so the landing arm's
/// read of `ev` consumes `e`'s fire, and the panels callee woken by the
/// screen change does not catch the key up.
const ALIAS: &str = r#"
let screen: [`Landing, `Panels] = `Landing;
let opened = 0;
let pan_handle = |e: string| -> i64 select e {
  "enter" => { opened <- e ~ opened + 1; 1 },
  _ => 0
};
let land_handle = |e: string| -> i64 select e {
  "enter" => { screen <- e ~ `Panels; 1 },
  _ => 0
};
let handle = |e: string| -> i64 select e {
  "" => 0,
  ev => select screen {
    `Landing => land_handle(ev),
    `Panels => pan_handle(e)
  }
};
let result = opened
"#;

async fn alias_read_consumes_the_formals_fire(mode: Mode) -> Result<()> {
    woken_callee_counts_once(ALIAS, mode, &["enter"], "enter").await
}

/// A bind delivered by a sampled scrutinee aliases the trigger only:
/// the levels under the sample's right side were banked, not fired, so
/// an arm reading such a bind does not consume their fires, and a
/// sibling arm that reads the level itself still catches up at wake.
const BANKED: &str = r#"
let toast: string = "";
let mode = false;
let ticks = 0;
let set_toast = |s: string| -> null { toast <- s; null };
let set_mode = |b: bool| -> null { mode <- b; null };
let handle = |k: string| -> i64 select k {
  "" => 0,
  key => select key ~ (toast, 1) {
    (_, one) => select mode {
      false => str::len(key) + one,
      true => { ticks <- toast ~ ticks + 1; 1 }
    }
  }
};
let result = ticks
"#;

async fn banked_bind_does_not_consume_the_level(mode: Mode) -> Result<()> {
    let mut f = Fixture::new(BANKED, mode).await?;
    // a key binds `one` from the banked tuple; then the toast fires
    // while the false arm, which reads `one`, is selected; then the
    // mode flips and the true arm must catch the toast up
    f.call("test::handle", ["x".into()]).await?;
    f.settle().await?;
    f.call("test::set_toast", ["hi".into()]).await?;
    f.settle().await?;
    f.call("test::set_mode", [Value::Bool(true)]).await?;
    assert_eq!(f.settle().await?, Some(Value::I64(1)));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn update_callable_keeps_the_site_for_the_same_lambda() -> Result<()> {
    let f = Fixture::new(PROG, Mode::Jit).await?;
    let handle = f.value_of("test::handle").await?;
    let poke = f.value_of("test::poke").await?;
    let gx = &f.ctx.rt;
    let mut current = None;
    gx.update_callable(&mut current, handle.clone()).await?;
    let first = current.as_ref().context("no callable")?.id();
    gx.update_callable(&mut current, handle).await?;
    assert_eq!(current.as_ref().context("no callable")?.id(), first);
    gx.update_callable(&mut current, poke).await?;
    assert_ne!(current.as_ref().context("no callable")?.id(), first);
    Ok(())
}

/// A handler's select that slept while another screen had the keys
/// wakes to a key it never saw. It selects the arm for that key, but the
/// key is a past event: the arm's bind is stale and `kk ~ ..` stays quiet.
const WAKE_SWITCH: &str = r#"
type Key = [`Enter, `Esc, `Other];
let screen: [`Landing, `Panels] = `Panels;
let closed = 0;
let pan_handle = |e: Key| -> i64 select e {
  kk@ `Enter => { screen <- kk ~ `Landing; 1 },
  kk@ `Esc => { closed <- kk ~ closed + 1; 2 },
  `Other => 0
};
let land_handle = |e: Key| -> i64 select e {
  kk@ `Esc => { screen <- kk ~ `Panels; 1 },
  _ => 0
};
let handle = |e: Key| -> i64 select e {
  `Other => 0,
  ev => select screen {
    `Landing => land_handle(ev),
    `Panels => pan_handle(ev)
  }
};
let result = closed
"#;

// Enter on the panels selects its Enter arm and moves to the landing, so
// the panels handler sleeps. Esc on the landing moves back: the panels
// handler wakes to Esc and must not count it.
async fn arm_wake_switch_binds_the_standing_key_stale(mode: Mode) -> Result<()> {
    woken_callee_counts_once(WAKE_SWITCH, mode, &["Enter", "Esc"], "Esc").await
}

modes!(
    handler_writes_through_ref_param,
    arm_wake_delivers_standing_args_stale,
    callable_body_flip_reads_standing_key_stale,
    arm_wake_does_not_redeliver_the_key_to_the_woken_callee,
    alias_read_consumes_the_formals_fire,
    banked_bind_does_not_consume_the_level,
    arm_wake_switch_binds_the_standing_key_stale,
);
