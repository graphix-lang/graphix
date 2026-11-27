//! The debugger's service over an in-memory stream (`design/debugger.md`):
//! each command against a running program.

use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use graphix_compiler::expr::ModPath;
use graphix_package_core::testing::{
    Events, Mode, TestCtx, fixture_runtime, next_update,
};
use graphix_rt::{
    CompRes, GXEvent, NoExt,
    debug::{
        Client, Debugger,
        proto::{self, Binding, Command, Event, Reply, Why},
    },
};
use netidx_value::Value;
use tokio::{io::DuplexStream, task::JoinHandle};

/// A runtime running a program, and the events its embedder is sent.
struct Running {
    ctx: TestCtx,
    events: Events,
    program: CompRes<NoExt>,
}

async fn run(program: &'static str, mode: Mode) -> Result<Running> {
    let (ctx, events) = fixture_runtime([], crate::TEST_REGISTER, mode, |_| {}).await?;
    let program = ctx.rt.compile(ArcStr::from(program)).await?;
    Ok(Running { ctx, events, program })
}

/// A session attached to a runtime.
struct Session {
    client: Client<DuplexStream>,
    served: JoinHandle<Result<()>>,
}

impl Running {
    async fn attach(&self) -> Result<Session> {
        let (ours, theirs) = tokio::io::duplex(1 << 16);
        let debugger = Debugger::new(self.ctx.rt.clone());
        let served = tokio::spawn(async move { debugger.serve(theirs).await });
        Ok(Session { client: Client::connect(ours).await?, served })
    }

    /// End the runtime and wait until it has: its last batch is taken.
    async fn shutdown(self) {
        let Running { ctx, mut events, program } = self;
        drop(program);
        ctx.shutdown().await;
        while events.recv().await.is_some() {}
    }

    /// Every value the embedder has been sent by the cycles so far.
    async fn sent(&mut self) -> Result<Vec<Value>> {
        self.ctx.rt.get_env().await?;
        let mut values = vec![];
        while let Ok(mut batch) = self.events.try_recv() {
            values.extend(batch.drain(..).filter_map(|e| match e {
                GXEvent::Updated(_, v) => Some(v),
                GXEvent::Env(_) | GXEvent::Idle => None,
            }))
        }
        Ok(values)
    }
}

impl Session {
    async fn bindings(&mut self, cmd: Command) -> Result<Vec<Binding>> {
        match self.client.request(cmd).await? {
            Reply::Bindings { bindings, more: 0 } => Ok(bindings),
            other => bail!("expected bindings: {other:?}"),
        }
    }

    async fn print(&mut self, target: &str) -> Result<Binding> {
        let mut found = self.bindings(Command::Print { target: target.into() }).await?;
        match found.len() {
            1 => Ok(found.pop().unwrap()),
            n => bail!("{target} names {n} bindings"),
        }
    }

    async fn why(&mut self, target: &str) -> Result<Why> {
        match self.client.request(Command::Why { target: target.into() }).await? {
            Reply::Why { why } => Ok(why),
            other => bail!("expected a why: {other:?}"),
        }
    }

    async fn watch(&mut self, target: &str) -> Result<(u64, Vec<Binding>)> {
        match self.client.request(Command::Watch { target: target.into() }).await? {
            Reply::Watching { watch, bindings } => Ok((watch, bindings)),
            other => bail!("expected a watch: {other:?}"),
        }
    }

    /// The next fire a watch reports: its watch, binding and value.
    async fn fired(&mut self) -> Result<(u64, String, Option<String>)> {
        loop {
            match self.client.event().await? {
                Some(Event::Fired { watch, name, value, .. }) => {
                    break Ok((watch, name.to_string(), value.map(|v| v.to_string())));
                }
                Some(Event::Value { .. } | Event::Paused { .. } | Event::Running) => (),
                None => bail!("the session ended"),
            }
        }
    }

    /// Evaluate `text`: its type, and the value its first cycle had.
    async fn eval(&mut self, text: &str) -> Result<(Option<ArcStr>, Option<ArcStr>)> {
        let Reply::Evaled { eval, typ } =
            self.client.request(Command::Eval { text: text.into() }).await?
        else {
            bail!("expected an eval's answer")
        };
        let value = match self.client.queued() {
            None => None,
            Some(Event::Value { eval: of, value }) if of == eval => Some(value),
            Some(other) => bail!("expected the value of eval {eval}: {other:?}"),
        };
        Ok((typ, value))
    }

    async fn refused(&mut self, cmd: Command) -> Result<String> {
        match self.client.request(cmd).await {
            Ok(reply) => bail!("expected a refusal: {reply:?}"),
            Err(e) => Ok(format!("{e:#}")),
        }
    }

    /// Close the session and wait for the service to end it.
    async fn close(self) -> Result<()> {
        drop(self.client);
        self.served.await?
    }
}

fn read<'a>(why: &'a Why, name: &str) -> Result<&'a Why> {
    let found = why.reads.iter().find(|r| r.binding.name == name);
    found.ok_or_else(|| anyhow!("{} does not read {name}: {why:?}", why.binding.name))
}

fn text(why: &Why) -> Option<&str> {
    why.source.as_ref().map(|s| s.text.as_str())
}

const PROGRAM: &str = r#"
let x = 41;
let y = x + 1;
let quiet: i64 = never();
let late = quiet + y
"#;

/// A binding's value and type; why one has none; a write to it; an
/// expression over the program; the program's bindings. In every mode.
#[tokio::test(flavor = "multi_thread")]
async fn a_session_asks_and_injects() -> Result<()> {
    for mode in Mode::ALL {
        let rt = run(PROGRAM, mode).await?;
        let mut s = rt.attach().await?;
        let y = s.print("y").await?;
        assert_eq!((y.typ.as_str(), y.value.as_deref()), ("i64", Some("42")), "{mode:?}");
        assert!(y.written.is_some(), "{y:?}");
        let quiet = s.print("quiet").await?;
        assert_eq!((quiet.value, quiet.written), (None, None));
        assert_eq!(s.print(&format!("#{}", y.id)).await?, y);

        let why = s.why("late").await?;
        assert_eq!(
            (&why.binding.value, text(&why)),
            (&None, Some("let late = quiet + y"))
        );
        let q = read(&why, "quiet")?;
        assert_eq!(
            (&q.binding.value, text(q)),
            (&None, Some("let quiet: i64 = never()"))
        );
        assert!(q.reads.is_empty(), "{q:?}");
        let y_read = read(&why, "y")?;
        assert_eq!(
            (y_read.binding.value.as_deref(), &y_read.source),
            (Some("42"), &None)
        );

        let (watch, watched) = s.watch("late").await?;
        assert_eq!(watched.len(), 1);
        let set = Command::Set { target: "quiet".into(), value: "x - 33".into() };
        match s.client.request(set).await? {
            Reply::Set { binding, value, held: false } => {
                assert_eq!((binding.name.as_str(), value.as_str()), ("quiet", "8"))
            }
            other => bail!("expected a set: {other:?}"),
        }
        assert_eq!(s.fired().await?, (watch, "late".into(), Some("50".into())));
        assert_eq!(s.print("late").await?.value.as_deref(), Some("50"));
        assert!(matches!(
            s.client.request(Command::Unwatch { watch }).await?,
            Reply::Unwatched
        ));
        let gone = s.refused(Command::Unwatch { watch }).await?;
        assert!(gone.contains("no watch"), "{gone}");

        assert_eq!(s.eval("y * 2").await?, (Some("i64".into()), Some("84".into())));
        assert_eq!(s.eval("let z = y + 100").await?, (None, None));
        assert_eq!(s.eval("z").await?.1.as_deref(), Some("142"));
        let refused = s.refused(Command::Eval { text: "z + \"s\"".into() }).await?;
        assert!(refused.contains("string"), "{refused}");

        let listed = s.bindings(Command::List { scope: None }).await?;
        let names = listed.iter().map(|b| b.name.as_str()).collect::<Vec<_>>();
        assert_eq!(names, ["x", "y", "quiet", "late", "z"], "{mode:?}: {listed:?}");

        let unknown = s.refused(Command::Print { target: "nosuch".into() }).await?;
        assert!(unknown.contains("no binding is named nosuch"), "{unknown}");
        let ill = Command::Set { target: "quiet".into(), value: "\"s\"".into() };
        let ill = s.refused(ill).await?;
        assert!(
            ill.contains("is not a i64") && ill.contains("quiet is not set"),
            "{ill}"
        );
        let none = Command::Set { target: "x".into(), value: "never()".into() };
        let none = s.refused(none).await?;
        assert!(none.contains("has no value"), "{none}");
        assert_eq!(s.print("x").await?.value.as_deref(), Some("41"));

        match s.client.request(Command::Info).await? {
            Reply::Info { info } => assert!(
                info.cycle > 0 && info.sessions == 1 && info.watches == 0,
                "{mode:?}: {info:?}"
            ),
            other => bail!("expected info: {other:?}"),
        }
        s.close().await?;
        rt.shutdown().await
    }
    Ok(())
}

/// A function's binding is one binding for each call that built an
/// instance: a name or a place answers with all of them, a command that
/// takes one refuses and lists them by id, and an id names one.
#[tokio::test(flavor = "multi_thread")]
async fn instances_are_told_apart_by_id() -> Result<()> {
    const PROGRAM: &str = r#"
let f = |x: i64| -> i64 { let inner = x + 1; inner * 2 };
let a = f(1);
let b = f(2)
"#;
    let rt = run(PROGRAM, Mode::Interp).await?;
    let mut s = rt.attach().await?;
    let inner = s.bindings(Command::Print { target: "inner".into() }).await?;
    let values = inner.iter().map(|b| b.value.as_deref()).collect::<Vec<_>>();
    assert_eq!(values, [Some("2"), Some("3")], "{inner:?}");
    let several = s.refused(Command::Why { target: "inner".into() }).await?;
    assert!(several.contains("inner names 2 bindings"), "{several}");
    for b in &inner {
        assert!(several.contains(&format!("#{} inner", b.id)), "{several}");
    }
    let why = s.why(&format!("#{}", inner[1].id)).await?;
    assert_eq!(text(&why), Some("let inner = x + 1"));
    assert_eq!(read(&why, "x")?.binding.value.as_deref(), Some("2"));
    let listed = s.bindings(Command::List { scope: None }).await?;
    let rows = listed.iter().map(|b| (b.name.as_str(), b.instances)).collect::<Vec<_>>();
    assert_eq!(rows, [("f", 1), ("x", 2), ("inner", 2), ("a", 1), ("b", 1)]);
    assert_eq!(s.watch("inner").await?.1.len(), 2);
    s.close().await?;
    rt.shutdown().await;
    Ok(())
}

/// What a session evaluates is its own: the embedder is sent none of its
/// values, and what it bound is deleted when it closes.
#[tokio::test(flavor = "multi_thread")]
async fn what_a_session_made_ends_with_it() -> Result<()> {
    const SECRET: &str = "only the session sees this";
    let mut rt = run(PROGRAM, Mode::Interp).await?;
    let mut s = rt.attach().await?;
    assert_eq!(s.eval("let z = 5").await?, (None, None));
    assert_eq!(s.eval("z + y").await?.1.as_deref(), Some("47"));
    let shown = s.eval(&format!("\"{SECRET}\"")).await?;
    assert_eq!(shown.1.as_deref(), Some(format!("\"{SECRET}\"").as_str()));
    let sent = rt.sent().await?;
    assert!(
        !sent.iter().any(|v| matches!(v, Value::String(s) if &**s == SECRET)),
        "{sent:?}"
    );
    assert!(!sent.contains(&Value::I64(47)), "{sent:?}");
    s.close().await?;
    let mut again = rt.attach().await?;
    let gone = again.refused(Command::Print { target: "z".into() }).await?;
    assert!(gone.contains("no binding is named z"), "{gone}");
    assert_eq!(again.print("y").await?.value.as_deref(), Some("42"));
    again.close().await?;
    rt.shutdown().await;
    Ok(())
}

/// A program a session watches computes what it computes alone, and the
/// session is told each fire in the cycle's order.
#[tokio::test(flavor = "multi_thread")]
async fn a_watched_program_computes_the_same() -> Result<()> {
    const PROGRAM: &str = r#"
let input: i64 = never();
let acc = 0;
acc <- input ~ acc + input;
acc * 2
"#;
    async fn driven(rt: &mut Running, s: Option<&mut Session>) -> Result<Vec<Value>> {
        let out = rt.program.exprs.last().map(|e| e.id).expect("the program's value");
        let named = ModPath::from_iter(["input"]);
        let input = rt.program.env.lookup_bind(&ModPath::root(), &named)?;
        let input = input.ok_or_else(|| anyhow!("the program binds input"))?.1.id;
        let mut outputs = vec![next_update(&rt.ctx.rt, &mut rt.events, out).await?];
        let mut s = s;
        for i in 1..=4i64 {
            rt.ctx.rt.set(input, i)?;
            outputs.push(next_update(&rt.ctx.rt, &mut rt.events, out).await?);
            if let Some(s) = s.as_deref_mut() {
                let (_, name, value) = s.fired().await?;
                assert_eq!((name.as_str(), value), ("input", Some(i.to_string())));
                let (_, name, value) = s.fired().await?;
                assert_eq!(
                    (name.as_str(), value),
                    ("acc", Some((i * (i + 1) / 2).to_string()))
                );
            }
        }
        Ok(outputs)
    }
    for mode in Mode::ALL {
        let mut alone = run(PROGRAM, mode).await?;
        let expected = driven(&mut alone, None).await?;
        assert_eq!(expected, [0, 2, 6, 12, 20].map(Value::I64), "{mode:?}");
        alone.shutdown().await;
        let mut watched = run(PROGRAM, mode).await?;
        let mut s = watched.attach().await?;
        s.watch("input").await?;
        s.watch("acc").await?;
        assert_eq!(s.eval("acc + 1000").await?.1.as_deref(), Some("1000"));
        assert_eq!(driven(&mut watched, Some(&mut s)).await?, expected, "{mode:?}");
        s.close().await?;
        watched.shutdown().await
    }
    Ok(())
}

/// A held program runs no cycle of its own: what its embedder writes
/// waits, a session still asks and evaluates, a step runs one cycle of
/// work and says what it delivered, and a break holds the program at the
/// cycle its condition is met. The last session to leave lets it run.
#[tokio::test(flavor = "multi_thread")]
async fn a_held_program_waits_and_steps() -> Result<()> {
    const PROGRAM: &str = r#"
let input: i64 = never();
let acc = 0;
acc <- input ~ acc + input;
acc * 2
"#;
    async fn held(
        s: &mut Session,
    ) -> Result<(proto::Paused, Vec<(String, Option<String>)>)> {
        loop {
            match s.client.event().await? {
                Some(Event::Paused { by, delivered, .. }) => {
                    let delivered = delivered.map(|c| c.delivered).unwrap_or_default();
                    let named = |d: proto::Delivered| {
                        (d.name.to_string(), d.value.map(|v| v.to_string()))
                    };
                    break Ok((by, delivered.into_iter().map(named).collect()));
                }
                Some(_) => (),
                None => bail!("the session ended"),
            }
        }
    }
    let fired = |name: &str, v: i64| (name.to_string(), Some(v.to_string()));
    for mode in Mode::ALL {
        let mut rt = run(PROGRAM, mode).await?;
        let out = rt.program.exprs.last().map(|e| e.id).expect("the program's value");
        let named = ModPath::from_iter(["input"]);
        let input = rt.program.env.lookup_bind(&ModPath::root(), &named)?;
        let input = input.ok_or_else(|| anyhow!("the program binds input"))?.1.id;
        assert_eq!(next_update(&rt.ctx.rt, &mut rt.events, out).await?, Value::I64(0));
        let mut s = rt.attach().await?;
        assert!(matches!(s.client.request(Command::Pause).await?, Reply::Paused { .. }));
        rt.ctx.rt.set(input, 1i64)?;
        assert_eq!(s.print("acc").await?.value.as_deref(), Some("0"), "{mode:?}");
        assert_eq!(s.print("input").await?.value, None, "{mode:?}");
        assert_eq!(s.eval("acc + 100").await?.1.as_deref(), Some("100"), "{mode:?}");
        assert!(matches!(
            s.client.request(Command::Step { cycles: None }).await?,
            Reply::Stepping
        ));
        let (by, delivered) = held(&mut s).await?;
        assert_eq!((by, delivered), (proto::Paused::Stepped, vec![fired("input", 1)]));
        assert_eq!(s.print("acc").await?.value.as_deref(), Some("0"), "{mode:?}");
        s.client.request(Command::Step { cycles: None }).await?;
        assert_eq!(held(&mut s).await?.1, [fired("acc", 1)], "{mode:?}");
        assert_eq!(next_update(&rt.ctx.rt, &mut rt.events, out).await?, Value::I64(2));

        let set = Command::Set { target: "input".into(), value: "2".into() };
        match s.client.request(set).await? {
            Reply::Set { held: true, .. } => (),
            other => bail!("expected a held set: {other:?}"),
        }
        assert_eq!(s.print("acc").await?.value.as_deref(), Some("1"), "{mode:?}");
        s.client.request(Command::Step { cycles: Some(2) }).await?;
        assert_eq!(held(&mut s).await?.1, [fired("acc", 3)], "{mode:?}");
        assert_eq!(next_update(&rt.ctx.rt, &mut rt.events, out).await?, Value::I64(6));

        let at = proto::When::Equals { value: "acc + 7".into() };
        let Reply::Watching { watch, .. } =
            s.client.request(Command::Break { target: "acc".into(), when: at }).await?
        else {
            bail!("expected a break")
        };
        assert!(matches!(s.client.request(Command::Continue).await?, Reply::Running));
        rt.ctx.rt.set(input, 3i64)?;
        assert_eq!(next_update(&rt.ctx.rt, &mut rt.events, out).await?, Value::I64(12));
        rt.ctx.rt.set(input, 4i64)?;
        let (by, delivered) = held(&mut s).await?;
        assert_eq!(
            (by, delivered),
            (proto::Paused::Break { watch }, vec![fired("acc", 10)])
        );
        assert_eq!(next_update(&rt.ctx.rt, &mut rt.events, out).await?, Value::I64(20));
        rt.ctx.rt.set(input, 5i64)?;
        assert_eq!(s.print("acc").await?.value.as_deref(), Some("10"), "{mode:?}");

        let Reply::Cycles { cycles } =
            s.client.request(Command::Cycles { last: Some(2) }).await?
        else {
            bail!("expected the cycles")
        };
        let logged = cycles
            .iter()
            .map(|c| c.delivered.iter().map(|d| d.name.as_str()).collect::<Vec<_>>())
            .collect::<Vec<_>>();
        assert_eq!(logged, [vec!["input"], vec!["acc"]], "{mode:?}: {cycles:?}");

        s.close().await?;
        assert_eq!(next_update(&rt.ctx.rt, &mut rt.events, out).await?, Value::I64(30));
        rt.shutdown().await
    }
    Ok(())
}

/// `why` says what the nodes of a binding's value know: the error a `$`
/// dropped, the arm a select holds, a sample whose trigger has not
/// fired, a fresh block not started, the step a seq is at, the call a
/// binding is inside and the call that passes a parameter, and the
/// select whose pattern names a binding.
#[tokio::test(flavor = "multi_thread")]
async fn why_says_what_the_nodes_know() -> Result<()> {
    const PROGRAM: &str = r#"
let input: i64 = never();
let a = [1, 2, 3];
let idx = a[9]$;
let mode: [`Edit, `List] = `Edit;
let doc: string = never();
let editor = |d: string| -> string "editing [d]";
let shown = select mode { `Edit => editor(doc), `List => "list" };
let sampled = input ~ a;
let started = fresh input { a };
let r = seq input { let got = a[7]$; got + 1 };
let scale = |x: i64| -> i64 { let doubled = x * 2; doubled + input };
let scaled = scale(3);
let opt: [i64, null] = null;
let seen = select opt { null => 0, n => n + 1 }
"#;
    fn says(why: &Why) -> Vec<&str> {
        why.because.iter().map(|r| r.says.as_str()).collect()
    }
    fn waits(why: &Why) -> Vec<&str> {
        let none = why.reads.iter().filter(|r| r.binding.value.is_none());
        none.map(|r| r.binding.name.as_str()).collect()
    }
    let rt = run(PROGRAM, Mode::Interp).await?;
    let mut s = rt.attach().await?;

    let idx = s.why("idx").await?;
    let dropped = "`$` dropped the error `ArrayIndexError(\"array index out of bounds\")";
    assert_eq!(says(&idx), [dropped], "{idx:?}");
    assert_eq!(idx.because[0].text.as_deref(), Some("a[9]$"));
    assert!(idx.reads.is_empty(), "{idx:?}");

    let shown = s.why("shown").await?;
    let held = "the select holds arm 1 of 2: `Edit; its argument was last `Edit";
    assert_eq!(says(&shown), [held], "{shown:?}");
    let call = &shown.because[0].parts[0];
    assert_eq!(
        (call.says.as_str(), call.text.as_deref()),
        ("in this call", Some("editor(doc)"))
    );
    assert_eq!(waits(&shown), ["doc"], "{shown:?}");
    assert_eq!(says(read(&shown, "doc")?), ["`never()` never has a value"]);

    let sampled = s.why("sampled").await?;
    assert_eq!(says(&sampled), ["its trigger has not fired"], "{sampled:?}");
    assert_eq!(waits(&sampled), ["input"]);

    let started = s.why("started").await?;
    let unborn = "the fresh block has not started: its event has not fired";
    assert_eq!((says(&started), waits(&started)), (vec![unborn], vec!["input"]));

    let idle = s.why("r").await?;
    let waiting = "the seq is not running: it waits for its trigger";
    assert_eq!((says(&idle), waits(&idle)), (vec![waiting], vec!["input"]), "{idle:?}");

    let scaled = s.why("scaled").await?;
    assert_eq!(says(&scaled), ["in this call"], "{scaled:?}");
    assert_eq!(waits(&scaled), ["input"]);
    let doubled = s.why("doubled").await?;
    let calls = doubled.calls.iter().map(|c| c.text.as_str()).collect::<Vec<_>>();
    assert_eq!(
        (calls, &doubled.by),
        (vec!["scale(3)"], &proto::BoundBy::Let),
        "{doubled:?}"
    );
    let x = s.why("x").await?;
    assert_eq!(x.by, proto::BoundBy::Param, "{x:?}");
    assert_eq!(text(&x), Some("scale(3)"));

    let n = s.why("n").await?;
    assert_eq!(n.by, proto::BoundBy::Pattern { arm: 2 }, "{n:?}");
    assert_eq!(
        says(&n),
        ["the select holds arm 1 of 2: null; its argument was last null"]
    );

    // two cycles of work after the trigger: its delivery, then the step
    s.client.request(Command::Pause).await?;
    let named = ModPath::from_iter(["input"]);
    let input = rt.program.env.lookup_bind(&ModPath::root(), &named)?;
    rt.ctx.rt.set(input.ok_or_else(|| anyhow!("the program binds input"))?.1.id, 1i64)?;
    s.client.request(Command::Step { cycles: Some(2) }).await?;
    assert!(matches!(s.client.request(Command::Wait).await?, Reply::Paused { .. }));
    let stepping = s.why("r").await?;
    assert_eq!(says(&stepping), ["the seq is at step 1 of 2"], "{stepping:?}");
    let at = &stepping.because[0];
    assert_eq!(at.text.as_deref(), Some("let got = a[7]$"));
    let inner = at.parts.iter().map(|r| r.says.as_str()).collect::<Vec<_>>();
    let dropped = "`$` dropped the error `ArrayIndexError(\"array index out of bounds\")";
    assert_eq!(inner, [dropped], "{stepping:?}");

    s.close().await?;
    rt.shutdown().await;
    Ok(())
}
