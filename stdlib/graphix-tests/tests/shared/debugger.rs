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
        proto::{Binding, Command, Event, Reply, Why},
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
                Some(Event::Value { .. }) => (),
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
            Reply::Set { binding, value } => {
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
