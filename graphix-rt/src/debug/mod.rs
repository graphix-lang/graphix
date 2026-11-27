//! The debugger's service (`design/debugger.md`): a REPL attached to the
//! running program plus debugger commands, over any byte stream. Nothing
//! listens unless an embedder calls [`Debugger::serve`] or one of the
//! listeners, and the embedder makes the socket or the pipe, so who may
//! connect is its own business: a session reads every value, runs any
//! Graphix in the program and injects events.

mod client;
pub mod proto;

pub use client::Client;

use crate::{
    CompExp, CompRes, GXExt, GXHandle, ToGX,
    gx::debug::{
        Counts, Fired, Fused, Held, Logged, Op, Paused, Stop, Why, authored, held,
    },
};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use compact_str::{CompactString, format_compact};
use graphix_compiler::{
    BindId, MINT_BASE, PrintFlag, SourcePosition,
    env::{Bind, Env},
    explain::{Because, Note, Reached},
    expr::{Expr, ExprId, ExprKind, ModPath, Origin, Source},
    format_with_flags,
    seek::{BindSource, BoundBy},
    typ::{Naked, TVal, Type},
};
use log::warn;
use netidx_core::atomic_id;
use netidx_value::Value;
use poolshark::local::LPooled;
use proto::{Command, Outcome, Reply, Request, Response, When};
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    fmt::Write,
    path::{Path, PathBuf},
};
use tokio::{
    io::{
        AsyncBufRead, AsyncBufReadExt, AsyncRead, AsyncWrite, AsyncWriteExt, BufReader,
        Lines,
    },
    select,
    sync::mpsc::{self, UnboundedReceiver},
};
use triomphe::Arc;

atomic_id!(SessionId);

/// The most bindings one answer lists.
const LISTED: usize = 512;
/// The most bindings one watch covers.
const WATCHED: usize = 64;

/// How a session shows a type.
pub type ShowType = fn(&Type) -> CompactString;

fn show_type(typ: &Type) -> CompactString {
    format_with_flags(PrintFlag::DerefTVars, || format_compact!("{typ}"))
}

/// The debugger of one runtime. Cloning it is cheap; each connection it
/// serves is a session of its own.
pub struct Debugger<X: GXExt> {
    gx: GXHandle<X>,
    show: ShowType,
}

impl<X: GXExt> Clone for Debugger<X> {
    fn clone(&self) -> Self {
        Self { gx: self.gx.clone(), show: self.show }
    }
}

impl<X: GXExt> Debugger<X> {
    pub fn new(gx: GXHandle<X>) -> Self {
        Self { gx, show: show_type }
    }

    /// Show types as `show` prints them.
    pub fn with_types(self, show: ShowType) -> Self {
        Self { show, ..self }
    }

    /// Serve one session over `stream` until the client closes it or the
    /// runtime ends. What the session compiled is deleted with it.
    pub async fn serve<S>(&self, stream: S) -> Result<()>
    where
        S: AsyncRead + AsyncWrite + Unpin,
    {
        let (rd, out) = tokio::io::split(stream);
        let mut lines = BufReader::new(rd).lines();
        let (tx, events) = mpsc::unbounded_channel();
        let attached = Attached { gx: self.gx.clone(), id: SessionId::new() };
        attached.op(Op::Attach { session: attached.id, events: tx })?;
        let env = self.gx.get_env().await?;
        let mut session = Session {
            attached,
            show: self.show,
            out,
            events,
            shown: None,
            kept: Vec::new(),
            made: 0,
            tokens: 0,
            env,
        };
        session.run(&mut lines).await
    }

    /// Serve every connection `listener` accepts, each a session, until
    /// the runtime ends.
    #[cfg(unix)]
    pub async fn listen_unix(&self, listener: tokio::net::UnixListener) -> Result<()> {
        loop {
            select! {
                _ = self.gx.0.tx.closed() => break Ok(()),
                conn = listener.accept() => {
                    let (stream, _) = conn.context("accepting a debugger")?;
                    self.spawn(stream)
                }
            }
        }
    }

    /// Serve every client that connects to the pipe, each a session,
    /// until the runtime ends: `first` is the pipe's first instance and
    /// `next` makes each one after it.
    #[cfg(windows)]
    pub async fn listen_pipe(
        &self,
        first: tokio::net::windows::named_pipe::NamedPipeServer,
        mut next: impl FnMut() -> std::io::Result<
            tokio::net::windows::named_pipe::NamedPipeServer,
        >,
    ) -> Result<()> {
        let mut server = first;
        loop {
            select! {
                _ = self.gx.0.tx.closed() => break Ok(()),
                conn = server.connect() => {
                    conn.context("accepting a debugger")?;
                    let waiting = next().context("making the pipe's next instance")?;
                    self.spawn(std::mem::replace(&mut server, waiting))
                }
            }
        }
    }

    fn spawn<S>(&self, stream: S)
    where
        S: AsyncRead + AsyncWrite + Unpin + Send + 'static,
    {
        let this = self.clone();
        tokio::spawn(async move {
            if let Err(e) = this.serve(stream).await {
                warn!("a debugger session ended: {e:#}")
            }
        });
    }
}

/// A session the runtime knows of, detached when dropped.
struct Attached<X: GXExt> {
    gx: GXHandle<X>,
    id: SessionId,
}

impl<X: GXExt> Attached<X> {
    fn op(&self, op: Op<X>) -> Result<()> {
        let sent = self.gx.0.tx.send(ToGX::Debug(op));
        sent.map_err(|_| self.gx.stopped("runtime is dead"))
    }
}

impl<X: GXExt> Drop for Attached<X> {
    fn drop(&mut self) {
        let _ = self.op(Op::Detach { session: self.id });
    }
}

/// The value an `eval` shows: its last expression, where that has one,
/// and what its values print under. The next `eval` deletes it.
struct Shown<X: GXExt> {
    eval: u64,
    env: Env,
    expr: CompExp<X>,
}

struct Session<X: GXExt, W> {
    attached: Attached<X>,
    show: ShowType,
    out: W,
    events: UnboundedReceiver<Fired>,
    shown: Option<Shown<X>>,
    /// What the evals bound and declared, held so it lives as long as
    /// the session.
    kept: Vec<CompExp<X>>,
    /// The evals and watches numbered so far.
    made: u64,
    /// The cycles and holds waited for so far.
    tokens: u64,
    /// The environment values print under: the last one a command read.
    env: Env,
}

/// A binding's id as a target writes it after `#`: a minted id by its
/// offset, one an image reserved by `r` and its number.
fn handle(id: BindId) -> CompactString {
    match id.inner().checked_sub(MINT_BASE) {
        Some(n) => format_compact!("{n}"),
        None => format_compact!("r{}", id.inner()),
    }
}

fn of_handle(h: &str) -> Option<u64> {
    match h.strip_prefix('r') {
        Some(n) => n.parse::<u64>().ok().filter(|n| *n < MINT_BASE),
        None => h.parse::<u64>().ok()?.checked_add(MINT_BASE),
    }
}

fn printed(env: &Env, typ: &Type, v: &Value) -> ArcStr {
    let mut s: LPooled<String> = LPooled::take();
    let _ = write!(s, "{}", TVal { env, typ, v });
    ArcStr::from(s.as_str())
}

fn file_of(ori: &Origin) -> Option<ArcStr> {
    match &ori.source {
        Source::File(p) => Some(format_compact!("{}", p.display()).as_str().into()),
        Source::Netidx(p) => Some(ArcStr::from(&**p)),
        Source::Vfs(m) => Some(m.clone()),
        Source::Program | Source::Unspecified => None,
    }
}

fn in_file(ori: &Origin, file: &str) -> bool {
    match &ori.source {
        Source::File(p) => p.ends_with(Path::new(file)),
        Source::Vfs(m) => m.as_str() == file,
        Source::Netidx(_) | Source::Program | Source::Unspecified => false,
    }
}

fn by_place(a: &Arc<Bind>, b: &Arc<Bind>) -> Ordering {
    let key = |b: &Arc<Bind>| (file_of(&b.ori), b.ori.load, b.pos, b.name.clone(), b.id);
    key(a).cmp(&key(b))
}

/// Whether two bindings are instances of one written binding.
fn same_place(a: &Arc<Bind>, b: &Arc<Bind>) -> bool {
    a.pos == b.pos
        && a.name == b.name
        && (Arc::ptr_eq(&a.ori, &b.ori) || *a.ori == *b.ori)
}

/// `file:line` or `file:line:column`, as a target writes a place.
fn place(target: &str) -> Option<(&str, i32, Option<i32>)> {
    let (head, last) = target.rsplit_once(':')?;
    let last = last.parse::<i32>().ok()?;
    let lined =
        head.rsplit_once(':').and_then(|(f, l)| Some((f, l.parse::<i32>().ok()?)));
    match lined {
        Some((file, line)) if !file.is_empty() => Some((file, line, Some(last))),
        _ if !head.is_empty() && !head.ends_with(':') => Some((head, last, None)),
        _ => None,
    }
}

/// The bindings `target` names (`proto::Command`), in place order.
fn resolve(env: &Env, target: &str) -> Result<LPooled<Vec<Arc<Bind>>>> {
    let target = target.trim();
    let mut found: LPooled<Vec<Arc<Bind>>> = LPooled::take();
    if let Some(n) = target.strip_prefix('#') {
        let id = of_handle(n).ok_or_else(|| anyhow!("#{n} is no binding's id"))?;
        let ided = env.by_id.iter().filter(|(b, _)| b.inner() == id);
        found.extend(ided.map(|(_, b)| b.clone()));
        if found.is_empty() {
            bail!("no binding has the id #{n}")
        }
    } else if let Some((file, line, column)) = place(target) {
        let at = |b: &Bind| {
            authored(env, b)
                && b.pos.line == line
                && column.is_none_or(|c| b.pos.column == c)
                && in_file(&b.ori, file)
        };
        found.extend(env.by_id.iter().map(|(_, b)| b).filter(|b| at(b)).cloned());
        if found.is_empty() {
            bail!(
                "no binding is introduced at {target} (inside fused code none is kept: \
                 `print` the place for the block's inputs and output)"
            )
        }
    } else {
        let name = ModPath::from_iter(target.split("::"));
        match env.lookup_bind(&ModPath::root(), &name) {
            Ok(Some((_, b))) => found.extend(env.by_id.get(&b.id).cloned()),
            Ok(None) | Err(_) => {
                let named = env.by_id.iter().map(|(_, b)| b);
                let named = named.filter(|b| b.name == target && authored(env, b));
                found.extend(named.cloned())
            }
        }
        if found.is_empty() {
            bail!("no binding is named {target}")
        }
    }
    found.sort_by(by_place);
    Ok(found)
}

/// Whether `list` shows `b`: one the program's author wrote, in `scope`
/// (a module path or a file) where one is given.
fn listed(env: &Env, b: &Bind, scope: Option<&str>) -> bool {
    authored(env, b)
        && match scope {
            None => !matches!(b.ori.source, Source::Vfs(_)),
            Some(s) if s.contains(['.', '/', '\\']) => in_file(&b.ori, s),
            Some(s) => {
                let module = ModPath::from_iter(s.split("::"));
                b.scope == module
                    || b.scope.starts_with(&*format_compact!("{}/", &**module))
            }
        }
}

/// The text of `spec` as its source wrote it, else as it prints.
fn written(spec: &Expr) -> ArcStr {
    let offset = |text: &str, at: SourcePosition| -> Option<usize> {
        let mut lines = text.split_inclusive('\n');
        let before: usize =
            lines.by_ref().take((at.line - 1).max(0) as usize).map(str::len).sum();
        let line = lines.next().unwrap_or("");
        let column = (at.column - 1).max(0) as usize;
        let within = line.char_indices().nth(column).map(|(i, _)| i);
        Some(before + within.or((column == line.chars().count()).then_some(line.len()))?)
    };
    let text = &spec.ori.text;
    let span = spec.end.get().and_then(|end| {
        let (from, to) = (offset(text, spec.pos)?, offset(text, end)?);
        text.get(from..to).filter(|s| !s.trim().is_empty())
    });
    // what a lowering rewrote keeps where it starts, not where it ends:
    // its line says more than its lowered form
    let line = || {
        let rest = text.get(offset(text, spec.pos)?..)?.lines().next()?;
        Some(rest.trim_end().trim_end_matches(';')).filter(|l| !l.is_empty())
    };
    match span {
        Some(s) => ArcStr::from(s.trim_end()),
        None => match spec.reads_lowered().then(line).flatten() {
            Some(l) => ArcStr::from(l),
            None => format_compact!("{spec}").as_str().into(),
        },
    }
}

/// Where an expression is written, and its text.
fn place_of(spec: &Expr) -> proto::Source {
    proto::Source { text: written(spec), line: spec.pos.line, column: spec.pos.column }
}

/// The statement step `step` of a seq runs, as written, where `binder`
/// is a `let` of a seq of `steps` statements.
fn seq_statement(binder: &Expr, step: usize, steps: usize) -> Option<ArcStr> {
    let ExprKind::Bind(b) = &binder.kind else { return None };
    let ExprKind::Seq(seq) = &b.value.kind else { return None };
    let stmt = seq.body.get(step).filter(|_| seq.body.len() == steps)?;
    Some(written(stmt))
}

fn source(s: &BindSource) -> proto::Source {
    proto::Source {
        text: written(&s.spec),
        line: s.spec.pos.line,
        column: s.spec.pos.column,
    }
}

impl<X: GXExt, W: AsyncWrite + Unpin> Session<X, W> {
    async fn send<T: Serialize>(&mut self, msg: &T) -> Result<()> {
        let mut line = serde_json::to_vec(msg)?;
        line.push(b'\n');
        self.out.write_all(&line).await?;
        Ok(self.out.flush().await?)
    }

    async fn run<R: AsyncBufRead + Unpin>(&mut self, lines: &mut Lines<R>) -> Result<()> {
        self.send(&proto::Hello { graphix_debug: proto::VERSION }).await?;
        let Some(hello) = lines.next_line().await? else { return Ok(()) };
        match serde_json::from_str::<proto::Hello>(&hello) {
            Ok(h) if h.graphix_debug == proto::VERSION => (),
            Ok(h) => bail!("the client speaks version {}", h.graphix_debug),
            Err(e) => bail!("the client's first line is no hello: {e}"),
        }
        loop {
            select! {
                line = lines.next_line() => match line? {
                    None => break Ok(()),
                    Some(line) if line.trim().is_empty() => (),
                    Some(line) => {
                        let response = match serde_json::from_str::<Request>(&line) {
                            Ok(req) => Response {
                                id: req.id,
                                result: match self.command(req.cmd).await {
                                    Ok(reply) => Outcome::Ok(reply),
                                    Err(e) => {
                                        Outcome::Err(format_compact!("{e:#}").as_str().into())
                                    }
                                },
                            },
                            Err(e) => Response {
                                id: 0,
                                result: Outcome::Err(
                                    format_compact!("not a request: {e}").as_str().into(),
                                ),
                            },
                        };
                        self.send(&response).await?
                    }
                },
                fired = self.events.recv() => match fired {
                    // the runtime ended
                    None => break Ok(()),
                    Some(fired) => self.fired(fired).await?,
                },
            }
        }
    }

    async fn fired(&mut self, fired: Fired) -> Result<()> {
        let event = match fired {
            Fired::Value { expr, value } => match &self.shown {
                Some(s) if s.expr.id == expr => proto::Event::Value {
                    eval: s.eval,
                    value: printed(&s.env, &s.expr.typ, &value),
                },
                Some(_) | None => return Ok(()),
            },
            Fired::Cycled { .. } | Fired::Held { .. } => return Ok(()),
            Fired::Watch { watch, bind, cycle, value } => proto::Event::Fired {
                watch,
                binding: handle(bind.id),
                name: bind.name.clone(),
                cycle,
                value: value.map(|v| printed(&self.env, &bind.typ, &v)),
            },
            Fired::Paused { cycle, by, delivered } => proto::Event::Paused {
                cycle,
                by: match by {
                    Paused::Asked => proto::Paused::Asked,
                    Paused::Stepped => proto::Paused::Stepped,
                    Paused::Break { watch } => proto::Paused::Break { watch },
                },
                delivered: delivered.as_ref().map(|l| self.cycle(l)),
            },
            Fired::Running => proto::Event::Running,
        };
        self.send(&event).await
    }

    fn cycle(&self, l: &Logged) -> proto::Cycle {
        let delivered = |(b, v): &(Arc<Bind>, Option<Value>)| proto::Delivered {
            binding: handle(b.id),
            name: b.name.clone(),
            value: v.as_ref().map(|v| printed(&self.env, &b.typ, v)),
        };
        proto::Cycle {
            cycle: l.cycle,
            delivered: l.delivered.iter().map(delivered).collect(),
            more: l.more,
        }
    }

    fn binding(&self, h: &Held) -> proto::Binding {
        let b = &h.bind;
        proto::Binding {
            id: handle(b.id),
            name: b.name.clone(),
            scope: format_compact!("{}", b.scope).as_str().into(),
            file: file_of(&b.ori),
            line: b.pos.line,
            column: b.pos.column,
            typ: (self.show)(&b.typ).as_str().into(),
            value: h.value.as_ref().map(|v| printed(&self.env, &b.typ, v)),
            written: h.written,
            instances: 1,
        }
    }

    fn why(&self, w: &Why) -> proto::Why {
        let s = w.source.as_ref();
        let by = match s.map(|s| s.by) {
            Some(BoundBy::Param) => proto::BoundBy::Param,
            Some(BoundBy::Pattern { arm }) => proto::BoundBy::Pattern { arm: arm + 1 },
            Some(BoundBy::Let | BoundBy::Fused) | None => proto::BoundBy::Let,
        };
        // fused code is where the nothing starts only if nothing it
        // reads lacks a value
        let waits = w.reads.iter().any(|r| r.held.value.is_none());
        let because = |s: &BindSource| self.reasons(&s.because, &s.spec, waits);
        proto::Why {
            binding: self.binding(&w.held),
            source: s.map(source),
            by,
            calls: s
                .map(|s| s.path.iter().map(|c| place_of(c)).collect())
                .unwrap_or_default(),
            because: s.map(because).unwrap_or_default(),
            reads: w.reads.iter().map(|r| self.why(r)).collect(),
            again: w.again,
        }
    }

    /// What the nodes of `binder`'s value say, for a person: each note a
    /// sentence over the part it is about. `waits` where a binding it
    /// reads has no value, when fused code has nothing to add.
    fn reasons(
        &self,
        because: &[Because],
        binder: &Expr,
        waits: bool,
    ) -> Vec<proto::Reason> {
        let told = |b: &Because| match b {
            Because::Reads(_) => None,
            Because::Of { note: Note::Fused, .. } if waits => None,
            Because::Of { spec, note, parts } => {
                let (text, says) = self.says(spec, note, binder);
                let parts = self.reasons(parts, binder, waits);
                Some(proto::Reason { text, says, parts })
            }
        };
        because.iter().filter_map(told).collect()
    }

    /// The part a note is about, as written, and what the note says.
    fn says(&self, spec: &Expr, note: &Note, binder: &Expr) -> (Option<ArcStr>, ArcStr) {
        let own = (!spec.reads_lowered() && !spec.is_lowered()).then(|| written(spec));
        let s = |t: CompactString| ArcStr::from(t.as_str());
        match note {
            Note::Never => (None, literal!("`never()` never has a value")),
            Note::Fused => (
                own,
                literal!("the nothing starts inside fused code: --no-fusion shows where"),
            ),
            Note::Sample { held: true } => (own, literal!("its trigger has not fired")),
            Note::Sample { held: false } => {
                (own, literal!("what it samples has no value"))
            }
            Note::Dropped { value: Value::Null, .. } => {
                (own, literal!("its operand is null, which it strips"))
            }
            Note::Dropped { value, raised } => {
                let error = match value {
                    Value::Error(e) => format_compact!("{}", Naked(e)),
                    v => format_compact!("{}", Naked(v)),
                };
                let did = if *raised { "`?` raised" } else { "`$` dropped" };
                (own, s(format_compact!("{did} the error {error}")))
            }
            Note::Select { arg, arm, arms } => {
                let arg = arg.as_ref().map(|(v, typ)| printed(&self.env, typ, v));
                let pattern = match (&spec.kind, arm) {
                    (ExprKind::Select(sel), Some(i)) => {
                        sel.arms.get(*i).map(|(p, _)| match &p.type_predicate {
                            Some(t) => {
                                format_compact!(": {t} as {}", p.structure_predicate)
                            }
                            None => format_compact!(": {}", p.structure_predicate),
                        })
                    }
                    _ => None,
                };
                let pattern = pattern.unwrap_or_default();
                let says = match (arm, arg) {
                    (Some(i), Some(arg)) => format_compact!(
                        "the select holds arm {} of {arms}{pattern}; its argument was last \
                         {arg}",
                        i + 1
                    ),
                    (Some(i), None) => {
                        format_compact!(
                            "the select holds arm {} of {arms}{pattern}",
                            i + 1
                        )
                    }
                    (None, Some(arg)) => format_compact!(
                        "the select holds no arm; its argument was last {arg}"
                    ),
                    (None, None) => {
                        format_compact!("the select's argument has had no value")
                    }
                };
                (None, s(says))
            }
            Note::Seq { step: None, .. } => {
                (None, literal!("the seq is not running: it waits for its trigger"))
            }
            Note::Seq { step: Some(i), steps } => {
                let says = format_compact!("the seq is at step {} of {steps}", i + 1);
                (seq_statement(binder, *i, *steps), s(says))
            }
            Note::Call { called: Reached::Nothing, fresh: true, .. } => (
                None,
                literal!("the fresh block has not started: its event has not fired"),
            ),
            Note::Call { awaits: true, .. } => {
                (own, literal!("it waits for the next event"))
            }
            Note::Call { called: Reached::Nothing, .. } => {
                (own, literal!("the call has not been made"))
            }
            Note::Call { called: Reached::Function, .. } => {
                (own, literal!("in this call"))
            }
            Note::Call { called: Reached::BuiltIn, .. } => {
                (own, literal!("the call has given no value"))
            }
        }
    }

    fn fused(&self, f: &Fused) -> Option<proto::Fused> {
        let (typ, value) = f.source.fused.as_ref()?;
        Some(proto::Fused {
            source: source(&f.source),
            typ: (self.show)(typ).as_str().into(),
            value: value.as_ref().map(|v| printed(&self.env, typ, v)),
            inputs: f.inputs.iter().map(|h| self.binding(h)).collect(),
        })
    }

    /// The bindings `target` names as they stand, and the environment
    /// they are printed under.
    async fn held(&mut self, target: &str) -> Result<LPooled<Vec<Held>>> {
        let target = CompactString::from(target);
        let read = self.attached.gx.with_ctx(move |ctx| -> Result<_> {
            let binds = resolve(&ctx.env, &target)?;
            let held = binds.iter().map(|b| held(ctx, b)).collect::<LPooled<Vec<_>>>();
            Ok((ctx.env.clone(), held))
        });
        let (env, held) = read.await??;
        self.env = env;
        Ok(held)
    }

    /// The one binding `target` names; where it names several, an error
    /// that lists them by id.
    async fn one(&mut self, target: &str) -> Result<Held> {
        let mut held = self.held(target).await?;
        if held.len() == 1 {
            return Ok(held.pop().unwrap());
        }
        let mut msg: LPooled<String> = LPooled::take();
        let _ =
            write!(msg, "{target} names {} bindings; name one by its id:", held.len());
        for h in held.iter().take(LISTED) {
            let b = self.binding(h);
            let file = b.file.as_deref().unwrap_or("");
            let _ =
                write!(msg, "\n  #{} {} at {file}:{}:{}", b.id, b.name, b.line, b.column);
            if let Some(v) = &b.value {
                let _ = write!(msg, " = {v}");
            }
        }
        bail!("{}", msg.as_str())
    }

    async fn compile(&mut self, text: ArcStr) -> Result<CompRes<X>> {
        let gx = self.attached.gx.clone();
        let session = self.attached.id;
        gx.exec(|res| ToGX::Debug(Op::Eval { session, text, rt: gx.clone(), res }))
            .await?
    }

    /// A token no cycle or hold this session waited for has had.
    fn token(&mut self) -> u64 {
        self.tokens += 1;
        self.tokens
    }

    /// Wait until a cycle has run after every write and compile this
    /// session has asked for ([`Self::until_cycled`]).
    async fn cycled(&mut self, capture: Option<ExprId>) -> Result<(Option<Value>, bool)> {
        let token = self.token();
        self.attached.op(Op::Cycle { session: self.attached.id, token })?;
        self.until_cycled(token, capture).await
    }

    /// Wait for the cycle that answers `token`, passing on what happens
    /// meanwhile. Returns what `capture` last produced before it ended,
    /// and whether the program was held, so the cycle delivered nothing
    /// of its own.
    async fn until_cycled(
        &mut self,
        token: u64,
        capture: Option<ExprId>,
    ) -> Result<(Option<Value>, bool)> {
        let mut captured = None;
        loop {
            match self.events.recv().await {
                None => bail!("the runtime ended"),
                Some(Fired::Cycled { token: t, held }) if t == token => {
                    break Ok((captured, held));
                }
                Some(Fired::Value { expr, value }) if Some(expr) == capture => {
                    captured = Some(value)
                }
                Some(fired) => self.fired(fired).await?,
            }
        }
    }

    /// The value of the expression `text`, evaluated once against the
    /// program as it stands, and how it prints.
    async fn value_of(&mut self, text: ArcStr) -> Result<(Value, ArcStr)> {
        let res = self.compile(text).await?;
        let Some(out) = res.exprs.last().filter(|e| e.output) else {
            bail!("a value is an expression")
        };
        let (Some(v), _) = self.cycled(Some(out.id)).await? else {
            bail!("the expression has no value")
        };
        let shown = printed(&res.env, &out.typ, &v);
        Ok((v, shown))
    }

    /// Watch the bindings `target` names; with `stop`, a break.
    async fn watch(&mut self, target: &str, stop: Option<Stop>) -> Result<Reply> {
        let held = self.held(target).await?;
        if held.len() > WATCHED {
            bail!("{target} names {} bindings, and a watch covers {WATCHED}", held.len())
        }
        self.made += 1;
        let (session, watch) = (self.attached.id, self.made);
        let ids = held.iter().map(|h| h.bind.id).collect();
        self.attached.op(Op::Watch { session, watch, ids, stop })?;
        let bindings = held.iter().map(|h| self.binding(h)).collect();
        Ok(Reply::Watching { watch, bindings })
    }

    async fn command(&mut self, cmd: Command) -> Result<Reply> {
        let gx = self.attached.gx.clone();
        let session = self.attached.id;
        match cmd {
            Command::Eval { text } => {
                let mut res = self.compile(text).await?;
                self.made += 1;
                let eval = self.made;
                self.env = res.env.clone();
                let Some(last) = res.exprs.pop() else {
                    self.shown = None;
                    return Ok(Reply::Evaled { eval, typ: None });
                };
                self.kept.extend(res.exprs.drain(..));
                let typ = last.output.then(|| (self.show)(&last.typ).as_str().into());
                match last.output {
                    true => self.shown = Some(Shown { eval, env: res.env, expr: last }),
                    false => {
                        self.shown = None;
                        self.kept.push(last)
                    }
                }
                self.cycled(None).await?;
                Ok(Reply::Evaled { eval, typ })
            }
            Command::Print { target } => {
                let held = match (self.held(&target).await, place(target.trim())) {
                    (Ok(held), _) => held,
                    (Err(e), None) => return Err(e),
                    (Err(e), Some((file, line, column))) => {
                        let file = PathBuf::from(file);
                        let ask =
                            |res| ToGX::Debug(Op::FusedAt { file, line, column, res });
                        let found = gx.exec(ask).await?;
                        let blocks: Vec<_> = found
                            .iter()
                            .take(LISTED)
                            .filter_map(|f| self.fused(f))
                            .collect();
                        return match blocks.is_empty() {
                            true => Err(e),
                            false => Ok(Reply::Fused { blocks }),
                        };
                    }
                };
                let more = held.len().saturating_sub(LISTED);
                let bindings =
                    held.iter().take(LISTED).map(|h| self.binding(h)).collect();
                Ok(Reply::Bindings { bindings, more })
            }
            Command::List { scope } => {
                let read = gx.with_ctx(move |ctx| {
                    let binds = ctx.env.by_id.iter().map(|(_, b)| b);
                    let mut binds = binds
                        .filter(|b| listed(&ctx.env, b, scope.as_deref()))
                        .cloned()
                        .collect::<LPooled<Vec<_>>>();
                    binds.sort_by(by_place);
                    let mut rows: LPooled<Vec<(Held, usize)>> = LPooled::take();
                    let mut last: Option<&Arc<Bind>> = None;
                    for b in binds.iter() {
                        match (last, rows.last_mut()) {
                            (Some(l), Some((_, n))) if same_place(l, b) => *n += 1,
                            _ => rows.push((held(ctx, b), 1)),
                        }
                        last = Some(b)
                    }
                    (ctx.env.clone(), rows)
                });
                let (env, rows) = read.await?;
                self.env = env;
                let more = rows.len().saturating_sub(LISTED);
                let row = |(h, n): &(Held, usize)| proto::Binding {
                    instances: *n,
                    ..self.binding(h)
                };
                let bindings = rows.iter().take(LISTED).map(row).collect();
                Ok(Reply::Bindings { bindings, more })
            }
            Command::Set { target, value } => {
                let h = self.one(&target).await?;
                let (v, shown) = self.value_of(value).await?;
                let (typ, tested) = (h.bind.typ.clone(), v.clone());
                if !gx.with_ctx(move |ctx| typ.is_a(&ctx.env, &tested)).await? {
                    let typ = (self.show)(&h.bind.typ);
                    bail!("{shown} is not a {typ}: {} is not set", h.bind.name)
                }
                let token = self.token();
                self.attached.op(Op::Write { session, id: h.bind.id, v, token })?;
                let (_, held) = self.until_cycled(token, None).await?;
                Ok(Reply::Set { binding: self.binding(&h), value: shown, held })
            }
            Command::Watch { target } => self.watch(&target, None).await,
            Command::Break { target, when } => {
                let stop = match when {
                    When::Value => Stop::Fires,
                    When::NoValue => Stop::NoValue,
                    When::Equals { value } => Stop::Equals(self.value_of(value).await?.0),
                };
                self.watch(&target, Some(stop)).await
            }
            Command::Pause => {
                let cycle =
                    gx.exec(|res| ToGX::Debug(Op::Pause { session, res })).await?;
                Ok(Reply::Paused { cycle })
            }
            Command::Wait => {
                let token = self.token();
                self.attached.op(Op::WhenHeld { session, token })?;
                loop {
                    match self.events.recv().await {
                        None => bail!("the runtime ended"),
                        Some(Fired::Held { token: t, cycle }) if t == token => {
                            break Ok(Reply::Paused { cycle });
                        }
                        Some(fired) => self.fired(fired).await?,
                    }
                }
            }
            Command::Continue => {
                self.attached.op(Op::Continue { session })?;
                Ok(Reply::Running)
            }
            Command::Step { cycles } => {
                self.attached.op(Op::Step { session, cycles: cycles.unwrap_or(1) })?;
                Ok(Reply::Stepping)
            }
            Command::Cycles { last } => {
                let last = last.unwrap_or(10);
                let log = gx.exec(|res| ToGX::Debug(Op::Cycles { last, res })).await?;
                self.env = gx.get_env().await?;
                Ok(Reply::Cycles { cycles: log.iter().map(|l| self.cycle(l)).collect() })
            }
            Command::Unwatch { watch } => {
                match gx
                    .exec(|res| ToGX::Debug(Op::Unwatch { session, watch, res }))
                    .await?
                {
                    true => Ok(Reply::Unwatched),
                    false => bail!("this session has no watch {watch}"),
                }
            }
            Command::Why { target } => {
                let id = self.one(&target).await?.bind.id;
                let why = gx.exec(|res| ToGX::Debug(Op::Why { id, res })).await?;
                let why = why.ok_or_else(|| anyhow!("{target} is gone"))?;
                Ok(Reply::Why { why: self.why(&why) })
            }
            Command::Info => {
                let read = gx.with_ctx(|ctx| {
                    let rt = &ctx.rt;
                    let (cycle, timers, tasks) =
                        (rt.cycle, rt.timers.len(), rt.tasks.len());
                    (cycle, ctx.env.by_id.len(), ctx.fusion.fused(), timers, tasks)
                });
                let (cycle, bindings, fused, timers, tasks) = read.await?;
                let Counts { sessions, watches } =
                    gx.exec(|res| ToGX::Debug(Op::Counts { res })).await?;
                let info = proto::Info {
                    cycle,
                    bindings,
                    fused,
                    timers,
                    tasks,
                    sessions,
                    watches,
                };
                Ok(Reply::Info { info })
            }
        }
    }
}
