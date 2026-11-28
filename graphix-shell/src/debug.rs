//! The shell's side of the debugger (`design/debugger.md`): the socket
//! `--debug` offers, and `graphix debug`, the client a person types at.

use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use graphix_rt::{
    GXExt, GXHandle,
    debug::{
        Client, Debugger,
        proto::{
            Binding, BoundBy, Command, Cycle, Event, Fused, Info, Outcome, Paused,
            Reason, Reply, Source, When, Why,
        },
    },
};
use log::warn;
use poolshark::local::LPooled;
use reedline::{DefaultPrompt, DefaultPromptSegment, ExternalPrinter, Reedline, Signal};
use serde_derive::Serialize;
use std::{
    fmt::Write as _,
    future::Future,
    io::{self, BufRead, IsTerminal, Write},
    path::{Path, PathBuf},
    process::Stdio,
    sync::{Arc, mpsc as smpsc},
    time::Duration,
};
use tokio::{
    io::{AsyncBufReadExt, AsyncRead, BufReader, Lines},
    select,
    sync::mpsc,
};

const HELP: &str = "\
A line that is none of these commands is Graphix, evaluated in the running
program as the REPL would: `x + 1`, `let y = x * 2`, `x <- 0`.

  eval EXPR           evaluate Graphix (for a line that starts with a command's name)
  print TARGET        a binding's type, value and the cycle it was last written
  list [MODULE|FILE]  the program's bindings
  set TARGET = EXPR   deliver a value to a binding
  watch TARGET        report each fire of a binding
  unwatch N           stop watch N
  why TARGET          why a binding has no value, or what it waits on
  info                the cycle, and what is pending
  pause               hold the program: it runs no cycle until continue or step
  continue            let it run
  step [N]            run until N more cycles (1) have done work, then hold it
  break TARGET        hold it when the binding is written;
                      `break T == EXPR` at that value, `break T none` when it
                      goes to no value. unwatch N removes it
  cycles [N]          what each of the last N cycles (10) delivered
  wait [SECONDS]      wait until the program is held (by a break or a step),
                      at most that long
  help, quit

A TARGET is a name (x, m::x), a place (file.gx:12 or file.gx:12:5), or a
binding's id as an answer shows it (#88).
";

/// The longest value a list or a why shows.
const BRIEF: usize = 72;
/// How long a `step` or a `wait` may take where commands are given or
/// piped in and nobody is there to end it.
const SCRIPT_LIMIT: Duration = Duration::from_secs(30);
/// How long a script waits for a program to offer its debugger (or its
/// limit, where that is shorter), and how often it looks.
const ATTACH_WAIT: Duration = Duration::from_secs(5);
const ATTACH_EVERY: Duration = Duration::from_millis(50);
/// The most lines of a source a why shows.
const SOURCE_LINES: usize = 4;

/// The directory the shell's debugger sockets live in.
pub fn socket_dir() -> PathBuf {
    match dirs::runtime_dir() {
        Some(dir) => dir.join("graphix"),
        None => {
            let user = std::env::var("USER").or_else(|_| std::env::var("USERNAME"));
            let user = user.unwrap_or_else(|_| "user".into());
            std::env::temp_dir().join(format_compact!("graphix-{user}").as_str())
        }
    }
}

/// Where the process `pid` offers its debugger when it is given no path.
#[cfg(unix)]
pub fn socket_of(pid: u32) -> PathBuf {
    socket_dir().join(format_compact!("debug-{pid}.sock").as_str())
}

#[cfg(windows)]
pub fn socket_of(pid: u32) -> PathBuf {
    PathBuf::from(format_compact!(r"\\.\pipe\graphix-debug-{pid}").as_str())
}

/// The shell's debugger of `gx`, which says `program` of it.
fn debugger<X: GXExt>(gx: &GXHandle<X>, program: Option<ArcStr>) -> Debugger<X> {
    let debugger = Debugger::new(gx.clone()).with_types(graphix_lsp::display_type);
    match program {
        Some(name) => debugger.named(name),
        None => debugger,
    }
}

/// What a shell offers its debugger on, given back when it is dropped:
/// a unix socket's file is removed. A pipe goes with its last handle.
pub(crate) struct Offered {
    #[cfg(unix)]
    socket: PathBuf,
}

#[cfg(unix)]
impl Drop for Offered {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.socket);
    }
}

/// Offer `gx`'s debugger at `path`: a unix socket the owner alone may
/// open, in a directory made the owner's alone.
#[cfg(unix)]
pub(crate) fn offer<X: GXExt>(
    gx: &GXHandle<X>,
    path: &Path,
    program: Option<ArcStr>,
) -> Result<Offered> {
    use std::{
        fs::{self, DirBuilder, Permissions},
        os::unix::fs::{DirBuilderExt, FileTypeExt, PermissionsExt},
    };
    if let Some(dir) = path.parent().filter(|d| !d.as_os_str().is_empty()) {
        let made = DirBuilder::new().recursive(true).mode(0o700).create(dir);
        made.with_context(|| format!("making {}", dir.display()))?;
    }
    if let Ok(meta) = fs::symlink_metadata(path) {
        if !meta.file_type().is_socket() {
            bail!("{} exists and is no socket", path.display())
        }
        if std::os::unix::net::UnixStream::connect(path).is_ok() {
            bail!("a program already offers its debugger at {}", path.display())
        }
        fs::remove_file(path)?;
    }
    let listener = tokio::net::UnixListener::bind(path)
        .with_context(|| format!("binding {}", path.display()))?;
    fs::set_permissions(path, Permissions::from_mode(0o600))?;
    let debugger = debugger(gx, program);
    tokio::spawn(async move {
        if let Err(e) = debugger.listen_unix(listener).await {
            warn!("the debugger stopped listening: {e:#}")
        }
    });
    eprintln!("debugger: graphix debug {}", path.display());
    Ok(Offered { socket: path.to_path_buf() })
}

// CR claude for claude: [risk/medium] The Windows side of the debugger was written
// with no Windows toolchain on the box: this `offer`, `open`, `socket_of` and
// `offered` under cfg(windows), and `Debugger::listen_pipe` in graphix-rt, have never
// been compiled or run. Build the workspace for a Windows target, then run the two
// tests in graphix-shell/tests/shell/debugger.rs there (the second is cfg(unix): it
// needs a Windows twin that names the pipe by process id). Also decide the pipe's
// security: the default descriptor lets the creator, administrators and SYSTEM
// connect, and `offered` cannot list pipes, so `graphix debug` with no target finds
// nothing on Windows. Found by reading; not run. (debugger-01)
// 2026-11-27 claude: Compiled: on washu-chan, which has the x86_64-pc-windows-gnu
// target, `cargo check --target x86_64-pc-windows-gnu -p graphix-rt -p graphix-shell
// --tests` is clean, after one fix it found (the offered socket's path was a dead
// field on Windows). Still never run: there is no Windows machine or wine on either
// box. What remains is the run, the pipe's security, and `graphix debug` with no
// target.
/// Offer `gx`'s debugger on the named pipe `path`.
#[cfg(windows)]
pub(crate) fn offer<X: GXExt>(
    gx: &GXHandle<X>,
    path: &Path,
    program: Option<ArcStr>,
) -> Result<Offered> {
    use tokio::net::windows::named_pipe::ServerOptions;
    let name = path.to_path_buf();
    let first = ServerOptions::new()
        .first_pipe_instance(true)
        .create(&name)
        .with_context(|| format!("making the pipe {}", path.display()))?;
    let debugger = debugger(gx, program);
    tokio::spawn(async move {
        let next = move || ServerOptions::new().create(&name);
        if let Err(e) = debugger.listen_pipe(first, next).await {
            warn!("the debugger stopped listening: {e:#}")
        }
    });
    eprintln!("debugger: graphix debug {}", path.display());
    Ok(Offered {})
}

#[cfg(unix)]
type Stream = tokio::net::UnixStream;
#[cfg(windows)]
type Stream = tokio::net::windows::named_pipe::NamedPipeClient;

#[cfg(unix)]
async fn open(path: &Path) -> io::Result<Stream> {
    Stream::connect(path).await
}

#[cfg(windows)]
async fn open(path: &Path) -> io::Result<Stream> {
    tokio::net::windows::named_pipe::ClientOptions::new().open(path)
}

/// A program that answers on its socket: the session opened to it, in
/// which it said what it is.
struct Offering {
    client: Client<Stream>,
}

/// The programs that answer on the sockets in [`socket_dir`].
#[cfg(unix)]
async fn offerings() -> Vec<Offering> {
    let mut live = Vec::new();
    let Ok(dir) = std::fs::read_dir(socket_dir()) else { return live };
    let mut sockets = dir.filter_map(|e| e.ok()).map(|e| e.path()).collect::<Vec<_>>();
    sockets.sort();
    for path in sockets {
        let name = path.file_name().unwrap_or_default().to_string_lossy();
        if name.starts_with("debug-")
            && name.ends_with(".sock")
            && let Ok(stream) = open(&path).await
            && let Ok(client) = Client::connect(stream).await
        {
            live.push(Offering { client })
        }
    }
    live
}

#[cfg(windows)]
async fn offerings() -> Vec<Offering> {
    Vec::new()
}

#[cfg(unix)]
fn is_socket(path: &Path) -> bool {
    use std::os::unix::fs::FileTypeExt;
    std::fs::symlink_metadata(path).is_ok_and(|m| m.file_type().is_socket())
}

#[cfg(windows)]
fn is_socket(path: &Path) -> bool {
    path.to_string_lossy().starts_with(r"\\.\pipe\")
}

/// How to name `o` to `graphix debug`, and what it is.
fn described(o: &Offering) -> CompactString {
    let Some(p) = o.client.program() else {
        return CompactString::const_new("a program that does not say what it is");
    };
    let cwd = p.cwd.as_deref().unwrap_or("");
    let name = p.name.as_deref().unwrap_or("a program");
    let name =
        Path::new(name).strip_prefix(cwd).ok().and_then(|n| n.to_str()).unwrap_or(name);
    format_compact!("graphix debug {}    # {name} in {cwd}", p.pid)
}

/// Why no program was attached to.
enum Missed {
    /// None answers yet: one that was just started may.
    NotYet(anyhow::Error),
    /// Waiting will not change it.
    Never(anyhow::Error),
}

/// A session with the program `target` names: a process id, a socket,
/// the file a program runs or the directory it runs in; with none, the
/// one program that offers a debugger, or of several the one that runs
/// in this directory.
async fn find(target: Option<&str>) -> Result<Client<Stream>, Missed> {
    let direct = |path: PathBuf| async move {
        let stream = open(&path).await;
        let stream = stream.with_context(|| format!("connecting to {}", path.display()));
        Client::connect(stream.map_err(Missed::NotYet)?).await.map_err(Missed::Never)
    };
    let real = |path: &Path| path.canonicalize().ok();
    let at = match target {
        None => None,
        Some(t) => match (t.parse::<u32>(), real(Path::new(t))) {
            (Ok(pid), _) => return direct(socket_of(pid)).await,
            (Err(_), Some(at)) if !is_socket(Path::new(t)) => Some(at),
            // a socket, there or not yet
            (Err(_), Some(_) | None) => return direct(PathBuf::from(t)).await,
        },
    };
    let mut live = offerings().await;
    let is_at = |o: &Offering, at: &Path| {
        let said = o.client.program();
        let named = |said: Option<&str>| said.is_some_and(|s| Path::new(s) == at);
        said.is_some_and(|p| named(p.name.as_deref()) || named(p.cwd.as_deref()))
    };
    let here = std::env::current_dir().ok().and_then(|d| real(&d));
    let chosen = |live: &[Offering], at: Option<&Path>| -> Vec<usize> {
        let all = 0..live.len();
        match at {
            Some(at) => all.filter(|i| is_at(&live[*i], at)).collect(),
            None => all.collect(),
        }
    };
    let mut picked = chosen(&live, at.as_deref());
    if at.is_none() && picked.len() > 1 {
        let here = chosen(&live, here.as_deref());
        if here.len() == 1 {
            picked = here
        }
    }
    match (picked.as_slice(), &at) {
        (&[i], _) => Ok(live.swap_remove(i).client),
        (&[], Some(at)) => Err(Missed::NotYet(anyhow!(
            "no program that runs {} offers a debugger",
            at.display()
        ))),
        (&[], None) => Err(Missed::NotYet(anyhow!(
            "no program offers a debugger: run one with `graphix --debug FILE`, or name \
             its file, its directory, its process id or its socket"
        ))),
        (several, _) => {
            let mut msg: LPooled<String> = LPooled::take();
            msg.push_str("several programs offer a debugger; name one:");
            for i in several {
                let _ = write!(msg, "\n  {}", described(&live[*i]));
            }
            Err(Missed::Never(anyhow!("{}", msg.as_str())))
        }
    }
}

enum Line {
    Empty,
    Help,
    Quit,
    /// `wait [SECONDS]`: until the program is held, or the limit.
    Wait(Option<Duration>),
    Ask(Command),
}

/// What a line a person typed asks.
fn parse(line: &str) -> Result<Line> {
    let line = line.trim();
    let (word, rest) = line.split_once(char::is_whitespace).unwrap_or((line, ""));
    let rest = rest.trim();
    let target = |usage: &str| match rest.is_empty() {
        true => bail!("usage: {usage}"),
        false => Ok(CompactString::from(rest)),
    };
    Ok(match word {
        "" => Line::Empty,
        "help" | "?" if rest.is_empty() => Line::Help,
        "quit" | "exit" if rest.is_empty() => Line::Quit,
        "info" if rest.is_empty() => Line::Ask(Command::Info),
        "eval" => Line::Ask(Command::Eval { text: target("eval EXPR")?.as_str().into() }),
        "print" => Line::Ask(Command::Print { target: target("print TARGET")? }),
        "watch" => Line::Ask(Command::Watch { target: target("watch TARGET")? }),
        "why" => Line::Ask(Command::Why { target: target("why TARGET")? }),
        "list" => Line::Ask(Command::List {
            scope: (!rest.is_empty()).then(|| CompactString::from(rest)),
        }),
        "wait" if rest.is_empty() => Line::Wait(None),
        "wait" => match rest.parse::<f64>() {
            Ok(secs) if secs.is_finite() && secs > 0. => {
                Line::Wait(Some(Duration::from_secs_f64(secs)))
            }
            Ok(_) | Err(_) => bail!("usage: wait [SECONDS]"),
        },
        "pause" if rest.is_empty() => Line::Ask(Command::Pause),
        "continue" if rest.is_empty() => Line::Ask(Command::Continue),
        "step" | "cycles" if !rest.is_empty() && rest.parse::<u64>().is_err() => {
            bail!("usage: {word} [N]")
        }
        "step" => Line::Ask(Command::Step { cycles: rest.parse().ok() }),
        "cycles" => Line::Ask(Command::Cycles { last: rest.parse().ok() }),
        "break" => {
            let (target, when) = match (rest.split_once("=="), rest.strip_suffix(" none"))
            {
                (Some((t, v)), _) if !v.trim().is_empty() => {
                    (t.trim(), When::Equals { value: v.trim().into() })
                }
                (Some(_), _) => bail!("usage: break TARGET == EXPR"),
                (None, Some(t)) => (t.trim(), When::NoValue),
                (None, None) => (rest, When::Value),
            };
            match target.is_empty() {
                true => bail!("usage: break TARGET [== EXPR | none]"),
                false => Line::Ask(Command::Break { target: target.into(), when }),
            }
        }
        "unwatch" => match rest.parse::<u64>() {
            Ok(watch) => Line::Ask(Command::Unwatch { watch }),
            Err(_) => bail!("usage: unwatch N"),
        },
        "set" => match rest.split_once('=') {
            Some((t, v)) if !t.trim().is_empty() && !v.trim().is_empty() => {
                Line::Ask(Command::Set {
                    target: t.trim().into(),
                    value: v.trim().into(),
                })
            }
            Some(_) | None => bail!("usage: set TARGET = EXPR"),
        },
        _ => Line::Ask(Command::Eval { text: line.into() }),
    })
}

fn brief(v: &str, max: Option<usize>) -> CompactString {
    match max {
        Some(max) if v.chars().count() > max || v.contains('\n') => {
            let cut = v.lines().next().unwrap_or("");
            let cut = cut.char_indices().nth(max).map_or(cut, |(i, _)| &cut[..i]);
            format_compact!("{cut}…")
        }
        Some(_) | None => CompactString::from(v),
    }
}

/// `file` from the directory this client runs in, where it is under it.
fn near(file: &str) -> &str {
    let here = std::env::current_dir().ok();
    let rel = here.as_deref().and_then(|d| Path::new(file).strip_prefix(d).ok());
    rel.and_then(|p| p.to_str()).unwrap_or(file)
}

fn show_binding(out: &mut String, b: &Binding, max: Option<usize>) {
    let _ = write!(out, "{}: {}", b.name, b.typ);
    match &b.value {
        Some(v) => {
            let _ = write!(out, " = {}", brief(v, max));
        }
        None => out.push_str(" has no value"),
    }
    let _ = write!(out, "    #{}", b.id);
    if let Some(file) = &b.file {
        let _ = write!(out, " {}:{}:{}", near(file), b.line, b.column);
    }
    match (b.written, &b.value) {
        (Some(cycle), Some(_)) => {
            let _ = write!(out, ", written in cycle {cycle}");
        }
        (Some(cycle), None) => {
            let _ = write!(out, ", none since cycle {cycle}");
        }
        (None, _) => out.push_str(", never written"),
    }
    if b.instances > 1 {
        let _ = write!(out, ", {} instances", b.instances);
    }
    out.push('\n')
}

fn show_source(out: &mut String, s: &Source, pad: usize) {
    for (i, line) in s.text.lines().enumerate() {
        if i == SOURCE_LINES {
            let _ = writeln!(out, "{:pad$}…", "");
            break;
        }
        let _ = writeln!(out, "{:pad$}{}", "", line.trim_end());
    }
}

fn show_fused(out: &mut String, f: &Fused) {
    let _ = write!(
        out,
        "a fused block at line {}, column {}: {}",
        f.source.line, f.source.column, f.typ
    );
    match &f.value {
        Some(v) => {
            let _ = writeln!(out, " = {}", brief(v, Some(BRIEF)));
        }
        None => out.push_str(" has no value\n"),
    }
    show_source(out, &f.source, 2);
    match f.inputs.is_empty() {
        true => out.push_str("  it reads nothing\n"),
        false => out.push_str("  it reads:\n"),
    }
    for b in &f.inputs {
        out.push_str("    ");
        show_binding(out, b, Some(BRIEF))
    }
}

/// The first line of a text a reason is about.
fn first_line(text: &str) -> CompactString {
    let line = text.lines().next().unwrap_or("").trim_end();
    match text.lines().nth(1) {
        Some(_) => format_compact!("{line} …"),
        None => brief(line, Some(BRIEF)),
    }
}

fn show_reasons(out: &mut String, reasons: &[Reason], pad: usize) {
    for r in reasons {
        match &r.text {
            Some(text) => {
                let _ = writeln!(out, "{:pad$}{}: {}", "", r.says, first_line(text));
            }
            None => {
                let _ = writeln!(out, "{:pad$}{}", "", r.says);
            }
        }
        show_reasons(out, &r.parts, pad + 2)
    }
}

fn show_why(out: &mut String, w: &Why, depth: usize) {
    let pad = depth * 2;
    let _ = write!(out, "{:pad$}", "");
    show_binding(out, &w.binding, Some(BRIEF));
    let pad = pad + 2;
    if w.again {
        let _ = writeln!(out, "{:pad$}met above: the program feeds it to itself", "");
        return;
    }
    for call in &w.calls {
        let _ = writeln!(
            out,
            "{:pad$}inside the call at line {}: {}",
            "",
            call.line,
            first_line(&call.text)
        );
    }
    match &w.source {
        Some(s) => {
            match &w.by {
                BoundBy::Let => (),
                BoundBy::Param => {
                    let _ =
                        writeln!(out, "{:pad$}a parameter, which this call passes:", "");
                }
                BoundBy::Pattern { arm } => {
                    let _ = writeln!(
                        out,
                        "{:pad$}a name in the pattern of arm {arm} of this select:",
                        ""
                    );
                }
            }
            show_source(out, s, pad);
            show_reasons(out, &w.because, pad);
            if !w.reads.is_empty() {
                let _ = writeln!(out, "{:pad$}it reads:", "");
                for r in &w.reads {
                    show_why(out, r, depth + 2)
                }
            }
            let waits = w.reads.iter().any(|r| r.binding.value.is_none());
            if w.binding.value.is_none() && !waits && w.because.is_empty() {
                let _ = writeln!(
                    out,
                    "{:pad$}everything it reads has a value: the nothing starts in this \
                     expression",
                    ""
                );
            }
        }
        None if w.binding.value.is_none() => {
            let _ = writeln!(out, "{:pad$}nothing in the program's nodes binds it", "");
        }
        None => (),
    }
}

fn show_info(out: &mut String, i: &Info) {
    let state = match (i.held, i.stepping) {
        (true, _) => CompactString::const_new("held"),
        (false, 0) => CompactString::const_new("running"),
        (false, n) => format_compact!("stepping, {n} cycles of work to go"),
    };
    let _ = writeln!(
        out,
        "cycle {}, {state}; {} bindings; {} fused blocks; {} timers and {} tasks pending; \
         {} sessions",
        i.cycle, i.bindings, i.fused, i.timers, i.tasks, i.sessions
    );
    for w in &i.watching {
        let _ = match &w.breaks {
            None => writeln!(out, "watch {}: {}", w.watch, w.name),
            Some(When::Value) => writeln!(out, "break {}: {}", w.watch, w.name),
            Some(When::NoValue) => writeln!(out, "break {}: {} none", w.watch, w.name),
            Some(When::Equals { value }) => {
                writeln!(out, "break {}: {} == {value}", w.watch, w.name)
            }
        };
    }
}

fn show_reply(out: &mut String, reply: &Reply) {
    match reply {
        Reply::Evaled { typ: Some(typ), .. } => {
            let _ = writeln!(out, "-: {typ}");
        }
        Reply::Evaled { typ: None, .. } | Reply::Unwatched => (),
        Reply::Bindings { bindings, more } => {
            let max = (bindings.len() > 1).then_some(BRIEF);
            for b in bindings {
                show_binding(out, b, max)
            }
            if *more > 0 {
                let _ = writeln!(out, "… and {more} more");
            }
        }
        Reply::Fused { blocks } => {
            out.push_str("fused code keeps no binding here; it is inside:\n");
            for f in blocks {
                show_fused(out, f)
            }
        }
        Reply::Set { binding, value, held: false } => {
            let _ = writeln!(out, "{} <- {value}", binding.name);
        }
        Reply::Set { binding, value, held: true } => {
            let _ = writeln!(
                out,
                "{} <- {value}    the program is held: it lands when it runs",
                binding.name
            );
        }
        Reply::Paused { cycle } => {
            let _ = writeln!(out, "held before cycle {cycle}");
        }
        Reply::Running => out.push_str("running\n"),
        Reply::Stepping => (),
        Reply::Cycles { cycles } => {
            for c in cycles {
                show_cycle(out, c);
                out.push('\n')
            }
        }
        Reply::Watching { watch, bindings } => {
            for b in bindings {
                let _ = write!(out, "watch {watch}: ");
                show_binding(out, b, Some(BRIEF))
            }
        }
        Reply::Why { why } => show_why(out, why, 0),
        Reply::Info { info } => show_info(out, info),
    }
}

/// `cycle N: a = 1, b has no value`.
fn show_cycle(out: &mut String, c: &Cycle) {
    let _ = write!(out, "cycle {}:", c.cycle);
    for (i, d) in c.delivered.iter().enumerate() {
        out.push_str(if i == 0 { " " } else { ", " });
        match &d.value {
            Some(v) => {
                let _ = write!(out, "{} = {}", d.name, brief(v, Some(BRIEF)));
            }
            None => {
                let _ = write!(out, "{} has no value", d.name);
            }
        }
    }
    if c.more > 0 {
        let _ = write!(out, ", and {} more", c.more);
    }
}

fn show_event(out: &mut String, event: &Event) {
    match event {
        Event::Running => out.push_str("running: another session continued the program"),
        Event::Paused { cycle, by: Paused::Asked, .. } => {
            let _ = write!(out, "held before cycle {cycle} by another session");
        }
        Event::Paused { cycle, by, delivered } => {
            if let Paused::Break { watch } = by {
                let _ = write!(out, "break {watch}: ");
            }
            match delivered {
                Some(c) => {
                    out.push_str("held after ");
                    show_cycle(out, c)
                }
                None => {
                    let _ = write!(out, "held after cycle {cycle}");
                }
            }
        }
        Event::Value { value, .. } => out.push_str(value),
        Event::Fired { watch, name, cycle, value: Some(v), .. } => {
            let _ = write!(out, "{name} = {v}    watch {watch}, cycle {cycle}");
        }
        Event::Fired { watch, name, cycle, value: None, .. } => {
            let _ = write!(out, "{name} has no value    watch {watch}, cycle {cycle}");
        }
    }
}

/// How a client runs its commands.
#[derive(Clone, Copy)]
struct How {
    /// Print each answer and event as a line of JSON.
    json: bool,
    /// How long a `step` or a `wait` may take; `None` for as long as it
    /// does.
    limit: Option<Duration>,
    /// Commands are run one after another with nobody to end a wait: a
    /// `step` is waited out before the next.
    script: bool,
}

/// `f`, or `None` where it takes longer than `limit`.
async fn within<T>(
    limit: Option<Duration>,
    f: impl Future<Output = Result<T>>,
) -> Result<Option<T>> {
    match limit {
        None => f.await.map(Some),
        Some(limit) => match tokio::time::timeout(limit, f).await {
            Ok(r) => r.map(Some),
            Err(_) => Ok(None),
        },
    }
}

/// The refusal of a wait that outlasted `limit`: `what` is so, and
/// `then` what follows.
fn outlasted(limit: Option<Duration>, what: &str, then: &str) -> Outcome {
    let secs = limit.map_or(0., |d| d.as_secs_f64());
    Outcome::Err(format_compact!("{what} after {secs} s{then}").as_str().into())
}

/// Wait until the program is held, at most `limit`: a refusal where it
/// is still running then.
async fn held(client: &mut Client<Stream>, limit: Option<Duration>) -> Result<Outcome> {
    match within(limit, client.ask(Command::Wait)).await? {
        Some(outcome) => Ok(outcome),
        None => Ok(outlasted(limit, "the program is still running", "")),
    }
}

/// Ask `cmd`. In a script a `step` is waited out, and one that finds no
/// work within the limit is ended: the program is held, as after a step.
async fn ask(client: &mut Client<Stream>, cmd: Command, how: How) -> Result<Outcome> {
    let stepped = how.script && matches!(cmd, Command::Step { .. });
    let outcome = client.ask(cmd).await?;
    if !stepped || !matches!(outcome, Outcome::Ok(Reply::Stepping)) {
        return Ok(outcome);
    }
    match within(how.limit, client.ask(Command::Wait)).await? {
        Some(Outcome::Ok(_)) => Ok(outcome),
        Some(refusal) => Ok(refusal),
        None => {
            client.ask(Command::Pause).await?;
            let what = "no cycle has done the program's work";
            Ok(outlasted(how.limit, what, ": the step is ended and the program held"))
        }
    }
}

/// One line of JSON for what `line` was answered.
#[derive(Serialize)]
struct Answered<'a> {
    cmd: &'a str,
    #[serde(flatten)]
    outcome: &'a Outcome,
}

/// Run one line a person typed and print what it answers and the events
/// that came before the answer. `Ok(None)` at `quit`; else whether the
/// line was answered and not refused.
async fn run_line(
    client: &mut Client<Stream>,
    line: &str,
    how: How,
) -> Result<Option<bool>> {
    let mut out: LPooled<String> = LPooled::take();
    let (outcome, waited) = match parse(line) {
        Err(e) => (Outcome::Err(format_compact!("{e:#}").as_str().into()), false),
        Ok(Line::Empty) => return Ok(Some(true)),
        Ok(Line::Quit) => return Ok(None),
        Ok(Line::Help) => {
            print!("{HELP}");
            return Ok(Some(true));
        }
        Ok(Line::Wait(limit)) => (held(client, limit.or(how.limit)).await?, true),
        Ok(Line::Ask(cmd)) => (ask(client, cmd, how).await?, false),
    };
    let ok = matches!(outcome, Outcome::Ok(_));
    match how.json {
        true => {
            // in the order they came: the events, then the answer
            while let Some(e) = client.queued() {
                let _ = writeln!(out, "{}", serde_json::to_string(&e)?);
            }
            let answered = Answered { cmd: line.trim(), outcome: &outcome };
            let _ = writeln!(out, "{}", serde_json::to_string(&answered)?);
        }
        false => {
            match &outcome {
                // the hold a wait waited for was told before its answer
                Outcome::Ok(Reply::Paused { .. }) if waited && client.has_queued() => (),
                Outcome::Ok(reply) => show_reply(&mut out, reply),
                Outcome::Err(e) => {
                    let _ = writeln!(out, "error: {e}");
                }
            }
            while let Some(e) = client.queued() {
                show_event(&mut out, &e);
                out.push('\n')
            }
        }
    }
    let mut stdout = io::stdout().lock();
    stdout.write_all(out.as_bytes())?;
    stdout.flush()?;
    Ok(Some(ok))
}

/// The lines a person types at a terminal, each with the signal that the
/// line has been answered and the next may be read; `None` at the end.
fn typed(
    printer: ExternalPrinter<String>,
) -> mpsc::UnboundedReceiver<(String, smpsc::Sender<()>)> {
    let (tx, rx) = mpsc::unbounded_channel();
    std::thread::spawn(move || {
        let mut editor = Reedline::create().with_external_printer(printer);
        let prompt = DefaultPrompt {
            left_prompt: DefaultPromptSegment::Basic("debug".into()),
            right_prompt: DefaultPromptSegment::Empty,
        };
        loop {
            match editor.read_line(&prompt) {
                Ok(Signal::Success(line)) => {
                    let (done, answered) = smpsc::channel();
                    if tx.send((line, done)).is_err() || answered.recv().is_err() {
                        break;
                    }
                }
                Ok(Signal::CtrlC) => (),
                Ok(_) | Err(_) => break,
            }
        }
    });
    rx
}

async fn at_terminal(
    client: &mut Client<Stream>,
    how: How,
    printer: ExternalPrinter<String>,
) -> Result<()> {
    let mut lines = typed(printer.clone());
    let ended = loop {
        select! {
            line = lines.recv() => match line {
                None => break false,
                Some((line, done)) => {
                    // the interrupt key abandons a command that waits
                    let ran = select! {
                        ran = run_line(client, &line, how) => ran?,
                        _ = tokio::signal::ctrl_c() => {
                            println!("abandoned: the program goes on as it was");
                            Some(false)
                        }
                    };
                    match ran {
                        None => break false,
                        Some(_) => drop(done.send(())),
                    }
                }
            },
            event = client.event() => match event? {
                None => break true,
                Some(e) => {
                    let mut out = String::new();
                    match how.json {
                        true => out.push_str(&serde_json::to_string(&e)?),
                        false => show_event(&mut out, &e),
                    }
                    let _ = printer.print(out);
                }
            },
        }
    };
    // the editor's thread may hold the terminal raw inside a read
    let _ = crossterm::terminal::disable_raw_mode();
    match ended {
        true => println!("\nthe program ended"),
        // what a session leaves is the program's: say so where it is held
        false => {
            if let Ok(Outcome::Ok(Reply::Info { info })) = client.ask(Command::Info).await
                && info.held
            {
                println!("the program is held: `graphix debug -c continue` lets it run")
            }
        }
    }
    Ok(())
}

/// Print each event as it comes, until the program ends or `limit`.
async fn follow(client: &mut Client<Stream>, how: How) -> Result<()> {
    let events = async {
        while let Some(e) = client.event().await? {
            let mut out: LPooled<String> = LPooled::take();
            match how.json {
                true => out.push_str(&serde_json::to_string(&e)?),
                false => show_event(&mut out, &e),
            }
            out.push('\n');
            let mut stdout = io::stdout().lock();
            stdout.write_all(out.as_bytes())?;
            stdout.flush()?
        }
        anyhow::Ok(())
    };
    within(how.limit, events).await.map(|_| ())
}

/// A session with the program `target` names ([`find`]). Where `wait`,
/// a program that does not answer yet is waited for that long: it may
/// have just been started.
async fn attach(target: Option<&str>, wait: Option<Duration>) -> Result<Client<Stream>> {
    let started = tokio::time::Instant::now();
    loop {
        match find(target).await {
            Ok(client) => break Ok(client),
            Err(Missed::Never(e)) => break Err(e),
            Err(Missed::NotYet(e)) if wait.is_none_or(|w| started.elapsed() >= w) => {
                break Err(e);
            }
            Err(Missed::NotYet(_)) => tokio::time::sleep(ATTACH_EVERY).await,
        }
    }
}

/// A program for the client to run itself.
pub struct Run {
    pub file: PathBuf,
    /// The program's arguments.
    pub args: Vec<String>,
    /// Hold it before its first cycle.
    pub held: bool,
    /// The shell's own flags to run it with (`--no-fusion`).
    pub flags: Vec<String>,
}

pub struct Args {
    /// The program ([`find`]).
    pub target: Option<String>,
    /// Commands to run in place of reading them.
    pub commands: Vec<String>,
    /// Run this program, attach to it, and stop it when done.
    pub run: Option<Run>,
    pub json: bool,
    /// After the commands, print each event (a watch's, a shown value's)
    /// as it comes, until the program ends or the limit.
    pub follow: bool,
    /// The seconds a `step`, a `wait` or a `follow` may take; 0 for no
    /// limit. Where absent: 30 for commands given or piped in, none at a
    /// terminal.
    pub timeout: Option<f64>,
}

/// A program the client started; the directory holds its socket.
struct Started {
    child: tokio::process::Child,
    /// The tasks passing on its output, which end when it does.
    passing: Vec<tokio::task::JoinHandle<()>>,
    _dir: tempfile::TempDir,
}

impl Started {
    /// Stop the program and pass on what is left of its output. Where
    /// its runtime no longer answers (`!alive`) it is ending by itself:
    /// it is given the time a shell takes to end, so what it says as it
    /// goes is heard. Returns how it ended, where it did by itself.
    async fn stop(mut self, alive: bool) -> Option<std::process::ExitStatus> {
        let ended = match alive {
            true => self.child.try_wait().ok().flatten(),
            false => {
                let ending =
                    tokio::time::timeout(crate::TEARDOWN_BOUND, self.child.wait());
                ending.await.ok().and_then(|status| status.ok())
            }
        };
        let _ = self.child.start_kill();
        let _ = self.child.wait().await;
        for task in self.passing {
            let _ = task.await;
        }
        ended
    }
}

/// A line of a program's output, marked as its own.
fn marked(mark: &str, line: &str, how: How) -> String {
    match how.json {
        true => format!("{}", serde_json::json!({"program": mark, "line": line})),
        false => format!("[{mark}] {line}"),
    }
}

/// Pass on each line left in `lines`: a program's output.
fn pass_on<R: AsyncRead + Unpin + Send + 'static>(
    mut lines: Lines<BufReader<R>>,
    mark: &'static str,
    how: How,
    say: Arc<dyn Fn(String) + Send + Sync>,
) -> tokio::task::JoinHandle<()> {
    tokio::spawn(async move {
        while let Ok(Some(line)) = lines.next_line().await {
            say(marked(mark, &line, how))
        }
    })
}

/// Run `run`'s program with its debugger offered on a socket of its own
/// and wait until it offers it: the line that says so on its stderr.
async fn start(
    run: &Run,
    how: How,
    say: Arc<dyn Fn(String) + Send + Sync>,
) -> Result<(Started, PathBuf)> {
    let dir = tempfile::tempdir().context("making a directory for the socket")?;
    let socket = match cfg!(windows) {
        false => dir.path().join("debug.sock"),
        true => socket_of(std::process::id()),
    };
    let mut cmd = tokio::process::Command::new(std::env::current_exe()?);
    cmd.args(&run.flags).arg(format_compact!("--debug={}", socket.display()).as_str());
    if run.held {
        cmd.arg("--debug-wait");
    }
    cmd.arg(&run.file).args(&run.args);
    cmd.stdin(Stdio::null()).stdout(Stdio::piped()).stderr(Stdio::piped());
    let mut child = cmd.kill_on_drop(true).spawn().context("running the program")?;
    let (stdout, stderr) = (child.stdout.take(), child.stderr.take());
    let stdout = BufReader::new(stdout.context("the program's stdout")?).lines();
    let out = pass_on(stdout, "out", how, say.clone());
    let mut said = BufReader::new(stderr.context("the program's stderr")?).lines();
    loop {
        match said.next_line().await? {
            Some(line) if line.starts_with("debugger: graphix debug ") => break,
            Some(line) => say(marked("err", &line, how)),
            None => {
                let status = child.wait().await?;
                bail!("the program ended before it offered a debugger ({status})")
            }
        }
    }
    let err = pass_on(said, "err", how, say);
    Ok((Started { child, passing: vec![out, err], _dir: dir }, socket))
}

/// `graphix debug`: attach to a program that offers its debugger, or run
/// one and attach to it. With commands, or with lines piped in, run each
/// and exit, failing if any was refused; at a terminal, read lines until
/// `quit`.
pub fn run(args: Args) -> Result<()> {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
        .context("building tokio runtime")?;
    // with nothing to run, a follow only listens: it reads no commands
    let script = !args.commands.is_empty() || args.follow || !io::stdin().is_terminal();
    let limit = match (args.timeout, script) {
        (Some(secs), _) if secs > 0. && secs.is_finite() => {
            Some(Duration::from_secs_f64(secs))
        }
        (Some(_), _) | (None, false) => None,
        (None, true) => Some(SCRIPT_LIMIT),
    };
    let how = How { json: args.json, limit, script };
    let asked = rt.block_on(async move {
        let printer = ExternalPrinter::default();
        let say: Arc<dyn Fn(String) + Send + Sync> = match script {
            true => Arc::new(|line| drop(writeln!(io::stdout(), "{line}"))),
            false => {
                let printer = printer.clone();
                Arc::new(move |line| drop(printer.print(line)))
            }
        };
        let (started, path) = match &args.run {
            None => (None, None),
            Some(run) => {
                let (started, path) = start(run, how, say).await?;
                (Some(started), Some(path))
            }
        };
        let mut alive = false;
        let asked = async {
            let mut client = match &path {
                Some(path) => attach(path.to_str(), None).await?,
                None => {
                    let wait = how.limit.map_or(ATTACH_WAIT, |l| l.min(ATTACH_WAIT));
                    attach(args.target.as_deref(), script.then_some(wait)).await?
                }
            };
            if let (false, Some(p)) = (script, client.program()) {
                let name = p.name.as_deref().unwrap_or("a program");
                eprintln!("attached to {name}, process {}", p.pid)
            }
            let mut ok = true;
            if !args.commands.is_empty() {
                for line in &args.commands {
                    match run_line(&mut client, line, how).await? {
                        None => break,
                        Some(answered) => ok &= answered,
                    }
                }
            } else if !script {
                at_terminal(&mut client, how, printer).await?
            } else if !args.follow {
                for line in io::stdin().lock().lines() {
                    match run_line(&mut client, &line?, how).await? {
                        None => break,
                        Some(answered) => ok &= answered,
                    }
                }
            }
            if args.follow {
                follow(&mut client, how).await?
            }
            // a program whose runtime still answers is stopped at once
            alive = started.is_some()
                && matches!(client.ask(Command::Info).await, Ok(Outcome::Ok(_)));
            anyhow::Ok(ok)
        };
        let asked = asked.await;
        let Some(started) = started else { return asked };
        match (started.stop(alive).await, asked) {
            (None, asked) => asked,
            (Some(status), asked) => {
                match how.json {
                    true => println!("{}", serde_json::json!({"program": "ended", "status": status.to_string()})),
                    false => eprintln!("the program ended by itself: {status}"),
                }
                Ok(asked.unwrap_or(false))
            }
        }
    });
    // the editor's thread may still be inside a read
    rt.shutdown_background();
    let closed = |e: &anyhow::Error| {
        let io = e.chain().find_map(|c| c.downcast_ref::<io::Error>());
        io.is_some_and(|e| e.kind() == io::ErrorKind::BrokenPipe)
    };
    match asked {
        // whoever read the output has what it wanted (`| head`)
        Err(e) if closed(&e) => Ok(()),
        Err(e) => Err(e),
        Ok(true) => Ok(()),
        Ok(false) => std::process::exit(1),
    }
}
