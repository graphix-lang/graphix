//! The shell's side of the debugger (`design/debugger.md`): the socket
//! `--debug` offers, and `graphix debug`, the client a person types at.

use anyhow::{Context, Result, bail};
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
use std::{
    fmt::Write as _,
    io::{self, BufRead, IsTerminal, Write},
    path::{Path, PathBuf},
    sync::mpsc as smpsc,
};
use tokio::{select, sync::mpsc};

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
  wait                wait until the program is held (by a break or a step)
  help, quit

A TARGET is a name (x, m::x), a place (file.gx:12 or file.gx:12:5), or a
binding's id as an answer shows it (#88).
";

/// The longest value a list or a why shows.
const BRIEF: usize = 72;
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
pub(crate) fn offer<X: GXExt>(gx: &GXHandle<X>, path: &Path) -> Result<Offered> {
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
    let debugger = Debugger::new(gx.clone()).with_types(graphix_lsp::display_type);
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
pub(crate) fn offer<X: GXExt>(gx: &GXHandle<X>, path: &Path) -> Result<Offered> {
    use tokio::net::windows::named_pipe::ServerOptions;
    let name = path.to_path_buf();
    let first = ServerOptions::new()
        .first_pipe_instance(true)
        .create(&name)
        .with_context(|| format!("making the pipe {}", path.display()))?;
    let debugger = Debugger::new(gx.clone()).with_types(graphix_lsp::display_type);
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

/// The sockets in [`socket_dir`] a program answers on.
#[cfg(unix)]
async fn offered() -> Vec<PathBuf> {
    let mut live = Vec::new();
    let Ok(dir) = std::fs::read_dir(socket_dir()) else { return live };
    for entry in dir.filter_map(|e| e.ok()) {
        let path = entry.path();
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if name.starts_with("debug-")
            && name.ends_with(".sock")
            && open(&path).await.is_ok()
        {
            live.push(path)
        }
    }
    live.sort();
    live
}

#[cfg(windows)]
async fn offered() -> Vec<PathBuf> {
    Vec::new()
}

/// The socket `target` names: a path, a process id, or with none the one
/// program that offers its debugger.
async fn socket(target: Option<&str>) -> Result<PathBuf> {
    match target {
        Some(t) => match t.parse::<u32>() {
            Ok(pid) => Ok(socket_of(pid)),
            Err(_) => Ok(PathBuf::from(t)),
        },
        None => {
            let mut live = offered().await;
            match live.len() {
                0 => bail!(
                    "no program offers a debugger: run one with `graphix --debug FILE`, \
                     or name its socket or process id"
                ),
                1 => Ok(live.pop().unwrap()),
                _ => {
                    let mut msg: LPooled<String> = LPooled::take();
                    msg.push_str("several programs offer a debugger; name one:");
                    for p in live.iter() {
                        let _ = write!(msg, "\n  graphix debug {}", p.display());
                    }
                    bail!("{}", msg.as_str())
                }
            }
        }
    }
}

enum Line {
    Empty,
    Help,
    Quit,
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
        "wait" if rest.is_empty() => Line::Ask(Command::Wait),
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
    let _ = writeln!(
        out,
        "cycle {}; {} bindings; {} fused blocks; {} timers and {} tasks pending; {} \
         sessions, {} watches",
        i.cycle, i.bindings, i.fused, i.timers, i.tasks, i.sessions, i.watches
    );
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

/// Run one line a person typed and print what it answers, then the
/// events that came before the answer; with `wait`, a `step` is waited
/// out too. `Ok(None)` at `quit`; else whether the line was answered and
/// not refused.
async fn run_line(
    client: &mut Client<Stream>,
    line: &str,
    wait: bool,
) -> Result<Option<bool>> {
    let mut out: LPooled<String> = LPooled::take();
    let mut stepping = false;
    let ok = match parse(line) {
        Err(e) => {
            let _ = writeln!(out, "error: {e:#}");
            false
        }
        Ok(Line::Empty) => true,
        Ok(Line::Quit) => return Ok(None),
        Ok(Line::Help) => {
            out.push_str(HELP);
            true
        }
        Ok(Line::Ask(cmd)) => {
            let waited = matches!(cmd, Command::Wait);
            match client.ask(cmd).await? {
                // the hold a wait waited for was told before its answer
                Outcome::Ok(Reply::Paused { .. }) if waited && client.has_queued() => {
                    true
                }
                Outcome::Ok(reply) => {
                    stepping = wait && matches!(reply, Reply::Stepping);
                    show_reply(&mut out, &reply);
                    true
                }
                Outcome::Err(e) => {
                    let _ = writeln!(out, "error: {e}");
                    false
                }
            }
        }
    };
    if stepping && let Outcome::Err(e) = client.ask(Command::Wait).await? {
        let _ = writeln!(out, "error: {e}");
    }
    while let Some(e) = client.queued() {
        show_event(&mut out, &e);
        out.push('\n')
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

async fn at_terminal(client: &mut Client<Stream>) -> Result<()> {
    let printer = ExternalPrinter::default();
    let mut lines = typed(printer.clone());
    let ended = loop {
        select! {
            line = lines.recv() => match line {
                None => break false,
                Some((line, done)) => match run_line(client, &line, false).await? {
                    None => break false,
                    Some(_) => {
                        let _ = done.send(());
                    }
                },
            },
            event = client.event() => match event? {
                None => break true,
                Some(e) => {
                    let mut out = String::new();
                    show_event(&mut out, &e);
                    let _ = printer.print(out);
                }
            },
        }
    };
    // the editor's thread may hold the terminal raw inside a read
    let _ = crossterm::terminal::disable_raw_mode();
    if ended {
        println!("\nthe program ended");
    }
    Ok(())
}

pub struct Args {
    /// The socket's path or the program's process id.
    pub target: Option<String>,
    /// Commands to run in place of reading them.
    pub commands: Vec<String>,
}

/// `graphix debug`: attach to a program that offers its debugger. With
/// commands, or with lines piped in, run each and exit, failing if any
/// was refused; at a terminal, read lines until `quit`.
pub fn run(args: Args) -> Result<()> {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
        .context("building tokio runtime")?;
    let all_answered = rt.block_on(async move {
        let path = socket(args.target.as_deref()).await?;
        let stream = open(&path).await;
        let stream =
            stream.with_context(|| format!("connecting to {}", path.display()))?;
        let mut client = Client::connect(stream).await?;
        let mut ok = true;
        if !args.commands.is_empty() {
            for line in &args.commands {
                match run_line(&mut client, line, true).await? {
                    None => break,
                    Some(answered) => ok &= answered,
                }
            }
        } else if io::stdin().is_terminal() {
            at_terminal(&mut client).await?
        } else {
            for line in io::stdin().lock().lines() {
                match run_line(&mut client, &line?, true).await? {
                    None => break,
                    Some(answered) => ok &= answered,
                }
            }
        }
        anyhow::Ok(ok)
    })?;
    // the editor's thread may still be inside a read
    rt.shutdown_background();
    if !all_answered {
        std::process::exit(1)
    }
    Ok(())
}
