//! The debugger's wire protocol (`design/debugger.md`): lines of JSON.
//! Each side opens with a [`Hello`]. A client sends [`Request`]s; the
//! service answers each with a [`Response`] carrying its `id` and sends an
//! [`Event`], which has none, whenever something a session watches
//! happens. A value crosses as Graphix text.

use arcstr::ArcStr;
use compact_str::CompactString;
use serde_derive::{Deserialize, Serialize};

pub const VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Hello {
    pub graphix_debug: u32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Request {
    pub id: u64,
    #[serde(flatten)]
    pub cmd: Command,
}

/// What a client asks. A `target` names bindings: a name as the program's
/// top level sees it (`x`, `m::x`), failing that every binding of that
/// name; a place (`file.gx:12`, `file.gx:12:5`); or one binding by the id
/// an answer gave (`#88`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "cmd", rename_all = "snake_case")]
pub enum Command {
    /// Compile `text` against the running program as a REPL input and run
    /// it: any Graphix. What it binds lives until the session ends; the
    /// value it shows, until the session's next `eval`.
    Eval {
        text: ArcStr,
    },
    Print {
        target: CompactString,
    },
    /// The program's bindings: all of them, those of a module (`m`), or
    /// those of a file (`file.gx`).
    List {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        scope: Option<CompactString>,
    },
    /// Deliver the value of the expression `value` to one binding, as an
    /// embedder's write does. The answer comes once the cycle that
    /// delivered it has run.
    Set {
        target: CompactString,
        value: ArcStr,
    },
    /// Send a [`Event::Fired`] at each fire of the target's bindings.
    Watch {
        target: CompactString,
    },
    Unwatch {
        watch: u64,
    },
    /// Why one binding has no value, or what it waits on.
    Why {
        target: CompactString,
    },
    Info,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Response {
    pub id: u64,
    #[serde(flatten)]
    pub result: Outcome,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Outcome {
    Ok(Reply),
    Err(ArcStr),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Reply {
    /// The input compiled and its first cycle ran. `typ` is its value's
    /// type where its last expression has one; each value that produces
    /// is an [`Event::Value`] naming `eval`, the first cycle's sent before
    /// this answer, until the session's next `eval`.
    Evaled {
        eval: u64,
        typ: Option<ArcStr>,
    },
    Bindings {
        bindings: Vec<Binding>,
        more: usize,
    },
    /// The place a `print` named is inside fused code, which keeps no
    /// binding: each block that covers it, one for each instance fused.
    Fused {
        blocks: Vec<Fused>,
    },
    /// `value` is delivered to `binding`, shown as it stood before.
    Set {
        binding: Binding,
        value: ArcStr,
    },
    Watching {
        watch: u64,
        bindings: Vec<Binding>,
    },
    Unwatched,
    Why {
        why: Why,
    },
    Info {
        info: Info,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum Event {
    Value {
        eval: u64,
        value: ArcStr,
    },
    /// A watched binding was delivered in `cycle`: its value, or `None`
    /// where it went to no value.
    Fired {
        watch: u64,
        binding: CompactString,
        name: CompactString,
        cycle: u64,
        value: Option<ArcStr>,
    },
}

/// One binding as the runtime holds it. `id` names it in a target
/// (`#id`). `value` is absent where it has none; `written` is the cycle
/// of its last delivery, absent where nothing was ever delivered to it.
/// `file` is absent for a binding no file wrote (a REPL input, a program
/// given as text).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Binding {
    pub id: CompactString,
    pub name: CompactString,
    pub scope: ArcStr,
    pub file: Option<ArcStr>,
    pub line: i32,
    pub column: i32,
    pub typ: ArcStr,
    pub value: Option<ArcStr>,
    pub written: Option<u64>,
    /// How many bindings the row stands for: more than one only in a
    /// `list`, which shows a function's binding once for all its instances.
    pub instances: usize,
}

/// The text a binding's value comes from, and where it starts: the
/// `let` that binds it, or a fused block.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Source {
    pub text: ArcStr,
    pub line: i32,
    pub column: i32,
}

/// A fused block: pure code compiled to native code, a box. What goes
/// in is the bindings it reads, each of which a `set` can write; what
/// comes out is its value.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Fused {
    pub source: Source,
    pub typ: ArcStr,
    pub value: Option<ArcStr>,
    pub inputs: Vec<Binding>,
}

/// Why a binding stands as it does: what gives it its value and each
/// binding that reads. A read with no value is followed to its own
/// source; one with a value is a leaf.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Why {
    pub binding: Binding,
    /// `None` where no `let` binds it: a parameter, a pattern's name.
    pub source: Option<Source>,
    pub reads: Vec<Why>,
    /// The walk met this binding above: the program feeds it to itself.
    pub again: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Info {
    pub cycle: u64,
    pub bindings: usize,
    pub fused: usize,
    pub timers: usize,
    pub tasks: usize,
    pub sessions: usize,
    pub watches: usize,
}
