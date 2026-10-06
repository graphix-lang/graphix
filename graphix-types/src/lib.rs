//! Graphix's static half: the syntax (parser, AST, printer, formatter,
//! module resolution), the types and their checker, the environment
//! the checker reads, and the image session that writes all of them.

#![recursion_limit = "256"]
#[macro_use]
extern crate combine;
#[macro_use]
extern crate serde_derive;
#[macro_use]
mod ids;

pub mod abstract_value;
pub(crate) mod dbgenv;
pub mod env;
pub mod expr;
pub mod ide;
pub mod image;
pub mod list;
#[doc(hidden)]
pub mod profile;
pub mod shared_map;
#[doc(hidden)]
pub mod stack;
pub mod tracked;
pub mod typ;

pub use combine::stream::position::SourcePosition;
pub use stack::set_stack_budget;

use ahash::AHashMap;
use arcstr::ArcStr;
use compact_str::CompactString;
use enumflags2::{BitFlags, bitflags};
use netidx_value::Abstract;
use parking_lot::Mutex;
use std::{
    any::{Any, TypeId},
    cell::Cell,
    sync::LazyLock,
    thread::LocalKey,
};
pub use uuid::Uuid;

#[derive(Debug, Clone, Copy)]
#[bitflags]
#[repr(u64)]
pub enum CFlag {
    WarnUnhandled,
    // CR claude for claude: [dead] Nothing reads WarnUnused: no unused-binding warning
    // exists in the workspace or ../netidx. Yet the shell sets it by default for
    // scripts and --check (graphix-shell/src/lib.rs:260), the LSP sets it
    // (lsp_backend.rs:62), `-W unused`/`-W no-unused` toggle it (main.rs:58-59, help at
    // main.rs:226), and book/src/shell.md:45 and 466-470 promise unused-variable
    // warnings by default. `graphix --check -W unused -W error` over two unused lets
    // prints nothing and exits 0. The flag still enters the program image key, so `-W
    // no-unused` misses the warm entry of an identical compile. Implement the warning
    // through Env::warn, or delete the variant, the RawFlag pair, the help line and the
    // book text. probe: design/review-2026-10-05/repro/shell-10.gx (shell-10)
    WarnUnused,
    WarningsAreErrors,
    /// Disable fusion: no kernels are built or spliced and the program
    /// runs purely through the node-walk.
    FusionDisabled,
    /// REPL policy: a colliding `use` shadows instead of erroring.
    ReplaceImports,
    /// Print each `seq`'s lowered machine to stdout as it is compiled
    /// (`graphix --expand`): the source position, then the program.
    ExpandSeq,
    /// Stop after the check: typecheck0 and the settle it records, no
    /// elaboration, analysis or fusion. The nodes compiled this way are
    /// only for inspection, never for running.
    CheckOnly,
}

/// Sets a thread-local `Cell` for a scope and puts the previous value
/// back when dropped, by an unwind too.
#[doc(hidden)]
pub struct Restore<T: Copy + 'static> {
    key: &'static LocalKey<Cell<T>>,
    prev: T,
}

impl<T: Copy + 'static> Restore<T> {
    pub fn replace(key: &'static LocalKey<Cell<T>>, v: T) -> Self {
        Self { key, prev: key.replace(v) }
    }

    /// The value the scope replaced.
    pub fn prev(&self) -> T {
        self.prev
    }
}

impl<T: Copy + 'static> Drop for Restore<T> {
    fn drop(&mut self) {
        self.key.set(self.prev)
    }
}

#[macro_export]
macro_rules! err {
    ($tag:expr, $err:literal) => {{
        let e: Value = ($tag.clone(), ::arcstr::literal!($err)).into();
        Value::Error(e.into())
    }};
}

#[macro_export]
macro_rules! errf {
    ($tag:expr, $fmt:expr, $($args:expr),*) => {{
        let msg: ArcStr = ::compact_str::format_compact!($fmt, $($args),*).as_str().into();
        let e: Value = ($tag.clone(), msg).into();
        Value::Error(e.into())
    }};
    ($tag:expr, $fmt:expr) => {{
        let msg: ArcStr = ::compact_str::format_compact!($fmt).as_str().into();
        let e: Value = ($tag.clone(), msg).into();
        Value::Error(e.into())
    }};
}

#[macro_export]
macro_rules! defetyp {
    ($(#[$attr:meta])* $vis:vis $name:ident, $tag_vis:vis $tag_name:ident, $tag:literal, $typ:expr) => {
        $(#[$attr])*
        $tag_vis static $tag_name: ArcStr = ::arcstr::literal!($tag);
        $(#[$attr])*
        $vis static $name: ::std::sync::LazyLock<$crate::typ::Type> =
            ::std::sync::LazyLock::new(|| {
                let scope = $crate::expr::ModPath::root();
                $crate::expr::parser::parse_type(&format!($typ, $tag))
                    .expect("failed to parse type")
                    .scope_refs(&scope)
            });
    };
}

defetyp!(#[doc(hidden)] pub CAST_ERR, pub CAST_ERR_TAG, "InvalidCast", "Error<`{}(string)>");

image_id!(LambdaId);

image_id!(LambdaInstanceId);

image_id!(BindId);

impl From<u64> for BindId {
    fn from(v: u64) -> Self {
        BindId(v)
    }
}

#[derive(Debug, Clone, Copy)]
#[bitflags]
#[repr(u64)]
pub enum PrintFlag {
    /// Print each type variable with its binding or "unbound".
    DerefTVars,
    /// Print core's short names for primitive sets (`Any`, `Number`).
    ReplacePrims,
    /// Print an Origin's location without its source.
    NoSource,
    /// Print an Origin without its parents.
    NoParents,
    /// Print what the author chose where the canonical form differs:
    /// fields and variants in written order, strings between their
    /// delimiters. The formatter's flag. Without it printed text is a
    /// function of the syntax alone, which program-visible text (a cast
    /// error, a null error) has to be: `WrittenAt` and `Expr::str_form`
    /// are not part of a session image, and a type is shared by content,
    /// so whose written order it carries is incidental.
    AsWritten,
}

thread_local! {
    static PRINT_FLAGS: Cell<BitFlags<PrintFlag>> = Cell::new(PrintFlag::ReplacePrims.into());
}

#[doc(hidden)]
pub fn print_as_written() -> bool {
    PRINT_FLAGS.get().contains(PrintFlag::AsWritten)
}

/// Run `f` with the given type-formatting flags on this thread.
pub fn format_with_flags<G: Into<BitFlags<PrintFlag>>, R, F: FnOnce() -> R>(
    flags: G,
    f: F,
) -> R {
    let _restore = Restore::replace(&PRINT_FLAGS, flags.into());
    f()
}

/// Library state for builtins, one value per type, shared by every
/// branch of a cycle: values are handles, read out as clones, and a value
/// that changes guards itself.
#[derive(Default)]
pub struct LibState(Mutex<AHashMap<TypeId, Box<dyn Any + Send + Sync>>>);

impl LibState {
    /// The library state of type `T`, created with `T::default` if absent.
    pub fn get_or_default<T>(&self) -> T
    where
        T: Default + Clone + Any + Send + Sync,
    {
        self.get_or_else(T::default)
    }

    /// The library state of type `T`, created with `f` if absent.
    pub fn get_or_else<T, F>(&self, f: F) -> T
    where
        T: Clone + Any + Send + Sync,
        F: FnOnce() -> T,
    {
        self.0
            .lock()
            .entry(TypeId::of::<T>())
            .or_insert_with(|| Box::new(f()) as Box<dyn Any + Send + Sync>)
            .downcast_ref::<T>()
            .unwrap()
            .clone()
    }

    /// True if `T` is present.
    pub fn contains<T>(&self) -> bool
    where
        T: Any + Send + Sync,
    {
        self.0.lock().contains_key(&TypeId::of::<T>())
    }

    /// The library state of type `T`, if registered.
    pub fn get<T>(&self) -> Option<T>
    where
        T: Clone + Any + Send + Sync,
    {
        self.0
            .lock()
            .get(&TypeId::of::<T>())
            .map(|t| t.downcast_ref::<T>().unwrap().clone())
    }

    /// Set the library state of type `T`, returning any existing state.
    pub fn set<T>(&self, t: T) -> Option<Box<T>>
    where
        T: Any + Send + Sync,
    {
        self.0
            .lock()
            .insert(TypeId::of::<T>(), Box::new(t) as Box<dyn Any + Send + Sync>)
            .map(|t| t.downcast::<T>().unwrap())
    }

    /// Remove and return the library state of type `T`.
    pub fn remove<T>(&self) -> Option<Box<T>>
    where
        T: Any + Send + Sync,
    {
        self.0.lock().remove(&TypeId::of::<T>()).map(|t| t.downcast::<T>().unwrap())
    }
}

/// A registry of abstract type UUIDs with a string tag per type. Each
/// monomorphization over Rt/UserEvent is a distinct type id and needs
/// its own UUID; the tag lets non-parameterized code (printers) know
/// what a value generally is.
#[derive(Default)]
pub struct AbstractTypeRegistry {
    by_tid: AHashMap<TypeId, Uuid>,
    by_uuid: AHashMap<Uuid, &'static str>,
}

impl AbstractTypeRegistry {
    fn with<V, F: FnMut(&mut AbstractTypeRegistry) -> V>(mut f: F) -> V {
        static REG: LazyLock<Mutex<AbstractTypeRegistry>> =
            LazyLock::new(|| Mutex::new(AbstractTypeRegistry::default()));
        let mut g = REG.lock();
        f(&mut *g)
    }

    /// The UUID of abstract type T.
    pub fn uuid<T: Any>(tag: &'static str) -> Uuid {
        Self::with(|rg| {
            *rg.by_tid.entry(TypeId::of::<T>()).or_insert_with(|| {
                let id = Uuid::new_v4();
                rg.by_uuid.insert(id, tag);
                id
            })
        })
    }

    /// The tag of this abstract type, if registered.
    pub fn tag(a: &Abstract) -> Option<&'static str> {
        Self::with(|rg| rg.by_uuid.get(&a.id()).map(|r| *r))
    }

    /// True if the abstract type has `tag`.
    pub fn is_a(a: &Abstract, tag: &str) -> bool {
        match Self::tag(a) {
            Some(t) => t == tag,
            None => false,
        }
    }
}

/// Format a generated block-scope component. The `#` prefix marks a
/// non-module level (identifiers cannot start with `#`); [`mod_root`]
/// strips them.
pub fn block_component(kind: &str, id: u64) -> CompactString {
    compact_str::format_compact!("#{kind}{id}")
}

/// True iff `part` is a generated block-scope component rather than
/// a module name.
pub fn is_block_component(part: &str) -> bool {
    part.starts_with('#')
}

/// True iff `part` is a `do` block's scope component: a loaded
/// script's top level is one.
pub fn is_do_block(part: &str) -> bool {
    part.strip_prefix("#do").is_some_and(|id| id.bytes().all(|b| b.is_ascii_digit()))
}

/// The module root of a lexical scope path: the path minus trailing
/// generated block components.
pub fn mod_root(mut scope: &str) -> &str {
    use netidx_core::path::Path;
    while let Some(base) = Path::basename(scope) {
        if !is_block_component(base) {
            break;
        }
        match Path::dirname(scope) {
            Some(d) => scope = d,
            None => return "/",
        }
    }
    scope
}
