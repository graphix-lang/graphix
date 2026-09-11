use crate::{
    PRINT_FLAGS, PrintFlag,
    expr::print::{PrettyBuf, PrettyDisplay},
    typ::{FnType, TVar, Type},
};
use anyhow::Result;
use arcstr::{ArcStr, literal};
use combine::stream::position::SourcePosition;
pub use modpath::ModPath;
use netidx_core::{path::Path, utils::Either};
use netidx_derive::Pack;
use netidx_value::Value;
pub use pattern::{Pattern, StructurePattern};
use poolshark::local::LPooled;
use regex::Regex;
pub use resolver::{
    BufferOverrides, FilesResolver, ModuleResolver, Resolution, ResolverFactory,
    ResolverRef, Resolvers, VfsEntry, VfsResolver, add_interface_modules, parse_modpath,
};
use serde::{
    Deserialize, Deserializer, Serialize, Serializer,
    de::{self, Visitor},
};
use std::{
    cell::RefCell,
    cmp::{Ordering, PartialEq, PartialOrd},
    fmt,
    ops::Deref,
    path::PathBuf,
    result,
    str::FromStr,
    sync::LazyLock,
};
use triomphe::Arc;

mod modpath;
pub mod parser;
mod pattern;
pub mod print;
mod resolver;
pub(crate) mod seq;
pub mod serialize;
#[cfg(test)]
mod test;

pub const VNAME: LazyLock<Regex> =
    LazyLock::new(|| Regex::new("^[a-z][a-z0-9_]*$").unwrap());

image_id!(ExprId);

const DEFAULT_ORIGIN: LazyLock<Arc<Origin>> =
    LazyLock::new(|| Arc::new(Origin::default()));

thread_local! {
    static ORIGIN: RefCell<Option<Arc<Origin>>> = RefCell::new(None);
}

pub(crate) fn set_origin(ori: Arc<Origin>) {
    ORIGIN.with_borrow_mut(|global| *global = Some(ori))
}

pub(crate) fn get_origin() -> Arc<Origin> {
    ORIGIN.with_borrow(|ori| {
        ori.as_ref().cloned().unwrap_or_else(|| DEFAULT_ORIGIN.clone())
    })
}

/// Swap the thread-local origin, returning the previous value. Brackets a
/// decode unit so decoded `Expr`s pick up their module origin via `get_origin`.
pub(crate) fn swap_origin(ori: Option<Arc<Origin>>) -> Option<Arc<Origin>> {
    ORIGIN.with_borrow_mut(|global| std::mem::replace(global, ori))
}

/// utility to read a file to an ArcStr with minimal allocation
pub async fn read_to_arcstr(path: impl AsRef<std::path::Path>) -> Result<ArcStr> {
    use tokio::io::AsyncReadExt;
    let mut buf: LPooled<Vec<u8>> = LPooled::take();
    let mut f = tokio::fs::File::open(path).await?;
    f.read_to_end(&mut *buf).await?;
    let s = str::from_utf8(&*buf)?;
    Ok(ArcStr::from(s))
}

#[derive(Debug)]
pub struct CouldNotResolve(ArcStr);

impl fmt::Display for CouldNotResolve {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "could not resolve module {}", self.0)
    }
}

#[derive(Debug, Clone, Pack)]
#[pack(unwrapped)]
pub struct Arg {
    pub labeled: Option<Option<Expr>>,
    pub pattern: StructurePattern,
    pub constraint: Option<Type>,
    // IDE metadata: excluded from equality and from the packed form.
    #[pack(skip)]
    pub pos: SourcePosition,
}

impl PartialEq for Arg {
    fn eq(&self, rhs: &Self) -> bool {
        self.labeled == rhs.labeled
            && self.pattern == rhs.pattern
            && self.constraint == rhs.constraint
    }
}

impl PartialOrd for Arg {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        match self.labeled.partial_cmp(&rhs.labeled)? {
            std::cmp::Ordering::Equal => (),
            o => return Some(o),
        }
        match self.pattern.partial_cmp(&rhs.pattern)? {
            std::cmp::Ordering::Equal => (),
            o => return Some(o),
        }
        self.constraint.partial_cmp(&rhs.constraint)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct Doc(pub Option<ArcStr>);

/// A `#[name(args, ...)]` / `#[name]` attribute attached above an expression.
#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct Attr {
    pub name: ArcStr,
    pub args: Arc<[Expr]>,
}

/// The `//` comment lines and `#[..]` attributes on their own line directly
/// above an `Expr`. Invisible to `Expr` equality.
#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct Decorations {
    pub comments: Arc<[ArcStr]>,
    pub attrs: Arc<[Attr]>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct TypeDefExpr {
    pub name: ArcStr,
    pub params: Arc<[(TVar, Option<Type>)]>,
    pub body: TypeDefBody,
}

/// What a `type` definition says about its name.
#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub enum TypeDefBody {
    /// `type T = typ` — a transparent alias of its body
    Alias(Type),
    /// `type T = Abstract<rep>` — a nominal type whose values are minted
    /// only by its constructor `T(..)`; `type T;` — the same with no
    /// representation in Graphix (an interface hiding one, or Rust-backed)
    Abstract(Option<Type>),
}

/// `trait Name { val m: fn(self, ..) -> T; val n: fn(self) -> U = |s| ..; .. }`.
/// A method's signature names the receiver as the type `self`; a method
/// with a `default` body is overridable, one without is required.
#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct TraitExpr {
    pub name: ArcStr,
    pub methods: Arc<[TraitMethod]>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct TraitMethod {
    pub doc: Doc,
    pub name: ArcStr,
    pub typ: Arc<FnType>,
    /// Index of the positional `self` parameter in `typ.args` — the
    /// argument whose type selects the implementation at a call.
    pub self_index: usize,
    pub default: Option<Expr>,
}

/// `impl<'a: C, ..> Trait for Target { let m = ..; .. }`. `params` are
/// the head's declared type variables (every one must occur in
/// `target`), `constraints` their bounds (a tvar may repeat under
/// `'a: A + B`), and `methods` the `let` bindings supplying the
/// trait's methods — empty for an interface declaration
/// (`impl Trait for Target;`).
#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct ImplExpr {
    pub trait_name: ModPath,
    pub params: Arc<[TVar]>,
    pub constraints: Arc<[(TVar, Type)]>,
    pub target: Type,
    pub methods: Arc<[Expr]>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct BindSig {
    pub name: ArcStr,
    pub typ: Type,
}

/// One imported name in a `use` declaration: the path as written —
/// leading `self`/`super`/`package` keywords and a final `*` glob are
/// encoded as literal path segments (all four are reserved words, so
/// no real segment collides) — plus the optional `as` rename.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash, Pack)]
#[pack(unwrapped)]
pub struct UseItem {
    pub path: ModPath,
    pub rename: Option<ArcStr>,
}

impl UseItem {
    pub fn plain(path: ModPath) -> Self {
        Self { path, rename: None }
    }

    /// The final segment is the glob marker.
    pub fn is_glob(&self) -> bool {
        Path::basename(&self.path.0) == Some("*")
    }

    /// The leading `self`/`super`/`package` keyword, if any.
    pub fn leading_keyword(&self) -> Option<&str> {
        Path::parts(&self.path.0)
            .next()
            .filter(|s| matches!(*s, "self" | "super" | "package"))
    }
}

impl fmt::Display for UseItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.rename {
            None => write!(f, "{}", self.path),
            Some(n) => write!(f, "{} as {n}", self.path),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub enum SigKind {
    TypeDef(TypeDefExpr),
    Trait(Arc<TraitExpr>),
    Impl(Arc<ImplExpr>),
    Bind(BindSig),
    Module(ArcStr),
    Use { reexport: bool, names: Arc<[UseItem]> },
}

#[derive(Debug, Clone, Pack)]
#[pack(unwrapped)]
pub struct SigItem {
    pub doc: Doc,
    pub kind: SigKind,
    // IDE metadata: excluded from equality and from the packed form.
    #[pack(skip)]
    pub pos: SourcePosition,
    #[pack(skip)]
    pub ori: Option<Arc<Origin>>,
}

impl PartialEq for SigItem {
    fn eq(&self, other: &Self) -> bool {
        self.doc == other.doc && self.kind == other.kind
    }
}

impl PartialOrd for SigItem {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.doc.partial_cmp(&other.doc)? {
            std::cmp::Ordering::Equal => self.kind.partial_cmp(&other.kind),
            ord => Some(ord),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct Sig {
    pub items: Arc<[SigItem]>,
    pub toplevel: bool,
}

impl Deref for Sig {
    type Target = [SigItem];

    fn deref(&self) -> &Self::Target {
        &*self.items
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub enum Sandbox {
    Unrestricted,
    Blacklist(Arc<[ModPath]>),
    Whitelist(Arc<[ModPath]>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub enum ModuleKind {
    Dynamic { sandbox: Sandbox, sig: Sig, source: Arc<Expr> },
    Resolved { exprs: Arc<[Expr]>, sig: Option<Sig>, from_interface: bool },
    Unresolved { from_interface: bool },
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct BindExpr {
    pub rec: bool,
    pub pattern: StructurePattern,
    pub typ: Option<Type>,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct LambdaExpr {
    pub args: Arc<[Arg]>,
    pub vargs: Option<Option<Type>>,
    pub rtype: Option<Type>,
    pub constraints: Arc<[(TVar, Type)]>,
    pub throws: Option<Type>,
    pub body: Either<Expr, ArcStr>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct CatchExpr {
    pub bind: ArcStr,
    pub constraint: Option<Type>,
    pub handler: Arc<Expr>,
    /// Compiler-only: this catch unconditionally rethrows before aborting.
    pub seq_abort: Option<Arc<Expr>>,
    /// Compiler-only: a seq `try`'s per-arm handler. The first error
    /// delivered per failure is written to this cell, and the handler's
    /// inferred throws are unioned into the cell's type.
    pub seq_capture: Option<ArcStr>,
}

/// `try { stmts } with(e[: T]) { stmts }` — a seq statement: an
/// error-triggered branch.
#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct TryWithExpr {
    pub body: Arc<[Expr]>,
    pub bind: ArcStr,
    pub constraint: Option<Type>,
    pub handler: Arc<[Expr]>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct StructWithExpr {
    pub source: Arc<Expr>,
    pub replace: Arc<[(ArcStr, Expr)]>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct StructExpr {
    pub args: Arc<[(ArcStr, Expr)]>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct ApplyExpr {
    pub args: Arc<[(Option<ArcStr>, Expr)]>,
    pub function: Arc<Expr>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct SelectExpr {
    pub arg: Arc<Expr>,
    pub arms: Arc<[(Pattern, Expr)]>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub enum ExprKind {
    NoOp,
    Constant(Value),
    Module {
        name: ArcStr,
        value: ModuleKind,
    },
    ExplicitParens(Arc<Expr>),
    Do {
        exprs: Arc<[Expr]>,
    },
    Use {
        reexport: bool,
        names: Arc<[UseItem]>,
    },
    Bind(Arc<BindExpr>),
    Ref {
        name: ModPath,
    },
    Connect {
        name: ModPath,
        value: Arc<Expr>,
        deref: bool,
    },
    StringInterpolate {
        args: Arc<[Expr]>,
    },
    StructRef {
        source: Arc<Expr>,
        field: ArcStr,
    },
    TupleRef {
        source: Arc<Expr>,
        field: usize,
    },
    ArrayRef {
        source: Arc<Expr>,
        i: Arc<Expr>,
    },
    ArraySlice {
        source: Arc<Expr>,
        start: Option<Arc<Expr>>,
        end: Option<Arc<Expr>>,
    },
    MapRef {
        source: Arc<Expr>,
        key: Arc<Expr>,
    },
    StructWith(StructWithExpr),
    Lambda(Arc<LambdaExpr>),
    TypeDef(TypeDefExpr),
    Trait(Arc<TraitExpr>),
    Impl(Arc<ImplExpr>),
    TypeCast {
        expr: Arc<Expr>,
        typ: Type,
    },
    Apply(ApplyExpr),
    /// `never<T>(args…)`: a value that never arrives, typed `T` where
    /// given and bottom otherwise; the arguments are kept live and
    /// consumed.
    Never {
        typ: Option<Type>,
        args: Arc<[Expr]>,
    },
    Any {
        args: Arc<[Expr]>,
    },
    Array {
        args: Arc<[Expr]>,
    },
    List {
        args: Arc<[Expr]>,
    },
    Map {
        args: Arc<[(Expr, Expr)]>,
    },
    Tuple {
        args: Arc<[Expr]>,
    },
    Variant {
        tag: ArcStr,
        args: Arc<[Expr]>,
    },
    /// `T(v)` — the constructor of the abstract type at `name`
    Construct {
        name: ModPath,
        arg: Arc<Expr>,
    },
    Struct(StructExpr),
    Select(SelectExpr),
    /// `seq [trigger] { stmts }` — a straight-line ceremony lowered to
    /// a select over a step variable.
    Seq {
        queued: bool,
        trigger: Option<Arc<Expr>>,
        body: Arc<[Expr]>,
    },
    /// `until expr` — wait until a bool level is true. Legal only as a
    /// seq step.
    Until(Arc<Expr>),
    /// `do { stmts }` — several seq statements as one arm. Legal only
    /// as a seq step.
    SeqDo {
        body: Arc<[Expr]>,
    },
    /// `try { stmts } with(e) { stmts }` — legal only as a seq step.
    TryWith(Arc<TryWithExpr>),
    Qop(Arc<Expr>),
    /// Compiler-generated forwarding; a nonthrowing region supplies bottom.
    Rethrow(Arc<Expr>),
    /// Compiler-generated sequence completion boundary.
    SeqGuard(Arc<Expr>),
    OrNever(Arc<Expr>),
    Catch(Arc<CatchExpr>),
    ByRef(Arc<Expr>),
    Deref(Arc<Expr>),
    Neg(Arc<Expr>),
    Eq {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    Ne {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    Lt {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    Gt {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    Lte {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    Gte {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    And {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    Or {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    Not {
        expr: Arc<Expr>,
    },
    Add {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    CheckedAdd {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    Sub {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    CheckedSub {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    Mul {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    CheckedMul {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    Div {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    CheckedDiv {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    Mod {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    CheckedMod {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    Sample {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
    /// `lhs ~! rhs` — the strict sample: `rhs` at each fire of `lhs`,
    /// bottom when `rhs` is bottom.
    StrictSample {
        lhs: Arc<Expr>,
        rhs: Arc<Expr>,
    },
}

impl ExprKind {
    pub fn to_expr(self, pos: SourcePosition) -> Expr {
        Expr { id: ExprId::new(), ori: get_origin(), pos, kind: self, dec: None }
    }

    /// does not provide any position information or comment
    pub fn to_expr_nopos(self) -> Expr {
        Expr {
            id: ExprId::new(),
            ori: get_origin(),
            pos: Default::default(),
            kind: self,
            dec: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Source {
    File(PathBuf),
    Netidx(Path),
    Internal(ArcStr),
    Unspecified,
}

impl Default for Source {
    fn default() -> Self {
        Self::Unspecified
    }
}

impl Source {
    pub fn has_filename(&self, name: &str) -> bool {
        match self {
            Self::File(buf) => match buf.file_name() {
                None => false,
                Some(os) => match os.to_str() {
                    None => false,
                    Some(s) => s == name,
                },
            },
            Self::Netidx(_) | Self::Internal(_) | Self::Unspecified => false,
        }
    }

    pub fn is_file(&self) -> bool {
        match self {
            Self::File(_) => true,
            Self::Netidx(_) | Self::Internal(_) | Self::Unspecified => false,
        }
    }

    pub fn to_value(&self) -> Value {
        match self {
            Self::File(pb) => {
                let s = pb.as_os_str().to_string_lossy();
                (literal!("File"), ArcStr::from(s)).into()
            }
            Self::Netidx(p) => (literal!("Netidx"), p.clone()).into(),
            Self::Internal(s) => (literal!("Internal"), s.clone()).into(),
            Self::Unspecified => literal!("Unspecified").into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
pub struct Origin {
    pub parent: Option<Arc<Origin>>,
    pub source: Source,
    pub text: ArcStr,
}

impl fmt::Display for Origin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let flags = PRINT_FLAGS.with(|f| f.get());
        match &self.source {
            Source::Unspecified => {
                if flags.contains(PrintFlag::NoSource) {
                    write!(f, "in expr")?
                } else {
                    write!(f, "in expr {}", self.text)?
                }
            }
            Source::File(n) => write!(f, "in file {n:?}")?,
            Source::Netidx(n) => write!(f, "in netidx {n}")?,
            Source::Internal(n) => write!(f, "in module {n}")?,
        }
        let mut p = &self.parent;
        if flags.contains(PrintFlag::NoParents) {
            Ok(())
        } else {
            loop {
                match p {
                    None => break Ok(()),
                    Some(parent) => {
                        writeln!(f, "")?;
                        write!(f, "    ")?;
                        match &parent.source {
                            Source::Unspecified => {
                                if flags.contains(PrintFlag::NoSource) {
                                    write!(f, "included from expr")?
                                } else {
                                    write!(f, "included from expr {}", parent.text)?
                                }
                            }
                            Source::File(n) => write!(f, "included from file {n:?}")?,
                            Source::Netidx(n) => write!(f, "included from netidx {n}")?,
                            Source::Internal(n) => write!(f, "included from module {n}")?,
                        }
                        p = &parent.parent;
                    }
                }
            }
        }
    }
}

impl Origin {
    pub fn to_value(&self) -> Value {
        let p = Value::from(self.parent.as_ref().map(|p| p.to_value()));
        [
            (literal!("parent"), p),
            (literal!("source"), self.source.to_value()),
            (literal!("text"), Value::from(self.text.clone())),
        ]
        .into()
    }

    pub fn from_str(s: &str) -> Self {
        Self { parent: None, source: Source::Unspecified, text: ArcStr::from(s) }
    }
}

#[derive(Clone)]
pub struct Expr {
    pub id: ExprId,
    pub ori: Arc<Origin>,
    pub pos: SourcePosition,
    pub kind: ExprKind,
    /// Comments/attributes on their own line directly above this
    /// expression. `None` unless the expression was decorated; not
    /// compared by equality.
    pub dec: Option<Box<Decorations>>,
}

/// Field drop glue runs after `drop` returns and cannot be stack-guarded,
/// so `kind` is taken out and dropped under the guard here; the glue then
/// drops a trivial `NoOp`.
impl Drop for Expr {
    fn drop(&mut self) {
        let kind = std::mem::replace(&mut self.kind, ExprKind::NoOp);
        crate::stack::ensure_sufficient(move || drop(kind))
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        print::write_leading(f, &self.dec)?;
        // Printing descends the whole tree, including arbitrary user
        // subexpressions on error paths.
        crate::stack::ensure_sufficient(|| write!(f, "{}", self.kind))
    }
}

impl PrettyDisplay for Expr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        print::write_leading(buf, &self.dec)?;
        self.kind.fmt_pretty(buf)
    }
}

impl PartialOrd for Expr {
    fn partial_cmp(&self, rhs: &Expr) -> Option<Ordering> {
        self.kind.partial_cmp(&rhs.kind)
    }
}

impl PartialEq for Expr {
    fn eq(&self, rhs: &Expr) -> bool {
        self.kind.eq(&rhs.kind)
    }
}

impl Expr {
    /// Whether `other` is a clone of this expression: the same id,
    /// origin and position over equal syntax (shared children compare
    /// by pointer).
    pub(crate) fn same_tree(&self, other: &Expr) -> bool {
        self.id == other.id
            && self.pos == other.pos
            && Arc::ptr_eq(&self.ori, &other.ori)
            && self.kind == other.kind
    }
}

impl Eq for Expr {}

impl Serialize for Expr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl Default for Expr {
    fn default() -> Self {
        ExprKind::Constant(Value::Null).to_expr(Default::default())
    }
}

impl FromStr for Expr {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> result::Result<Self, Self::Err> {
        parser::parse_one(s)
    }
}

#[derive(Clone, Copy)]
struct ExprVisitor;

impl<'de> Visitor<'de> for ExprVisitor {
    type Value = Expr;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "expected expression")
    }

    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Expr::from_str(s).map_err(de::Error::custom)
    }

    fn visit_borrowed_str<E>(self, s: &'de str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Expr::from_str(s).map_err(de::Error::custom)
    }

    fn visit_string<E>(self, s: String) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Expr::from_str(&s).map_err(de::Error::custom)
    }
}

impl<'de> Deserialize<'de> for Expr {
    fn deserialize<D>(de: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        de.deserialize_str(ExprVisitor)
    }
}

impl Expr {
    pub fn new(kind: ExprKind, pos: SourcePosition) -> Self {
        Expr { id: ExprId::new(), ori: get_origin(), pos, kind, dec: None }
    }

    /// fold over self and all of self's sub expressions
    pub fn fold<T, F: FnMut(T, &Self) -> T>(&self, init: T, f: &mut F) -> T {
        crate::stack::ensure_sufficient(|| self.fold_inner(init, f))
    }

    fn fold_inner<T, F: FnMut(T, &Self) -> T>(&self, init: T, f: &mut F) -> T {
        let mut acc = Some(f(init, self));
        self.for_each_child(&mut |c| {
            let v = acc.take().unwrap();
            acc = Some(c.fold(v, f));
        });
        acc.unwrap()
    }

    /// Visit each direct sub-expression in the one canonical child order,
    /// shared by `fold` and `map_children`. A new `ExprKind` child is
    /// added here and nowhere else.
    pub fn for_each_child(&self, f: &mut impl FnMut(&Expr)) {
        use ExprKind::*;
        match &self.kind {
            NoOp | Constant(_) | Use { .. } | Ref { .. } | TypeDef(_) => (),
            Module { value: ModuleKind::Resolved { exprs, .. }, .. } => {
                exprs.iter().for_each(|e| f(e))
            }
            Module { value: ModuleKind::Dynamic { source, .. }, .. } => f(source),
            Module { value: ModuleKind::Unresolved { .. }, .. } => (),
            ExplicitParens(x)
            | Qop(x)
            | Rethrow(x)
            | SeqGuard(x)
            | OrNever(x)
            | ByRef(x)
            | Deref(x)
            | Neg(x)
            | Until(x)
            | Not { expr: x }
            | Construct { arg: x, .. }
            | TypeCast { expr: x, .. }
            | Connect { value: x, .. }
            | StructRef { source: x, .. }
            | TupleRef { source: x, .. } => f(x),
            Do { exprs: xs }
            | StringInterpolate { args: xs }
            | Any { args: xs }
            | Never { args: xs, .. }
            | Array { args: xs }
            | List { args: xs }
            | Tuple { args: xs }
            | Variant { args: xs, .. }
            | SeqDo { body: xs } => xs.iter().for_each(|e| f(e)),
            Bind(b) => f(&b.value),
            ArrayRef { source, i } => {
                f(source);
                f(i);
            }
            ArraySlice { source, start, end } => {
                f(source);
                start.iter().for_each(|e| f(e));
                end.iter().for_each(|e| f(e));
            }
            MapRef { source, key } => {
                f(source);
                f(key);
            }
            Map { args } => args.iter().for_each(|(k, v)| {
                f(k);
                f(v);
            }),
            Struct(StructExpr { args }) => args.iter().for_each(|(_, e)| f(e)),
            StructWith(StructWithExpr { source, replace }) => {
                f(source);
                replace.iter().for_each(|(_, e)| f(e));
            }
            Apply(ApplyExpr { function, args }) => {
                f(function);
                args.iter().for_each(|(_, e)| f(e));
            }
            Lambda(l) => {
                for a in l.args.iter() {
                    if let Some(Some(d)) = &a.labeled {
                        f(d);
                    }
                }
                if let Either::Left(b) = &l.body {
                    f(b);
                }
            }
            Trait(t) => {
                t.methods.iter().for_each(|m| m.default.iter().for_each(|e| f(e)))
            }
            Impl(im) => im.methods.iter().for_each(|e| f(e)),
            Select(SelectExpr { arg, arms }) => {
                f(arg);
                for (p, e) in arms.iter() {
                    p.guard.iter().for_each(|g| f(g));
                    f(e);
                }
            }
            Catch(c) => {
                f(&c.handler);
                c.seq_abort.iter().for_each(|e| f(e));
            }
            Seq { trigger, body, .. } => {
                trigger.iter().for_each(|t| f(t));
                body.iter().for_each(|e| f(e));
            }
            TryWith(t) => {
                t.body.iter().for_each(|e| f(e));
                t.handler.iter().for_each(|e| f(e));
            }
            Eq { lhs, rhs }
            | Ne { lhs, rhs }
            | Lt { lhs, rhs }
            | Gt { lhs, rhs }
            | Lte { lhs, rhs }
            | Gte { lhs, rhs }
            | And { lhs, rhs }
            | Or { lhs, rhs }
            | Add { lhs, rhs }
            | CheckedAdd { lhs, rhs }
            | Sub { lhs, rhs }
            | CheckedSub { lhs, rhs }
            | Mul { lhs, rhs }
            | CheckedMul { lhs, rhs }
            | Div { lhs, rhs }
            | CheckedDiv { lhs, rhs }
            | Mod { lhs, rhs }
            | CheckedMod { lhs, rhs }
            | Sample { lhs, rhs }
            | StrictSample { lhs, rhs } => {
                f(lhs);
                f(rhs);
            }
        }
    }

    /// This node rebuilt with each direct sub-expression replaced by
    /// `f(child)`, in `for_each_child`'s order, with a fresh id.
    pub fn map_children(&self, f: &mut impl FnMut(&Expr) -> Expr) -> Expr {
        use ExprKind::*;
        let a = |f: &mut dyn FnMut(&Expr) -> Expr, x: &Arc<Expr>| Arc::new(f(x));
        let xs = |f: &mut dyn FnMut(&Expr) -> Expr, xs: &Arc<[Expr]>| {
            Arc::from_iter(xs.iter().map(|x| f(x)))
        };
        let kind = match &self.kind {
            NoOp
            | Constant(_)
            | Use { .. }
            | Ref { .. }
            | TypeDef(_)
            | Module { value: ModuleKind::Unresolved { .. }, .. } => self.kind.clone(),
            Module {
                name,
                value: ModuleKind::Resolved { exprs, sig, from_interface },
            } => Module {
                name: name.clone(),
                value: ModuleKind::Resolved {
                    exprs: xs(f, exprs),
                    sig: sig.clone(),
                    from_interface: *from_interface,
                },
            },
            Module { name, value: ModuleKind::Dynamic { sandbox, sig, source } } => {
                Module {
                    name: name.clone(),
                    value: ModuleKind::Dynamic {
                        sandbox: sandbox.clone(),
                        sig: sig.clone(),
                        source: a(f, source),
                    },
                }
            }
            ExplicitParens(x) => ExplicitParens(a(f, x)),
            Qop(x) => Qop(a(f, x)),
            Rethrow(x) => Rethrow(a(f, x)),
            SeqGuard(x) => SeqGuard(a(f, x)),
            OrNever(x) => OrNever(a(f, x)),
            ByRef(x) => ByRef(a(f, x)),
            Deref(x) => Deref(a(f, x)),
            Neg(x) => Neg(a(f, x)),
            Until(x) => Until(a(f, x)),
            Not { expr } => Not { expr: a(f, expr) },
            Construct { name, arg } => Construct { name: name.clone(), arg: a(f, arg) },
            TypeCast { expr, typ } => TypeCast { expr: a(f, expr), typ: typ.clone() },
            Connect { name, value, deref } => {
                Connect { name: name.clone(), value: a(f, value), deref: *deref }
            }
            StructRef { source, field } => {
                StructRef { source: a(f, source), field: field.clone() }
            }
            TupleRef { source, field } => {
                TupleRef { source: a(f, source), field: *field }
            }
            Do { exprs } => Do { exprs: xs(f, exprs) },
            StringInterpolate { args } => StringInterpolate { args: xs(f, args) },
            Any { args } => Any { args: xs(f, args) },
            Never { typ, args } => Never { typ: typ.clone(), args: xs(f, args) },
            Array { args } => Array { args: xs(f, args) },
            List { args } => List { args: xs(f, args) },
            Tuple { args } => Tuple { args: xs(f, args) },
            Variant { tag, args } => Variant { tag: tag.clone(), args: xs(f, args) },
            SeqDo { body } => SeqDo { body: xs(f, body) },
            Bind(b) => Bind(Arc::new(BindExpr {
                rec: b.rec,
                pattern: b.pattern.clone(),
                typ: b.typ.clone(),
                value: f(&b.value),
            })),
            ArrayRef { source, i } => ArrayRef { source: a(f, source), i: a(f, i) },
            ArraySlice { source, start, end } => ArraySlice {
                source: a(f, source),
                start: start.as_ref().map(|e| a(f, e)),
                end: end.as_ref().map(|e| a(f, e)),
            },
            MapRef { source, key } => MapRef { source: a(f, source), key: a(f, key) },
            Map { args } => {
                Map { args: Arc::from_iter(args.iter().map(|(k, v)| (f(k), f(v)))) }
            }
            Struct(StructExpr { args }) => Struct(StructExpr {
                args: Arc::from_iter(args.iter().map(|(n, e)| (n.clone(), f(e)))),
            }),
            StructWith(StructWithExpr { source, replace }) => {
                StructWith(StructWithExpr {
                    source: a(f, source),
                    replace: Arc::from_iter(
                        replace.iter().map(|(n, e)| (n.clone(), f(e))),
                    ),
                })
            }
            Apply(ApplyExpr { function, args }) => Apply(ApplyExpr {
                function: a(f, function),
                args: Arc::from_iter(args.iter().map(|(n, e)| (n.clone(), f(e)))),
            }),
            Lambda(l) => Lambda(Arc::new(LambdaExpr {
                args: Arc::from_iter(l.args.iter().map(|arg| Arg {
                    labeled: match &arg.labeled {
                        Some(Some(d)) => Some(Some(f(d))),
                        other => other.clone(),
                    },
                    pattern: arg.pattern.clone(),
                    constraint: arg.constraint.clone(),
                    pos: arg.pos,
                })),
                vargs: l.vargs.clone(),
                rtype: l.rtype.clone(),
                constraints: l.constraints.clone(),
                throws: l.throws.clone(),
                body: match &l.body {
                    Either::Left(b) => Either::Left(f(b)),
                    Either::Right(s) => Either::Right(s.clone()),
                },
            })),
            Trait(t) => Trait(Arc::new(TraitExpr {
                name: t.name.clone(),
                methods: Arc::from_iter(t.methods.iter().map(|m| TraitMethod {
                    doc: m.doc.clone(),
                    name: m.name.clone(),
                    typ: m.typ.clone(),
                    self_index: m.self_index,
                    default: m.default.as_ref().map(|e| f(e)),
                })),
            })),
            Impl(im) => Impl(Arc::new(ImplExpr {
                trait_name: im.trait_name.clone(),
                params: im.params.clone(),
                constraints: im.constraints.clone(),
                target: im.target.clone(),
                methods: xs(f, &im.methods),
            })),
            Select(SelectExpr { arg, arms }) => Select(SelectExpr {
                arg: a(f, arg),
                arms: Arc::from_iter(arms.iter().map(|(p, e)| {
                    let mut p = p.clone();
                    p.guard = p.guard.as_ref().map(|g| f(g));
                    (p, f(e))
                })),
            }),
            Catch(c) => Catch(Arc::new(CatchExpr {
                bind: c.bind.clone(),
                constraint: c.constraint.clone(),
                handler: a(f, &c.handler),
                seq_abort: c.seq_abort.as_ref().map(|e| a(f, e)),
                seq_capture: c.seq_capture.clone(),
            })),
            Seq { queued, trigger, body } => Seq {
                queued: *queued,
                trigger: trigger.as_ref().map(|t| a(f, t)),
                body: xs(f, body),
            },
            TryWith(t) => TryWith(Arc::new(TryWithExpr {
                body: xs(f, &t.body),
                bind: t.bind.clone(),
                constraint: t.constraint.clone(),
                handler: xs(f, &t.handler),
            })),
            Eq { lhs, rhs } => Eq { lhs: a(f, lhs), rhs: a(f, rhs) },
            Ne { lhs, rhs } => Ne { lhs: a(f, lhs), rhs: a(f, rhs) },
            Lt { lhs, rhs } => Lt { lhs: a(f, lhs), rhs: a(f, rhs) },
            Gt { lhs, rhs } => Gt { lhs: a(f, lhs), rhs: a(f, rhs) },
            Lte { lhs, rhs } => Lte { lhs: a(f, lhs), rhs: a(f, rhs) },
            Gte { lhs, rhs } => Gte { lhs: a(f, lhs), rhs: a(f, rhs) },
            And { lhs, rhs } => And { lhs: a(f, lhs), rhs: a(f, rhs) },
            Or { lhs, rhs } => Or { lhs: a(f, lhs), rhs: a(f, rhs) },
            Add { lhs, rhs } => Add { lhs: a(f, lhs), rhs: a(f, rhs) },
            CheckedAdd { lhs, rhs } => CheckedAdd { lhs: a(f, lhs), rhs: a(f, rhs) },
            Sub { lhs, rhs } => Sub { lhs: a(f, lhs), rhs: a(f, rhs) },
            CheckedSub { lhs, rhs } => CheckedSub { lhs: a(f, lhs), rhs: a(f, rhs) },
            Mul { lhs, rhs } => Mul { lhs: a(f, lhs), rhs: a(f, rhs) },
            CheckedMul { lhs, rhs } => CheckedMul { lhs: a(f, lhs), rhs: a(f, rhs) },
            Div { lhs, rhs } => Div { lhs: a(f, lhs), rhs: a(f, rhs) },
            CheckedDiv { lhs, rhs } => CheckedDiv { lhs: a(f, lhs), rhs: a(f, rhs) },
            Mod { lhs, rhs } => Mod { lhs: a(f, lhs), rhs: a(f, rhs) },
            CheckedMod { lhs, rhs } => CheckedMod { lhs: a(f, lhs), rhs: a(f, rhs) },
            Sample { lhs, rhs } => Sample { lhs: a(f, lhs), rhs: a(f, rhs) },
            StrictSample { lhs, rhs } => StrictSample { lhs: a(f, lhs), rhs: a(f, rhs) },
        };
        Expr {
            id: ExprId::new(),
            ori: self.ori.clone(),
            pos: self.pos,
            kind,
            dec: self.dec.clone(),
        }
    }
}

pub struct ErrorContext(pub Expr);

impl fmt::Debug for ErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl std::error::Error for ErrorContext {}

pub struct ParserContext {
    pub ori: Arc<Origin>,
    pub pos: SourcePosition,
}

impl fmt::Debug for ParserContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for ParserContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.ori.source {
            Source::File(p) => {
                write!(f, "parse error at {} in file {}", self.pos, p.display())
            }
            Source::Netidx(p) => {
                write!(f, "parse error at {} in netidx {p}", self.pos)
            }
            Source::Internal(_) | Source::Unspecified => {
                write!(f, "parse error at {}", self.pos)
            }
        }
    }
}

impl std::error::Error for ParserContext {}

impl fmt::Display for ErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::fmt::Write;
        const MAX: usize = 38;
        thread_local! {
            static BUF: RefCell<String> = RefCell::new(String::new());
        }
        BUF.with_borrow_mut(|buf| {
            buf.clear();
            write!(buf, "{}", self.0).unwrap();
            let snippet: &str = if buf.len() <= MAX {
                &buf
            } else {
                let mut end = MAX;
                while !buf.is_char_boundary(end) {
                    end += 1
                }
                &buf[0..end]
            };
            let suffix = if buf.len() > MAX { ".." } else { "" };
            match &self.0.ori.source {
                Source::File(p) => write!(
                    f,
                    "at: {} in file {}, in: {snippet}{suffix}",
                    self.0.pos,
                    p.display()
                ),
                Source::Netidx(p) => {
                    write!(f, "at: {} in netidx {p}, in: {snippet}{suffix}", self.0.pos)
                }
                Source::Internal(_) | Source::Unspecified => {
                    write!(f, "at: {}, in: {snippet}{suffix}", self.0.pos)
                }
            }
        })
    }
}
