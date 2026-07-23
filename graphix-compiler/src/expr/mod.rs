use crate::{
    PRINT_FLAGS, PrintFlag,
    expr::print::{PrettyBuf, PrettyDisplay},
    typ::{TVar, Type},
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
pub mod serialize;
#[cfg(test)]
mod test;

pub const VNAME: LazyLock<Regex> =
    LazyLock::new(|| Regex::new("^[a-z][a-z0-9_]*$").unwrap());

atomic_id!(ExprId);

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

/// Swap the thread-local origin, returning the previous value. Used by the
/// AST decoder (`serialize`) to bracket a decode unit so decoded `Expr`s pick
/// up the right module origin via `get_origin`, then restore on completion.
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
    // source position is IDE metadata, excluded from `Arg` equality and from
    // the packed form (restored as the `1,1` default on decode).
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

/// A single `#[name(args, ...)]` / `#[name]` attribute attached above an
/// expression. (Parsed and acted on by a later change; the field exists
/// now so the `Decorations` shape is stable.)
#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct Attr {
    pub name: ArcStr,
    pub args: Arc<[Expr]>,
}

/// Source decorations attached to the `Expr` they sit above — the `//`
/// comment lines and `#[..]` attributes on their own line directly above
/// the expression, plus any `trailing` comments dangling after the last
/// expression of a block/file (the one position with no expression below
/// to attach to). `None` for the overwhelming majority of expressions, so
/// it costs one word and no allocation. Invisible to `Expr` equality
/// (comments don't affect semantics — see `PartialEq for Expr`).
#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct Decorations {
    pub comments: Arc<[ArcStr]>,
    pub attrs: Arc<[Attr]>,
    pub trailing: Arc<[ArcStr]>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct TypeDefExpr {
    pub name: ArcStr,
    pub params: Arc<[(TVar, Option<Type>)]>,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct BindSig {
    pub name: ArcStr,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub enum SigKind {
    TypeDef(TypeDefExpr),
    Bind(BindSig),
    Module(ArcStr),
    Use(ModPath),
}

#[derive(Debug, Clone, Pack)]
#[pack(unwrapped)]
pub struct SigItem {
    pub doc: Doc,
    pub kind: SigKind,
    // pos/ori are IDE metadata, excluded from `SigItem` equality and dropped
    // from the packed form (decode to the default / None).
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
pub struct TryCatchExpr {
    pub bind: ArcStr,
    pub constraint: Option<Type>,
    pub handler: Arc<Expr>,
    pub exprs: Arc<[Expr]>,
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
    Module { name: ArcStr, value: ModuleKind },
    ExplicitParens(Arc<Expr>),
    Do { exprs: Arc<[Expr]> },
    Use { name: ModPath },
    Bind(Arc<BindExpr>),
    Ref { name: ModPath },
    Connect { name: ModPath, value: Arc<Expr>, deref: bool },
    StringInterpolate { args: Arc<[Expr]> },
    StructRef { source: Arc<Expr>, field: ArcStr },
    TupleRef { source: Arc<Expr>, field: usize },
    ArrayRef { source: Arc<Expr>, i: Arc<Expr> },
    ArraySlice { source: Arc<Expr>, start: Option<Arc<Expr>>, end: Option<Arc<Expr>> },
    MapRef { source: Arc<Expr>, key: Arc<Expr> },
    StructWith(StructWithExpr),
    Lambda(Arc<LambdaExpr>),
    TypeDef(TypeDefExpr),
    TypeCast { expr: Arc<Expr>, typ: Type },
    Apply(ApplyExpr),
    Any { args: Arc<[Expr]> },
    Array { args: Arc<[Expr]> },
    Map { args: Arc<[(Expr, Expr)]> },
    Tuple { args: Arc<[Expr]> },
    Variant { tag: ArcStr, args: Arc<[Expr]> },
    Struct(StructExpr),
    Select(SelectExpr),
    Qop(Arc<Expr>),
    OrNever(Arc<Expr>),
    TryCatch(Arc<TryCatchExpr>),
    ByRef(Arc<Expr>),
    Deref(Arc<Expr>),
    Neg(Arc<Expr>),
    Eq { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Ne { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Lt { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Gt { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Lte { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Gte { lhs: Arc<Expr>, rhs: Arc<Expr> },
    And { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Or { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Not { expr: Arc<Expr> },
    Add { lhs: Arc<Expr>, rhs: Arc<Expr> },
    CheckedAdd { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Sub { lhs: Arc<Expr>, rhs: Arc<Expr> },
    CheckedSub { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Mul { lhs: Arc<Expr>, rhs: Arc<Expr> },
    CheckedMul { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Div { lhs: Arc<Expr>, rhs: Arc<Expr> },
    CheckedDiv { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Mod { lhs: Arc<Expr>, rhs: Arc<Expr> },
    CheckedMod { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Sample { lhs: Arc<Expr>, rhs: Arc<Expr> },
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

// hallowed are the ori
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
    /// expression (and trailing dangling comments). `None` unless the
    /// expression was decorated; not compared by equality.
    pub dec: Option<Box<Decorations>>,
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(dec) = &self.dec {
            for c in dec.comments.iter() {
                writeln!(f, "//{c}")?;
            }
            for a in dec.attrs.iter() {
                writeln!(f, "{a}")?;
            }
        }
        write!(f, "{}", self.kind)
    }
}

impl PrettyDisplay for Expr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        use std::fmt::Write;
        if let Some(dec) = &self.dec {
            for c in dec.comments.iter() {
                writeln!(buf, "//{c}")?;
            }
            for a in dec.attrs.iter() {
                writeln!(buf, "{a}")?;
            }
        }
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
        let init = f(init, self);
        match &self.kind {
            ExprKind::Constant(_)
            | ExprKind::NoOp
            | ExprKind::Use { name: _ }
            | ExprKind::Ref { name: _ }
            | ExprKind::TypeDef(_) => init,
            ExprKind::ExplicitParens(e) => e.fold(init, f),
            ExprKind::StructRef { source, field: _ }
            | ExprKind::TupleRef { source, field: _ } => source.fold(init, f),

            ExprKind::Map { args } => args.iter().fold(init, |init, (k, v)| {
                let init = k.fold(init, f);
                v.fold(init, f)
            }),
            ExprKind::MapRef { source, key } => {
                let init = source.fold(init, f);
                key.fold(init, f)
            }
            ExprKind::Module {
                name: _,
                value: ModuleKind::Resolved { exprs, sig: _, from_interface: _ },
            } => exprs.iter().fold(init, |init, e| e.fold(init, f)),
            ExprKind::Module {
                name: _,
                value: ModuleKind::Dynamic { sandbox: _, sig: _, source },
            } => source.fold(init, f),
            ExprKind::Module {
                name: _,
                value: ModuleKind::Unresolved { from_interface: _ },
            } => init,
            ExprKind::Do { exprs } => exprs.iter().fold(init, |init, e| e.fold(init, f)),
            ExprKind::Bind(b) => b.value.fold(init, f),
            ExprKind::StructWith(StructWithExpr { source, replace }) => {
                let init = source.fold(init, f);
                replace.iter().fold(init, |init, (_, e)| e.fold(init, f))
            }
            ExprKind::Connect { name: _, value, deref: _ } => value.fold(init, f),
            ExprKind::Lambda(l) => {
                // Fold labeled-arg DEFAULT expressions (`#x = expr`) — they
                // are real sub-expressions that can reference captures, so a
                // complete tree walk must visit them. Then the body.
                let init = l.args.iter().fold(init, |init, a| match &a.labeled {
                    Some(Some(default)) => default.fold(init, f),
                    _ => init,
                });
                match &l.body {
                    Either::Left(e) => e.fold(init, f),
                    Either::Right(_) => init,
                }
            }
            ExprKind::TypeCast { expr, typ: _ } => expr.fold(init, f),
            ExprKind::Apply(ApplyExpr { args, function }) => {
                let init = function.fold(init, f);
                args.iter().fold(init, |init, (_, e)| e.fold(init, f))
            }
            ExprKind::Any { args }
            | ExprKind::Array { args }
            | ExprKind::Tuple { args }
            | ExprKind::Variant { tag: _, args }
            | ExprKind::StringInterpolate { args } => {
                args.iter().fold(init, |init, e| e.fold(init, f))
            }
            ExprKind::ArrayRef { source, i } => {
                let init = source.fold(init, f);
                i.fold(init, f)
            }
            ExprKind::ArraySlice { source, start, end } => {
                let init = source.fold(init, f);
                let init = match start {
                    None => init,
                    Some(e) => e.fold(init, f),
                };
                match end {
                    None => init,
                    Some(e) => e.fold(init, f),
                }
            }
            ExprKind::Struct(StructExpr { args }) => {
                args.iter().fold(init, |init, (_, e)| e.fold(init, f))
            }
            ExprKind::Select(SelectExpr { arg, arms }) => {
                let init = arg.fold(init, f);
                arms.iter().fold(init, |init, (p, e)| {
                    let init = match p.guard.as_ref() {
                        None => init,
                        Some(g) => g.fold(init, f),
                    };
                    e.fold(init, f)
                })
            }
            ExprKind::TryCatch(tc) => {
                let init = tc.exprs.iter().fold(init, |init, e| e.fold(init, f));
                tc.handler.fold(init, f)
            }
            ExprKind::Qop(e)
            | ExprKind::OrNever(e)
            | ExprKind::ByRef(e)
            | ExprKind::Deref(e)
            | ExprKind::Neg(e)
            | ExprKind::Not { expr: e } => e.fold(init, f),
            ExprKind::Add { lhs, rhs }
            | ExprKind::CheckedAdd { lhs, rhs }
            | ExprKind::Sub { lhs, rhs }
            | ExprKind::CheckedSub { lhs, rhs }
            | ExprKind::Mul { lhs, rhs }
            | ExprKind::CheckedMul { lhs, rhs }
            | ExprKind::Div { lhs, rhs }
            | ExprKind::CheckedDiv { lhs, rhs }
            | ExprKind::Mod { lhs, rhs }
            | ExprKind::CheckedMod { lhs, rhs }
            | ExprKind::And { lhs, rhs }
            | ExprKind::Or { lhs, rhs }
            | ExprKind::Eq { lhs, rhs }
            | ExprKind::Ne { lhs, rhs }
            | ExprKind::Gt { lhs, rhs }
            | ExprKind::Lt { lhs, rhs }
            | ExprKind::Gte { lhs, rhs }
            | ExprKind::Lte { lhs, rhs }
            | ExprKind::Sample { lhs, rhs } => {
                let init = lhs.fold(init, f);
                rhs.fold(init, f)
            }
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
