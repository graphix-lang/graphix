use crate::{
    expr::print::{PrettyBuf, PrettyDisplay},
    typ::{TVar, Type},
    PrintFlag, PRINT_FLAGS,
};
use anyhow::Result;
use arcstr::{literal, ArcStr};
use combine::stream::position::SourcePosition;
pub use modpath::ModPath;
use netidx::{path::Path, subscriber::Value, utils::Either};
pub use pattern::{Pattern, StructurePattern};
use regex::Regex;
pub use resolver::ModuleResolver;
use serde::{
    de::{self, Visitor},
    Deserialize, Deserializer, Serialize, Serializer,
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

#[derive(Debug)]
pub struct CouldNotResolve(ArcStr);

impl fmt::Display for CouldNotResolve {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "could not resolve module {}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Arg {
    pub labeled: Option<Option<Expr>>,
    pub pattern: StructurePattern,
    pub constraint: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Doc(Option<ArcStr>);

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct TypeDefExpr {
    pub name: ArcStr,
    pub params: Arc<[(TVar, Option<Type>)]>,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct BindSig {
    pub name: ArcStr,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum SigKind {
    TypeDef(TypeDefExpr),
    Bind(BindSig),
    Module(ArcStr),
    Use(ModPath),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct SigItem {
    pub doc: Doc,
    pub kind: SigKind,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Sandbox {
    Unrestricted,
    Blacklist(Arc<[ModPath]>),
    Whitelist(Arc<[ModPath]>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ModuleKind {
    Dynamic { sandbox: Sandbox, sig: Sig, source: Arc<Expr> },
    Resolved { exprs: Arc<[Expr]>, sig: Option<Sig>, from_interface: bool },
    Unresolved { from_interface: bool },
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct BindExpr {
    pub rec: bool,
    pub pattern: StructurePattern,
    pub typ: Option<Type>,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct LambdaExpr {
    pub args: Arc<[Arg]>,
    pub vargs: Option<Option<Type>>,
    pub rtype: Option<Type>,
    pub constraints: Arc<[(TVar, Type)]>,
    pub throws: Option<Type>,
    pub body: Either<Expr, ArcStr>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct TryCatchExpr {
    pub bind: ArcStr,
    pub constraint: Option<Type>,
    pub handler: Arc<Expr>,
    pub exprs: Arc<[Expr]>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct StructWithExpr {
    pub source: Arc<Expr>,
    pub replace: Arc<[(ArcStr, Expr)]>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct StructExpr {
    pub args: Arc<[(ArcStr, Expr)]>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ApplyExpr {
    pub args: Arc<[(Option<ArcStr>, Expr)]>,
    pub function: Arc<Expr>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct SelectExpr {
    pub arg: Arc<Expr>,
    pub arms: Arc<[(Pattern, Expr)]>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
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
    Sub { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Mul { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Div { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Mod { lhs: Arc<Expr>, rhs: Arc<Expr> },
    Sample { lhs: Arc<Expr>, rhs: Arc<Expr> },
}

impl ExprKind {
    pub fn to_expr(self, pos: SourcePosition) -> Expr {
        Expr { id: ExprId::new(), ori: get_origin(), pos, kind: self }
    }

    /// does not provide any position information or comment
    pub fn to_expr_nopos(self) -> Expr {
        Expr { id: ExprId::new(), ori: get_origin(), pos: Default::default(), kind: self }
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
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub id: ExprId,
    pub ori: Arc<Origin>,
    pub pos: SourcePosition,
    pub kind: ExprKind,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl PrettyDisplay for Expr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
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
        Expr { id: ExprId::new(), ori: get_origin(), pos, kind }
    }

    /// fold over self and all of self's sub expressions
    pub fn fold<T, F: FnMut(T, &Self) -> T>(&self, init: T, f: &mut F) -> T {
        let init = f(init, self);
        match &self.kind {
            ExprKind::Constant(_)
            | ExprKind::NoOp
            | ExprKind::Use { .. }
            | ExprKind::Ref { .. }
            | ExprKind::TypeDef { .. } => init,
            ExprKind::ExplicitParens(e) => e.fold(init, f),
            ExprKind::StructRef { source, .. } | ExprKind::TupleRef { source, .. } => {
                source.fold(init, f)
            }

            ExprKind::Map { args } => args.iter().fold(init, |init, (k, v)| {
                let init = k.fold(init, f);
                v.fold(init, f)
            }),
            ExprKind::MapRef { source, key } => {
                let init = source.fold(init, f);
                key.fold(init, f)
            }
            ExprKind::Module { value: ModuleKind::Resolved { exprs, .. }, .. } => {
                exprs.iter().fold(init, |init, e| e.fold(init, f))
            }
            ExprKind::Module {
                value: ModuleKind::Dynamic { sandbox: _, sig: _, source },
                ..
            } => source.fold(init, f),
            ExprKind::Module { value: ModuleKind::Unresolved { .. }, .. } => init,
            ExprKind::Do { exprs } => exprs.iter().fold(init, |init, e| e.fold(init, f)),
            ExprKind::Bind(b) => b.value.fold(init, f),
            ExprKind::StructWith(StructWithExpr { replace, .. }) => {
                replace.iter().fold(init, |init, (_, e)| e.fold(init, f))
            }
            ExprKind::Connect { value, .. } => value.fold(init, f),
            ExprKind::Lambda(l) => match &l.body {
                Either::Left(e) => e.fold(init, f),
                Either::Right(_) => init,
            },
            ExprKind::TypeCast { expr, .. } => expr.fold(init, f),
            ExprKind::Apply(ApplyExpr { args, function: _ }) => {
                args.iter().fold(init, |init, (_, e)| e.fold(init, f))
            }
            ExprKind::Any { args }
            | ExprKind::Array { args }
            | ExprKind::Tuple { args }
            | ExprKind::Variant { args, .. }
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
            | ExprKind::Not { expr: e } => e.fold(init, f),
            ExprKind::Add { lhs, rhs }
            | ExprKind::Sub { lhs, rhs }
            | ExprKind::Mul { lhs, rhs }
            | ExprKind::Div { lhs, rhs }
            | ExprKind::Mod { lhs, rhs }
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
            if buf.len() <= MAX {
                write!(f, "at: {}, in: {buf}", self.0.pos)
            } else {
                let mut end = MAX;
                while !buf.is_char_boundary(end) {
                    end += 1
                }
                let buf = &buf[0..end];
                write!(f, "at: {}, in: {buf}..", self.0.pos)
            }
        })
    }
}
