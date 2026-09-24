use crate::{
    PRINT_FLAGS, PrintFlag,
    expr::print::{PrettyBuf, PrettyDisplay},
    typ::{FnType, TVar, Type},
};
use arcstr::{ArcStr, literal};
pub use binop::BinOp;
use combine::stream::position::SourcePosition;
pub use context::{At, ErrorContext, ErrorSite, ParserContext};
pub use modpath::ModPath;
use netidx_core::{pack::PackError, path::Path, utils::Either};
use netidx_derive::Pack;
use netidx_value::Value;
pub(crate) use pattern::union_members;
pub use pattern::{Pattern, StructurePattern};
use poolshark::local::LPooled;
pub use resolver::{
    BufferOverrides, FilesResolver, ModuleResolver, Resolution, ResolverFactory,
    ResolverRef, Resolvers, RootFile, VfsEntry, VfsResolver, add_interface_modules,
    parse_modpath, read_optional, read_to_arcstr,
};
use smallvec::SmallVec;
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

mod binop;
mod context;
pub mod format;
mod modpath;
pub mod parser;
mod pattern;
pub mod print;
mod resolver;
pub(crate) mod seq;
pub mod serialize;
#[cfg(test)]
mod test;

image_id!(ExprId);

static DEFAULT_ORIGIN: LazyLock<Arc<Origin>> =
    LazyLock::new(|| Arc::new(Origin::default()));

thread_local! {
    static ORIGIN: RefCell<Option<Arc<Origin>>> = RefCell::new(None);
}

/// The source the expressions built on this thread come from, while the
/// scope lives: a parse, a decode, a lowering. Scopes nest; outside every
/// scope an expression takes the default origin, never one another
/// context left behind on a shared thread.
pub(crate) struct OriginScope(Option<Arc<Origin>>);

impl OriginScope {
    pub(crate) fn enter(ori: Arc<Origin>) -> Self {
        OriginScope(ORIGIN.with_borrow_mut(|cur| cur.replace(ori)))
    }
}

impl Drop for OriginScope {
    fn drop(&mut self) {
        let outer = self.0.take();
        ORIGIN.with_borrow_mut(|cur| *cur = outer)
    }
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

// XCR claude for eric: [readability] `labeled: Option<Option<Expr>>`, LambdaExpr's
// `vargs` and `body: Either<Expr, ArcStr>` as named enums would touch lambda.rs,
// callsite.rs, traits.rs, fusion/lowering.rs, seq.rs and stdlib core (calls, fusion-b,
// seq-ops): worth one mechanical pass after the merge, not nine parallel ones.
#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct Arg {
    pub labeled: Option<Option<Expr>>,
    pub pattern: StructurePattern,
    pub constraint: Option<Type>,
    pub pos: WrittenAt,
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

/// The `//` lines above an interface item or a trait method. Like an
/// expression's comments they decide nothing: equal to every other.
#[derive(Debug, Clone, Default)]
pub struct Comments(Option<Arc<[ArcStr]>>);

impl Comments {
    pub fn of(lines: impl ExactSizeIterator<Item = ArcStr>) -> Self {
        Self((lines.len() > 0).then(|| Arc::from_iter(lines)))
    }

    pub fn lines(&self) -> &[ArcStr] {
        self.0.as_deref().unwrap_or(&[])
    }
}

impl PartialEq for Comments {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl PartialOrd for Comments {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        Some(Ordering::Equal)
    }
}

impl netidx_core::pack::Pack for Comments {
    fn encoded_len(&self) -> usize {
        self.0.encoded_len()
    }

    fn encode(&self, buf: &mut impl bytes::BufMut) -> result::Result<(), PackError> {
        self.0.encode(buf)
    }

    fn decode(buf: &mut impl bytes::Buf) -> result::Result<Self, PackError> {
        Ok(Self(netidx_core::pack::Pack::decode(buf)?))
    }
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
    pub name: Name,
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
    pub name: Name,
    pub methods: Arc<[TraitMethod]>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct TraitMethod {
    pub comments: Comments,
    pub doc: Doc,
    pub name: Name,
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
    // XCR claude for eric: a method is an Expr in all but name: Impl::compile needs
    // its id, pos and decorations (a core trait's gets `#[sync]`) and the bind's
    // rec and type, so a method struct would copy Expr's fields. The parser refuses
    // every other shape; traits.rs's two `unreachable!`s are the whole cost.
    pub methods: Arc<[Expr]>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct BindSig {
    pub name: Name,
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
    pub rename: Option<Name>,
    /// Where each segment of `path` stands in the use tree.
    pub at: WrittenPath,
}

impl UseItem {
    /// The names of one use statement in the order every statement
    /// holds them, so that a printed statement reads back equal.
    pub fn sorted(names: impl IntoIterator<Item = UseItem>) -> Arc<[UseItem]> {
        let mut names: LPooled<Vec<UseItem>> = names.into_iter().collect();
        names.sort_by(print::cmp_use_items);
        Arc::from_iter(names.drain(..))
    }

    pub fn plain(path: ModPath) -> Self {
        Self { path, rename: None, at: WrittenPath::default() }
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
    Module(Name),
    Use { reexport: bool, names: Arc<[UseItem]> },
}

#[derive(Debug, Clone, Pack)]
#[pack(unwrapped)]
pub struct SigItem {
    pub comments: Comments,
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

/// What starts a seq run: the expression whose fire enters the machine,
/// or `let pattern = expr`, which also names that fire's value inside
/// the body.
#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub enum SeqTrigger {
    Expr(Arc<Expr>),
    /// `let pattern[: typ] = value`; `rec` is refused at lowering
    Bind(Arc<BindExpr>),
}

impl SeqTrigger {
    pub fn expr(&self) -> &Expr {
        match self {
            SeqTrigger::Expr(e) => e,
            SeqTrigger::Bind(b) => &b.value,
        }
    }

    pub fn map(&self, f: impl FnOnce(&Expr) -> Expr) -> SeqTrigger {
        match self {
            SeqTrigger::Expr(e) => SeqTrigger::Expr(Arc::new(f(e))),
            SeqTrigger::Bind(b) => SeqTrigger::Bind(Arc::new(BindExpr {
                rec: b.rec,
                pattern: b.pattern.clone(),
                typ: b.typ.clone(),
                value: f(&b.value),
            })),
        }
    }
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

// XCR claude for eric: agreed, but the roles are built in seq.rs, read in node/error.rs,
// fusion/mod.rs and node_shape.rs (seq-ops, fusion-b, core-misc): a `CatchRole` enum is
// one pass across those after the merge. Recommend `role: CatchRole { User,
// SeqMachine { abort, manual, pc }, SeqJump { jump, capture } }`.
#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct CatchExpr {
    pub bind: Name,
    pub constraint: Option<Type>,
    pub handler: Arc<Expr>,
    /// Compiler-only: this catch unconditionally rethrows before aborting.
    pub seq_abort: Option<Arc<Expr>>,
    /// Compiler-only: a seq `try`'s per-arm handler. The first error
    /// delivered per failure is written to this cell, and the handler's
    /// inferred throws are unioned into the cell's type.
    pub seq_capture: Option<ArcStr>,
    /// Compiler-only: a seq's `abort(..)` event. A fired production
    /// requests the abort action without an error.
    pub seq_manual: Option<Arc<Expr>>,
    /// Compiler-only: the machine's step variable, written idle when the
    /// machine sleeps.
    pub seq_pc: Option<ArcStr>,
}

/// `try { stmts } with(e[: T]) { stmts }` — a seq statement: an
/// error-triggered branch.
#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct TryWithExpr {
    pub body: Arc<[Expr]>,
    pub bind: Name,
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
        name: Name,
        value: ModuleKind,
    },
    ExplicitParens(Arc<Expr>),
    // XCR claude for eric: agreed; the rename touches node/bind.rs, node/compiler.rs,
    // seq.rs, graphix-rt and graphix-fuzz besides this package, so it is a one-line
    // sed (`ExprKind::Do` -> `ExprKind::Block`, `Do {` under the globs) after the merge.
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
        field: Name,
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
    // XCR claude for eric: the parser now refuses both where written (a `flush` in a
    // `seq` head, a `let rec` trigger); `SeqKind { Plain, Queued { flush } }` would also
    // reshape seq.rs's desugar (seq-ops), so it is left for that package.
    Seq {
        queued: bool,
        trigger: Option<SeqTrigger>,
        /// `abort(e)`: a fire of `e` during a run ends it
        abort: Option<Arc<Expr>>,
        /// `flush(e)`, `seqq` only: an abort that also empties the queue
        flush: Option<Arc<Expr>>,
        body: Arc<[Expr]>,
    },
    /// `until expr` — wait until a bool level is true. Legal only as a
    /// seq step.
    Until(Arc<Expr>),
    /// `try { stmts } with(e) { stmts }` — legal only as a seq step.
    TryWith(Arc<TryWithExpr>),
    Qop(Arc<Expr>),
    /// Compiler-generated forwarding; a nonthrowing region supplies bottom.
    Rethrow(Arc<Expr>),
    /// Compiler-generated sequence completion boundary.
    SeqGuard(Arc<Expr>),
    /// Compiler-generated: a fired production fails the enclosing seq
    /// machine's run.
    SeqAbort(Arc<Expr>),
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
        Expr::new(self, pos)
    }

    /// An expression written nowhere.
    pub fn to_expr_nopos(self) -> Expr {
        Expr::new(self, Default::default())
    }

    /// The direct sub-expressions, in [`Expr::for_each_child`]'s order.
    pub fn for_each_child<'a>(&'a self, f: &mut impl FnMut(&'a Expr)) {
        use ExprKind::*;
        match self {
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
            | SeqAbort(x)
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
            | Variant { args: xs, .. } => xs.iter().for_each(|e| f(e)),
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
                c.seq_manual.iter().for_each(|e| f(e));
            }
            Seq { trigger, abort, flush, body, .. } => {
                trigger.iter().for_each(|t| f(t.expr()));
                abort.iter().for_each(|e| f(e));
                flush.iter().for_each(|e| f(e));
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
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
pub enum Source {
    File(PathBuf),
    Netidx(Path),
    Internal(ArcStr),
    #[default]
    Unspecified,
}

/// Where the source is, as `in <source>` reads it; `Unspecified` says
/// nothing.
impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Source::File(p) => write!(f, "file {}", p.display()),
            Source::Netidx(p) => write!(f, "netidx {p}"),
            Source::Internal(m) => write!(f, "module {m}"),
            Source::Unspecified => write!(f, "expr"),
        }
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
        let write = |f: &mut fmt::Formatter<'_>, o: &Origin| {
            write!(f, "{}", o.source)?;
            match o.source {
                Source::Unspecified if !flags.contains(PrintFlag::NoSource) => {
                    write!(f, " {}", o.text)
                }
                _ => Ok(()),
            }
        };
        write!(f, "in ")?;
        write(f, self)?;
        if !flags.contains(PrintFlag::NoParents) {
            let mut p = &self.parent;
            while let Some(parent) = p {
                write!(f, "\n    included from ")?;
                write(f, parent)?;
                p = &parent.parent;
            }
        }
        Ok(())
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

    /// The origin of source text that came from nowhere in particular.
    pub fn unspecified(s: &str) -> Self {
        Self { parent: None, source: Source::Unspecified, text: ArcStr::from(s) }
    }
}

/// Where in its source something was written, for the IDE and the
/// formatter. It never decides anything: every `WrittenAt` is equal to
/// every other, hashes to nothing and packs to nothing, so a type that
/// holds one derives its own comparisons as if it did not. So
/// `at == WrittenAt::NOWHERE` is always true: ask [`WrittenAt::get`].
#[derive(Debug, Clone, Copy)]
pub struct WrittenAt(pub SourcePosition);

impl Default for WrittenAt {
    fn default() -> Self {
        Self::NOWHERE
    }
}

impl WrittenAt {
    /// Not written: built by the compiler.
    pub const NOWHERE: Self = Self(SourcePosition { line: 0, column: 0 });

    /// Where it was written; `None` for what the compiler built or
    /// unpacked.
    pub fn get(&self) -> Option<SourcePosition> {
        (self.0 != Self::NOWHERE.0).then_some(self.0)
    }

    /// The key that sorts things into the order they were written in;
    /// what was never written sorts first.
    pub fn order(&self) -> (i32, i32) {
        (self.0.line, self.0.column)
    }
}

impl PartialEq for WrittenAt {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl Eq for WrittenAt {}

impl PartialOrd for WrittenAt {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WrittenAt {
    fn cmp(&self, _: &Self) -> Ordering {
        Ordering::Equal
    }
}

impl std::hash::Hash for WrittenAt {
    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
}

impl netidx_core::pack::Pack for WrittenAt {
    fn encoded_len(&self) -> usize {
        0
    }

    fn encode(&self, _: &mut impl bytes::BufMut) -> result::Result<(), PackError> {
        Ok(())
    }

    fn decode(_: &mut impl bytes::Buf) -> result::Result<Self, PackError> {
        Ok(Self::NOWHERE)
    }
}

/// Where each segment of a path was written. Like [`WrittenAt`], it
/// decides nothing: equal to every other, hashes and packs to nothing.
#[derive(Debug, Clone, Default)]
pub struct WrittenPath(pub SmallVec<[SourcePosition; 4]>);

impl PartialEq for WrittenPath {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl Eq for WrittenPath {}

impl PartialOrd for WrittenPath {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        Some(Ordering::Equal)
    }
}

impl std::hash::Hash for WrittenPath {
    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
}

impl netidx_core::pack::Pack for WrittenPath {
    fn encoded_len(&self) -> usize {
        0
    }

    fn encode(&self, _: &mut impl bytes::BufMut) -> result::Result<(), PackError> {
        Ok(())
    }

    fn decode(_: &mut impl bytes::Buf) -> result::Result<Self, PackError> {
        Ok(Self::default())
    }
}

/// A name where it is declared or selected: the identifier and where it
/// was written. A `Name` is its identifier to every comparison, hash
/// and encoding (see [`WrittenAt`]).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name {
    pub name: ArcStr,
    pub at: WrittenAt,
}

impl Name {
    pub fn written(name: ArcStr, at: SourcePosition) -> Self {
        Self { name, at: WrittenAt(at) }
    }

    /// Where the name was written, else `enclosing` for a name the
    /// compiler built or unpacked.
    pub fn pos_or(&self, enclosing: SourcePosition) -> SourcePosition {
        self.at.get().unwrap_or(enclosing)
    }
}

impl<T: Into<ArcStr>> From<T> for Name {
    fn from(name: T) -> Self {
        Self { name: name.into(), at: WrittenAt::NOWHERE }
    }
}

impl Deref for Name {
    type Target = ArcStr;

    fn deref(&self) -> &ArcStr {
        &self.name
    }
}

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        &self.name
    }
}

impl std::borrow::Borrow<str> for Name {
    fn borrow(&self) -> &str {
        &self.name
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.name, f)
    }
}

impl PartialEq<str> for Name {
    fn eq(&self, other: &str) -> bool {
        &*self.name == other
    }
}

impl netidx_core::pack::Pack for Name {
    fn encoded_len(&self) -> usize {
        self.name.encoded_len()
    }

    fn encode(&self, buf: &mut impl bytes::BufMut) -> result::Result<(), PackError> {
        self.name.encode(buf)
    }

    fn decode(buf: &mut impl bytes::Buf) -> result::Result<Self, PackError> {
        Ok(Self::from(ArcStr::decode(buf)?))
    }
}

/// The delimiters a string literal was written between.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum StrForm {
    /// `"text [splice]"`
    #[default]
    Quoted,
    /// `r"text"`, `r#"text"#`
    Raw,
    /// `"""text \[splice]"""`
    Template,
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
    pub dec: Option<Arc<Decorations>>,
    /// How a string literal was delimited, so that it prints as written;
    /// not compared by equality, and not part of the packed form.
    pub str_form: StrForm,
    /// Where the expression's text ends (exclusive), if it was parsed.
    pub end: WrittenAt,
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
        crate::stack::ensure_sufficient(|| write!(f, "{}", print::Bare(self)))
    }
}

impl PrettyDisplay for Expr {
    fn decorated(&self) -> bool {
        print::decorated(self)
    }

    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        print::write_leading(buf, &self.dec)?;
        print::Bare(self).fmt_pretty(buf)
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
    /// Record where the expression's text ends. The first writer is the
    /// parser nearest the node, so it wins.
    pub fn ending(mut self, end: SourcePosition) -> Self {
        if self.end.get().is_none() {
            self.end = WrittenAt(end);
        }
        self
    }

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

impl Default for Expr {
    fn default() -> Self {
        let mut e = ExprKind::Constant(Value::Null).to_expr(Default::default());
        e.ori = DEFAULT_ORIGIN.clone();
        e
    }
}

impl FromStr for Expr {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> result::Result<Self, Self::Err> {
        parser::parse_one(s)
    }
}

impl Expr {
    /// This string literal, recorded as written between `form`'s delimiters.
    pub(crate) fn written_as(mut self, form: StrForm) -> Self {
        self.str_form = form;
        self
    }

    pub fn new(kind: ExprKind, pos: SourcePosition) -> Self {
        Expr {
            id: ExprId::new(),
            ori: get_origin(),
            pos,
            kind,
            dec: None,
            str_form: Default::default(),
            end: Default::default(),
        }
    }

    /// This expression with `kind` in place of its own, under a fresh id.
    pub fn with_kind(&self, kind: ExprKind) -> Self {
        Expr {
            id: ExprId::new(),
            ori: self.ori.clone(),
            pos: self.pos,
            kind,
            dec: self.dec.clone(),
            str_form: self.str_form,
            end: self.end,
        }
    }

    /// A compiler-built expression standing at `at`'s origin and position,
    /// undecorated and with no written end.
    pub(crate) fn synth(at: &Expr, kind: ExprKind) -> Self {
        Expr {
            id: ExprId::new(),
            ori: at.ori.clone(),
            pos: at.pos,
            kind,
            dec: None,
            str_form: Default::default(),
            end: Default::default(),
        }
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
    /// shared by `fold` and, in agreement (a test holds them to it),
    /// `map_children`.
    pub fn for_each_child<'a>(&'a self, f: &mut impl FnMut(&'a Expr)) {
        self.kind.for_each_child(f)
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
            SeqAbort(x) => SeqAbort(a(f, x)),
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
                    comments: m.comments.clone(),
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
                seq_manual: c.seq_manual.as_ref().map(|e| a(f, e)),
                seq_pc: c.seq_pc.clone(),
            })),
            Seq { queued, trigger, abort, flush, body } => Seq {
                queued: *queued,
                trigger: trigger.as_ref().map(|t| t.map(|e| f(e))),
                abort: abort.as_ref().map(|e| a(f, e)),
                flush: flush.as_ref().map(|e| a(f, e)),
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
        self.with_kind(kind)
    }
}
