use crate::SourcePosition;
use crate::image::ImageBuf;
use crate::{
    BindId, CFlag, CompileCtx, ExecCtx, Notify, PrintFlag, Rt, Scope, Tag, TagValue,
    env::Env,
    expr::{
        AbstractOf, ExprId, Name, Origin, Pattern, StructurePattern, WrittenAt,
        union_members, union_members_with,
    },
    format_with_flags,
    node::{Held, compiler, list},
    rep,
    typ::{AbstractId, ContainsFlags, IsAFlags, ShallowScrutinee, TVar, Type},
};
use ahash::AHashMap;
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, encode_varint, varint_len};
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use smallvec::{SmallVec, smallvec};
use std::fmt::Debug;
use triomphe::Arc;

/// The three shapes the exact-length slice pattern compiles against:
/// a tuple (fixed arity), an array, or the native List.
#[derive(Debug, Clone, Copy, PartialEq, Eq, netidx_derive::Pack)]
#[pack(unwrapped)]
pub enum SliceKind {
    Tuple,
    Array,
    List,
}

/// How many fields a struct a struct pattern matches has.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StructWidth {
    /// The field count of the struct type the pattern was compiled or
    /// realigned against.
    Exact(usize),
    /// A partial pattern over a predicate inferred from the fields it
    /// names, which nothing completed.
    AtLeast(usize),
}

impl StructWidth {
    pub fn admits(self, len: usize) -> bool {
        match self {
            Self::Exact(n) => len == n,
            Self::AtLeast(n) => len >= n,
        }
    }

    fn parts(self) -> (bool, usize) {
        match self {
            Self::Exact(n) => (false, n),
            Self::AtLeast(n) => (true, n),
        }
    }
}

impl Pack for StructWidth {
    fn encoded_len(&self) -> usize {
        self.parts().encoded_len()
    }

    fn encode(&self, buf: &mut impl bytes::BufMut) -> Result<(), PackError> {
        self.parts().encode(buf)
    }

    fn decode(buf: &mut impl bytes::Buf) -> Result<Self, PackError> {
        Ok(match <(bool, usize)>::decode(buf)? {
            (false, n) => Self::Exact(n),
            (true, n) => Self::AtLeast(n),
        })
    }
}

#[derive(Debug)]
pub enum StructPatternNode {
    Ignore,
    Literal(Value),
    Bind(BindId),
    Slice {
        kind: SliceKind,
        all: Option<BindId>,
        binds: Box<[StructPatternNode]>,
    },
    SlicePrefix {
        /// true = the native-list prefix form `[<h, rest..>]`; `tail`
        /// binds the tail as a List, sharing structure.
        list: bool,
        all: Option<BindId>,
        prefix: Box<[StructPatternNode]>,
        tail: Option<BindId>,
    },
    SliceSuffix {
        all: Option<BindId>,
        head: Option<BindId>,
        suffix: Box<[StructPatternNode]>,
    },
    Struct {
        all: Option<BindId>,
        width: StructWidth,
        binds: Box<[(ArcStr, usize, StructPatternNode)]>,
    },
    Variant {
        tag: ArcStr,
        all: Option<BindId>,
        binds: Box<[StructPatternNode]>,
    },
    /// `T(p)`: a value of the abstract type `id`, its payload
    /// destructured by `bind` against `rep` (the type's
    /// representation at this scrutinee's parameters)
    Abstract {
        id: AbstractId,
        all: Option<BindId>,
        rep: Type,
        bind: Box<StructPatternNode>,
    },
    /// Or-alternatives share alternative 0's BindIds, so the id walks
    /// (`ids`, `fresh_ids`, `delete`) visit alternative 0 only; `is_match`
    /// is any-of and `bind` delivers the first matching alternative.
    Or {
        alts: Box<[StructPatternNode]>,
    },
}

/// How pattern-leaf names bind during compile: `Fresh` allocates;
/// `Record` allocates and records `name → (id, type)` (an or-pattern's
/// first alternative); `Reuse` looks the id up instead of allocating and
/// adds nothing to the env. A name the alternatives share types as the
/// union of their types: under a written predicate here, under an
/// inferred one once the select narrows them.
enum BindMode<'a> {
    Fresh,
    Record(&'a mut AHashMap<ArcStr, (BindId, Type)>),
    Reuse(&'a mut AHashMap<ArcStr, (BindId, Type)>),
}

impl BindMode<'_> {
    fn reborrow(&mut self) -> BindMode<'_> {
        match self {
            Self::Fresh => BindMode::Fresh,
            Self::Record(m) => BindMode::Record(&mut **m),
            Self::Reuse(m) => BindMode::Reuse(&mut **m),
        }
    }
}

/// What stays fixed across one pattern's compile.
#[derive(Clone, Copy)]
struct PatCx<'a> {
    scope: &'a Scope,
    pos: SourcePosition,
    ori: &'a Arc<Origin>,
    /// The predicate was inferred from this pattern, so an or-pattern's
    /// Set holds one member per alternative, in order.
    inferred: bool,
}

fn leaf_bind<R: Rt>(
    ctx: &mut CompileCtx<R>,
    cx: PatCx,
    name: &Name,
    typ: &Type,
    mode: &mut BindMode,
) -> Result<BindId> {
    let pos = name.pos_or(cx.pos);
    let name = &name.name;
    let fresh = |ctx: &mut CompileCtx<R>| {
        ctx.env
            .bind_variable(&cx.scope.lexical, name, typ.clone(), pos, cx.ori.clone())
            .id
    };
    match mode {
        BindMode::Fresh => Ok(fresh(ctx)),
        BindMode::Record(map) => {
            let id = fresh(ctx);
            map.insert(name.clone(), (id, typ.clone()));
            Ok(id)
        }
        BindMode::Reuse(map) => match map.get(name) {
            // under an inferred predicate the alternatives' types are
            // judged once the select narrows them (`leaves`)
            Some((id, _)) if cx.inferred => Ok(*id),
            Some((id, t0)) => {
                let id = *id;
                let probe = BitFlags::empty();
                if !(t0.contains_with_flags(probe, &ctx.env, typ)?
                    && typ.contains_with_flags(probe, &ctx.env, t0)?)
                {
                    let u = Type::union(&ctx.env, &[t0, typ])?;
                    map.insert(name.clone(), (id, u.clone()));
                    ctx.env.retype(id, u);
                }
                Ok(id)
            }
            None => bail!(
                "or-pattern alternatives must bind the same names ({name} is \
                 not bound by the first alternative)"
            ),
        },
    }
}

/// Bind a `name@` capture: the whole value at this position.
fn bind_all<R: Rt>(
    ctx: &mut CompileCtx<R>,
    cx: PatCx,
    all: &Option<Name>,
    typ: &Type,
    mode: &mut BindMode,
) -> Result<Option<BindId>> {
    all.as_ref().map(|n| leaf_bind(ctx, cx, n, typ, mode)).transpose()
}

/// The members of an inferred Set, one per child: an or-pattern's
/// alternatives, or a slice's elements followed by its rest.
pub(crate) fn set_members(typ: &Type, n: usize) -> Option<Arc<[Type]>> {
    typ.with_deref(|t| match t {
        Some(Type::Set(ts)) if ts.len() == n => Some(ts.clone()),
        _ => None,
    })
}

/// The members of the union `t`, an open cell as itself.
fn members_or_open(env: &Env, t: &Type, out: &mut SmallVec<[Type; 8]>) -> Result<()> {
    crate::stack::ensure_sufficient(|| match &t.deref_cloned() {
        None => {
            out.push(t.clone());
            Ok(())
        }
        Some(Type::Set(ts)) => ts.iter().try_for_each(|t| members_or_open(env, t, out)),
        Some(_) => union_members(env, t, out),
    })
}

/// `typ` through its bindings and aliases: the constructor at its head.
/// The chase fills no alias's cell: a name the body names may yet be
/// declared by a later statement.
fn expand(env: &Env, typ: &Type) -> Option<Type> {
    let mut t = typ.deref_cloned()?;
    while let Type::Ref(_) = t {
        t = t.lookup_ref_peek(env).ok()?.deref_cloned()?;
    }
    Some(t)
}

fn struct_fields(env: &Env, typ: &Type) -> Option<Arc<[(ArcStr, Type, WrittenAt)]>> {
    typ.with_deref(|t| match t {
        Some(t @ Type::Ref(_)) => match &t.lookup_ref(env) {
            Ok(Type::Struct(elts)) => Some(elts.clone()),
            Ok(_) | Err(_) => None,
        },
        Some(Type::Struct(elts)) => Some(elts.clone()),
        _ => None,
    })
}

/// One leaf name of a pattern with the part of a type it stands over.
/// `fresh`: the name's own cell is fresh and takes `typ` once the select
/// narrows the arm: a capture (`name@`, a slice's rest), which stands
/// over a whole position, or a name or-alternatives share, which is the
/// union of theirs. Any other plain bind is its position's own cell.
pub(super) struct Leaf {
    pub(super) id: BindId,
    pub(super) typ: Type,
    pub(super) fresh: bool,
}

/// An abstract pattern with its representation at each instance that
/// reaches it, and whether `Any` does.
struct PayloadReps<'a> {
    pat: &'a StructPatternNode,
    reps: SmallVec<[Type; 2]>,
    over_any: bool,
}

/// `leaves` gathered over several members or alternatives, each name
/// once, at the union of its types.
fn merge_leaves(
    env: &Env,
    all: SmallVec<[Leaf; 4]>,
    out: &mut SmallVec<[Leaf; 4]>,
) -> Result<()> {
    let mut by_id: SmallVec<[(BindId, bool, SmallVec<[Type; 2]>); 4]> = SmallVec::new();
    for l in all {
        match by_id.iter_mut().find(|(id, _, _)| *id == l.id) {
            Some((_, _, ts)) => ts.push(l.typ),
            None => by_id.push((l.id, l.fresh, smallvec![l.typ])),
        }
    }
    for (id, fresh, ts) in by_id {
        let typ = match &ts[..] {
            [t] => t.clone(),
            ts => Type::union(env, &ts.iter().collect::<SmallVec<[&Type; 2]>>())?,
        };
        out.push(Leaf { id, typ, fresh })
    }
    Ok(())
}

/// What holds a pattern that meets a type variable held rigid.
#[derive(Clone, Copy)]
pub(super) enum Holder {
    Arm,
    Let,
    Formal,
}

// XCR claude for claude: [readability/low] D6's advice, from review r14's final lows; no
// soundness. (1) Over a nullable scrutinee it sends a fused form to one that does not
// fuse: `'b: [`A(i64), `B(i64)] |x: ['b, null]| -> i64 #[native] select x { null => 0,
// `A(z) => z, `B(z) => z }` fuses on main; the advised `` `A(i64) as `A(z) `` does not
// ("type predicate `A(i64) not lowerable"), while a test holding the whole scrutinee,
// `[`A(i64), `B(i64), null] as `A(z)`, fuses: advise that form (or lower the case,
// written-composite-test-fuse-01). (2) A let's advice through an abstract type in a
// struct field names an annotation that is refused (`in_abstract_param` uses
// deref_cloned, which does not expand `W<'b>` inside the struct; a direct `x: W<'b>` is
// told "declare no 'b"). (3) Wording: "`T as `Z`, or bound 'b" for a 'b already bound
// (k11; the working twin is `` `Z as `Z ``); tr3 tells 'b: 'a to "bound 'b"; a6, s4, p4,
// p7, p8 say "a type variable of the definition" without its name (p4's is a rank-2
// formal's quantifier); written_refusal says "'c holds `_`" when the part is the
// undeclared 'c itself. (4) The advice for a bound naming an unbounded variable
// (`'b: ['c, `X]`) suggests a test the rule then refuses: say "bound 'c" there. (5) The
// docs' "a name keeps the variable under a written test" sentence does not cover the top
// position, where a test over the variable itself types the name (CLAUDE.md,
// design/tvar_constraints.md, the skill). probes:
// ~/ptmp/cr29/scratch/review-r14/B-r3-1/{f/u10*,c/a7*,b/k11*,i/tr3,c/a6,g/s4,a/p4}.gx,
// ~/ptmp/cr29/scratch/review-r14/B-r3-2/{sm/u3*,w/t1-t4}.gx (d6-advice-01)
// 2026-11-05 claude: Addressed in one commit. (1) over a nullable variable (['b, null])
// the advice is the test over the whole scrutinee, `[null, `A(i64), `B(i64)] as `A(z)`,
// which fuses under #[native]; the member-only form stays for the variable alone and for
// other unions (an abstract member needs it: whole-union `W(..)` is refused). (2)
// in_abstract_param now expands a typedef name through lookup_ref_probe, so a struct
// field W<'b> gets the 'declare no b' advice. (3) 'or bound b' is left off when b
// already has a bound (Any); a variable bounded by another is told to replace that
// bound; an unnamed variable is described by its typed bound; written_refusal says
// '$part is a type variable nothing declares' / 'holds _, which is bottom' as the part
// is. (4) a bound holding a rigid variable with no typed bound (b: [c, X]) says 'bound c
// by one'. (5) CLAUDE.md, design/tvar_constraints.md and the skill say a name over the
// variable at the top of a test is typed by the test. Pins in lang/select.rs that fail
// without it: nullable_declared_variable_advice,
// test_beside_a_variable_bounded_by_any_advice, variable_bounded_by_a_variable_advice,
// bound_naming_an_unbounded_variable_advice, unnamed_variable_advice_names_its_bound,
// abstract_parameter_in_a_struct_field_advice, plus the three written_test_* pins
// reworded; each has a passing twin. Messages only, no new refusals; needs no soak.
// 2026-11-05 claude: Repair (review r17): (1) over a nullable variable beside an
// abstract member the advice was the whole-scrutinee test, `[null, W<i64>, `B] as W(n)`,
// which is refused (an abstract pattern over a union); rigid_written takes the whole
// form only when no member the pattern may hold is abstract (is_abstract), else the
// member's, `W<i64> as W(n)`. (2) The bound-it advice fired whenever a typed bound named
// a variable with no bound, `'c: Any` included, though the test asked for does not hold
// it: rigid_written no longer gives up on a bound holding a variable, and the advice
// names one only where the test it would write names it (or there is none), and only one
// with neither a typed bound nor a declared constraint. The control's abstract-parameter
// advice now also reaches `['b, W<'b>]` (in_abstract_param expands names):
// lang::select::abstract_pattern_beside_a_declared_variable_advice asserts it. Pins that
// fail without the repair:
// lang::select::nullable_variable_beside_an_abstract_member_advice,
// bound_holding_a_variable_the_test_leaves_out_advice,
// bound_holding_a_variable_bounded_by_any_advice; twins _written pass.
// 2026-11-05 claude: Repair (review r18): the member form reached only the top of the
// advised test, so an abstract pattern in a struct field over a nullable variable ('b:
// [W<i64>, `B] |x: {f: ['b, null]}| with {f: W(n)}) was advised { f: [null, W<i64>, `B]
// } as { f: W(n) }, which is refused. rigid_written now narrows each union an abstract
// part of the pattern tests, inside tuples, variants and struct fields, to the members
// that part may hold (abstract_parts_held): { f: W<i64> } as { f: W(n) }. Pin that fails
// without the repair: lang::select::nullable_variable_beside_an_abstract_field_advice;
// its twin nullable_variable_beside_an_abstract_field_written runs. Left, low: over 'b:
// [`A('c), `B] with an `A(z) arm the advice still says bound 'c, though `A('c) as `A(z)
// also checks: telling the two apart needs the advised test judged by
// meets_rigid_written, which sees the bound's 'c as an open cell (the check judges the
// written test's rigid 'c), so the advice that works both ways stands (d6_c_inner.gx).
// 2026-11-06 claude: Repair (review r19): a struct field typed through a typedef union
// (type U<'x> = ['x, null], x: {f: U<'b>}) was advised { f: U<[W<i64>, `B]> } as { f:
// W(n) }, which is refused (W(..) patterns can't match the union): abstract_parts_held
// narrowed only a written union. It narrows a named one too, so the advice is { f:
// W<i64> } as { f: W(n) }. Pin that fails without it:
// lang::select::nullable_typedef_variable_beside_an_abstract_field_advice; its twin
// nullable_typedef_variable_beside_an_abstract_field_written runs.
/// The refusal of `pat`, over a value of `typ`, testing or binding inside
/// a value of `tv`, a type variable its definition holds rigid
/// ([`StructurePattern::meets_rigid`]), told as the type to write.
pub(super) fn rigid_refusal(
    env: &Env,
    ids: &AbstractOf,
    pat: &StructurePattern,
    typ: &Type,
    tv: &TVar,
    holder: Holder,
) -> anyhow::Error {
    let named = tv.written_open().is_some();
    let var = match named {
        true => format_compact!("'{}", tv.name),
        false => format_compact!("the variable"),
    };
    let bounds = tv.typed_bounds(env);
    let what = match (named, &bounds[..]) {
        (true, _) => format_compact!(
            "a value of {var}, a type variable of the definition, which types none of \
             its parts"
        ),
        (false, [b]) => format_compact!(
            "a value of a type variable of the definition bounded by {b}, which types \
             none of its parts"
        ),
        (false, _) => format_compact!(
            "a value of a type variable of the definition, which types none of its \
             parts"
        ),
    };
    let alone = is_alone(typ, tv);
    if alone
        && let Some(b) =
            bounds.iter().find(|b| !pat.may_hold(env, ids, b).unwrap_or(true))
    {
        let what = format_compact!("{pat} matches no value of {var}, whose bound is {b}");
        return match holder {
            Holder::Arm => anyhow!("unreachable arm: {what}, unused match cases"),
            Holder::Let | Holder::Formal => anyhow!("{what}"),
        };
    }
    if let Holder::Arm = holder
        && in_abstract_param(env, typ, tv)
    {
        let test = match &bounds[..] {
            [b] => format_compact!("`{b} as ..`"),
            _ => format_compact!("`T as ..`"),
        };
        return anyhow!(
            "{pat} tests inside {what}; an abstract type's parameter holds the variable, \
             and a type test of the abstract matches only the parameters it writes: bind \
             the payload by a name and test that, {test}, or declare no {var}, writing \
             its bound in its place"
        );
    }
    let written = rigid_written(env, ids, pat, typ, tv);
    let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    if let Some(t) = &written {
        t.collect_tvars(&mut named);
    }
    let in_test = |c: &TVar| written.is_none() || named.contains_key(&c.name);
    if let Some(c) = unbounded_in_bounds(env, tv, &in_test) {
        let (head, fix) = match holder {
            Holder::Arm => (
                format_compact!("{pat} tests inside {what}"),
                format_compact!("write the type it tests, `T as {pat}`"),
            ),
            Holder::Let => (
                format_compact!("the pattern {pat} binds inside {what}"),
                format_compact!(
                    "annotate the binding with the type it binds, `let {pat}: T = ..`"
                ),
            ),
            Holder::Formal => (
                format_compact!("the parameter {pat} binds inside {what}"),
                format_compact!("annotate it with the type it binds, `{pat}: T`"),
            ),
        };
        return anyhow!(
            "{head}: the bound of {var} holds {c}, which no type bounds; bound {c} by one, \
             `{c}: T`, then {fix}"
        );
    }
    let or_bound = or_bound(tv, &var);
    match (holder, written) {
        (Holder::Arm, Some(t)) if t.holds_fn(env) => anyhow!(
            "{pat} tests inside {what}, and no type test holds its function type: \
             declare no {var}, writing its bound in its place"
        ),
        (Holder::Arm, Some(t)) => {
            anyhow!("{pat} tests inside {what}: write the type it tests, `{t} as {pat}`")
        }
        (Holder::Arm, None) if alone && bounds.is_empty() => anyhow!(
            "{pat} tests inside {what}: {}, `{var}: T`, and write that type, `T as {pat}`",
            bounding(tv, &var, "tests")
        ),
        (Holder::Arm, None) => anyhow!(
            "{pat} tests inside {what}: write the type it tests, `T as {pat}`{or_bound}"
        ),
        (Holder::Let, _) if in_abstract_param(env, typ, tv) => anyhow!(
            "the pattern {pat} binds inside {what}; an abstract type's parameter holds \
             the variable, which no annotation widens: declare no {var}, writing the type \
             it binds in its place"
        ),
        (Holder::Let, Some(t)) => anyhow!(
            "the pattern {pat} binds inside {what}: annotate the binding, `let {pat}: \
             {t} = ..`"
        ),
        (Holder::Let, None) if alone && bounds.is_empty() => anyhow!(
            "the pattern {pat} binds inside {what}: {}, `{var}: T`, and annotate the \
             binding with it, `let {pat}: T = ..`",
            bounding(tv, &var, "binds")
        ),
        (Holder::Let, None) => anyhow!(
            "the pattern {pat} binds inside {what}: annotate the binding with the type \
             it binds, `let {pat}: T = ..`{or_bound}"
        ),
        (Holder::Formal, Some(t)) => anyhow!(
            "the parameter {pat} binds inside {what}: annotate it with the type it \
             binds, `{pat}: {t}`"
        ),
        (Holder::Formal, None) => anyhow!(
            "the parameter {pat} binds inside {what}: annotate it with the type it \
             binds, `{pat}: T`"
        ),
    }
}

/// The refusal of `pat`, under a type test over a value of `typ`, testing
/// or binding inside a value of `tv`, a type variable its definition holds
/// rigid, where the test's part over it, `part`, holds `_` or an open cell
/// ([`StructurePattern::meets_rigid_written`]).
pub(super) fn written_refusal(
    env: &Env,
    ids: &AbstractOf,
    pat: &StructurePattern,
    typ: &Type,
    tv: &TVar,
    part: &Type,
) -> anyhow::Error {
    let var = match tv.written_open().is_some() {
        true => format_compact!("'{}", tv.name),
        false => format_compact!("the variable"),
    };
    let fix = match rigid_written(env, ids, pat, typ, tv) {
        Some(t) => format_compact!("write the type it tests, `{t} as {pat}`"),
        None => {
            format_compact!("write the type it tests, `T as {pat}`{}", or_bound(tv, &var))
        }
    };
    let why = match (part.holds_bottom(env), part) {
        (true, _) => format_compact!("{part} holds `_`, which is bottom"),
        (false, Type::TVar(t)) if t.written_open().is_some() => {
            format_compact!("{part} is a type variable nothing declares")
        }
        (false, _) => match part.first_free().filter(|c| c.written_open().is_some()) {
            Some(c) => {
                format_compact!("{part} holds {c}, a type variable nothing declares")
            }
            None => format_compact!("{part} holds a type variable nothing declares"),
        },
    };
    anyhow!(
        "{pat} tests inside a value of {var}, a type variable of the definition, and its \
         type test types none of it: {why}; {fix}"
    )
}

/// Whether `typ` is the variable `tv` itself.
fn is_alone(typ: &Type, tv: &TVar) -> bool {
    matches!(typ, Type::TVar(t) if t.open_cell().is_some_and(|c| c.same_cell(tv)))
}

/// How to give `tv` the type a pattern `verb`s (tests or binds) as its
/// bound: in place of the bound it has.
fn bounding(tv: &TVar, var: &str, verb: &str) -> CompactString {
    let cs = tv.cell_constraints();
    match &cs[..] {
        [] => format_compact!("bound {var} by the type it {verb}"),
        [c] => format_compact!("replace the bound of {var}, {c}, by the type it {verb}"),
        _ => format_compact!("replace the bounds of {var} by the type it {verb}"),
    }
}

/// ", or bound {var}" where `tv` has no bound declared yet.
fn or_bound(tv: &TVar, var: &str) -> CompactString {
    match tv.cell_constraints().is_empty() {
        true => format_compact!(", or bound {var}"),
        false => CompactString::default(),
    }
}

/// Whether `c` is a variable a program wrote, held rigid with no bound of
/// its own: a test written over it cannot name it.
fn unbounded(env: &Env, c: &TVar) -> bool {
    c.is_rigid()
        && c.written_open().is_some()
        && c.typed_bounds(env).is_empty()
        && c.cell_constraints().is_empty()
}

/// A variable a bound of `tv` holds that is [`unbounded`] and `wanted`.
fn unbounded_in_bounds(
    env: &Env,
    tv: &TVar,
    wanted: &dyn Fn(&TVar) -> bool,
) -> Option<TVar> {
    tv.typed_bounds(env)
        .iter()
        .find_map(|b| b.first_open(&|c| unbounded(env, c) && wanted(c)))
}

/// Whether `typ` is a variable beside `null`, whose test fuses written over
/// the whole of it and not over the members a pattern may hold.
fn nullable_variable(env: &Env, typ: &Type) -> bool {
    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
    union_members_with(env, typ, &mut members, &mut |_| ()).is_ok()
        && !members.is_empty()
        && members
            .iter()
            .all(|m| matches!(m, Type::Primitive(p) if *p == BitFlags::from(Typ::Null)))
}

/// The type `pat` tests written out: `typ` with `tv` at its one typed
/// bound. Where `typ` is the variable alone or the variable beside `null`,
/// only the members of that union `pat` may hold. `None` where `tv` has no one typed bound.
fn rigid_written(
    env: &Env,
    ids: &AbstractOf,
    pat: &StructurePattern,
    typ: &Type,
    tv: &TVar,
) -> Option<Type> {
    let bounds = tv.typed_bounds(env);
    let [bound] = &bounds[..] else { return None };
    let bound = rigid_as_bounds(env, bound)?;
    let is_tv = |t: &TVar| t.open_cell().is_some_and(|c| c.same_cell(tv));
    let t = typ.written_form(&mut |t| is_tv(t).then(|| bound.clone()))?;
    let t = slices_written(pat, &t)?;
    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
    union_members(env, &t, &mut members).ok()?;
    let held: SmallVec<[Type; 8]> = members
        .iter()
        .filter(|m| pat.may_hold(env, ids, m).unwrap_or(true))
        .cloned()
        .collect();
    if !is_alone(typ, tv)
        && nullable_variable(env, typ)
        && !held.iter().any(|m| is_abstract(env, m))
    {
        return Some(t);
    }
    let t = match &held[..] {
        [] => t,
        _ if held.len() == members.len() => t,
        [m] => m.clone(),
        _ => Type::Set(Arc::from_iter(held)),
    };
    Some(abstract_parts_held(env, ids, pat, &t))
}

/// `t` with each union, written or named, that a part of `pat` tests by
/// an abstract pattern narrowed to the members that part may hold: an
/// abstract pattern over a union is refused.
fn abstract_parts_held(
    env: &Env,
    ids: &AbstractOf,
    pat: &StructurePattern,
    t: &Type,
) -> Type {
    use StructurePattern as P;
    let parts = |ps: &[StructurePattern], ts: &[Type]| -> Arc<[Type]> {
        ps.iter()
            .zip(ts.iter())
            .map(|(p, t)| abstract_parts_held(env, ids, p, t))
            .collect()
    };
    crate::stack::ensure_sufficient(|| match (pat, t) {
        (P::Abstract { .. }, Type::Set(_) | Type::Ref(_)) => {
            let mut members: SmallVec<[Type; 8]> = SmallVec::new();
            if union_members(env, t, &mut members).is_err() {
                return t.clone();
            }
            let held: SmallVec<[Type; 8]> = members
                .into_iter()
                .filter(|m| pat.may_hold(env, ids, m).unwrap_or(true))
                .collect();
            match &held[..] {
                [m] => m.clone(),
                _ => t.clone(),
            }
        }
        (P::Tuple { binds, .. }, Type::Tuple(ts)) if binds.len() == ts.len() => {
            Type::Tuple(parts(binds, ts))
        }
        (P::Variant { tag, binds, .. }, Type::Variant(u, ts, at))
            if tag == u && binds.len() == ts.len() =>
        {
            Type::Variant(u.clone(), parts(binds, ts), *at)
        }
        (P::Struct { binds, .. }, Type::Struct(fs)) => Type::Struct(
            fs.iter()
                .map(|(n, ft, at)| {
                    let ft = match binds.iter().find(|(m, _, _)| m == n) {
                        Some((_, p, _)) => abstract_parts_held(env, ids, p, ft),
                        None => ft.clone(),
                    };
                    (n.clone(), ft, *at)
                })
                .collect(),
        ),
        _ => t.clone(),
    })
}

/// Whether `t` is an abstract type, written or named: an abstract
/// pattern over a union holding it is refused, so its test is the member.
fn is_abstract(env: &Env, t: &Type) -> bool {
    match t {
        Type::Abstract { .. } => true,
        Type::Ref(_) => {
            matches!(t.lookup_ref_probe(env), Some((_, Type::Abstract { .. })))
        }
        _ => false,
    }
}

/// `t`, the type a test over `pat` writes, with each application of an
/// open constructor (`'c<T>`) a slice of `pat` stands over written as the
/// slice's collection, `Array<T>` or `List<T>`; `None` where one is left,
/// which no test can write.
fn slices_written(pat: &StructurePattern, t: &Type) -> Option<Type> {
    fn elems(binds: &[StructurePattern], list: bool, t: &Type) -> Type {
        let et = match (list, t) {
            (false, Type::Array(et)) | (true, Type::List(et)) | (_, Type::App(_, et)) => {
                et
            }
            _ => return t.clone(),
        };
        let et = binds.iter().fold((**et).clone(), |et, p| go(p, &et));
        match list {
            true => Type::List(Arc::new(et)),
            false => Type::Array(Arc::new(et)),
        }
    }
    fn each(binds: &[StructurePattern], ts: &[Type]) -> Arc<[Type]> {
        binds.iter().zip(ts.iter()).map(|(p, t)| go(p, t)).collect()
    }
    fn go(pat: &StructurePattern, t: &Type) -> Type {
        use StructurePattern as P;
        crate::stack::ensure_sufficient(|| match (pat, t) {
            (_, Type::Set(ts)) => Type::Set(ts.iter().map(|t| go(pat, t)).collect()),
            (P::Or(alts), _) => alts.iter().fold(t.clone(), |t, a| go(a, &t)),
            (P::Tuple { binds, .. }, Type::Tuple(ts)) if binds.len() == ts.len() => {
                Type::Tuple(each(binds, ts))
            }
            (P::Variant { tag, binds, .. }, Type::Variant(vt, ts, at))
                if vt == tag && binds.len() == ts.len() =>
            {
                Type::Variant(vt.clone(), each(binds, ts), *at)
            }
            (P::Struct { binds, .. }, Type::Struct(fs)) => Type::Struct(
                fs.iter()
                    .map(|(n, ft, at)| match binds.iter().find(|(b, _, _)| b == n) {
                        Some((_, p, _)) => (n.clone(), go(p, ft), *at),
                        None => (n.clone(), ft.clone(), *at),
                    })
                    .collect(),
            ),
            (P::Slice { list, binds, .. }, _)
            | (P::SlicePrefix { list, prefix: binds, .. }, _) => elems(binds, *list, t),
            (P::SliceSuffix { suffix, .. }, _) => elems(suffix, false, t),
            _ => t.clone(),
        })
    }
    fn holds_app(t: &Type) -> bool {
        crate::stack::ensure_sufficient(|| match t {
            Type::App(..) => true,
            t => {
                let mut found = false;
                t.for_each_child(&mut |c| found = found || holds_app(c));
                found
            }
        })
    }
    let t = go(pat, t);
    (!holds_app(&t)).then_some(t)
}

/// Whether `t` holds `tv` in an abstract type's parameter, which no
/// annotation widens.
fn in_abstract_param(env: &Env, t: &Type, tv: &TVar) -> bool {
    fn holds(t: &Type, tv: &TVar) -> bool {
        crate::stack::ensure_sufficient(|| match t.deref_cloned() {
            None => is_alone(t, tv),
            Some(t) => {
                let mut found = false;
                t.for_each_child(&mut |c| found = found || holds(c, tv));
                found
            }
        })
    }
    crate::stack::ensure_sufficient(|| {
        let t = match t {
            Type::Ref(_) => match t.lookup_ref_probe(env) {
                Some((_, a @ Type::Abstract { .. })) => Some(a),
                _ => Some(t.clone()),
            },
            t => t.deref_cloned(),
        };
        match t {
            None => false,
            Some(Type::Abstract { ref params, .. }) => {
                params.iter().any(|p| holds(p, tv))
            }
            Some(t) => {
                let mut found = false;
                t.for_each_child(&mut |c| found = found || in_abstract_param(env, c, tv));
                found
            }
        }
    })
}

/// `t` as a program writes it, each type variable a definition holds
/// rigid with one typed bound written as that bound, whose values a test
/// over a value of it meets.
fn rigid_as_bounds(env: &Env, t: &Type) -> Option<Type> {
    crate::stack::ensure_sufficient(|| {
        t.written_form(&mut |tv| {
            let c = tv.open_cell().filter(|c| c.is_rigid())?;
            match &c.typed_bounds(env)[..] {
                [b] => rigid_as_bounds(env, b),
                _ => None,
            }
        })
    })
}

/// Each abstract pattern of a pattern paired with the type its compile
/// resolved it to: a name looked up again at the check could find a type a
/// later statement declares.
pub(super) struct CompiledAbstracts<'a>(
    SmallVec<[(&'a StructurePattern, AbstractId); 2]>,
);

impl<'a> CompiledAbstracts<'a> {
    /// The pairs of `spec` and `node`, the pattern compiled from it.
    pub(super) fn new(node: &StructPatternNode, spec: &'a StructurePattern) -> Self {
        let mut pairs = SmallVec::new();
        node.pair_abstracts(spec, &mut pairs);
        Self(pairs)
    }

    pub(super) fn get(&self, p: &StructurePattern) -> Option<AbstractId> {
        self.0.iter().find(|(q, _)| std::ptr::eq(*q, p)).map(|(_, id)| *id)
    }
}

impl StructPatternNode {
    /// Each name beneath a part of this pattern that tests inside a value
    /// of a rigid cell `typ` holds at that part's position, in any
    /// alternative or member.
    fn binds_inside_rigid(
        &self,
        env: &Env,
        typ: &Type,
        out: &mut SmallVec<[BindId; 4]>,
    ) -> Result<()> {
        crate::stack::ensure_sufficient(|| match self {
            Self::Ignore | Self::Literal(_) | Self::Bind(_) => Ok(()),
            Self::Or { alts } => {
                alts.iter().try_for_each(|a| a.binds_inside_rigid(env, typ, out))
            }
            _ => {
                let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                let mut rigid = false;
                crate::expr::union_members_with(env, typ, &mut members, &mut |tv| {
                    rigid |= tv.open_cell().is_some_and(|c| c.is_rigid())
                })?;
                if rigid {
                    for c in self.children() {
                        c.ids(&mut |id| out.push(id))
                    }
                }
                for (_, ts) in self.member_parts(env, typ, false) {
                    for (c, t) in self.children().into_iter().zip(ts) {
                        if let Some((t, _)) = t {
                            c.binds_inside_rigid(env, &t, out)?
                        }
                    }
                }
                Ok(())
            }
        })
    }

    fn pair_abstracts<'a>(
        &self,
        spec: &'a StructurePattern,
        out: &mut SmallVec<[(&'a StructurePattern, AbstractId); 2]>,
    ) {
        use StructurePattern as P;
        let zip = |ns: &[Self], ss: &'a [StructurePattern], out: &mut SmallVec<_>| {
            for (n, s) in ns.iter().zip(ss.iter()) {
                n.pair_abstracts(s, out)
            }
        };
        crate::stack::ensure_sufficient(|| match (self, spec) {
            (Self::Abstract { id, bind, .. }, P::Abstract { bind: sb, .. }) => {
                out.push((spec, *id));
                bind.pair_abstracts(sb, out)
            }
            (Self::Or { alts }, P::Or(sa)) => zip(alts, sa, out),
            (Self::Variant { binds, .. }, P::Variant { binds: sb, .. })
            | (Self::Slice { binds, .. }, P::Tuple { binds: sb, .. })
            | (Self::Slice { binds, .. }, P::Slice { binds: sb, .. })
            | (
                Self::SlicePrefix { prefix: binds, .. },
                P::SlicePrefix { prefix: sb, .. },
            )
            | (
                Self::SliceSuffix { suffix: binds, .. },
                P::SliceSuffix { suffix: sb, .. },
            ) => zip(binds, sb, out),
            (Self::Struct { binds, .. }, P::Struct { binds: sb, .. }) => {
                for ((_, _, n), (_, s, _)) in binds.iter().zip(sb.iter()) {
                    n.pair_abstracts(s, out)
                }
            }
            _ => (),
        })
    }

    /// This pattern's children, in the order [`Self::child_types`]
    /// gives their types.
    fn children(&self) -> SmallVec<[&Self; 8]> {
        match self {
            Self::Ignore | Self::Literal(_) | Self::Bind(_) => SmallVec::new(),
            Self::Or { alts: ps }
            | Self::Variant { binds: ps, .. }
            | Self::Slice { binds: ps, .. }
            | Self::SlicePrefix { prefix: ps, .. }
            | Self::SliceSuffix { suffix: ps, .. } => ps.iter().collect(),
            Self::Struct { binds, .. } => binds.iter().map(|(_, _, p)| p).collect(),
            Self::Abstract { bind, .. } => smallvec![&**bind],
        }
    }

    /// The part of `typ`, a type of this pattern's shape, each child
    /// stands over, with a struct child's field index; `None` when `typ`
    /// is not of the shape, and a `None` part for a field it lacks. Under
    /// an inferred predicate an or-pattern's alternatives and a slice's
    /// elements each stand over their own member of a Set; under a
    /// written one, over the whole type or its element type.
    fn child_types(
        &self,
        env: &Env,
        typ: &Type,
        inferred: bool,
    ) -> Option<SmallVec<[Option<(Type, usize)>; 8]>> {
        let expanded = expand(env, typ);
        let typ = expanded.as_ref().unwrap_or(typ);
        fn at(ts: &[Type]) -> SmallVec<[Option<(Type, usize)>; 8]> {
            ts.iter().enumerate().map(|(i, t)| Some((t.clone(), i))).collect()
        }
        let parts = |n: usize, variant: Option<&ArcStr>| {
            typ.with_deref(|t| match (t, variant) {
                (Some(Type::Tuple(ts)), None) if ts.len() == n => Some(at(ts)),
                (Some(Type::Variant(tag, ts, _)), Some(want))
                    if tag == want && ts.len() == n =>
                {
                    Some(at(ts))
                }
                _ => None,
            })
        };
        let elems = |n: usize, rest: bool, list: bool| {
            let et = typ.with_deref(|t| match t {
                Some(Type::Array(et)) if !list => Some((**et).clone()),
                Some(Type::List(et)) if list => Some((**et).clone()),
                _ => None,
            })?;
            match inferred {
                true => set_members(&et, n + rest as usize),
                false => Some(Arc::from_iter((0..n).map(|_| et.clone()))),
            }
        };
        match self {
            Self::Ignore | Self::Literal(_) | Self::Bind(_) => Some(SmallVec::new()),
            Self::Or { alts } => match inferred {
                true => set_members(typ, alts.len()).map(|ts| at(&ts)),
                false => Some(
                    alts.iter()
                        .enumerate()
                        .map(|(i, _)| Some((typ.clone(), i)))
                        .collect(),
                ),
            },
            Self::Variant { tag, binds, .. } => parts(binds.len(), Some(tag)),
            Self::Slice { kind: SliceKind::Tuple, binds, .. } => parts(binds.len(), None),
            Self::Slice { kind, binds, .. } => {
                elems(binds.len(), false, *kind == SliceKind::List)
                    .map(|ts| at(&ts[..binds.len()]))
            }
            Self::SlicePrefix { prefix: ps, list, .. } => {
                elems(ps.len(), true, *list).map(|ts| at(&ts[..ps.len()]))
            }
            Self::SliceSuffix { suffix: ps, .. } => {
                elems(ps.len(), true, false).map(|ts| at(&ts[..ps.len()]))
            }
            Self::Struct { binds, .. } => {
                let fields = struct_fields(env, typ)?;
                Some(
                    binds
                        .iter()
                        .map(|(name, _, _)| {
                            let i = fields.iter().position(|(n, _, _)| n == name)?;
                            Some((fields[i].1.clone(), i))
                        })
                        .collect(),
                )
            }
            Self::Abstract { rep, .. } => {
                let rep = match typ.deref_cloned() {
                    None | Some(Type::Set(_)) => rep.clone(),
                    Some(t) => self.member_rep(env, &t)?,
                };
                Some(smallvec![Some((rep, 0))])
            }
        }
    }

    /// An abstract pattern's representation at `m`, one member of the
    /// type at its position: at `m`'s parameters when `m` is an instance of
    /// its type, at `Any` for each parameter when `m` is `Any`; `None` for
    /// a member it cannot match.
    fn member_rep(&self, env: &Env, m: &Type) -> Option<Type> {
        let Self::Abstract { id, rep, .. } = self else { return None };
        let at = |params: &[Type]| {
            env.abstract_reps
                .get(id)
                .map_or_else(|| rep.clone(), |r| r.instantiate_with(params))
        };
        match &expand(env, m)? {
            Type::Abstract { id: t, params } if t == id => Some(at(params)),
            Type::Any => {
                let n = env.abstract_reps.get(id).map_or(0, |r| r.params.len());
                let anys: SmallVec<[Type; 4]> = (0..n).map(|_| Type::Any).collect();
                Some(at(&anys))
            }
            _ => None,
        }
    }

    /// [`Self::child_types`] of each member of `typ` this pattern's head
    /// can match: a union at a constructor pattern's position stands for
    /// each of its members in turn, and a struct pattern matches only a
    /// member with every field it names, at a width it admits.
    fn member_parts(
        &self,
        env: &Env,
        typ: &Type,
        inferred: bool,
    ) -> SmallVec<[(Type, SmallVec<[Option<(Type, usize)>; 8]>); 2]> {
        let union = expand(env, typ).is_some_and(|t| matches!(t, Type::Set(_)));
        let distributes = !matches!(
            self,
            Self::Ignore | Self::Literal(_) | Self::Bind(_) | Self::Or { .. }
        );
        if !(union && distributes) {
            let ts = self.child_types(env, typ, inferred);
            return ts.map(|ts| (typ.clone(), ts)).into_iter().collect();
        }
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        if union_members(env, typ, &mut members).is_err() {
            return SmallVec::new();
        }
        members
            .iter()
            .filter_map(|m| {
                let ts = self.child_types(env, m, inferred)?;
                let fits = match self {
                    Self::Struct { width, .. } => {
                        ts.iter().all(Option::is_some)
                            && struct_fields(env, m)
                                .is_some_and(|fs| width.admits(fs.len()))
                    }
                    _ => true,
                };
                fits.then(|| (m.clone(), ts))
            })
            .collect()
    }

    /// Whether this pattern's children stand over an inferred predicate,
    /// given that it does: an abstract's payload checks against its
    /// declared representation.
    fn children_inferred(&self, inferred: bool) -> bool {
        inferred && !matches!(self, Self::Abstract { .. })
    }

    /// The `all@` capture and the slice rest, each standing over the
    /// whole of this pattern's type.
    fn whole_binds(&self) -> impl Iterator<Item = BindId> {
        let rest = match self {
            Self::SlicePrefix { tail: rest, .. }
            | Self::SliceSuffix { head: rest, .. } => *rest,
            _ => None,
        };
        self.capture().into_iter().chain(rest)
    }

    /// The `name@` capture of the whole value this pattern tests.
    fn capture(&self) -> Option<BindId> {
        match self {
            Self::Ignore | Self::Literal(_) | Self::Bind(_) | Self::Or { .. } => None,
            Self::SlicePrefix { all, .. }
            | Self::SliceSuffix { all, .. }
            | Self::Slice { all, .. }
            | Self::Struct { all, .. }
            | Self::Variant { all, .. }
            | Self::Abstract { all, .. } => *all,
        }
    }

    /// Narrow each abstract payload's own representation, which its binds
    /// and captures are typed by, to the union of its representation at
    /// every instance `members`, the scrutinee's members the arm can match,
    /// hold at its position ([`Self::member_rep`]): an inferred abstract
    /// predicate takes its parameters from the scrutinee, never from its
    /// payload.
    fn narrow_payloads(&self, env: &Env, members: &[Type]) -> Result<()> {
        let mut reps: SmallVec<[PayloadReps; 2]> = SmallVec::new();
        for m in members {
            self.payload_reps(env, m, &mut reps);
        }
        for PayloadReps { pat, reps, over_any } in reps.iter() {
            if let Self::Abstract { rep, .. } = pat {
                let rs: SmallVec<[&Type; 2]> = reps.iter().collect();
                let at = Type::union(env, &rs)?;
                at.narrow_pattern(env, rep)?;
                // narrowing against `Any` decides nothing: each part the
                // payload leaves open is `Any`
                if *over_any {
                    rep.contains(env, &at)?;
                }
            }
        }
        Ok(())
    }

    /// Each abstract pattern in this one with its representation at every
    /// instance `typ` holds at its position.
    fn payload_reps<'a>(
        &'a self,
        env: &Env,
        typ: &Type,
        out: &mut SmallVec<[PayloadReps<'a>; 2]>,
    ) {
        crate::stack::ensure_sufficient(|| {
            if let Self::Abstract { .. } = self {
                let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                if union_members(env, typ, &mut members).is_err() {
                    return;
                }
                for m in members.iter() {
                    let Some(r) = self.member_rep(env, m) else { continue };
                    let any = matches!(m, Type::Any);
                    match out.iter_mut().find(|e| std::ptr::eq(e.pat, self)) {
                        Some(e) => {
                            e.reps.push(r);
                            e.over_any |= any
                        }
                        None => out.push(PayloadReps {
                            pat: self,
                            reps: smallvec![r],
                            over_any: any,
                        }),
                    }
                }
                return;
            }
            for (_, ts) in self.member_parts(env, typ, false) {
                for (c, t) in self.children().into_iter().zip(ts) {
                    if let Some((t, _)) = t {
                        c.payload_reps(env, &t, out)
                    }
                }
            }
        })
    }

    /// Every leaf name with the part of `typ` it stands over, `typ`
    /// being a type of this pattern's shape. An or-pattern's
    /// alternatives share their names: each is reported once, at the
    /// union of the alternatives' parts; with `checking`, alternatives a
    /// run-time test cannot tell apart are refused, as separate arms
    /// would be.
    pub(super) fn leaves(
        &self,
        env: &Env,
        typ: &Type,
        inferred: bool,
        checking: bool,
        out: &mut SmallVec<[Leaf; 4]>,
    ) -> Result<()> {
        crate::stack::ensure_sufficient(|| {
            self.leaves_inner(env, typ, inferred, checking, out)
        })
    }

    fn leaves_inner(
        &self,
        env: &Env,
        typ: &Type,
        inferred: bool,
        checking: bool,
        out: &mut SmallVec<[Leaf; 4]>,
    ) -> Result<()> {
        if let Self::Bind(id) = self {
            out.push(Leaf { id: *id, typ: typ.clone(), fresh: false });
            return Ok(());
        }
        let sub = self.children_inferred(inferred);
        if let Self::Abstract { rep, bind, .. } = self {
            for id in self.whole_binds() {
                out.push(Leaf { id, typ: typ.normalize(), fresh: true })
            }
            return bind.leaves(env, rep, false, checking, out);
        }
        let Self::Or { alts } = self else {
            let members = self.member_parts(env, typ, inferred);
            let mut all: SmallVec<[Leaf; 4]> = SmallVec::new();
            for (m, ts) in members.iter() {
                for id in self.whole_binds() {
                    all.push(Leaf { id, typ: m.normalize(), fresh: true })
                }
                for (c, t) in self.children().into_iter().zip(ts.iter()) {
                    if let Some((t, _)) = t {
                        c.leaves(env, t, sub, checking, &mut all)?
                    }
                }
            }
            // over a raw member, a head of another shape matches none of it
            if members.is_empty() && (inferred || expand(env, typ).is_none()) {
                for id in self.whole_binds() {
                    all.push(Leaf { id, typ: typ.normalize(), fresh: true })
                }
            }
            return merge_leaves(env, all, out);
        };
        let Some(ts) = self.child_types(env, typ, inferred) else { return Ok(()) };
        let ts: SmallVec<[Type; 4]> = ts.into_iter().flatten().map(|(t, _)| t).collect();
        if checking {
            for (i, a) in alts.iter().enumerate() {
                let fp = a.footprint(env, &ts[i], sub);
                for (j, b) in ts.iter().enumerate() {
                    if i != j && fp.rep_collision(env, b).is_some() {
                        let (a, b) = (ts[i].resolve_tvars(), b.resolve_tvars());
                        return format_with_flags(PrintFlag::DerefTVars, || {
                            bail!(
                                "these or-pattern alternatives can't tell {a} from {b}: \
                                 an alternative is taken by its structure, which a value \
                                 of either type passes; write them as separate arms"
                            )
                        });
                    }
                }
            }
        }
        let mut all: SmallVec<[Leaf; 4]> = SmallVec::new();
        for (a, t) in alts.iter().zip(ts.iter()) {
            a.leaves(env, t, sub, checking, &mut all)?;
        }
        for l in all.iter_mut() {
            l.fresh = true
        }
        merge_leaves(env, all, out)
    }

    /// Each element test of a slice under this pattern, with the element
    /// type of what reaches the slice: any member of it may stand at the
    /// element's position, however the predicate's Set covers it as a
    /// type. `pred` is this pattern's inferred predicate as narrowed,
    /// `typ` what reaches it.
    pub(super) fn element_tests(
        &self,
        env: &Env,
        pred: &Type,
        typ: &Type,
        out: &mut SmallVec<[(Type, Type); 4]>,
    ) {
        crate::stack::ensure_sufficient(|| {
            let children = self.children();
            if children.is_empty() || !self.children_inferred(true) {
                return;
            }
            let Some(ts) = self.child_types(env, pred, true) else { return };
            let slice = matches!(
                self,
                Self::Slice { kind: SliceKind::Array | SliceKind::List, .. }
                    | Self::SlicePrefix { .. }
                    | Self::SliceSuffix { .. }
            );
            for (_, parts) in self.member_parts(env, typ, false) {
                for ((c, t), p) in children.iter().zip(&ts).zip(&parts) {
                    let (Some((t, _)), Some((p, _))) = (t, p) else { continue };
                    if slice && !matches!(c, Self::Bind(_) | Self::Ignore) {
                        out.push((t.clone(), p.clone()))
                    }
                    c.element_tests(env, t, p, out)
                }
            }
        })
    }

    /// What this pattern's structure test admits over `typ`: `typ` with
    /// every position the structure does not test as `Any`. An
    /// or-alternative is taken by its structure alone, so a value of
    /// another alternative's type this admits would bind through it.
    fn footprint(&self, env: &Env, typ: &Type, inferred: bool) -> Type {
        crate::stack::ensure_sufficient(|| {
            let sub = self.children_inferred(inferred);
            let parts = |ts: Option<SmallVec<[Option<(Type, usize)>; 8]>>| {
                let ts = ts?;
                let fps = self.children().into_iter().zip(ts).map(|(c, t)| match t {
                    Some((t, _)) => c.footprint(env, &t, sub),
                    None => Type::Any,
                });
                Some(fps.collect::<SmallVec<[Type; 8]>>())
            };
            match self {
                Self::Ignore | Self::Bind(_) => Type::Any,
                Self::Literal(v) => Type::of_literal(v).unwrap_or_else(|_| typ.clone()),
                Self::Abstract { .. } => typ.clone(),
                Self::Slice { kind: SliceKind::List, .. }
                | Self::SlicePrefix { list: true, .. } => Type::List(Arc::new(Type::Any)),
                Self::Slice { kind: SliceKind::Array, .. }
                | Self::SlicePrefix { .. }
                | Self::SliceSuffix { .. } => Type::Array(Arc::new(Type::Any)),
                Self::Slice { kind: SliceKind::Tuple, .. } => {
                    match parts(self.child_types(env, typ, inferred)) {
                        Some(ps) => Type::Tuple(Arc::from_iter(ps)),
                        None => typ.clone(),
                    }
                }
                Self::Variant { tag, .. } => {
                    match parts(self.child_types(env, typ, inferred)) {
                        Some(ps) => Type::Variant(
                            tag.clone(),
                            Arc::from_iter(ps),
                            WrittenAt::NOWHERE,
                        ),
                        None => typ.clone(),
                    }
                }
                Self::Or { .. } => match parts(self.child_types(env, typ, inferred)) {
                    Some(ps) => Type::Set(Arc::from_iter(ps)),
                    None => typ.clone(),
                },
                Self::Struct { binds, .. } => {
                    let Some(fields) = struct_fields(env, typ) else {
                        return typ.clone();
                    };
                    let fields = fields.iter().map(|(n, t, at)| {
                        let t = match binds.iter().find(|(bn, _, _)| bn == n) {
                            Some((_, _, p)) => p.footprint(env, t, sub),
                            None => Type::Any,
                        };
                        (n.clone(), t, *at)
                    });
                    Type::Struct(Arc::from_iter(fields))
                }
            }
        })
    }

    /// Under an inferred predicate each element of an array or list slice
    /// stands over its own member, but the run-time test admits a value
    /// of any member at every position: every name an element binds holds
    /// the union of its part of each member the element's structure admits.
    /// `matched`, when not empty, is the scrutinee's members this slice can
    /// match: an array or list member's elements are its own, not the
    /// predicate's cells, which stand for any other member (`Any`).
    fn hold_slice_members(&self, env: &Env, typ: &Type, matched: &[Type]) -> Result<()> {
        crate::stack::ensure_sufficient(|| {
            let Some(ts) = self.child_types(env, typ, true) else { return Ok(()) };
            if let Self::Slice { kind: SliceKind::Array | SliceKind::List, .. }
            | Self::SlicePrefix { .. }
            | Self::SliceSuffix { .. } = self
            {
                let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                let mut own = matched.is_empty();
                for m in matched {
                    match &expand(env, m) {
                        Some(Type::Array(et) | Type::List(et)) => {
                            members_or_open(env, &et, &mut members)?
                        }
                        _ => own = true,
                    }
                }
                if own && let Some(Type::Array(et) | Type::List(et)) = &expand(env, typ) {
                    members_or_open(env, &et, &mut members)?
                }
                let mut leaves: SmallVec<[Leaf; 4]> = SmallVec::new();
                let mut parts: AHashMap<BindId, SmallVec<[Type; 4]>> =
                    AHashMap::default();
                for c in self.children() {
                    parts.clear();
                    for m in members.iter() {
                        if !c.footprint(env, m, false).could_match(env, m)? {
                            continue;
                        }
                        leaves.clear();
                        c.leaves(env, m, false, false, &mut leaves)?;
                        for l in leaves.drain(..) {
                            parts.entry(l.id).or_default().push(l.typ)
                        }
                    }
                    for (id, ts) in parts.iter() {
                        if let Some(b) = env.by_id.get(id) {
                            let ts: SmallVec<[&Type; 4]> = ts.iter().collect();
                            b.typ.check_contains(env, &Type::union_exact(env, &ts)?)?
                        }
                    }
                }
            }
            if !self.children_inferred(true) {
                return Ok(());
            }
            for (c, t) in self.children().into_iter().zip(ts) {
                if let Some((t, _)) = t {
                    c.hold_slice_members(env, &t, &[])?
                }
            }
            Ok(())
        })
    }

    /// The id of every [`Leaf::fresh`] name: each capture and every
    /// name an or-pattern binds.
    pub(super) fn fresh_ids(&self, f: &mut impl FnMut(BindId)) {
        crate::stack::ensure_sufficient(|| match self {
            Self::Or { .. } => self.ids(&mut *f),
            // a payload compiled against its written rep: its binds are typed
            Self::Abstract { .. } => self.whole_binds().for_each(&mut *f),
            _ => {
                self.whole_binds().for_each(&mut *f);
                for c in self.children() {
                    c.fresh_ids(f)
                }
            }
        })
    }

    /// Re-derive the struct binders' field indexes from a completed
    /// type predicate: a partial pattern compiles against the fields it
    /// names, so its indexes are wrong once the select typecheck
    /// completes the predicate from the scrutinee.
    pub(super) fn realign(&mut self, env: &Env, typ: &Type) -> Result<()> {
        self.realign_with(env, typ, true)
    }

    fn realign_with(&mut self, env: &Env, typ: &Type, inferred: bool) -> Result<()> {
        crate::stack::ensure_sufficient(|| {
            let Some(ts) = self.child_types(env, typ, inferred) else { return Ok(()) };
            let sub = self.children_inferred(inferred);
            match self {
                Self::Struct { binds, width, all: _ } => {
                    if let Some(fields) = struct_fields(env, typ) {
                        *width = StructWidth::Exact(fields.len())
                    }
                    for ((name, index, p), t) in binds.iter_mut().zip(ts) {
                        let Some((t, i)) = t else {
                            bail!("no such struct field {name} in {typ}")
                        };
                        *index = i;
                        p.realign_with(env, &t, sub)?
                    }
                    Ok(())
                }
                Self::Ignore | Self::Literal(_) | Self::Bind(_) => Ok(()),
                Self::Or { alts: ps }
                | Self::Variant { binds: ps, .. }
                | Self::Slice { binds: ps, .. }
                | Self::SlicePrefix { prefix: ps, .. }
                | Self::SliceSuffix { suffix: ps, .. } => {
                    for (p, t) in ps.iter_mut().zip(ts) {
                        if let Some((t, _)) = t {
                            p.realign_with(env, &t, sub)?
                        }
                    }
                    Ok(())
                }
                Self::Abstract { bind, .. } => match ts.into_iter().next().flatten() {
                    Some((t, _)) => bind.realign_with(env, &t, sub),
                    None => Ok(()),
                },
            }
        })
    }

    /// Compile `spec` against the explicit type `type_predicate` (a
    /// `let` or lambda parameter's type).
    pub fn compile<R: Rt>(
        ctx: &mut CompileCtx<R>,
        type_predicate: &Type,
        spec: &StructurePattern,
        scope: &Scope,
        pos: SourcePosition,
        ori: Arc<Origin>,
    ) -> Result<Self> {
        let cx = PatCx { scope, pos, ori: &ori, inferred: false };
        Self::compile_with(ctx, cx, type_predicate, spec)
    }

    fn compile_with<R: Rt>(
        ctx: &mut CompileCtx<R>,
        cx: PatCx,
        type_predicate: &Type,
        spec: &StructurePattern,
    ) -> Result<Self> {
        if !spec.binds_uniq() {
            bail!("bound variables must have unique names")
        }
        Self::compile_int(ctx, cx, type_predicate, spec, BindMode::Fresh)
    }

    fn compile_int<R: Rt>(
        ctx: &mut CompileCtx<R>,
        cx: PatCx,
        type_predicate: &Type,
        spec: &StructurePattern,
        mode: BindMode,
    ) -> Result<Self> {
        crate::stack::ensure_sufficient(|| {
            Self::compile_int_inner(ctx, cx, type_predicate, spec, mode)
        })
    }

    /// A slice pattern's parts against an `Array`/`List` type: the
    /// `all@` capture, the `rest..` bind (array-typed) and the elements,
    /// each against its own member of an inferred element Set.
    fn compile_slice<R: Rt>(
        ctx: &mut CompileCtx<R>,
        cx: PatCx,
        typ: &Type,
        list: bool,
        open: bool,
        all: &Option<Name>,
        rest: &Option<Name>,
        elems: &[StructurePattern],
        mut mode: BindMode,
    ) -> Result<(Option<BindId>, Option<BindId>, Box<[Self]>)> {
        let want = if list {
            Type::List(Arc::new(Type::empty_tvar()))
        } else {
            Type::Array(Arc::new(Type::empty_tvar()))
        };
        typ.check_contains(&ctx.env, &want)?;
        let et = typ.with_deref(|t| match t {
            Some(Type::Array(et)) if !list => Some((**et).clone()),
            Some(Type::List(et)) if list => Some((**et).clone()),
            _ => None,
        });
        let Some(et) = et else {
            return format_with_flags(PrintFlag::DerefTVars, || {
                bail!("slice patterns can't match {typ}")
            });
        };
        let members = match cx.inferred {
            true => set_members(&et, elems.len() + open as usize),
            false => None,
        };
        let all = bind_all(ctx, cx, all, typ, &mut mode)?;
        let rest = rest.as_ref().map(|n| leaf_bind(ctx, cx, n, typ, &mut mode));
        let rest = rest.transpose()?;
        let elems = elems
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let et = members.as_ref().map_or(&et, |ts| &ts[i]);
                Self::compile_int(ctx, cx, et, p, mode.reborrow())
            })
            .collect::<Result<Box<[Self]>>>()?;
        Ok((all, rest, elems))
    }

    fn compile_int_inner<R: Rt>(
        ctx: &mut CompileCtx<R>,
        cx: PatCx,
        type_predicate: &Type,
        spec: &StructurePattern,
        mut mode: BindMode,
    ) -> Result<Self> {
        // an alias of an alias expands to its body: typedefs are
        // contractive, so the chain ends. The chase fills no cell, and stops
        // at a name a later statement declares, which means that type: a
        // name binds at it, and a pattern that tests or takes it apart is
        // refused
        let mut type_predicate = type_predicate.clone();
        while let Type::Ref(tr) = &type_predicate {
            if super::later_typedef(ctx, tr) {
                if let StructurePattern::Bind(_) | StructurePattern::Ignore = spec {
                    break;
                }
                bail!(
                    "{} names the type a later statement of this block declares: \
                     declare it before this pattern",
                    tr.name
                )
            }
            type_predicate = type_predicate.lookup_ref_peek(&ctx.env)?;
        }
        let type_predicate = &type_predicate;
        // under a written type, a constructor pattern at a union narrows to
        // the members it can match, as an arm narrows over its scrutinee;
        // its own test then decides them (`covers`)
        let constructor = matches!(
            spec,
            StructurePattern::Tuple { .. }
                | StructurePattern::Variant { .. }
                | StructurePattern::Struct { .. }
                | StructurePattern::Slice { .. }
                | StructurePattern::SlicePrefix { .. }
                | StructurePattern::SliceSuffix { .. }
        );
        if constructor
            && !cx.inferred
            && type_predicate.with_deref(|t| matches!(t, Some(Type::Set(_))))
        {
            let env = &ctx.env;
            let shape = spec.infer_type_predicate(env, &cx.scope.lexical)?;
            // the written type is the run-time test: the completion shapes
            // only the binds
            let shape = match spec.complete_type_predicate(env, &shape, type_predicate)? {
                Some(t) => t,
                None => shape,
            };
            let shape = shape.any_as_tvar();
            if !type_predicate.narrow_pattern(env, &shape)? {
                type_predicate.check_contains(env, &shape)?;
            }
            let prior: SmallVec<[(BindId, Type); 4]> = match &mode {
                BindMode::Reuse(map) => map.values().cloned().collect(),
                BindMode::Fresh | BindMode::Record(_) => SmallVec::new(),
            };
            let node = Self::compile_int_inner(ctx, cx, &shape, spec, mode.reborrow())?;
            for Leaf { id, typ, .. } in
                node.names_by_member(&ctx.env, type_predicate, &shape)?
            {
                let typ = match prior.iter().find(|(p, _)| *p == id) {
                    Some((_, t0)) => Type::union(&ctx.env, &[t0, &typ])?,
                    None => typ,
                };
                if let BindMode::Record(map) | BindMode::Reuse(map) = &mut mode
                    && let Some((_, t)) = map.values_mut().find(|(p, _)| *p == id)
                {
                    *t = typ.clone()
                }
                ctx.env.retype(id, typ)
            }
            let env = &ctx.env;
            if let Ok(rest) = type_predicate.diff(env, &shape)
                && node.footprint(env, &shape, false).rep_collision(env, &rest).is_some()
            {
                let (a, b) = (shape.resolve_tvars(), rest.resolve_tvars());
                return format_with_flags(PrintFlag::DerefTVars, || {
                    bail!(
                        "this pattern can't tell {a} from {b}: its structure test \
                         passes a value of either; test the type first"
                    )
                });
            }
            return Ok(node);
        }
        let t = match &spec {
            StructurePattern::Or(alts) => {
                if alts.len() < 2 {
                    bail!("an or-pattern needs at least two alternatives")
                }
                let mut names0: SmallVec<[&ArcStr; 8]> = smallvec![];
                alts[0].with_names(&mut |n| names0.push(n));
                names0.sort();
                for alt in &alts[1..] {
                    let mut names: SmallVec<[&ArcStr; 8]> = smallvec![];
                    alt.with_names(&mut |n| names.push(n));
                    names.sort();
                    let mut d = names.clone();
                    d.dedup();
                    if d.len() != names.len() {
                        bail!("bound variables must have unique names")
                    }
                    if names != names0 {
                        bail!("or-pattern alternatives must bind the same names")
                    }
                }
                for (i, alt) in alts.iter().enumerate() {
                    if alts[..i].iter().any(|prev| prev == alt) {
                        bail!(
                            "unreachable or-pattern alternative: duplicate of an \
                             earlier alternative"
                        )
                    }
                }
                // Each alternative compiles against its own member of an
                // inferred predicate; under an explicit `T as p1 | p2`
                // every alternative checks against T.
                let alt_types = match cx.inferred {
                    true => set_members(type_predicate, alts.len()),
                    false => None,
                };
                let mut local = AHashMap::default();
                let (map, reuse_first) = match &mut mode {
                    BindMode::Fresh => (&mut local, false),
                    BindMode::Record(m) => (&mut **m, false),
                    BindMode::Reuse(m) => (&mut **m, true),
                };
                let compiled = alts
                    .iter()
                    .enumerate()
                    .map(|(i, alt)| {
                        let typ = alt_types.as_ref().map_or(type_predicate, |ts| &ts[i]);
                        let mode = if i == 0 && !reuse_first {
                            BindMode::Record(&mut *map)
                        } else {
                            BindMode::Reuse(&mut *map)
                        };
                        Self::compile_int(ctx, cx, typ, alt, mode)
                    })
                    .collect::<Result<Box<[Self]>>>()?;
                // a bare name or `_` matches every value of every member; any
                // other alternative only its own member's, judged by the
                // select arm by arm
                let wild = |p: &Self| match cx.inferred {
                    true => matches!(p, Self::Bind(_) | Self::Ignore),
                    false => p.covers(&ctx.env, type_predicate, false),
                };
                for i in 1..compiled.len() {
                    if compiled[..i].iter().any(wild) {
                        bail!(
                            "unreachable or-pattern alternative: an earlier \
                             alternative already matches anything"
                        )
                    }
                }
                Self::Or { alts: compiled }
            }
            StructurePattern::Ignore => Self::Ignore,
            StructurePattern::Literal(v, _) => {
                type_predicate.check_contains(&ctx.env, &Type::of_literal(v)?)?;
                Self::Literal(v.clone())
            }
            StructurePattern::Bind(name) => {
                Self::Bind(leaf_bind(ctx, cx, name, type_predicate, &mut mode)?)
            }
            StructurePattern::SlicePrefix { list, all, prefix, tail } => {
                let (all, tail, prefix) = Self::compile_slice(
                    ctx,
                    cx,
                    type_predicate,
                    *list,
                    true,
                    all,
                    tail,
                    prefix,
                    mode,
                )?;
                Self::SlicePrefix { list: *list, all, prefix, tail }
            }
            StructurePattern::SliceSuffix { all, head, suffix } => {
                let (all, head, suffix) = Self::compile_slice(
                    ctx,
                    cx,
                    type_predicate,
                    false,
                    true,
                    all,
                    head,
                    suffix,
                    mode,
                )?;
                Self::SliceSuffix { all, head, suffix }
            }
            StructurePattern::Slice { list, all, binds } => {
                let (all, _, binds) = Self::compile_slice(
                    ctx,
                    cx,
                    type_predicate,
                    *list,
                    false,
                    all,
                    &None,
                    binds,
                    mode,
                )?;
                let kind = if *list { SliceKind::List } else { SliceKind::Array };
                Self::Slice { kind, all, binds }
            }
            StructurePattern::Tuple { all, binds } => {
                // an open predicate takes the tuple's shape; a tuple has it,
                // and a check would expand the names its elements hold
                if !matches!(type_predicate.deref_cloned(), Some(Type::Tuple(_))) {
                    type_predicate.check_contains(
                        &ctx.env,
                        &Type::Tuple(Arc::from_iter(
                            binds.iter().map(|_| Type::empty_tvar()),
                        )),
                    )?;
                }
                let Some(Type::Tuple(elts)) = &type_predicate.deref_cloned() else {
                    return format_with_flags(PrintFlag::DerefTVars, || {
                        bail!("tuple patterns can't match {type_predicate}")
                    });
                };
                if binds.len() != elts.len() {
                    bail!("expected a tuple of length {}", elts.len())
                }
                let all = bind_all(ctx, cx, all, type_predicate, &mut mode)?;
                let binds = elts
                    .iter()
                    .zip(binds.iter())
                    .map(|(t, b)| Self::compile_int(ctx, cx, t, b, mode.reborrow()))
                    .collect::<Result<Box<[Self]>>>()?;
                Self::Slice { kind: SliceKind::Tuple, all, binds }
            }
            StructurePattern::Variant { all, tag, binds } => {
                if !matches!(type_predicate.deref_cloned(), Some(Type::Variant(..))) {
                    type_predicate.check_contains(
                        &ctx.env,
                        &Type::Variant(
                            tag.clone(),
                            Arc::from_iter(binds.iter().map(|_| Type::empty_tvar())),
                            WrittenAt::NOWHERE,
                        ),
                    )?;
                }
                let Some(Type::Variant(ttag, elts, _)) = &type_predicate.deref_cloned()
                else {
                    return format_with_flags(PrintFlag::DerefTVars, || {
                        bail!("variant patterns can't match {type_predicate}")
                    });
                };
                if *ttag != *tag {
                    bail!("pattern cannot match type, tag mismatch {ttag} vs {tag}")
                }
                if binds.len() != elts.len() {
                    bail!("expected a variant with {} args", elts.len())
                }
                let all = bind_all(ctx, cx, all, type_predicate, &mut mode)?;
                let binds = elts
                    .iter()
                    .zip(binds.iter())
                    .map(|(t, b)| Self::compile_int(ctx, cx, t, b, mode.reborrow()))
                    .collect::<Result<Box<[Self]>>>()?;
                Self::Variant { tag: tag.clone(), all, binds }
            }
            StructurePattern::Abstract { all, name, at, bind } => {
                let td = ctx
                    .env
                    .lookup_typedef(&cx.scope.lexical, name)?
                    .ok_or_else(|| anyhow!("unknown type {name}"))?;
                if let Some(pos) = at.get() {
                    ctx.env.push_typedef_ref(&td, name, &cx.scope.lexical, pos, cx.ori);
                }
                let Type::Abstract { id, .. } = td.typ() else {
                    bail!("{name} is not an abstract type, so it has no constructor")
                };
                let id = *id;
                let Some(r) = ctx.env.abstract_rep(id, &cx.scope.lexical) else {
                    bail!(
                        "the definition of {name} is not visible here, so its values \
                         cannot be destructured"
                    )
                };
                let (atyp, rep) = r.instantiate(id);
                let own = cx.inferred.then(|| r.instantiate(id).1);
                type_predicate.check_contains(&ctx.env, &atyp)?;
                // the predicate is this abstract type, as a variant pattern's is
                // its variant: a union around it would make the arm refutable by
                // a tag test nothing performs
                if !matches!(type_predicate.deref_cloned(), Some(Type::Abstract { id: t, .. }) if t == id)
                {
                    return format_with_flags(PrintFlag::DerefTVars, || {
                        bail!(
                            "{name}(..) patterns can't match {type_predicate}: test the tag first, `{name} as x`"
                        )
                    });
                }
                let all = bind_all(ctx, cx, all, type_predicate, &mut mode)?;
                // the payload checks against the declared representation; under
                // an inferred predicate, at parameters of its own, the
                // predicate's being the scrutinee's (`leaves` types its binds)
                let rep = own.unwrap_or(rep);
                let rep_cx = PatCx { inferred: false, ..cx };
                let bind = Box::new(Self::compile_int(ctx, rep_cx, &rep, bind, mode)?);
                Self::Abstract { id, all, rep, bind }
            }
            StructurePattern::Struct { exhaustive, all, binds } => {
                match &type_predicate {
                    Type::Struct(_) => (),
                    _ if *exhaustive => type_predicate.check_contains(
                        &ctx.env,
                        &Type::Struct(Arc::from_iter(binds.iter().map(
                            |(name, _, _)| {
                                (name.clone(), Type::empty_tvar(), WrittenAt::NOWHERE)
                            },
                        ))),
                    )?,
                    _ => bail!("non exhaustive struct matches require type annotations"),
                }
                let Some(Type::Struct(elts)) = &type_predicate.deref_cloned() else {
                    return format_with_flags(PrintFlag::DerefTVars, || {
                        bail!("struct patterns can't match {type_predicate}")
                    });
                };
                let fields = binds
                    .iter()
                    .map(|(field, pat, _)| {
                        elts.iter()
                            .position(|(name, _, _)| field == name)
                            .map(|i| (i, pat))
                            .ok_or_else(|| anyhow!("no such struct field {field}"))
                    })
                    .collect::<Result<SmallVec<[(usize, &StructurePattern); 8]>>>()?;
                if *exhaustive && fields.len() < elts.len() {
                    bail!("missing bindings for struct fields")
                }
                let width = if !*exhaustive && cx.inferred {
                    StructWidth::AtLeast(elts.len())
                } else {
                    StructWidth::Exact(elts.len())
                };
                let all = bind_all(ctx, cx, all, type_predicate, &mut mode)?;
                let binds = fields
                    .into_iter()
                    .map(|(i, pat)| {
                        let (name, typ, _) = &elts[i];
                        let p = Self::compile_int(ctx, cx, typ, pat, mode.reborrow())?;
                        Ok((name.clone(), i, p))
                    })
                    .collect::<Result<Box<[(ArcStr, usize, Self)]>>>()?;
                Self::Struct { all, width, binds }
            }
        };
        Ok(t)
    }

    /// Each name this pattern binds with the union of its part of each
    /// member of `typ` that `shape`, the one view of them all it was
    /// compiled against, can match.
    pub(super) fn names_by_member(
        &self,
        env: &Env,
        typ: &Type,
        shape: &Type,
    ) -> Result<SmallVec<[Leaf; 4]>> {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        union_members(env, typ, &mut members)?;
        let mut parts: SmallVec<[Leaf; 4]> = SmallVec::new();
        for m in members.iter() {
            if shape.could_match(env, m)? {
                self.leaves(env, m, false, false, &mut parts)?
            }
        }
        let mut names = SmallVec::new();
        merge_leaves(env, parts, &mut names)?;
        Ok(names)
    }

    /// Retype each name a `let`'s pattern, compiled against `shape`, the
    /// value's type `typ` narrowed per member, binds by
    /// [`Self::names_by_member`].
    pub(super) fn retype_by_member<R: Rt>(
        &self,
        ctx: &mut CompileCtx<R>,
        typ: &Type,
        shape: &Type,
    ) -> Result<()> {
        for Leaf { id, typ, .. } in self.names_by_member(&ctx.env, typ, shape)? {
            ctx.env.retype(id, typ)
        }
        Ok(())
    }

    /// For a tuple destructure pattern `(a, b, …)` with only simple
    /// `Bind`/`Ignore` leaves and no whole-binding, return each `Bind`
    /// leaf's `(BindId, tuple position)`. `None` for any other shape.
    pub fn tuple_leaves(&self) -> Option<Vec<(BindId, usize)>> {
        match self {
            Self::Slice { kind: SliceKind::Tuple, all: None, binds } => {
                let mut out = Vec::with_capacity(binds.len());
                for (i, b) in binds.iter().enumerate() {
                    match b {
                        Self::Bind(id) => out.push((*id, i)),
                        Self::Ignore => {}
                        _ => return None,
                    }
                }
                Some(out)
            }
            _ => None,
        }
    }

    /// For a single-name binding pattern (`x` in `|x| body`), the bound
    /// `BindId`; `None` for destructures / ignores / literals.
    pub fn single_bind_id(&self) -> Option<BindId> {
        match self {
            Self::Bind(id) => Some(*id),
            _ => None,
        }
    }

    pub fn ids<'a>(&'a self, f: &mut (dyn FnMut(BindId) + 'a)) {
        crate::stack::ensure_sufficient(|| self.ids_inner(f))
    }

    fn ids_inner<'a>(&'a self, f: &mut (dyn FnMut(BindId) + 'a)) {
        match self {
            Self::Bind(id) => f(*id),
            Self::Or { alts } => alts[0].ids(f),
            _ => {
                self.whole_binds().for_each(&mut *f);
                for c in self.children() {
                    c.ids(f)
                }
            }
        }
    }

    pub fn bind<F: FnMut(BindId, Value)>(&self, v: &Value, f: &mut F) {
        crate::stack::ensure_sufficient(|| self.bind_inner(v, f))
    }

    fn bind_inner<F: FnMut(BindId, Value)>(&self, v: &Value, f: &mut F) {
        match &self {
            Self::Abstract { id, all, bind, .. } => {
                if let Some(bid) = all {
                    f(*bid, v.clone())
                }
                if let Some(g) = crate::abstract_value::get(v)
                    && g.id == *id
                {
                    bind.bind(&g.payload, f)
                }
            }
            Self::Ignore | Self::Literal(_) => (),
            Self::Bind(id) => f(*id, v.clone()),
            // the first matching alternative delivers the shared ids
            Self::Or { alts } => {
                for a in alts.iter() {
                    if a.is_match(v) {
                        return a.bind(v, f);
                    }
                }
            }
            Self::Slice {
                kind: kind @ (SliceKind::Tuple | SliceKind::Array),
                all,
                binds,
            } => match slice_elems(*kind, v) {
                Some(a) if a.len() == binds.len() => {
                    if let Some(id) = all {
                        f(*id, v.clone());
                    }
                    for (j, n) in binds.iter().enumerate() {
                        n.bind(&a[j], f)
                    }
                }
                _ => (),
            },
            Self::Slice { kind: SliceKind::List, all, binds } => {
                if let Some(id) = all {
                    f(*id, v.clone());
                }
                list::zip_prefix(v, binds, |n, h| {
                    n.bind(h, f);
                    true
                });
            }
            Self::Variant { tag: _, all, binds } => {
                if let Some(id) = all {
                    f(*id, v.clone())
                }
                if let Some((_, payload)) = rep::variant(v)
                    && payload.len() == binds.len()
                {
                    for (n, v) in binds.iter().zip(payload) {
                        n.bind(v, f)
                    }
                }
            }
            Self::SlicePrefix { list: false, all, prefix, tail } => {
                match rep::array_elems(v) {
                    Some(a) if a.len() >= prefix.len() => {
                        if let Some(id) = all {
                            f(*id, v.clone())
                        }
                        for (j, n) in prefix.iter().enumerate() {
                            n.bind(&a[j], f)
                        }
                        if let Some(id) = tail {
                            let ss = a.subslice(prefix.len()..).unwrap();
                            f(*id, Value::Array(ss))
                        }
                    }
                    _ => (),
                }
            }
            // heads bind by walking the spine; the tail bind is the k-th
            // tail itself
            Self::SlicePrefix { list: true, all, prefix, tail } => {
                if let Some(id) = all {
                    f(*id, v.clone())
                }
                let rest = list::zip_prefix(v, prefix, |n, h| {
                    n.bind(h, f);
                    true
                });
                if let (Some(rest), Some(id)) = (rest, tail) {
                    f(*id, rest.clone())
                }
            }
            Self::SliceSuffix { all, head, suffix } => match rep::array_elems(v) {
                Some(a) if a.len() >= suffix.len() => {
                    // binds read from the same offset `is_match` skips to
                    let split = a.len() - suffix.len();
                    if let Some(id) = all {
                        f(*id, v.clone())
                    }
                    if let Some(id) = head {
                        let ss = a.subslice(..split).unwrap();
                        f(*id, Value::Array(ss))
                    }
                    let tail = a.subslice(split..).unwrap();
                    for (j, n) in suffix.iter().enumerate() {
                        n.bind(&tail[j], f)
                    }
                }
                _ => (),
            },
            Self::Struct { all, width, binds } => match rep::struct_fields(v) {
                Some(a) if width.admits(a.len()) => {
                    if let Some(id) = all {
                        f(*id, v.clone())
                    }
                    for (name, i, n) in binds.iter() {
                        if let Some((field, v)) = rep::field_at(a, *i)
                            && field == name
                        {
                            n.bind(v, f)
                        }
                    }
                }
                _ => (),
            },
        }
    }

    pub fn is_match(&self, v: &Value) -> bool {
        crate::stack::ensure_sufficient(|| self.is_match_inner(v))
    }

    fn is_match_inner(&self, v: &Value) -> bool {
        match &self {
            Self::Abstract { id, bind, .. } => match crate::abstract_value::get(v) {
                Some(g) => g.id == *id && bind.is_match(&g.payload),
                None => false,
            },
            Self::Ignore | Self::Bind(_) => true,
            Self::Or { alts } => alts.iter().any(|a| a.is_match(v)),
            Self::Literal(o) => v == o,
            Self::Slice {
                kind: kind @ (SliceKind::Tuple | SliceKind::Array),
                all: _,
                binds,
            } => match slice_elems(*kind, v) {
                Some(a) => {
                    a.len() == binds.len()
                        && binds.iter().zip(a.iter()).all(|(b, v)| b.is_match(v))
                }
                None => false,
            },
            // exactly `binds.len()` cells, each head matching, ending at nil
            Self::Slice { kind: SliceKind::List, all: _, binds } => {
                list::zip_prefix(v, binds, |b, h| b.is_match(h)).is_some_and(list::is_nil)
            }
            Self::Variant { tag, all: _, binds } => match rep::variant(v) {
                Some((t, payload)) => {
                    t == tag
                        && payload.len() == binds.len()
                        && binds.iter().zip(payload).all(|(b, v)| b.is_match(v))
                }
                None => false,
            },
            Self::SlicePrefix { list: false, all: _, prefix, tail: _ } => {
                match rep::array_elems(v) {
                    Some(a) => {
                        a.len() >= prefix.len()
                            && prefix.iter().zip(a.iter()).all(|(b, v)| b.is_match(v))
                    }
                    _ => false,
                }
            }
            Self::SlicePrefix { list: true, all: _, prefix, tail: _ } => {
                list::zip_prefix(v, prefix, |b, h| b.is_match(h)).is_some()
            }
            Self::SliceSuffix { all: _, head: _, suffix } => match rep::array_elems(v) {
                Some(a) => {
                    a.len() >= suffix.len()
                        && suffix
                            .iter()
                            .zip(a.iter().skip(a.len() - suffix.len()))
                            .all(|(b, v)| b.is_match(v))
                }
                _ => false,
            },
            // a field is the pair at its index naming it: a struct of
            // another shape that has the index fails
            Self::Struct { all: _, width, binds } => match rep::struct_fields(v) {
                Some(a) => {
                    width.admits(a.len())
                        && binds.iter().all(|(name, i, p)| match rep::field_at(a, *i) {
                            Some((field, v)) if field == name => p.is_match(v),
                            _ => false,
                        })
                }
                _ => false,
            },
        }
    }

    // CR claude for claude: [bug/low] Bool literals in a tuple beside an arm that takes a
    // `null` at another position cover nothing: over `x: (bool, [i64, null])`, `select x {
    // (_, null) => 1, (true, _) => 0, (false, _) => 2 }` and `{ (true, null) => 0, (true,
    // _) => 2, (false, _) => 1 }` are refused "missing match cases: no arm covers (bool,
    // [i64, null])" on main (968fa9bc0) as on cr49-B, where `{ (true, _) => 0, (false, _)
    // => 1 }` runs. A written test meets it too, since matched_part answers None for any
    // literal but `null`: `(true, _) => 0, (bool, i64) as (false, n) => n, (bool, null) as
    // (false, _) => 1` is refused alike. D6 (b) sends a pattern inside a declared
    // variable to a written test, so the shape is met more often: under `'b: [i64,
    // string]`, `(true, _)`, `(bool, i64) as (false, n)`, `(bool, string) as (false, s)`
    // and `(false, null)` over `(bool, ['b, null])` ran on main through the D6 hole and
    // are refused now. probes: ~/ptmp/cr29/scratch/r14-B/fix1/{q10t,q10r,q10p}.gx,
    // r14-B/r2/q10c.gx, ~/ptmp/cr29/scratch/review-r11/N-r3-2/q/q10.gx
    // (bool-beside-null-cover-01)
    // 2026-11-04 claude: Partly fixed: the pool reads a primitive member beside bool and
    // null as a head only a name or _ admits (Head::Rest), so q10t and q10r check and run
    // ([1, 2, 1] and [0, 1, 1]); pins
    // lang::select_undecided::bool_heads_beside_a_null_arm_cover,
    // bool_heads_after_a_null_head_cover, rest_head_left_uncovered_refused. The
    // written-test forms (q10p, q10c, q10 under 'b: [i64, string]) stay refused 'missing
    // match cases': a written test whose type does not hold the whole member pools nothing
    // (LiteralPool::claim skips a member the test does not contain) and matched_part
    // answers None for a bool literal; covering them needs the pool to read a written test
    // as the member's part it holds, a change to how written tests pool, not this rule's.
    // 2026-11-06 claude: Parked (r17, lane M): the fix (sonnet a9176bf8a plus three
    // repairs) failed review three times in new ways: the pool's combination cap refusing
    // programs main runs (r17 review), a select whose written tests name enough primitives
    // still refused (lane M r1), and a test applying a typedef twice claiming a primitive
    // it does not hold, so the check passed and the run aborted (lane M r2, bn_td_twice).
    // Its last repair passed review, but by the park rule it is reverted; the commits stay
    // on branch cr52-m's history (e2e5f58c2, 909a37a78, c98a34ae5, 8537fead4). The two
    // gaps it left (a written test naming the declared variable at the position, wc_s8;
    // two positions narrowed by four tests, wc_s9) stand on main too. probes
    // ~/ptmp/cr29/scratch/rv17/typing-a/wc_*.gx,
    // ~/ptmp/cr29/scratch/review-r16/m-r*/bn_*.gx
    /// The part of `typ` this pattern covers when its only refutable parts
    /// are `null` literals and constructors: `typ` with `null` at each
    /// `null` and, where a constructor stands over a union, the members
    /// it takes, so `{a: null}` over `{a: [i64, null]}` covers `{a: null}`
    /// and `` (`B, n) `` over `` ([`A, `B], i64) `` covers `` (`B, i64) ``.
    /// `None` for any other pattern, or where no member passes.
    pub(super) fn matched_part(&self, env: &Env, typ: &Type) -> Option<Type> {
        let parts = |binds: &[Self], ts: &[Type]| {
            (ts.len() == binds.len()).then_some(())?;
            let parts = binds
                .iter()
                .zip(ts.iter())
                .map(|(b, t)| b.matched_part(env, t))
                .collect::<Option<SmallVec<[Type; 8]>>>()?;
            Some(Arc::from_iter(parts))
        };
        crate::stack::ensure_sufficient(|| match self {
            Self::Bind(_) | Self::Ignore => Some(typ.clone()),
            Self::Literal(Value::Null) => {
                let null = Type::Primitive(Typ::Null.into());
                null.could_match(env, typ).unwrap_or(false).then_some(null)
            }
            Self::Slice { kind: SliceKind::Tuple, binds, all: _ } => {
                match &expand(env, typ)? {
                    Type::Tuple(ts) => Some(Type::Tuple(parts(binds, ts)?)),
                    t => self.members_part(env, t),
                }
            }
            Self::Variant { tag, binds, all: _ } => match &expand(env, typ)? {
                Type::Variant(t, ts, at) if t == tag => {
                    Some(Type::Variant(t.clone(), parts(binds, ts)?, *at))
                }
                t => self.members_part(env, t),
            },
            Self::Struct { binds, width, all: _ } => match &expand(env, typ)? {
                Type::Struct(fs) => {
                    width.admits(fs.len()).then_some(())?;
                    let fields = fs
                        .iter()
                        .map(|(n, t, at)| {
                            let t = match binds.iter().find(|(m, _, _)| m == n) {
                                Some((_, _, p)) => p.matched_part(env, t)?,
                                None => t.clone(),
                            };
                            Some((n.clone(), t, *at))
                        })
                        .collect::<Option<SmallVec<[_; 8]>>>()?;
                    Some(Type::Struct(Arc::from_iter(fields)))
                }
                t => self.members_part(env, t),
            },
            _ => None,
        })
    }

    /// [`Self::matched_part`] over `t`, a union: the parts of its members.
    fn members_part(&self, env: &Env, t: &Type) -> Option<Type> {
        let Type::Set(ms) = t else { return None };
        let mut parts: SmallVec<[Type; 4]> =
            ms.iter().filter_map(|m| self.matched_part(env, m)).collect();
        match parts.len() {
            0 => None,
            1 => parts.pop(),
            _ => Some(Type::Set(Arc::from_iter(parts)).normalize()),
        }
    }

    /// Whether every value of `typ` passes this pattern's structure:
    /// the pattern is irrefutable over `typ`. A constructor pattern
    /// covers only a position whose type is that constructor, so its own
    /// tag, arity or field test decides nothing there; `inferred` pairs
    /// an or-pattern's alternatives and a slice's elements with their
    /// members, as [`Self::child_types`] does.
    pub fn covers(&self, env: &Env, typ: &Type, inferred: bool) -> bool {
        crate::stack::ensure_sufficient(|| self.covers_inner(env, typ, inferred))
    }

    fn covers_inner(&self, env: &Env, typ: &Type, inferred: bool) -> bool {
        if let Self::Bind(_) | Self::Ignore = self {
            return true;
        }
        // alternatives cover a type when each of its members is covered by
        // one of them
        if let Self::Or { alts } = self {
            let mut members: SmallVec<[Type; 8]> = SmallVec::new();
            return union_members(env, typ, &mut members).is_ok()
                && !members.is_empty()
                && members.iter().all(|m| alts.iter().any(|a| a.covers(env, m, false)));
        }
        // an abstract pattern's run-time test reads the tag alone: it covers
        // a union of its type's instances whose every representation its
        // payload covers
        if let Self::Abstract { id, bind, .. } = self {
            let mut members: SmallVec<[Type; 8]> = SmallVec::new();
            return union_members(env, typ, &mut members).is_ok()
                && !members.is_empty()
                && members.iter().all(|m| {
                    matches!(expand(env, m), Some(Type::Abstract { id: t, .. }) if t == *id)
                        && self.member_rep(env, m).is_some_and(|r| bind.covers(env, &r, false))
                });
        }
        let shape = expand(env, typ);
        let shaped = match (self, &shape) {
            (
                Self::Literal(_)
                | Self::Slice { kind: SliceKind::Array | SliceKind::List, .. }
                | Self::SlicePrefix { .. }
                | Self::SliceSuffix { .. },
                _,
            ) => return false,
            (
                Self::Slice { kind: SliceKind::Tuple, binds, .. },
                Some(Type::Tuple(ts)),
            ) => ts.len() == binds.len(),
            (Self::Variant { tag, binds, .. }, Some(Type::Variant(t, ts, _))) => {
                t == tag && ts.len() == binds.len()
            }
            (Self::Struct { width, .. }, Some(Type::Struct(fs))) => {
                width.admits(fs.len())
            }
            _ => false,
        };
        let Some(ts) = shaped.then(|| self.child_types(env, typ, inferred)).flatten()
        else {
            return false;
        };
        let sub = self.children_inferred(inferred);
        self.children()
            .into_iter()
            .zip(ts)
            .all(|(c, t)| t.is_some_and(|(t, _)| c.covers(env, &t, sub)))
    }

    /// Whether a value of `typ` may pass this structure, by shape alone:
    /// false only where no member of `typ` has a constructor, tag, arity
    /// or field the structure tests, here or below.
    pub(crate) fn may_pass(&self, env: &Env, typ: &Type) -> bool {
        crate::stack::ensure_sufficient(|| {
            let Some(t) = expand(env, typ) else { return true };
            match (self, &t) {
                (Self::Or { alts }, _) => alts.iter().any(|a| a.may_pass(env, &t)),
                (
                    Self::Ignore
                    | Self::Literal(_)
                    | Self::Bind(_)
                    | Self::Abstract { .. },
                    _,
                ) => true,
                (_, Type::Set(ms)) => ms.iter().any(|m| self.may_pass(env, m)),
                (
                    _,
                    Type::Tuple(_)
                    | Type::Variant(..)
                    | Type::Struct(_)
                    | Type::Array(_)
                    | Type::List(_),
                ) => self.child_types(env, &t, false).is_some_and(|ts| {
                    self.children()
                        .into_iter()
                        .zip(ts)
                        .all(|(c, t)| t.is_some_and(|(t, _)| c.may_pass(env, &t)))
                }),
                _ => true,
            }
        })
    }

    /// Whether an abstract pattern is in this one.
    pub(super) fn holds_abstract(&self) -> bool {
        crate::stack::ensure_sufficient(|| {
            matches!(self, Self::Abstract { .. })
                || self.children().into_iter().any(|c| c.holds_abstract())
        })
    }

    /// True when the pattern matches any value of the scrutinee's type:
    /// `Select`'s wildcard test. A variant pattern with an all-bind
    /// payload is irrefutable given its tag matched, but its inferred
    /// predicate still carries the tag test.
    pub fn matches_anything(&self) -> bool {
        crate::stack::ensure_sufficient(|| self.matches_anything_inner())
    }

    /// Shape test only: an array slice pattern of any element
    /// refutability.
    pub fn is_array_slice(&self) -> bool {
        match self {
            Self::Slice { kind: SliceKind::Array | SliceKind::List, .. }
            | Self::SlicePrefix { .. }
            | Self::SliceSuffix { .. } => true,
            Self::Or { alts } => alts.iter().any(|a| a.is_array_slice()),
            _ => false,
        }
    }

    /// The length range of an array slice pattern, `Some((k, exact))`:
    /// it matches only arrays of length == k (`exact`) or >= k,
    /// regardless of whether its element sub-patterns can refute.
    pub fn array_len_range(&self) -> Option<(usize, bool)> {
        match self {
            Self::Slice { kind: SliceKind::Array | SliceKind::List, all: _, binds } => {
                Some((binds.len(), true))
            }
            Self::SlicePrefix { list: _, all: _, prefix, tail: _ } => {
                Some((prefix.len(), false))
            }
            Self::SliceSuffix { all: _, head: _, suffix } => Some((suffix.len(), false)),
            _ => None,
        }
    }

    /// The pattern's array-length coverage claim: the length range, but
    /// only when every element sub-pattern matches anything. The type
    /// half of the claim is the caller's to verify.
    /// Under a written type test, `explicit`, the elements must also
    /// cover its element type: nothing else tests them.
    pub fn array_len_coverage(
        &self,
        env: &Env,
        explicit: Option<&Type>,
    ) -> Option<(usize, bool)> {
        let all_cover = match self {
            Self::Slice { kind: SliceKind::Array | SliceKind::List, all: _, binds } => {
                binds.iter().all(|p| p.matches_anything())
            }
            Self::SlicePrefix { list: _, all: _, prefix, tail: _ } => {
                prefix.iter().all(|p| p.matches_anything())
            }
            Self::SliceSuffix { all: _, head: _, suffix } => {
                suffix.iter().all(|p| p.matches_anything())
            }
            _ => false,
        };
        let typed = match explicit {
            None => true,
            Some(t) => {
                let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                let listed = union_members(env, t, &mut members).is_ok();
                let mut shaped = members
                    .iter()
                    .filter_map(|m| self.child_types(env, m, false))
                    .peekable();
                listed
                    && shaped.peek().is_some()
                    && shaped.all(|ts| {
                        self.children().into_iter().zip(ts).all(|(c, t)| {
                            t.is_some_and(|(t, _)| c.covers(env, &t, false))
                        })
                    })
            }
        };
        if all_cover && typed { self.array_len_range() } else { None }
    }

    /// Whether this slice pattern's elements each cover the element type
    /// of `m`, an array or list type: it claims its lengths of `m` though
    /// its elements test what another member's may not hold.
    pub(super) fn elements_cover(&self, env: &Env, m: &Type) -> bool {
        self.array_len_range().is_some()
            && self.child_types(env, m, false).is_some_and(|ts| {
                self.children()
                    .into_iter()
                    .zip(ts)
                    .all(|(c, t)| t.is_some_and(|(t, _)| c.covers(env, &t, false)))
            })
    }

    fn matches_anything_inner(&self) -> bool {
        match &self {
            Self::Bind(_) | Self::Ignore => true,
            // an alternative matches anything of its own member only
            Self::Or { .. }
            | Self::Literal(_)
            | Self::Variant { .. }
            | Self::Abstract { .. } => false,
            Self::Slice { kind: SliceKind::Tuple, all: _, binds } => {
                binds.iter().all(|p| p.matches_anything())
            }
            Self::Struct { all: _, width: _, binds } => {
                binds.iter().all(|(_, _, p)| p.matches_anything())
            }
            Self::Slice { kind: SliceKind::Array | SliceKind::List, .. }
            | Self::SlicePrefix { .. }
            | Self::SliceSuffix { .. } => false,
        }
    }

    pub fn delete<R: Rt>(&self, ctx: &mut ExecCtx<'_, R>) {
        self.ids(&mut |id| {
            if ctx.control.lives().orphan_if_pinned(id) {
                ctx.env.unbind_name(id);
            } else {
                ctx.rt.store_release(&id);
                ctx.env.unbind_variable(id);
            }
        })
    }
}

/// One arm's consultation verdict. `NoStruct`: the type/structure test
/// failed and the guard was not consulted. `GuardBottom`: the structure
/// matched and the guard's current channel is bottom, so the selection
/// is undecidable and the select bottoms.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ArmMatch {
    NoStruct,
    GuardFalse,
    GuardBottom,
    Matched,
}

#[derive(Debug)]
pub struct PatternNode<R: Rt> {
    pub explicit_type_predicate: bool,
    pub type_predicate: Type,
    pub structure_predicate: StructPatternNode,
    pub guard: Option<Held<R>>,
}

impl<R: Rt> PatternNode<R> {
    fn with(
        explicit_type_predicate: bool,
        type_predicate: Type,
        structure_predicate: StructPatternNode,
        guard: Option<Held<R>>,
    ) -> Self {
        Self { explicit_type_predicate, type_predicate, structure_predicate, guard }
    }

    pub(crate) fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.explicit_type_predicate.encode(buf)?;
        self.type_predicate.encode(buf)?;
        self.structure_predicate.encode(buf)?;
        self.guard.is_some().encode(buf)?;
        match &self.guard {
            Some(g) => g.image_encode(buf),
            None => Ok(()),
        }
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let explicit_type_predicate = bool::decode(buf)?;
        let type_predicate = Type::decode(buf)?;
        let structure_predicate = StructPatternNode::decode(buf)?;
        let guard = match bool::decode(buf)? {
            true => Some(Held::image_decode(ctx, buf)?),
            false => None,
        };
        Ok(Self::with(
            explicit_type_predicate,
            type_predicate,
            structure_predicate,
            guard,
        ))
    }

    /// The arm's coverage atoms: each or-alternative paired with its
    /// member of an inferred predicate (under an explicit `T as p1 |
    /// p2`, with T); any other arm is one atom.
    pub(super) fn atoms(&self) -> SmallVec<[(&StructPatternNode, Type); 4]> {
        match &self.structure_predicate {
            StructPatternNode::Or { alts } => {
                let ts = match self.explicit_type_predicate {
                    true => None,
                    false => set_members(&self.type_predicate, alts.len()),
                };
                let typ = |i: usize| match &ts {
                    Some(ts) => ts[i].clone(),
                    None => self.type_predicate.clone(),
                };
                alts.iter().enumerate().map(|(i, a)| (a, typ(i))).collect()
            }
            sp => smallvec![(sp, self.type_predicate.clone())],
        }
    }

    /// Does the arm match every value of `scrut` by its structure alone?
    /// Its predicate is inferred, its structure matches anything, and
    /// the predicate's shape covers the scrutinee's, a declared variable
    /// held rigid: it is any value of its bound.
    pub(super) fn matches_every(&self, env: &Env, scrut: &Type) -> Result<bool> {
        Ok(!self.explicit_type_predicate
            && self.structure_predicate.matches_anything()
            && self.type_predicate.contains_with_flags(
                ContainsFlags::RigidCheck.into(),
                env,
                scrut,
            )?)
    }

    pub(super) fn capture_type(&self, env: &Env, reaching: &Type) -> Result<Type> {
        if !self.explicit_type_predicate {
            return Ok(self.type_predicate.any_as_tvar());
        }
        if self.type_predicate.has_unbound() {
            reaching.narrow_pattern(env, &self.type_predicate)?;
        }
        self.type_predicate.matching_type(env, reaching)
    }

    /// Type the arm's binds from `narrowed`, the arm's predicate as the
    /// select narrowed it against what reaches the arm: each capture
    /// takes the part it stands over (a `_` slot and a field a partial
    /// pattern leaves out have the scrutinee's type), and a name
    /// or-alternatives share the union of theirs
    /// ([`StructPatternNode::leaves`]). A whole capture (`a@ (_, _)`)
    /// stands over one of `reaching`, the members of what reaches the arm
    /// as they were before the narrowing, at a time, so it takes the
    /// union of its part of each member the arm can match, where a plain
    /// bind takes the union of its position's parts. Over `scrut`, an open
    /// cell, `reaching` is empty and the whole capture is the cell.
    pub(super) fn bind_narrowed(
        &self,
        env: &Env,
        narrowed: &Type,
        scrut: &Type,
        reaching: &[Type],
        checking: bool,
    ) -> Result<()> {
        // the members of what reaches the arm it can match; for its whole
        // captures, `Any` is each member of the predicate a value passing
        // the arm's test is of
        let mut matched: SmallVec<[Type; 8]> = SmallVec::new();
        let mut wholes: SmallVec<[Type; 8]> = SmallVec::new();
        if !self.explicit_type_predicate {
            for m in reaching {
                if self.type_predicate.could_match(env, m)? {
                    matched.push(m.clone());
                    match m {
                        Type::Any => {
                            union_members(env, &self.type_predicate, &mut wholes)?
                        }
                        m => wholes.push(m.clone()),
                    }
                }
            }
            self.structure_predicate.narrow_payloads(env, &matched)?;
            self.structure_predicate.hold_slice_members(env, narrowed, &matched)?;
        }
        let mut leaves = SmallVec::new();
        self.structure_predicate.leaves(
            env,
            narrowed,
            !self.explicit_type_predicate,
            checking,
            &mut leaves,
        )?;
        if self.explicit_type_predicate && scrut.has_rigid() {
            self.type_inside_rigid(env, scrut, &mut leaves)?
        }
        if !wholes.is_empty() {
            self.wholes_by_member(env, &wholes, &mut leaves)?;
        }
        if !self.explicit_type_predicate
            && reaching.is_empty()
            && let Some(id) = self.structure_predicate.capture()
        {
            for l in leaves.iter_mut().filter(|l| l.id == id) {
                l.typ = scrut.clone()
            }
        }
        for l in leaves.iter().filter(|l| l.fresh || self.explicit_type_predicate) {
            if let Some(b) = env.by_id.get(&l.id) {
                b.typ.check_contains(env, &l.typ)?;
            }
        }
        Ok(())
    }

    /// Under a written test, each bind in `leaves`, typed over what passes
    /// the test among `scrut`'s values, typed instead by the test's part
    /// where it stands inside a rigid cell `scrut` holds, whose values no
    /// pattern takes apart; a name over a value holding the cell keeps it.
    fn type_inside_rigid(
        &self,
        env: &Env,
        scrut: &Type,
        leaves: &mut SmallVec<[Leaf; 4]>,
    ) -> Result<()> {
        let parts = self.type_predicate.matching_parts(env, scrut)?;
        let mut inside: SmallVec<[Leaf; 4]> = SmallVec::new();
        self.structure_predicate.leaves(env, &parts, false, false, &mut inside)?;
        let mut within: SmallVec<[BindId; 4]> = SmallVec::new();
        self.structure_predicate.binds_inside_rigid(env, scrut, &mut within)?;
        for l in inside {
            match leaves.iter_mut().find(|k| k.id == l.id) {
                Some(k) if k.typ.has_rigid() && within.contains(&l.id) => {
                    k.typ = Type::union(env, &[&k.typ, &l.typ])?
                }
                Some(k) if k.typ.has_rigid() => (),
                Some(k) => *k = l,
                None => leaves.push(l),
            }
        }
        Ok(())
    }

    /// Each whole capture in `leaves` typed as the union of its part of
    /// each of `members`, the members of a union reaching the arm that its
    /// predicate can match: each member stands at the pattern's position
    /// whole, its unions distributed at each constructor the pattern tests.
    fn wholes_by_member(
        &self,
        env: &Env,
        members: &[Type],
        leaves: &mut SmallVec<[Leaf; 4]>,
    ) -> Result<()> {
        if !leaves.iter().any(|l| l.fresh) {
            return Ok(());
        }
        let mut parts: AHashMap<BindId, SmallVec<[Type; 4]>> = AHashMap::default();
        for m in members.iter() {
            let mut ls: SmallVec<[Leaf; 4]> = SmallVec::new();
            self.structure_predicate.leaves(env, m, false, false, &mut ls)?;
            for l in ls.into_iter().filter(|l| l.fresh) {
                parts.entry(l.id).or_default().push(l.typ)
            }
        }
        for l in leaves.iter_mut().filter(|l| l.fresh) {
            if let Some(ts) = parts.get(&l.id) {
                let ts: SmallVec<[&Type; 4]> = ts.iter().collect();
                l.typ = Type::union_exact(env, &ts)?;
            }
        }
        Ok(())
    }

    pub(super) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: &Pattern,
        scope: &Scope,
        top_id: ExprId,
        pos: SourcePosition,
        ori: Arc<Origin>,
    ) -> Result<Self> {
        let (explicit, type_predicate) = match &spec.type_predicate {
            Some(t) => {
                let t = t.scope_refs(&scope.lexical);
                crate::warn_later_block_names(ctx, flags, &t)?;
                t.refuse_quantified_fns(&ctx.env)?;
                super::lambda::body_annotation(ctx, &t);
                (true, t.lookup_ref(&ctx.env)?)
            }
            None => {
                let typ = spec
                    .structure_predicate
                    .infer_type_predicate(&ctx.env, &scope.lexical)?;
                (false, typ)
            }
        };
        // an explicit predicate on an abstract type is a nominal tag
        // test that compares the parameters a Graphix-minted value
        // carries; a Rust-backed value carries its id alone, and the
        // select refuses a test over two of its instantiations
        match &type_predicate {
            Type::Fn(_) => bail!("can't match on Fn type"),
            t if explicit && t.holds_fn(&ctx.env) => {
                bail!(
                    "can't match on a type holding a function: a test tells a function from a value, not one signature from another"
                )
            }
            Type::App(..) | Type::Hole => bail!("can't match on a type constructor"),
            Type::Bound(_) => {
                bail!("can't match on a constraint")
            }
            Type::Bottom
            | Type::Abstract { .. }
            | Type::Any
            | Type::Primitive(_)
            | Type::Set(_)
            | Type::TVar(_)
            | Type::Error(_)
            | Type::Array(_)
            | Type::List(_)
            | Type::Map { .. }
            | Type::ByRef(..)
            | Type::Tuple(_)
            | Type::Variant(_, _, _)
            | Type::Struct(_)
            | Type::Ref(_) => (),
        }
        let sp = &spec.structure_predicate;
        if explicit {
            let ids = StructurePattern::abstracts_at(&ctx.env, &scope.lexical);
            if let Some(tv) = sp.meets_rigid(&ctx.env, &ids, &type_predicate)? {
                let tp = &type_predicate;
                return Err(rigid_refusal(&ctx.env, &ids, sp, tp, &tv, Holder::Arm));
            }
        }
        let cx = PatCx { scope, pos, ori: &ori, inferred: !explicit };
        let structure_predicate = StructPatternNode::compile_with(
            ctx,
            cx,
            &type_predicate,
            &spec.structure_predicate,
        )?;
        let mut retype = |id| ctx.env.retype(id, Type::empty_tvar());
        if explicit {
            structure_predicate.ids(&mut retype);
        } else {
            structure_predicate.fresh_ids(&mut retype);
        }
        let guard = spec
            .guard
            .as_ref()
            .map(|g| compiler::compile(ctx, flags, g.clone(), &scope, top_id))
            .transpose()?
            .map(Held::new);
        Ok(Self::with(explicit, type_predicate, structure_predicate, guard))
    }

    /// Deliver the scrutinee's destructured leaves to this arm's binds,
    /// carrying the scrutinee's production tag: a stale scrutinee binds
    /// stale leaves, never fired ones.
    pub(super) fn bind_event(
        &self,
        ctx: &mut ExecCtx<'_, R>,
        v: &Value,
        tag: crate::Tag,
    ) {
        self.structure_predicate
            .bind(v, &mut |id, v| ctx.deliver(id, TagValue::tagged(v, tag), Notify::Walk))
    }

    /// [`Self::bind_event`] for a guard's tick, returning the store
    /// entries it replaced for [`Self::retract`].
    pub(super) fn bind_tentative(
        &self,
        ctx: &mut ExecCtx<'_, R>,
        v: &Value,
        tag: crate::Tag,
    ) -> SmallVec<[(BindId, Option<(TagValue, u64)>); 4]> {
        let mut saved = SmallVec::new();
        self.structure_predicate
            .ids(&mut |id| saved.push((id, ctx.rt.store_get(&id).cloned())));
        self.bind_event(ctx, v, tag);
        saved
    }

    /// Undo a [`Self::bind_tentative`]: the overlay and the store as
    /// they were, so an arm not taken leaves no value behind.
    pub(super) fn retract(
        &self,
        ctx: &mut ExecCtx<'_, R>,
        saved: SmallVec<[(BindId, Option<(TagValue, u64)>); 4]>,
    ) {
        for (id, prev) in saved {
            ctx.event.variables.remove(&id);
            ctx.rt.store_put(id, prev);
        }
    }

    /// Update the guard, which the chain reaches, and return its
    /// production tag (`None` = no guard). The caller reads channel
    /// bottomness off `guard.tag`.
    pub(super) fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> Option<Tag> {
        match &mut self.guard {
            None => None,
            Some(g) => Some(g.update(ctx)),
        }
    }

    /// The chain no longer reaches the guard: it sleeps.
    pub(super) fn sleep_guard(&mut self, ctx: &mut ExecCtx<'_, R>) {
        if let Some(g) = &mut self.guard {
            g.sleep(ctx)
        }
    }

    /// The O(1) shallow discriminator for this arm's predicate
    /// against the select's scrutinee type ([`Type::shallow_discriminant`]);
    /// `None` = run the full `is_a` walk. Read once every tvar in the
    /// predicate is settled, at the select's first consult.
    pub(super) fn shallow_discriminant(
        &self,
        env: &Env,
        scrutinee: &ShallowScrutinee,
    ) -> Option<Type> {
        let shallow = scrutinee.discriminant(env, &self.type_predicate).filter(|_| {
            !self.explicit_type_predicate || self.covers_what_it_meets(env, scrutinee)
        });
        if crate::dbgenv::gxdbg_shallow() {
            match &shallow {
                Some(t) => eprintln!("SHALLOW {} => {t}", self.type_predicate),
                None => eprintln!("SHALLOW {} => deep", self.type_predicate),
            }
        }
        shallow
    }

    /// Whether a written predicate holds the whole of every member of
    /// `scrutinee` it could match, so telling the members apart decides
    /// it: `Array<i64> as a` over `Array<[i64, string]>` tests elements.
    fn covers_what_it_meets(&self, env: &Env, scrutinee: &ShallowScrutinee) -> bool {
        scrutinee.members(env).is_some_and(|members| {
            self.type_predicate.holds_what_it_meets(env, members, Type::could_match)
        })
    }

    /// Whether the arm's type and structure predicates admit `v`, given
    /// the arm's shallow discriminator. The checker narrows the arm's
    /// binds by exactly this, so a value that fails it is never
    /// delivered to them.
    pub(super) fn shape_matches(
        &self,
        env: &Env,
        shallow: Option<&Type>,
        v: &Value,
    ) -> bool {
        // the type predicate is checked whether written or inferred: a
        // tuple and an array are the same `Value::Array` at runtime. An
        // inferred predicate is checked permissively (an abstract's
        // hidden rep cannot be verified); an explicit one strictly.
        let t = shallow.unwrap_or(&self.type_predicate);
        let typed = if self.explicit_type_predicate {
            t.is_a(env, v)
        } else {
            t.is_a_with(env, IsAFlags::MatchAbstract.into(), v)
        };
        typed && self.structure_predicate.is_match(v)
    }

    /// The arm's verdict over a value whose shape test gave `shaped`
    /// ([`Self::shape_matches`]).
    pub(super) fn arm_match(&self, shaped: bool) -> ArmMatch {
        if !shaped {
            return ArmMatch::NoStruct;
        }
        match &self.guard {
            None => ArmMatch::Matched,
            Some(g) => {
                if g.tag.is_bottom() {
                    return ArmMatch::GuardBottom;
                }
                match g.value.as_ref().and_then(|v| v.clone().get_as::<bool>()) {
                    Some(true) => ArmMatch::Matched,
                    // a non-bool guard is a type error upstream
                    Some(false) | None => ArmMatch::GuardFalse,
                }
            }
        }
    }

    pub(super) fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        if let Some(n) = &mut self.guard {
            n.node.delete(ctx)
        }
        self.structure_predicate.delete(ctx)
    }
}

/// The elements a tuple or array slice pattern reads from `v`.
fn slice_elems(kind: SliceKind, v: &Value) -> Option<&[Value]> {
    match kind {
        SliceKind::Tuple => rep::tuple_elems(v),
        _ => rep::array_elems(v).map(|a| &a[..]),
    }
}

fn boxed_len(items: &[StructPatternNode]) -> usize {
    varint_len(items.len() as u64) + items.iter().map(|p| p.encoded_len()).sum::<usize>()
}

fn boxed_encode(
    items: &[StructPatternNode],
    buf: &mut impl bytes::BufMut,
) -> Result<(), PackError> {
    encode_varint(items.len() as u64, buf);
    for p in items {
        p.encode(buf)?;
    }
    Ok(())
}

/// A decoded count, capped by the bytes left: every element takes at
/// least one, so a corrupt count fails the decode instead of sizing an
/// allocation.
fn boxed_decode(
    buf: &mut impl bytes::Buf,
) -> Result<Box<[StructPatternNode]>, PackError> {
    let n = crate::image::count_decode(buf)?;
    (0..n).map(|_| StructPatternNode::decode(buf)).collect()
}

/// The compiled pattern is data: bind ids, literals and shapes.
impl Pack for StructPatternNode {
    fn encoded_len(&self) -> usize {
        crate::stack::ensure_sufficient(|| self.encoded_len_inner())
    }

    fn encode(&self, buf: &mut impl bytes::BufMut) -> Result<(), PackError> {
        crate::stack::ensure_sufficient(|| self.encode_inner(buf))
    }

    fn decode(buf: &mut impl bytes::Buf) -> Result<Self, PackError> {
        crate::stack::ensure_sufficient(|| Self::decode_inner(buf))
    }
}

impl StructPatternNode {
    fn encoded_len_inner(&self) -> usize {
        1 + match self {
            Self::Ignore => 0,
            Self::Literal(v) => v.encoded_len(),
            Self::Bind(id) => id.encoded_len(),
            Self::Slice { kind, all, binds } => {
                kind.encoded_len() + all.encoded_len() + boxed_len(binds)
            }
            Self::SlicePrefix { list, all, prefix, tail } => {
                list.encoded_len()
                    + all.encoded_len()
                    + boxed_len(prefix)
                    + tail.encoded_len()
            }
            Self::SliceSuffix { all, head, suffix } => {
                all.encoded_len() + head.encoded_len() + boxed_len(suffix)
            }
            Self::Struct { all, width, binds } => {
                all.encoded_len()
                    + width.encoded_len()
                    + varint_len(binds.len() as u64)
                    + binds
                        .iter()
                        .map(|(n, i, p)| {
                            n.encoded_len() + i.encoded_len() + p.encoded_len()
                        })
                        .sum::<usize>()
            }
            Self::Variant { tag, all, binds } => {
                tag.encoded_len() + all.encoded_len() + boxed_len(binds)
            }
            Self::Abstract { id, all, rep, bind } => {
                id.encoded_len()
                    + all.encoded_len()
                    + rep.encoded_len()
                    + bind.encoded_len()
            }
            Self::Or { alts } => boxed_len(alts),
        }
    }

    fn encode_inner(&self, buf: &mut impl bytes::BufMut) -> Result<(), PackError> {
        match self {
            Self::Ignore => buf.put_u8(0),
            Self::Literal(v) => {
                buf.put_u8(1);
                v.encode(buf)?
            }
            Self::Bind(id) => {
                buf.put_u8(2);
                id.encode(buf)?
            }
            Self::Slice { kind, all, binds } => {
                buf.put_u8(3);
                kind.encode(buf)?;
                all.encode(buf)?;
                boxed_encode(binds, buf)?
            }
            Self::SlicePrefix { list, all, prefix, tail } => {
                buf.put_u8(4);
                list.encode(buf)?;
                all.encode(buf)?;
                boxed_encode(prefix, buf)?;
                tail.encode(buf)?
            }
            Self::SliceSuffix { all, head, suffix } => {
                buf.put_u8(5);
                all.encode(buf)?;
                head.encode(buf)?;
                boxed_encode(suffix, buf)?
            }
            Self::Struct { all, width, binds } => {
                buf.put_u8(6);
                all.encode(buf)?;
                width.encode(buf)?;
                encode_varint(binds.len() as u64, buf);
                for (n, i, p) in binds.iter() {
                    n.encode(buf)?;
                    i.encode(buf)?;
                    p.encode(buf)?;
                }
            }
            Self::Variant { tag, all, binds } => {
                buf.put_u8(7);
                tag.encode(buf)?;
                all.encode(buf)?;
                boxed_encode(binds, buf)?
            }
            Self::Abstract { id, all, rep, bind } => {
                buf.put_u8(8);
                id.encode(buf)?;
                all.encode(buf)?;
                rep.encode(buf)?;
                bind.encode(buf)?
            }
            Self::Or { alts } => {
                buf.put_u8(9);
                boxed_encode(alts, buf)?
            }
        }
        Ok(())
    }

    fn decode_inner(buf: &mut impl bytes::Buf) -> Result<Self, PackError> {
        if !buf.has_remaining() {
            return Err(PackError::BufferShort);
        }
        Ok(match buf.get_u8() {
            0 => Self::Ignore,
            1 => Self::Literal(Pack::decode(buf)?),
            2 => Self::Bind(Pack::decode(buf)?),
            3 => Self::Slice {
                kind: Pack::decode(buf)?,
                all: Pack::decode(buf)?,
                binds: boxed_decode(buf)?,
            },
            4 => Self::SlicePrefix {
                list: Pack::decode(buf)?,
                all: Pack::decode(buf)?,
                prefix: boxed_decode(buf)?,
                tail: Pack::decode(buf)?,
            },
            5 => Self::SliceSuffix {
                all: Pack::decode(buf)?,
                head: Pack::decode(buf)?,
                suffix: boxed_decode(buf)?,
            },
            6 => {
                let all = Pack::decode(buf)?;
                let width = Pack::decode(buf)?;
                let n = crate::image::count_decode(buf)?;
                let binds = (0..n)
                    .map(|_| {
                        Ok((Pack::decode(buf)?, Pack::decode(buf)?, Pack::decode(buf)?))
                    })
                    .collect::<Result<_, PackError>>()?;
                Self::Struct { all, width, binds }
            }
            7 => Self::Variant {
                tag: Pack::decode(buf)?,
                all: Pack::decode(buf)?,
                binds: boxed_decode(buf)?,
            },
            8 => Self::Abstract {
                id: Pack::decode(buf)?,
                all: Pack::decode(buf)?,
                rep: Pack::decode(buf)?,
                bind: Box::new(Pack::decode(buf)?),
            },
            9 => Self::Or { alts: boxed_decode(buf)? },
            _ => return Err(PackError::UnknownTag),
        })
    }
}
