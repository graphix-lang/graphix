use crate::{
    AbstractTypeRegistry, CAST_ERR_TAG, PrintFlag,
    abstract_value::{self, CastHook},
    env::Env,
    errf,
    expr::{ModPath, union_members},
    format_with_flags, list, rep,
    stack::ensure_sufficient,
    typ::{
        AbstractId, Bound, CoreTrait, TVar, Type, TypeRef, params_size,
        setops::union_identical,
        tval::{NakedPrefix, PlainScalar, has_form, member_of},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use enumflags2::{BitFlags, bitflags, make_bitflags};
use immutable_chunkmap::map::Map;
use netidx_value::{Typ, ValArray, Value};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{borrow::Cow, convert::Infallible, fmt, mem, ops::ControlFlow, sync::OnceLock};
use triomphe::Arc;

#[derive(Debug, Clone, Copy)]
#[bitflags]
#[repr(u8)]
pub enum IsAFlags {
    /// A `Type::Abstract` test also accepts a Rust-backed abstract
    /// value whose wrapper UUID is not the type's path-derived one
    /// (packages registering ad-hoc UUIDs), and a Graphix-minted box
    /// answers by its tag alone, its payload pattern testing the rest.
    /// An explicit `T as t` is strict.
    MatchAbstract,
    /// The type-blind leaves (`Any`, `⊥`, an unbound tvar) match
    /// nothing instead of everything: "does this type describe v"
    /// rather than "could v inhabit it".
    Strict,
}

/// A failed cast. Formatting is deferred to the report: a union tries
/// its members and discards every failure but the last.
#[derive(Debug, Clone)]
struct CastFail {
    why: Cow<'static, str>,
    to: Type,
    v: Value,
}

impl fmt::Display for CastFail {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "can't cast {} to {}: {}", NakedPrefix(&self.v), self.to, self.why)
    }
}

/// A cast level's result: `None` when the value already has the type.
type Cast = std::result::Result<Option<Value>, CastFail>;

/// The static source type of `v` at a cast position, with names, cells
/// and applications looked through; `None` when unknown. Of a union,
/// the member whose outer form `v` has, or the kind of collection all
/// such members are; `None` when they differ.
fn src_head(cx: &mut CastCx, src: Option<&Type>, env: &Env, v: &Value) -> Option<Type> {
    let mut cur = src?.clone();
    loop {
        cur = match &cur {
            Type::TVar(_) => cur.deref_cloned()?,
            Type::App(c, a) => Type::app_filled(c, a)?,
            Type::Ref(tr) => cx.expand(env, &cur, tr)?,
            Type::Set(ms) => {
                let mut heads: SmallVec<[Type; 4]> = SmallVec::new();
                for m in ms.iter() {
                    let h = src_head(cx, Some(m), env, v)?;
                    if has_form(&h, v) {
                        heads.push(h)
                    }
                }
                return match &heads[..] {
                    [one] => Some(one.clone()),
                    [_, ..] if heads.iter().all(|h| matches!(h, Type::Array(_))) => {
                        Some(Type::Array(Arc::new(Type::Any)))
                    }
                    [_, ..] if heads.iter().all(|h| matches!(h, Type::List(_))) => {
                        Some(Type::List(Arc::new(Type::Any)))
                    }
                    _ => None,
                };
            }
            Type::Any | Type::Bottom | Type::Hole => return None,
            _ => return Some(cur),
        };
    }
}

/// `f` over the values of `elts`, rebuilt only if some value changed.
fn cast_elts<T>(
    elts: &[T],
    get: impl Fn(&T) -> &Value,
    mut f: impl FnMut(usize, &Value) -> Cast,
) -> std::result::Result<Option<LPooled<Vec<Value>>>, CastFail> {
    let mut out: Option<LPooled<Vec<Value>>> = None;
    for (i, e) in elts.iter().enumerate() {
        let v = get(e);
        match f(i, v)? {
            Some(c) => out
                .get_or_insert_with(|| elts[..i].iter().map(|e| get(e).clone()).collect())
                .push(c),
            None => {
                if let Some(o) = out.as_mut() {
                    o.push(v.clone())
                }
            }
        }
    }
    Ok(out)
}

/// A type reference as a path walk keys it: its definition and the
/// parameters it is applied to, so `Id<i64>` and `Id<&i64>` are two.
type RefKey = (usize, Arc<[Type]>);

/// How many values deep a cast reads: data nested deeper is refused, so
/// foreign data bounds neither the walk's stack nor its memory.
pub const MAX_DATA_DEPTH: usize = 1024;

/// The data a cast reads.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CastInput {
    Data,
    /// JSON or TOML: an abstract value is an object of one entry keyed by
    /// its type's name, except under `Transparent`; with `exact_floats`
    /// a decimal stands for a float read exactly.
    Text {
        exact_floats: bool,
    },
    Marked,
}

/// A cast in progress: the refs expanded on the way to each value.
struct CastCx {
    hist: LPooled<AHashSet<(RefKey, usize)>>,
    /// The params sizes on the path of each (definition, value address).
    sizes: LPooled<AHashMap<(usize, usize), SmallVec<[usize; 2]>>>,
    /// Each reference's expansion, by definition and params allocation,
    /// with the params held so the allocation stays theirs.
    expanded: LPooled<AHashMap<(usize, usize), (Arc<[Type]>, Type)>>,
    /// Each union's reading of a value inside another union's, by the
    /// union's members, the value's address and `holds`, with the union
    /// held so the members stay its: a union tries several members over
    /// one value, so without it a failing read is exponential in the
    /// data's depth.
    read: LPooled<AHashMap<(usize, usize, Holds), (Type, Cast, NumReads)>>,
    /// Whether the walk is inside a union's reading.
    in_union: bool,
    /// How many values on the path are built by the walk, not held by
    /// the cast's value: their addresses are reused, so no reading of a
    /// value under one is remembered.
    built: usize,
    /// How many times the walk refused a type that recursed without
    /// consuming the value, which depends on the path: no reading that
    /// met one is remembered.
    cuts: usize,
    /// The address of the value being cast and how many values deep it is.
    at: usize,
    depth: usize,
    /// How much of a value a conversion may lose: a union tries its
    /// members at each in turn, the least first.
    holds: Holds,
    /// How the reading so far took the numbers it met.
    nums: NumReads,
    /// The last text the walk parsed and the scalar it parses as, the
    /// text held so its address names it.
    text: Option<(ArcStr, Option<Value>)>,
    input: CastInput,
    check_keys: bool,
}

/// How much of a value a reading keeps, the most first: all of it, all of
/// it rounded to a real member's precision (an f64 into an f32, a decimal
/// or a long integer into an f64), or less (a fraction an integer drops).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Holds {
    Exactly,
    Rounded,
    Partly,
}

/// Which side of the line between integers and reals a number is on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NumKind {
    Integer,
    Real,
}

impl NumKind {
    fn of(t: Typ) -> Option<NumKind> {
        match (t.is_integer(), t.is_real()) {
            (true, _) => Some(NumKind::Integer),
            (_, true) => Some(NumKind::Real),
            _ => None,
        }
    }
}

/// How a reading took the numbers it met, values or text that reads as
/// one: how many it read across the line between integers and reals, and
/// how many as no number.
#[derive(Debug, Clone, Copy, Default)]
struct NumReads {
    crossed: usize,
    as_other: usize,
}

impl NumReads {
    /// Count a value of the kind `kind` says read as `c`.
    fn read(&mut self, c: &Value, kind: impl FnOnce() -> Option<NumKind>) {
        let to = NumKind::of(Typ::get(c));
        match (kind(), to) {
            (None, _) => (),
            (Some(k), Some(to)) if k == to => (),
            (Some(_), Some(_)) => self.crossed += 1,
            (Some(_), None) => self.as_other += 1,
        }
    }

    fn add(&mut self, d: NumReads) {
        self.crossed += d.crossed;
        self.as_other += d.as_other;
    }

    fn since(self, base: NumReads) -> NumReads {
        NumReads {
            crossed: self.crossed - base.crossed,
            as_other: self.as_other - base.as_other,
        }
    }

    /// Whether every number kept its kind.
    fn kept(self) -> bool {
        self.crossed == 0 && self.as_other == 0
    }
}

// XCR claude for claude: [design/low] What the one exactness rule (holds-exactly-class-01)
// leaves open, each a semantics question; JsonData/TomlData refuse the unions these make
// read back as another member (textdata.rs follows the pick). (1) An exact integer comes
// before a rounding real: cast<[i64, f32]>(1234567900.0) is i64, so an f32 1234567936's
// text "1234568000.0" reads back as i64 in [i64, f32] while the value stays f32. Options:
// keep exactness before kind, or a reading keeping every number its kind before one that
// crosses, across the Holds rounds (JsonData could then admit [f32, i64]). (2) A scale-0
// decimal prints "3", so cast<[i64, decimal]>(3.0) is decimal 3 whose text reads back as
// i64 3, and decimal 3 into [i64, f64] is f64 3.0 while its text is i64 3 (main: i64 for
// both). Options: a scale-0 decimal prints with a point, a decimal made from a whole float
// keeps scale 1, or leave it. (3) The rounds come before the class: an exact reading
// outside the value's class beats a rounded one inside it, so cast<[f32, string]>(0.1) is
// "0.1" and [(string, f32), (i64, f64)] reads ("1", f32:0.1) as (1, 0.1). Options: keep
// it, or the members keeping the class first across the rounds (JsonData could then admit
// both). (4) Readings outside the class go by the union's order, which follows the
// members' canonical order and so their typedef names: [(i64, duration), (u8, string)]
// reads (1, 3.5s) as (u8:1, "3.5") where [(u8, duration), (i64, string)] keeps (u8:1,
// 3.5s); cast<[duration, decimal]>("3.0") is a duration while the value 3.0 is decimal,
// and [DU, D] under typedef names reads that text as decimal; duration 3s into [i64, f64]
// is i64 3, its text "3.0" f64. Options: order numeric text by the class of the value it
// parses as (as values are), or leave it. (5) Where every reading crosses, the first
// member in canonical order wins however many leaves it crosses:
// cast<[(f32, f32, i64), (i64, i64, f32)]>([1.0, 2.0, 3.0]) is (1, 2, f32:3.0). Options:
// keep it, or the fewest crossings. (6) An integer stays an integer in a union whose
// members are named (type B = u8; cast<[f64, B]>(2) is u8 2), a lane judgment (13b0e1a9a)
// not yet ruled. Probes: ~/tmp/cr29/scratch/review7/B/r1/{order2,tdef}.gx,
// ~/tmp/cr29/scratch/laneB/r3/{strhole,strhole2,f32str}.gx (holds-exactly-open-01)
// 2026-10-30 claude: One more for this list, found in round 3: a bool converts to a number
// (1 or 0), but holds_exactly never holds it (no number converts back to a bool), so
// holds_rounded makes a bool's reading into a real Rounded and into an integer Partly:
// cast<[f32, u8]>(true) is f32:1.0 on this lane where main gives u8:1, and a bool reads
// into an f64 member in the same round as an f64 rounding into an f32 member. Options: a
// bool held exactly by a number of 1 or 0 (cast<[f32, u8]>(true) is u8:1 again, and
// textdata's leaves read Bool exactly), or keep it. textdata follows the cast as it is
// (holds-exactly-class-01's round 3 note). Probe: ~/tmp/cr29/scratch/laneB/r4/hole3.gx
// 2026-10-31 claude: Eric ruled (round 5 D2, 10-31): 1 keep exactness before kind; 2 a
// decimal made from a whole float keeps scale 1; 3 the members that keep the value's class
// come first, across the rounds; 4 numeric text is ordered by the class of the value it
// parses as; 5 keep the canonical-order tie-break; 6 confirmed; 7 a bool is held exactly
// by a number of 1 or 0 (cast<[f32, u8]>(true) is u8 1). JsonData no longer follows the
// cast's pick: D6 refuses untagged unions in JSON/TOML.
// 2026-10-31 claude: Built Eric's round 5 D2 rulings: (3) a union tries the members of the
// value's class at every degree of exactness before any other member (Type::tier,
// cast_member's groups; a primitive set alike), so cast<[f32, string]>(0.1) is f32 0.1,
// and cast<[i64, string]>(3.5) and cast<[i64, duration]>(3.5) are i64 3 (main: "3.5" and
// 3.5s, read as the class-first rule's consequence); (4) text then tries the members of
// the class of the scalar it parses as (parsed_scalar), so cast<[duration,
// decimal]>("3.0") is decimal 3.0, cast<[i64, duration]>("3.5") and cast<[i64,
// datetime]>("3.0") i64 3; (7) a bool is held exactly by the number 1 or 0
// (holds_exactly), so cast<[f32, u8]>(true) is u8 1, cast<[f64, i64]>(false) i64 0, and by
// the canonical-order tie-break cast<[i64, string]>(true) is i64 1 and cast<[f32,
// string]>(true) f32 1.0 (main and the lane base: "true"); (2) a decimal made of a whole
// float keeps scale 1 (float_decimal), so cast<decimal>(3.0) prints 3.0 and its text reads
// back as the decimal. 1, 5 and 6 unchanged. Pins: lang::types::{cast_union_order,
// cast_float_value_and_text_read_alike, cast_whole_float_without_a_real_member}; on the
// lane base binary every item reads otherwise (~/ptmp/cr29/scratch/r6-B/union_order.gx,
// whole.gx).
// 2026-11-01 claude: Review of round 6 (B-r1-2): ruling 3 held only where the union was a
// flat primitive set; a member beside the set (a variant, an array, a named type) made the
// set's own class walk run inside each of the union's rounds, so cast<[f32, string,
// `A]>(0.1) was "0.1", cast<[i64, string, F]>(3.5) with type F = f32 "3.5" and cast<[i64,
// duration, `A]>(3.5) 3.5s. cast_member now reads a union by its members past every union
// and name in it, a primitive set split into its parts by class (members_by_class), so
// each part is tried in its class's group: all three are f32 0.1, f32 3.5 and i64 3, as
// the flat spellings read. Pin: lang::types::cast_union_order_past_nested_members (fails
// in every mode with members_by_class not looking into members). The earlier note's 'a
// primitive set alike' held for a flat set only.
// 2026-11-01 claude: Round 6 fix 3 (review B-r3-2, medium cost): the
// class-of-the-parsed-text rule (4) parsed every text cast into a union as a literal
// before any member, and again in holds_exactly, holds_rounded and the number kind
// (json::read of 300k "A" into Array<[`A, null]> 147.7 G instructions, main 4.7 G). A text
// now parses once per value (CastCx::scalar) and only where a member may read a number,
// bool, datetime or duration (takes_scalar); no reading changes. Pin:
// typ::cast::tests::a_union_parses_its_text_once_and_only_for_a_scalar_member (commit
// 4915c4c73).
/// A union's reading of a value among those its members offer, in its
/// order: the first, except that in a round that drops no part one reading
/// a number across the line between integers and reals gives way to a
/// later one keeping every number its kind.
struct Choice {
    whole: bool,
    got: Option<(Value, NumReads)>,
}

impl Choice {
    fn new(cx: &CastCx) -> Self {
        Choice { whole: cx.holds < Holds::Partly, got: None }
    }

    fn offer(&mut self, c: Value, d: NumReads) {
        let replaces = match &self.got {
            None => true,
            Some((_, g)) => self.whole && g.crossed > 0 && d.kept(),
        };
        if replaces {
            self.got = Some((c, d))
        }
    }

    /// Whether no later reading would replace the one held.
    fn settled(&self) -> bool {
        self.got.as_ref().is_some_and(|(_, g)| !self.whole || g.crossed == 0)
    }

    /// The reading chosen, its count added to `cx`'s.
    fn take(self, cx: &mut CastCx) -> Option<Value> {
        self.got.map(|(c, d)| {
            cx.nums.add(d);
            c
        })
    }
}

impl CastCx {
    fn new(input: CastInput, check_keys: bool) -> Self {
        CastCx {
            hist: LPooled::take(),
            sizes: LPooled::take(),
            expanded: LPooled::take(),
            read: LPooled::take(),
            in_union: false,
            built: 0,
            cuts: 0,
            at: 0,
            depth: 0,
            holds: Holds::Partly,
            nums: NumReads::default(),
            text: None,
            input,
            check_keys,
        }
    }

    /// The scalar the text `v` parses as ([`parsed_scalar`]), parsed once
    /// however many members of a union read it; `None` for a value that
    /// is no text.
    fn scalar(&mut self, v: &Value) -> Option<Value> {
        let Value::String(s) = v else { return None };
        if let Some((t, p)) = &self.text
            && ArcStr::ptr_eq(t, s)
        {
            return p.clone();
        }
        let p = parsed_scalar(s);
        self.text = Some((s.clone(), p.clone()));
        p
    }

    /// `t`'s reading of `v` and how it took the numbers it met, the count
    /// left as it was: a union adds only its chosen reading's.
    fn trial(
        &mut self,
        env: &Env,
        t: &Type,
        src: Option<&Type>,
        v: &Value,
    ) -> (Cast, NumReads) {
        let base = self.nums;
        let r = t.cast_int(env, self, src, v);
        let d = self.nums.since(base);
        self.nums = base;
        (r, d)
    }

    /// The expansion of the reference `t`, made once a cast.
    fn expand(&mut self, env: &Env, t: &Type, tr: &TypeRef) -> Option<Type> {
        let Some(def) = tr.def_key() else { return t.lookup_ref(env).ok() };
        let ek = (def, tr.params.as_ptr().addr());
        if let Some((_, t)) = self.expanded.get(&ek) {
            return Some(t.clone());
        }
        let x = t.lookup_ref(env).ok()?;
        self.expanded.insert(ek, (tr.params.clone(), x.clone()));
        Some(x)
    }

    /// `f` over a value the walk built.
    fn over_built<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.built += 1;
        let r = f(self);
        self.built -= 1;
        r
    }

    /// `v` as the f64 a float position reads, when it is a decimal that
    /// stands for a float.
    fn float_of(&self, v: &Value) -> Option<Value> {
        match v {
            Value::Decimal(_)
                if self.input == (CastInput::Text { exact_floats: true }) =>
            {
                convert(v.clone(), Typ::F64)
            }
            _ => None,
        }
    }
}

/// An `is_a` walk's state: the references on its path with the value
/// each met, and each reference's expansion, made once a walk. Within a
/// walk a reference is its definition and its params' allocation, and
/// the expansions held keep those allocations alive.
#[derive(Default)]
struct IsAHist {
    path: AHashSet<((usize, usize), usize)>,
    expanded: AHashMap<(usize, usize), (Arc<[Type]>, Option<Type>)>,
}

fn ref_key(tr: &TypeRef) -> Option<RefKey> {
    tr.def_key().map(|k| (k, tr.params.clone()))
}

/// A type reference as a walk deciding a property that composes over a
/// type's parts keys it: its definition and whether each parameter has
/// the property. Exact, and finite where the parameters grow as the
/// definition recurses (`type N<'a> = [null, ('a, N<Array<'a>>)]`).
type ShapeKey = (usize, SmallVec<[bool; 4]>);

/// What a walk knows of one application: it is being decided, at its
/// depth in the walk, what was decided, or a verdict that holds if the
/// application being decided at depth `read` turns out `cycle` too.
enum Verdict<T> {
    Deciding(usize),
    Decided(T),
    Provisional { read: usize, t: T },
}

struct Verdicts<T> {
    known: LPooled<AHashMap<ShapeKey, Verdict<T>>>,
    /// The provisional verdicts, each with the depth it was decided at.
    pending: LPooled<Vec<(ShapeKey, usize)>>,
    /// The number of applications being decided.
    depth: usize,
    /// The shallowest application being decided that the decision in
    /// progress read.
    read: usize,
    /// The shared nodes a search walked and found nothing under, so a
    /// type that shares them (a DAG) is searched once; only outside any
    /// application being decided, whose provisional verdicts a node's may
    /// rest on.
    walked: LPooled<AHashSet<usize>>,
    /// The types the walk built outside any application being decided,
    /// alive until it ends: a node `walked` holds keeps its address.
    built: LPooled<Vec<Type>>,
}

impl<T> Verdicts<T> {
    fn new() -> Self {
        Self {
            known: LPooled::take(),
            pending: LPooled::take(),
            depth: 0,
            read: usize::MAX,
            walked: LPooled::take(),
            built: LPooled::take(),
        }
    }

    /// `f` over `t`, a type built for this walk.
    fn over_built<R>(&mut self, t: Type, f: impl FnOnce(&mut Self, &Type) -> R) -> R {
        let r = f(self, &t);
        if self.depth == 0 {
            self.built.push(t)
        }
        r
    }

    /// Settle the provisional verdicts that read the application decided
    /// at `depth` as `t`: refuted, every one decided inside it may have
    /// assumed it and is dropped; confirmed outright, those that read it
    /// first are decided; confirmed provisionally, they read what it read.
    fn settle(&mut self, depth: usize, refuted: bool, read: usize) {
        let mut i = 0;
        while i < self.pending.len() {
            let (k, at) = &self.pending[i];
            let Some(Verdict::Provisional { read: r, .. }) = self.known.get_mut(k) else {
                self.pending.swap_remove(i);
                continue;
            };
            if refuted && *at > depth {
                self.known.remove(k);
                self.pending.swap_remove(i);
            } else if *r < depth || refuted {
                i += 1;
            } else if read < depth {
                *r = read;
                i += 1;
            } else {
                let (k, _) = self.pending.swap_remove(i);
                if let Some(v) = self.known.get_mut(&k)
                    && let Verdict::Provisional { t, .. } =
                        mem::replace(v, Verdict::Deciding(0))
                {
                    *v = Verdict::Decided(t)
                }
            }
        }
    }
}

/// `decide` the application `k` once per walk. A repeat while it is being
/// decided answers `cycle`, an assumption that holds coinductively for
/// `k` itself; a decision that read the assumption of an application
/// further out answers `cycle` provisionally, since that application may
/// yet fail it, and is settled when that application is.
fn decide_once<T: Clone + PartialEq>(
    seen: &mut Verdicts<T>,
    k: ShapeKey,
    cycle: T,
    decide: impl FnOnce(&mut Verdicts<T>) -> T,
) -> T {
    match seen.known.get(&k) {
        Some(Verdict::Deciding(depth)) => {
            seen.read = seen.read.min(*depth);
            cycle
        }
        Some(Verdict::Provisional { read, t }) => {
            seen.read = seen.read.min(*read);
            t.clone()
        }
        Some(Verdict::Decided(t)) => t.clone(),
        None => {
            let depth = seen.depth;
            seen.depth += 1;
            seen.known.insert(k.clone(), Verdict::Deciding(depth));
            let outer = mem::replace(&mut seen.read, usize::MAX);
            let t = decide(seen);
            seen.depth -= 1;
            let refuted = t != cycle;
            seen.settle(depth, refuted, seen.read);
            if seen.read < depth && !refuted {
                seen.known.insert(
                    k.clone(),
                    Verdict::Provisional { read: seen.read, t: t.clone() },
                );
                seen.pending.push((k, depth));
            } else {
                seen.known.insert(k, Verdict::Decided(t.clone()));
            }
            seen.read = seen.read.min(outer);
            t
        }
    }
}

impl Type {
    fn check_cast_int(
        &self,
        env: &Env,
        seen: &mut Verdicts<Option<ArcStr>>,
    ) -> Result<()> {
        ensure_sufficient(|| self.check_cast_inner(env, seen))
    }

    fn check_cast_inner(
        &self,
        env: &Env,
        seen: &mut Verdicts<Option<ArcStr>>,
    ) -> Result<()> {
        match self {
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => t.check_cast_int(env, seen),
                None => bail!("can't cast a value to a type constructor"),
            },
            Type::Hole => bail!("can't cast a value to a type constructor"),
            Type::Primitive(_) | Type::Any => Ok(()),
            Type::Fn(_) => bail!("can't cast a value to a function"),
            Type::Bottom => bail!("can't cast a value to bottom"),
            Type::Abstract { params, .. } => {
                params.iter().try_for_each(|p| p.check_cast_int(env, seen))
            }
            Type::TVar(_) => match self.deref_cloned() {
                Some(t) => t.check_cast_int(env, seen),
                None => bail!("can't cast a value to a free type variable"),
            },
            Type::ByRef(..) => bail!("can't cast a reference"),
            Type::Ref(tr) => {
                let t = self.lookup_ref(env)?;
                let shape = tr
                    .params
                    .iter()
                    .map(|p| p.check_cast_int(env, seen).is_ok())
                    .collect();
                let Some(k) = tr.def_key().map(|k| (k, shape)) else {
                    return t.check_cast_int(env, seen);
                };
                let refused = decide_once(seen, k, None, |seen| {
                    t.check_cast_int(env, seen)
                        .err()
                        .map(|e| ArcStr::from(format_compact!("{e:#}").as_str()))
                });
                match refused {
                    None => Ok(()),
                    Some(why) => bail!("{why}"),
                }
            }
            t => {
                let mut r = Ok(());
                t.for_each_child(&mut |c| {
                    if r.is_ok() {
                        r = c.check_cast_int(env, seen)
                    }
                });
                r
            }
        }
    }

    pub fn check_cast(&self, env: &Env) -> Result<()> {
        self.check_cast_int(env, &mut Verdicts::new())
    }

    /// Whether a map type is written in this type, through the
    /// definitions it names (an abstract's hidden one excepted).
    pub fn holds_map(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::Map { .. } => Some(true),
            Type::Abstract { .. } => Some(false),
            _ => None,
        })
    }

    /// Whether a part of this type reads a number exactly: an integer or
    /// a decimal, or an abstract type, whose `Cast` reads what it likes.
    pub fn holds_exact_number(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::Primitive(p) => Some(p.intersects(Typ::integer() | Typ::Decimal)),
            Type::Abstract { .. } => Some(true),
            _ => None,
        })
    }

    /// Whether a value of this type can hold a reference, outside a
    /// function or an abstract type with no `Cast` implementation, which
    /// a cast never takes apart (one with an implementation may read its
    /// payload at its parameters). A cast refuses such a source: a
    /// reference names a cell, it is no data to read.
    #[doc(hidden)]
    pub fn holds_ref(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::ByRef(..) => Some(true),
            Type::Fn(_) => Some(false),
            Type::Abstract { id, .. } if !env.casts(*id) => Some(false),
            _ => None,
        })
    }

    /// Whether a reference this type test holds sits where `reaching`
    /// admits `Any`: there the test would take a reference to anything
    /// for one to its type.
    pub fn ref_over_any(&self, env: &Env, reaching: &Type) -> bool {
        fn go(env: &Env, seen: &mut RepSeen, p: &Type, r: &Type) -> bool {
            ensure_sufficient(|| {
                let (Some(p), Some(r)) = (p.deref_cloned(), r.deref_cloned()) else {
                    return false;
                };
                walk(
                    seen,
                    pair_at(&p, &r),
                    false,
                    || true,
                    |seen| {
                        let expand = |t: &Type| match t {
                            Type::Ref(_) => t.lookup_ref(env).ok(),
                            t => Some(t.clone()),
                        };
                        let (Some(p), Some(r)) = (expand(&p), expand(&r)) else {
                            return false;
                        };
                        match (&p, &r) {
                            (_, Type::Any) => p.holds_ref(env),
                            (Type::Set(ps), _) => ps.iter().any(|p| go(env, seen, p, &r)),
                            (_, Type::Set(rs)) => rs.iter().any(|r| go(env, seen, &p, r)),
                            (Type::Tuple(ps), Type::Tuple(rs))
                                if ps.len() == rs.len() =>
                            {
                                ps.iter().zip(rs.iter()).any(|(p, r)| go(env, seen, p, r))
                            }
                            (Type::Array(p), Type::Array(r))
                            | (Type::List(p), Type::List(r))
                            | (Type::Error(p), Type::Error(r))
                            | (Type::ByRef(_, p), Type::ByRef(_, r)) => {
                                go(env, seen, p, r)
                            }
                            (Type::Struct(ps), Type::Struct(rs)) => {
                                ps.iter().any(|(n, p, _)| {
                                    rs.iter()
                                        .any(|(m, r, _)| n == m && go(env, seen, p, r))
                                })
                            }
                            (Type::Variant(pt, ps, _), Type::Variant(rt, rs, _))
                                if pt == rt && ps.len() == rs.len() =>
                            {
                                ps.iter().zip(rs.iter()).any(|(p, r)| go(env, seen, p, r))
                            }
                            (
                                Type::Map { key: pk, value: pv },
                                Type::Map { key: rk, value: rv },
                            ) => go(env, seen, pk, rk) || go(env, seen, pv, rv),
                            _ => false,
                        }
                    },
                )
            })
        }
        self.holds_ref(env) && go(env, &mut RepSeen::default(), self, reaching)
    }

    /// Whether the type holds `Any` or an open cell anywhere: a part a
    /// value-level predicate cannot be decided of.
    #[doc(hidden)]
    pub fn holds_unknown(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::Any => Some(true),
            Type::TVar(tv) if tv.binding().is_none() => Some(true),
            _ => None,
        })
    }

    /// Whether a value of this type can hold a function outside an
    /// abstract type: a runtime type test tells a function from a value
    /// but not one signature from another.
    #[doc(hidden)]
    pub fn holds_fn(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::Fn(_) => Some(true),
            Type::Abstract { .. } => Some(false),
            _ => None,
        })
    }

    /// Whether a value of this type can hold a reference, a function or
    /// `Any` anywhere: what a builtin given one may reach through.
    #[doc(hidden)]
    pub fn reaches_out(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::ByRef(..) | Type::Fn(_) | Type::Any => Some(true),
            _ => None,
        })
    }

    /// Whether a value of this type can hold an abstract value, whose
    /// core-trait impls (`Eq`, `Ord`, `Display`) a comparison or a print
    /// runs.
    #[doc(hidden)]
    pub fn holds_abstract(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::Abstract { .. } | Type::Any => Some(true),
            _ => None,
        })
    }

    /// Whether a value of this type can hold an abstract value that
    /// crosses no data: a Rust-backed one, or a Graphix one with no `Cast`
    /// implementation. No cast or read reaches one and no writer writes
    /// it.
    pub fn holds_uncastable(&self, env: &Env) -> bool {
        self.uncastable(env).is_some()
    }

    /// Whether `Data ⊇ self` holds as the type stands: no reference, no
    /// function and no abstract type [`Self::holds_uncastable`] refuses;
    /// open cells hold (a bind hands them the conjunct).
    pub(crate) fn data_holds(&self, env: &Env) -> bool {
        !self.holds_beyond_data(env, false)
    }

    /// Whether `Concrete ⊇ self` holds as the type stands: [`Self::data_holds`]
    /// and no ⊥ (a value read from data can't be any of them), judged
    /// through type references; bound cells by their bindings, open cells
    /// admitted.
    pub(crate) fn concrete_holds(&self, env: &Env) -> bool {
        !self.holds_beyond_data(env, true)
    }

    /// Whether some part of this type is no data: a reference, a function,
    /// an abstract type with no cast, or, with `bottom`, ⊥.
    fn holds_beyond_data(&self, env: &Env, bottom: bool) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::ByRef(..) | Type::Fn(_) => Some(true),
            Type::Bottom => bottom.then_some(true),
            Type::Abstract { id, .. } if !env.casts(*id) => Some(true),
            _ => None,
        })
    }

    /// The first abstract type [`Self::holds_uncastable`] finds.
    pub fn uncastable(&self, env: &Env) -> Option<Type> {
        self.uncastable_but(env, &|_| false)
    }

    /// Every abstract type this type names, through its definitions.
    pub(crate) fn abstract_ids(&self, env: &Env, out: &mut Vec<AbstractId>) {
        let out = std::cell::RefCell::new(out);
        self.holds(env, &mut Verdicts::new(), &|t| {
            if let Type::Abstract { id, .. } = t {
                out.borrow_mut().push(*id);
            }
            None
        });
    }

    /// [`Self::uncastable`], passing over the abstract types `skip` says.
    pub(crate) fn uncastable_but(
        &self,
        env: &Env,
        skip: &dyn Fn(AbstractId) -> bool,
    ) -> Option<Type> {
        let found = std::cell::RefCell::new(None);
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::Abstract { id, .. } if !env.casts(*id) && !skip(*id) => {
                *found.borrow_mut() = Some(t.clone());
                Some(true)
            }
            _ => None,
        });
        found.into_inner()
    }

    /// Whether some part of this type is one `leaf` says yes to; `leaf`
    /// stops the walk at a part it decides.
    fn holds(
        &self,
        env: &Env,
        seen: &mut Verdicts<bool>,
        leaf: &dyn Fn(&Type) -> Option<bool>,
    ) -> bool {
        ensure_sufficient(|| {
            if let Some(v) = leaf(self) {
                return v;
            }
            match self {
                // an open cell will hold what bounds it from below
                Type::TVar(_) => match self.deref_cloned() {
                    Some(t) => seen.over_built(t, |seen, t| t.holds(env, seen, leaf)),
                    None => self.open_lower().iter().any(|l| l.holds(env, seen, leaf)),
                },
                // an open constructor is unknown like an open cell: its
                // argument decides
                Type::App(c, a) => match Type::app_filled(c, a) {
                    Some(f) => seen.over_built(f, |seen, f| f.holds(env, seen, leaf)),
                    None => a.holds(env, seen, leaf),
                },
                Type::Ref(tr) => {
                    let Ok(t) = self.lookup_ref(env) else { return false };
                    let shape =
                        tr.params.iter().map(|p| p.holds(env, seen, leaf)).collect();
                    match tr.def_key() {
                        None => seen.over_built(t, |seen, t| t.holds(env, seen, leaf)),
                        Some(k) => decide_once(seen, (k, shape), false, |seen| {
                            t.holds(env, seen, leaf)
                        }),
                    }
                }
                t => {
                    let node = super::node_addr(t).filter(|_| seen.depth == 0);
                    if node.is_some_and(|n| seen.walked.contains(&n)) {
                        return false;
                    }
                    let mut r = false;
                    t.for_each_child(&mut |c| r = r || c.holds(env, seen, leaf));
                    if !r && let Some(n) = node {
                        seen.walked.insert(n);
                    }
                    r
                }
            }
        })
    }

    fn cast_int(
        &self,
        env: &Env,
        cx: &mut CastCx,
        src: Option<&Type>,
        v: &Value,
    ) -> Cast {
        let at = (v as *const Value).addr();
        if at == cx.at {
            return ensure_sufficient(|| self.cast_inner(env, cx, src, v));
        }
        if cx.depth == MAX_DATA_DEPTH {
            return Err(self.cast_fail("the data is nested too deep", v));
        }
        let outer = mem::replace(&mut cx.at, at);
        cx.depth += 1;
        let r = ensure_sufficient(|| self.cast_inner(env, cx, src, v));
        cx.depth -= 1;
        cx.at = outer;
        r
    }

    fn cast_fail(&self, why: impl Into<Cow<'static, str>>, v: &Value) -> CastFail {
        CastFail { why: why.into(), to: self.clone(), v: v.clone() }
    }

    fn cast_inner(
        &self,
        env: &Env,
        cx: &mut CastCx,
        src: Option<&Type>,
        v: &Value,
    ) -> Cast {
        match self {
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => t.cast_int(env, cx, src, v),
                None => Ok(None),
            },
            Type::Hole => Err(self.cast_fail("a type constructor", v)),
            Type::Bound(_) => Err(self.cast_fail("a constraint", v)),
            Type::Bottom | Type::Any => Ok(cx.float_of(v)),
            Type::Fn(_) => match v {
                Value::Abstract(a) if AbstractTypeRegistry::is_a(a, "lambda") => Ok(None),
                _ => Err(self.cast_fail("not a function", v)),
            },
            Type::Abstract { .. } => {
                self.cast_abstract(env, cx, v, matches!(cx.input, CastInput::Text { .. }))
            }
            Type::ByRef(..) => {
                Err(self.cast_fail("a reference can't be read from data", v))
            }
            Type::Primitive(s) => {
                if !s.intersects(Typ::integer() | Typ::Decimal)
                    && let Some(f) = cx.float_of(v)
                {
                    let c = self.cast_inner(env, cx, src, &f)?;
                    return Ok(Some(c.unwrap_or(f)));
                }
                // a tuple, struct, variant or list is no primitive; a bare
                // variant reads as a string by its tag
                if rep::kind(v).is_some() {
                    return match rep::variant(v) {
                        Some((tag, [])) if s.contains(Typ::String) => {
                            Ok(Some(Value::String(tag.clone())))
                        }
                        _ => Err(self.cast_fail("no primitive holds it", v)),
                    };
                }
                if s.contains(Typ::get(v)) {
                    return Ok(None);
                }
                if let Value::Error(_) = v {
                    return Err(self.cast_fail("an error is not data", v));
                }
                if let Value::Abstract(_) = v {
                    return Err(
                        self.cast_fail("an abstract value has no primitive form", v)
                    );
                }
                // the value's class, then the class of the scalar text parses
                // as, then the rest; a real value, or text that parses as one,
                // tries the reals of each first
                let parsed = match s.intersects(text_scalars()) {
                    true => cx.scalar(v),
                    false => None,
                };
                let like = parsed.as_ref().unwrap_or(v);
                let own = own_class(v);
                let as_parsed =
                    parsed.as_ref().map_or(BitFlags::empty(), own_class) & !own;
                let reals = match Typ::get(like).is_real() {
                    true => Typ::real(),
                    false => BitFlags::empty(),
                };
                let kind = || NumKind::of(Typ::get(like));
                let to = |t| convert_text(v.clone(), t, |_| parsed.clone());
                let read =
                    [own, as_parsed, !own & !as_parsed].into_iter().find_map(|class| {
                        let tier = *s & class;
                        let members = (tier & reals).iter().chain((tier & !reals).iter());
                        let mut exact = members.clone().filter_map(|t| {
                            to(t).filter(|c| holds_exactly(v, parsed.as_ref(), c))
                        });
                        exact
                            .next()
                            .or_else(|| {
                                (cx.holds >= Holds::Rounded)
                                    .then(|| {
                                        members.clone().filter(|t| t.is_real()).find_map(
                                            |t| {
                                                to(t).filter(|c| {
                                                    holds_rounded(v, parsed.as_ref(), c)
                                                })
                                            },
                                        )
                                    })
                                    .flatten()
                            })
                            // CR claude for eric: [design/low] Class first across the
                            // rounds (D2 ruling 3) lets an infinity a finite value
                            // overflows to beat an exact member of another class: this
                            // round takes any conversion, so cast<[f32, string]>(1e300)
                            // is f32 inf and -1e300 -inf (main: the string), though
                            // holds_rounded refuses such an infinity as a rounding. The
                            // whole value is lost, not a fraction. Options: keep it; or
                            // an infinity reached from a finite value is a last resort,
                            // tried after every group, so cast<f32>(1e300) and
                            // cast<[f32, i64]>(1e300) stay inf and [f32, string] reads
                            // the string. Probes: ~/ptmp/cr29/scratch/r6-B/realord.gx,
                            // ~/ptmp/cr29/scratch/review-r6/B-r1-1/p07_inf.gx
                            // (class-first-overflow-01)
                            .or_else(|| {
                                (cx.holds == Holds::Partly)
                                    .then(|| members.clone().find_map(to))
                                    .flatten()
                            })
                    });
                match read {
                    Some(c) => {
                        if cx.in_union {
                            cx.nums.read(&c, kind)
                        }
                        Ok(Some(c))
                    }
                    None => Err(self.cast_fail("no primitive conversion", v)),
                }
            }
            Type::TVar(tv) => match tv.binding() {
                Some(t) => t.cast_int(env, cx, src, v),
                None => Ok(cx.float_of(v)),
            },
            Type::Error(e) => {
                let src = src_head(cx, src, env, v);
                let src = match &src {
                    Some(Type::Error(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    Value::Error(inner) => Ok(e
                        .cast_int(env, cx, src, inner)?
                        .map(|c| Value::Error(c.into()))),
                    // data is never read into an error
                    v => Err(self.cast_fail("not an error", v)),
                }
            }
            // a list reads as the array of its elements, a map as the
            // array of its (key, value) pairs
            Type::Array(et) => {
                let src = src_head(cx, src, env, v);
                let src = match &src {
                    Some(Type::Array(s) | Type::List(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    v if list::is_list(v) => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        for el in list::heads(v) {
                            let c = et.cast_int(env, cx, src, el)?;
                            out.push(c.unwrap_or_else(|| el.clone()));
                        }
                        Ok(Some(Value::Array(ValArray::from_iter_exact(out.drain(..)))))
                    }
                    v if let Some(elts) = rep::array_elems(v) => Ok(cast_elts(
                        &elts[..],
                        |v| v,
                        |_, el| et.cast_int(env, cx, src, el),
                    )?
                    .map(|mut a| Value::Array(ValArray::from_iter_exact(a.drain(..))))),
                    Value::Map(m) => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        for (k, val) in m.into_iter() {
                            let pair = rep::tuple([k.clone(), val.clone()]);
                            let c =
                                cx.over_built(|cx| et.cast_int(env, cx, None, &pair))?;
                            out.push(c.unwrap_or(pair));
                        }
                        Ok(Some(Value::Array(ValArray::from_iter_exact(out.drain(..)))))
                    }
                    v => Err(self.cast_fail("not an array", v)),
                }
            }
            Type::List(et) => {
                let src = src_head(cx, src, env, v);
                let src = match &src {
                    Some(Type::Array(s) | Type::List(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    v if list::is_list(v) => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        let mut changed = false;
                        for el in list::heads(v) {
                            match et.cast_int(env, cx, src, el)? {
                                Some(c) => {
                                    changed = true;
                                    out.push(c)
                                }
                                None => out.push(el.clone()),
                            }
                        }
                        Ok(changed.then(|| list::from_iter(out.drain(..))))
                    }
                    v if let Some(elts) = rep::array_elems(v) => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        for el in elts.iter() {
                            let c = et.cast_int(env, cx, src, el)?;
                            out.push(c.unwrap_or_else(|| el.clone()));
                        }
                        Ok(Some(list::from_iter(out.drain(..))))
                    }
                    v => Err(self.cast_fail("not a list", v)),
                }
            }
            Type::Map { key, value } => {
                let src = src_head(cx, src, env, v);
                let (ks, vs) = match &src {
                    Some(Type::Map { key, value }) => (Some(&**key), Some(&**value)),
                    _ => (None, None),
                };
                let check_keys = cx.check_keys && key.orders_refs(env);
                let mut entry = |k: &Value,
                                 v: &Value|
                 -> std::result::Result<_, CastFail> {
                    let ck = key.cast_int(env, cx, ks, k)?;
                    if check_keys
                        && !crate::reference::orderable(ck.as_ref().unwrap_or(k))
                    {
                        return Err(self.cast_fail(
                            "references have no order, including under Any",
                            k,
                        ));
                    }
                    let cv = value.cast_int(env, cx, vs, v)?;
                    Ok((ck.is_some() || cv.is_some()).then(|| {
                        (ck.unwrap_or_else(|| k.clone()), cv.unwrap_or_else(|| v.clone()))
                    }))
                };
                match v {
                    Value::Map(m) => {
                        let mut out: LPooled<Vec<(Value, Value)>> = LPooled::take();
                        let mut changed = false;
                        for (k, v) in m.into_iter() {
                            match entry(k, v)? {
                                Some(kv) => {
                                    changed = true;
                                    out.push(kv)
                                }
                                None => out.push((k.clone(), v.clone())),
                            }
                        }
                        if !changed {
                            return Ok(None);
                        }
                        let n = out.len();
                        let map = Map::from_iter(out.drain(..));
                        match map.len() == n {
                            true => Ok(Some(Value::Map(map))),
                            false => Err(self.cast_fail("two keys cast to one key", v)),
                        }
                    }
                    v if let Some(a) = rep::array_elems(v) => {
                        let mut out: LPooled<Vec<(Value, Value)>> = LPooled::take();
                        for p in a.iter() {
                            match rep::read_tuple(p).map(|p| p.elems()) {
                                Some(p @ [_, _]) => {
                                    let kv = entry(&p[0], &p[1])?;
                                    out.push(
                                        kv.unwrap_or_else(|| {
                                            (p[0].clone(), p[1].clone())
                                        }),
                                    )
                                }
                                _ => {
                                    return Err(
                                        self.cast_fail("expected an array of pairs", v)
                                    );
                                }
                            }
                        }
                        let n = out.len();
                        let map = Map::from_iter(out.drain(..));
                        match map.len() == n {
                            true => Ok(Some(Value::Map(map))),
                            false => Err(self.cast_fail("two keys cast to one key", v)),
                        }
                    }
                    _ => Err(self.cast_fail("not a map", v)),
                }
            }
            Type::Tuple(ts) => {
                let src = src_head(cx, src, env, v);
                let ss = match &src {
                    Some(Type::Tuple(ss)) if ss.len() == ts.len() => Some(ss),
                    _ => None,
                };
                match rep::read_tuple(v) {
                    Some(parts) if parts.elems().len() == ts.len() => {
                        let elts = parts.elems();
                        let cast = cast_elts(
                            elts,
                            |v| v,
                            |i, el| ts[i].cast_int(env, cx, ss.map(|ss| &ss[i]), el),
                        )?;
                        Ok(match (cast, parts.held()) {
                            (None, true) => None,
                            (None, false) => Some(rep::tuple(elts.iter().cloned())),
                            (Some(mut a), _) => Some(rep::tuple(a.drain(..))),
                        })
                    }
                    Some(_) => Err(self.cast_fail("tuple size mismatch", v)),
                    None => Err(self.cast_fail("not a tuple", v)),
                }
            }
            Type::Struct(ts) => {
                let Some(parts) = rep::read_struct(v) else {
                    return Err(self.cast_fail("not a struct", v));
                };
                let elts = parts.elems();
                if elts.len() > ts.len() {
                    return Err(self.cast_fail("struct size mismatch", v));
                }
                let mut fields: SmallVec<[(&ArcStr, &Value); 8]> =
                    match elts.iter().map(rep::field).collect::<Option<_>>() {
                        Some(f) => f,
                        None => {
                            return Err(self.cast_fail("expected an array of pairs", v));
                        }
                    };
                let mut sorted = fields.is_sorted_by_key(|(n, _)| *n);
                if !sorted {
                    fields.sort_by_key(|(n, _)| *n);
                }
                // a field the data omits reads as null where its type admits
                // null (an optional field, a key a document leaves out)
                static NULL: Value = Value::Null;
                let mut omitted: SmallVec<[(usize, Option<Value>); 2]> = SmallVec::new();
                if fields.len() < ts.len() {
                    let mut filled: SmallVec<[(&ArcStr, &Value); 8]> = SmallVec::new();
                    let mut have = fields.iter().copied().peekable();
                    for (i, (fname, ftyp, _)) in ts.iter().enumerate() {
                        match have.peek() {
                            Some((n, _)) if *n == fname => filled.extend(have.next()),
                            _ => match ftyp.cast_int(env, cx, None, &NULL) {
                                Ok(read) => {
                                    omitted.push((i, read));
                                    filled.push((fname, &NULL))
                                }
                                Err(_) => {
                                    return Err(self.cast_fail("struct size mismatch", v));
                                }
                            },
                        }
                    }
                    if have.next().is_some() {
                        return Err(self.cast_fail("struct fields mismatch", v));
                    }
                    fields = filled;
                    sorted = false;
                }
                if ts.iter().zip(fields.iter()).any(|((fname, _, _), (n, _))| n != &fname)
                {
                    return Err(self.cast_fail("struct fields mismatch", v));
                }
                let src = src_head(cx, src, env, v);
                let ss = match &src {
                    Some(Type::Struct(ss)) if ss.len() == ts.len() => Some(ss),
                    _ => None,
                };
                let cast = cast_elts(
                    &fields[..],
                    |(_, fv)| *fv,
                    |i, fv| match omitted.iter_mut().find(|(j, _)| *j == i) {
                        Some((_, read)) => Ok(read.take()),
                        None => ts[i].1.cast_int(env, cx, ss.map(|ss| &ss[i].1), fv),
                    },
                )?;
                if sorted && cast.is_none() && parts.held() {
                    return Ok(None);
                }
                let mut out: LPooled<Vec<Value>> = LPooled::take();
                for (i, (n, fv)) in fields.iter().enumerate() {
                    let fv = match &cast {
                        Some(c) => c[i].clone(),
                        None => (*fv).clone(),
                    };
                    out.push(rep::make_field((*n).clone(), fv));
                }
                Ok(Some(rep::structure_of_fields(out.drain(..))))
            }
            Type::Variant(tag, ts, _) if ts.is_empty() => match rep::read_variant(v) {
                Some((t, parts)) if t == tag && parts.elems().is_empty() => {
                    Ok((!parts.held()).then(|| rep::make_variant(tag.clone(), [])))
                }
                _ => Err(self.cast_fail("variant tag mismatch", v)),
            },
            Type::Variant(tag, ts, _) => {
                let src = src_head(cx, src, env, v);
                let ss = match &src {
                    Some(Type::Variant(stag, ss, _))
                        if stag == tag && ss.len() == ts.len() =>
                    {
                        Some(ss)
                    }
                    _ => None,
                };
                let payload = |i: usize| ss.map(|ss| &ss[i]);
                let read = rep::read_variant(v);
                match (read, rep::array_elems(v)) {
                    (Some((t, parts)), _) if parts.elems().len() == ts.len() => {
                        if t != tag {
                            return Err(self.cast_fail("variant tag mismatch", v));
                        }
                        let elts = parts.elems();
                        let cast = cast_elts(
                            elts,
                            |v| v,
                            |i, el| ts[i].cast_int(env, cx, payload(i), el),
                        )?;
                        Ok(match (cast, parts.held()) {
                            (None, true) => None,
                            (None, false) => {
                                Some(rep::make_variant(tag.clone(), elts.iter().cloned()))
                            }
                            (Some(mut a), _) => {
                                Some(rep::make_variant(tag.clone(), a.drain(..)))
                            }
                        })
                    }
                    (Some((t, _)), _) if t != tag => {
                        Err(self.cast_fail("variant tag mismatch", v))
                    }
                    (Some(_), _) => Err(self.cast_fail("variant length mismatch", v)),
                    _ => Err(self.cast_fail("not a variant", v)),
                }
            }
            // `cx` is the current path, not a visited set: a name met
            // again on the same value expands without consuming it.
            Type::Ref(tr) => {
                let Some(key) = ref_key(tr).map(|k| (k, (v as *const Value).addr()))
                else {
                    return Err(self.cast_fail("undefined type", v));
                };
                let Some(t) = cx.expand(env, self, tr) else {
                    return Err(self.cast_fail("undefined type", v));
                };
                // the same definition over the same value with larger params
                // grows at every level, the value never consumed
                let size = params_size(&key.0.1);
                let at = (key.0.0, key.1);
                let grows =
                    cx.sizes.get(&at).is_some_and(|ss| ss.iter().any(|s| *s < size));
                if grows || !cx.hist.insert(key.clone()) {
                    cx.cuts += 1;
                    return Err(
                        self.cast_fail("the type recurses without consuming it", v)
                    );
                }
                cx.sizes.entry(at).or_default().push(size);
                let r = t.cast_int(env, cx, src, v);
                if let Some(ss) = cx.sizes.get_mut(&at) {
                    ss.pop();
                    if ss.is_empty() {
                        cx.sizes.remove(&at);
                    }
                }
                cx.hist.remove(&key);
                r
            }
            // The members of the value's class come first: one it already
            // inhabits, a tuple or a struct its unmarked data reads back as,
            // then the rest of the class; then, for text, the members of the
            // class of the scalar it parses as; then the rest. Within each, a
            // member that holds the value exactly comes before one that holds
            // it rounded to a real's precision, and that before one that
            // would drop part of it (a fraction, a sub-second part)
            // (`Holds`); short of a dropped part, one keeping every number an
            // integer or a real as it was before one reading one across
            // (`Choice`).
            Type::Set(ts) => {
                let key = (cx.in_union && cx.built == 0)
                    .then(|| (ts.as_ptr().addr(), (v as *const Value).addr(), cx.holds));
                if let Some(k) = &key
                    && let Some((_, r, d)) = cx.read.get(k)
                {
                    let (r, d) = (r.clone(), *d);
                    cx.nums.add(d);
                    return r;
                }
                let base = cx.nums;
                let (in_union, cuts) = (mem::replace(&mut cx.in_union, true), cx.cuts);
                let array = rep::array_elems(v).is_some()
                    && (cx.input == CastInput::Marked
                        || matches!(src_head(cx, src, env, v), Some(Type::Array(_))));
                let r = self.cast_member(env, cx, src, v, ts, array);
                cx.in_union = in_union;
                if let Some(k) = key
                    && cx.cuts == cuts
                {
                    cx.read.insert(k, (self.clone(), r.clone(), cx.nums.since(base)));
                }
                r
            }
        }
    }

    /// The abstract type `self`'s reading of `v`. Text (`wrapped`) holds
    /// it as an object of one entry keyed by the type's name, the data
    /// inside; `Transparent<T>` holds what `T` holds there without the
    /// object. A written `from_value` reads the data inside as a cast of
    /// data does, nested abstract values bare, as the writer writes them.
    fn cast_abstract(
        &self,
        env: &Env,
        cx: &mut CastCx,
        v: &Value,
        wrapped: bool,
    ) -> Cast {
        let Type::Abstract { id, params } = self else {
            unreachable!("an abstract type")
        };
        let Some(rep) = env.abstract_reps.get(id) else {
            // a Rust-backed abstract is Rust's: only its own values
            return match self.is_a(env, v) {
                true => Ok(None),
                false => Err(self.cast_fail("not this abstract type", v)),
            };
        };
        let held = abstract_value::get(v).filter(|g| g.id == *id);
        if held.is_some_and(|g| g.params == *params) {
            return Ok(None);
        }
        let wrap = |payload: Option<Value>, data: &Value| {
            Ok(Some(abstract_value::wrap(
                *id,
                rep.name.clone(),
                params.clone(),
                payload.unwrap_or_else(|| data.clone()),
            )))
        };
        let inner = || rep.instantiate_with(params);
        if wrapped && *id == AbstractId::transparent() {
            let inner = inner();
            let payload = match rep_head(env, &inner).as_deref() {
                Some(t @ Type::Abstract { id, .. }) => {
                    t.cast_abstract(env, cx, v, *id == AbstractId::transparent())?
                }
                _ => inner.cast_int(env, cx, None, v)?,
            };
            return wrap(payload, v);
        }
        let v = match wrapped {
            false => v,
            true => match unwrapped(&rep.name, v) {
                Some(inside) => inside,
                None => {
                    return Err(self.cast_fail(
                        format!("expected an object whose one key is {}", rep.name),
                        v,
                    ));
                }
            },
        };
        match abstract_value::from_value(self, v) {
            None => Err(self.cast_fail("it has no Cast implementation", v)),
            Some(CastHook::Ran(None)) => {
                Err(self.cast_fail("its Cast implementation produced nothing", v))
            }
            Some(CastHook::Ran(Some(Value::Error(e)))) => {
                Err(self.cast_fail(refusal(&e), v))
            }
            Some(CastHook::Ran(Some(r))) => Ok(Some(r)),
            Some(CastHook::Representation) => {
                let data = held.map_or(v, |g| &g.payload);
                wrap(inner().cast_int(env, cx, None, data)?, data)
            }
        }
    }

    /// The union `ts`'s reading of `v`, as `cast_int` takes one, over its
    /// members past every union in them ([`members_by_class`]): when `v`
    /// is statically an array (`array`), an array member it inhabits; else
    /// a member whose tag it carries; else the members of `v`'s class (for
    /// unmarked data a tuple of its length and then a struct first), then
    /// for text the members of the class of the scalar it parses as, then
    /// the rest, each group as [`Self::tier`] reads it.
    fn cast_member(
        &self,
        env: &Env,
        cx: &mut CastCx,
        src: Option<&Type>,
        v: &Value,
        ts: &[Type],
        array: bool,
    ) -> Cast {
        let own = match rep::kind(v) {
            None => own_class(v),
            Some(_) => BitFlags::empty(),
        };
        let mut members: SmallVec<[Cow<Type>; 8]> = SmallVec::new();
        let by_class = |classes, out: &mut SmallVec<_>| {
            let mut named: LPooled<Vec<RefKey>> = LPooled::take();
            for t in ts.iter() {
                members_by_class(env, Cow::Borrowed(t), classes, out, &mut named, 0)
            }
        };
        by_class([own, BitFlags::empty()], &mut members);
        // text parses only where a member may read the scalar it parses as
        let parsed = match v {
            Value::String(_) if members.iter().any(|t| takes_scalar(env, t)) => {
                cx.scalar(v)
            }
            _ => None,
        };
        let as_parsed = parsed.as_ref().map_or(BitFlags::empty(), own_class) & !own;
        if !as_parsed.is_empty() {
            members.clear();
            by_class([own, as_parsed], &mut members);
        }
        let ts = || members.iter().map(|t| &**t);
        if array {
            let arrays: SmallVec<[&Type; 4]> = ts()
                .filter(|t| match rep_head(env, t).as_deref() {
                    Some(Type::Array(_)) => true,
                    Some(Type::Primitive(p)) => p.contains(Typ::Array),
                    _ => false,
                })
                .collect();
            let converts = cx.input == CastInput::Marked;
            if let Ok(r) = self.tier(env, cx, src, v, &[arrays], converts) {
                return Ok(r);
            }
        }
        // a member whose tag the value carries is its reading, whatever a
        // tagless reading of an earlier member says
        let tagged: SmallVec<[&Type; 4]> =
            ts().filter(|t| carries_tag(env, t, v)).collect();
        if !tagged.is_empty() {
            return self
                .tier(env, cx, src, v, &[tagged], true)
                .map_err(|e| e.expect("a tagged member was tried"));
        }
        // a writer wrote the tuple or the struct the data reads back as
        let rank = |t: &Type| match (v, rep_head(env, t).as_deref()) {
            (Value::Array(a), Some(Type::Tuple(es)))
                if rep::kind(v).is_none()
                    && es.len() == a.len()
                    && es.iter().zip(a.iter()).all(|(e, x)| in_own_class(env, e, x)) =>
            {
                Some(0)
            }
            (Value::Array(a), Some(Type::Struct(fs)))
                if rep::kind(v).is_none() && fields_in_own_class(env, fs, a) =>
            {
                Some(1)
            }
            _ => None,
        };
        // CR claude for claude: [bug/low] A union cast tries members in the order the
        // union holds them, and Real = [Float, decimal] holds the primitive decimal
        // before the named Float, so cast<Number>("3.0") and cast<Real>("3.0") are
        // decimal while cast<[f32, f64, decimal]>("3.0") is f32,
        // str::parse<Number>("3.0") is f64, and cast<Real>(u8:3) is decimal where the
        // flat spelling gives f32. CLAUDE.md: a union target takes the first member in
        // canonical order, narrowest first, and a transparent typedef is its expansion.
        // Order the members by the expansion's primitive order, or try first the member
        // the parsed literal inhabits (which also makes cast agree with str::parse).
        // Probes: ~/tmp/cr29/scratch/review2/RV3/{real_order,parse_vs_cast2}.gx
        // (real-member-order-01)
        // 2026-10-31 claude: Lane B round 6 (D2) leaves this as it was: on cr33-B as on
        // main, cast<Number>("3.0") and cast<Real>("3.0") are decimal:3.0 while
        // cast<[f32, f64, decimal]>("3.0") is f32:3.0, cast<Number>("0.1") is
        // decimal:0.1 where the flat spelling gives f64 0.1, and cast<Real>(u8:3) is
        // decimal where the flat spelling gives f32 (probes
        // ~/ptmp/cr29/scratch/r6-B/real_order{,2}.gx). D2's ruling 5 (keep the
        // canonical tie-break) picks the first suggested fix, the expansion's primitive
        // order: Real and Number would then read "0.1" as f64 and "3.0" as f32, a
        // visible change for whoever reads text into Number. Not in lane B's brief, so
        // left for a later round.
        // 2026-11-01 claude: Round 6 review (B-r1-2) found another case of this order:
        // with type F = f32 and type U = u8, cast<[F, U]>(true) is f32 1.0 while
        // cast<[f32, u8]>(true) is u8 1 (main gives the same pair): a bool has no
        // number kind, so Choice prefers neither and the named members' canonical order
        // (by name) decides. cast_member now looks through a member naming a union or a
        // primitive set spanning classes (members_by_class) but keeps a member naming
        // one class whole, so the named order still decides here. Probe:
        // ~/ptmp/cr29/scratch/r6-B/realord.gx.
        let mut groups: [SmallVec<[&Type; 4]>; 5] = Default::default();
        for t in ts() {
            let g = match rank(t) {
                Some(r) => r,
                None if in_own_class(env, t, v) => 2,
                None if parsed.as_ref().is_some_and(|p| in_own_class(env, t, p)) => 3,
                None => 4,
            };
            groups[g].push(t)
        }
        let [tuples, structs, own, as_parsed, rest] = groups;
        for tier in [&[tuples, structs, own][..], &[as_parsed], &[rest]] {
            if let Ok(r) = self.tier(env, cx, src, v, tier, true) {
                return Ok(r);
            }
        }
        Err(self.cast_fail("no member admits it", v))
    }

    /// The reading of `v` by the members `groups` hold, each round up to
    /// `cx.holds` in turn: a member that holds `v` as it is, else what the
    /// round's [`Choice`] took, offered each member's conversion (only
    /// where `converts`) in order, a group at a time, the next group only
    /// where the choice may still change. `Err` when no round read it,
    /// with the last refusal when a member was tried.
    fn tier(
        &self,
        env: &Env,
        cx: &mut CastCx,
        src: Option<&Type>,
        v: &Value,
        groups: &[SmallVec<[&Type; 4]>],
        converts: bool,
    ) -> std::result::Result<Option<Value>, Option<CastFail>> {
        if groups.iter().all(|g| g.is_empty()) {
            return Err(None);
        }
        let outer = cx.holds;
        let mut refused = None;
        for holds in [Holds::Exactly, Holds::Rounded, Holds::Partly] {
            if holds > outer {
                break;
            }
            cx.holds = holds;
            let mut choice = Choice::new(cx);
            for group in groups.iter() {
                for t in group.iter() {
                    match cx.trial(env, t, src, v) {
                        (Ok(None), d) => {
                            cx.holds = outer;
                            cx.nums.add(d);
                            return Ok(None);
                        }
                        (Ok(Some(c)), d) if converts => choice.offer(c, d),
                        (Ok(Some(_)), _) => (),
                        (Err(e), _) => refused = Some(e),
                    }
                }
                if choice.settled() {
                    break;
                }
            }
            cx.holds = outer;
            if let Some(c) = choice.take(cx) {
                return Ok(Some(c));
            }
        }
        Err(refused)
    }

    /// Cast `v` of unknown static type to this type; a failure is the
    /// `InvalidCast` error value. An unmarked array reads as a writer's
    /// data: a union reads it as a tagged variant, a tuple or a struct
    /// before an array.
    pub fn cast_value(&self, env: &Env, v: Value) -> Value {
        self.cast_with(env, None, v, CastInput::Data, true).unwrap_or_else(|e| e)
    }

    /// Read binary data whose composite kinds are preserved: an unmarked
    /// array is an array, including at nested union positions.
    pub fn cast_marked_value(&self, env: &Env, v: Value) -> Value {
        self.cast_with(env, None, v, CastInput::Marked, true).unwrap_or_else(|e| e)
    }

    /// [`Self::cast_value`] with its refusal apart from a cast value that
    /// is an error: `Err` is the `InvalidCast` error value.
    pub fn try_cast_value(&self, env: &Env, v: Value) -> Result<Value, Value> {
        self.cast_with(env, None, v, CastInput::Data, true)
    }

    /// [`Self::cast_value`] of a `v` statically of type `src`, which
    /// decides whether an unmarked array is an array or a writer's data:
    /// a union reads an array as an array member it inhabits first.
    pub fn cast_from(&self, env: &Env, src: &Type, v: Value) -> Value {
        self.cast_with(env, Some(src), v, CastInput::Data, true).unwrap_or_else(|e| e)
    }

    /// A cast with its input format and static map-key check decision.
    /// `check_keys` may be false only when the target contains neither Any
    /// nor an abstract type whose representation a cast can enter.
    pub fn cast_with(
        &self,
        env: &Env,
        src: Option<&Type>,
        v: Value,
        input: CastInput,
        check_keys: bool,
    ) -> Result<Value, Value> {
        match self.cast_int(env, &mut CastCx::new(input, check_keys), src, &v) {
            Ok(None) => Ok(v),
            Ok(Some(v)) => Ok(v),
            Err(e) => Err(errf!(CAST_ERR_TAG, "{e}")),
        }
    }

    fn is_a_int(
        &self,
        env: &Env,
        hist: &mut IsAHist,
        flags: BitFlags<IsAFlags>,
        v: &Value,
    ) -> bool {
        ensure_sufficient(|| self.is_a_int_inner(env, hist, flags, v))
    }

    fn is_a_int_inner(
        &self,
        env: &Env,
        hist: &mut IsAHist,
        flags: BitFlags<IsAFlags>,
        v: &Value,
    ) -> bool {
        match self {
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => t.is_a_int(env, hist, flags, v),
                None => !flags.contains(IsAFlags::Strict),
            },
            Type::Hole | Type::Bound(_) => false,
            // `hist` is the current path, not a visited set: a repeat
            // on the path is a name expanding without consuming value
            // structure; a repeat off the path is union backtracking.
            // a test commits nothing, and expands each ref once a walk
            Type::Ref(tr) => {
                let def =
                    tr.def_key().or_else(|| tr.resolve_in(env).map(|r| r.def_key()));
                let Some(def) = def else { return false };
                let rk = (def, (*tr.params).as_ptr().addr());
                let t = match hist.expanded.get(&rk) {
                    Some((_, t)) => t.clone(),
                    None => {
                        let t = self.lookup_ref_with(env, false).ok().flatten();
                        hist.expanded.insert(rk, (tr.params.clone(), t.clone()));
                        t
                    }
                };
                let Some(t) = t else { return false };
                let key = (rk, (v as *const Value).addr());
                hist.path.insert(key.clone()) && {
                    let r = t.is_a_int(env, hist, flags, v);
                    hist.path.remove(&key);
                    r
                }
            }
            Type::Primitive(t) => rep::kind(v).is_none() && t.contains(Typ::get(&v)),
            Type::Abstract { id, params } => match v {
                Value::Abstract(a) => {
                    match a.downcast_ref::<crate::abstract_value::GxAbstract>() {
                        Some(g) => {
                            g.id == *id
                                && (flags.contains(IsAFlags::MatchAbstract)
                                    || g.params.len() == params.len()
                                    // parameters are invariant: each one equal
                                    && params.iter().zip(g.params.iter()).all(|(p, gp)| {
                                    let holds = |a: &Type, b: &Type| {
                                        a.contains_with_flags(BitFlags::empty(), env, b)
                                            .unwrap_or(false)
                                    };
                                    holds(p, gp) && holds(gp, p)
                                }))
                        }
                        None => {
                            a.id().as_u64_pair().1 == id.inner()
                                || flags.contains(IsAFlags::MatchAbstract)
                        }
                    }
                }
                _ => false,
            },
            Type::Any => !flags.contains(IsAFlags::Strict),
            // `Any` elements: test the runtime class without walking.
            Type::Array(et)
                if matches!(&**et, Type::Any) && !flags.contains(IsAFlags::Strict) =>
            {
                rep::array_elems(v).is_some()
            }
            Type::Array(et) => match rep::array_elems(v) {
                Some(a) => a.iter().all(|v| et.is_a_int(env, hist, flags, v)),
                None => false,
            },
            Type::List(et)
                if matches!(&**et, Type::Any) && !flags.contains(IsAFlags::Strict) =>
            {
                list::is_list(v)
            }
            // Walk the spine iteratively (heads recurse).
            Type::List(et) => {
                let mut cur = v;
                loop {
                    if list::is_nil(cur) {
                        break true;
                    }
                    match list::split(cur) {
                        Some((h, t)) => {
                            if !et.is_a_int(env, hist, flags, h) {
                                break false;
                            }
                            cur = t;
                        }
                        None => break false,
                    }
                }
            }
            Type::Map { key, value }
                if matches!(&**key, Type::Any)
                    && matches!(&**value, Type::Any)
                    && !flags.contains(IsAFlags::Strict) =>
            {
                matches!(v, Value::Map(_))
            }
            Type::Map { key, value } => match v {
                Value::Map(m) => m.into_iter().all(|(k, v)| {
                    key.is_a_int(env, hist, flags, k)
                        && value.is_a_int(env, hist, flags, v)
                }),
                _ => false,
            },
            Type::Error(e) => match v {
                Value::Error(v) => e.is_a_int(env, hist, flags, v),
                _ => false,
            },
            Type::ByRef(..) => crate::reference::cell(v).is_some(),
            Type::Tuple(ts) => match rep::tuple_elems(v) {
                Some(elts) => {
                    elts.len() == ts.len()
                        && ts
                            .iter()
                            .zip(elts.iter())
                            .all(|(t, v)| t.is_a_int(env, hist, flags, v))
                }
                _ => false,
            },
            Type::Struct(ts) => match rep::struct_fields(v) {
                Some(elts) => {
                    elts.len() == ts.len()
                        && ts.iter().zip(elts.iter()).all(|((n, t, _), v)| {
                            match rep::field(v) {
                                Some((key, v)) => {
                                    n == key && t.is_a_int(env, hist, flags, v)
                                }
                                None => false,
                            }
                        })
                }
                _ => false,
            },
            Type::Variant(tag, ts, _) => match rep::variant(v) {
                Some((t, payload)) => {
                    t == tag
                        && payload.len() == ts.len()
                        && ts
                            .iter()
                            .zip(payload)
                            .all(|(t, v)| t.is_a_int(env, hist, flags, v))
                }
                None => false,
            },
            Type::TVar(tv) => match tv.binding() {
                None => !flags.contains(IsAFlags::Strict),
                Some(t) => t.is_a_int(env, hist, flags, v),
            },
            Type::Fn(_) => match v {
                Value::Abstract(a) if AbstractTypeRegistry::is_a(a, "lambda") => true,
                _ => false,
            },
            Type::Bottom => !flags.contains(IsAFlags::Strict),
            Type::Set(ts) => ts.iter().any(|t| t.is_a_int(env, hist, flags, v)),
        }
    }

    /// True if v is structurally compatible with the type.
    pub fn is_a(&self, env: &Env, v: &Value) -> bool {
        self.is_a_int(env, &mut IsAHist::default(), BitFlags::empty(), v)
    }

    /// [`Self::is_a`] with flags.
    pub fn is_a_with(&self, env: &Env, flags: BitFlags<IsAFlags>, v: &Value) -> bool {
        self.is_a_int(env, &mut IsAHist::default(), flags, v)
    }

    /// The shallow discriminator for a select arm's INFERRED type
    /// predicate: `self` with every payload position replaced by `Any`,
    /// when the outermost shape alone tells `self`'s values apart from
    /// the other members of `scrutinee`. `None` keeps the full walk
    /// (members not enumerable, two share a shape, a union below the top
    /// whose members share one, which a nested structure test cannot tell
    /// apart, or no payload, or the one member of a shape is one `self`
    /// cannot match). Sound only for inferred predicates, which typecheck
    /// unified against their member; explicit `x as T` stays strict.
    pub fn shallow_discriminant(&self, env: &Env, scrutinee: &Type) -> Option<Type> {
        ShallowScrutinee::new(scrutinee.clone()).discriminant(env, self)
    }
}

/// The scrutinee and its referenced definitions must stay fixed while these facts are used.
#[derive(Debug)]
pub struct ShallowScrutinee {
    typ: Type,
    members: OnceLock<Option<SmallVec<[Type; 8]>>>,
    facts: OnceLock<Option<Box<[(Type, MemberFacts)]>>>,
}

impl ShallowScrutinee {
    pub fn new(typ: Type) -> Self {
        Self { typ, members: OnceLock::new(), facts: OnceLock::new() }
    }

    pub fn members(&self, env: &Env) -> Option<&[Type]> {
        self.members
            .get_or_init(|| {
                let mut members = SmallVec::new();
                union_members(env, &self.typ, &mut members).ok()?;
                Some(members)
            })
            .as_deref()
    }

    fn facts(&self, env: &Env) -> Option<&[(Type, MemberFacts)]> {
        self.facts
            .get_or_init(|| {
                let mut scrut: LPooled<Vec<Type>> = LPooled::take();
                let mut seen: LPooled<Vec<RefKey>> = LPooled::take();
                flatten_union_members(&self.typ, env, &mut scrut, &mut seen)?;
                let mut walked = ShapeSeen::default();
                let mut below = |m: &Type| {
                    m.try_for_each_child(&mut |c| match shapes_collide(
                        c,
                        env,
                        &mut walked,
                    ) {
                        true => ControlFlow::Break(()),
                        false => ControlFlow::Continue(()),
                    })
                    .is_break()
                };
                if scrut.iter().any(|m| below(m)) {
                    return None;
                }
                Some(scrut.iter().map(|m| (m.clone(), member_facts(m))).collect())
            })
            .as_deref()
    }

    pub fn discriminant(&self, env: &Env, predicate: &Type) -> Option<Type> {
        let mut preds: LPooled<Vec<Type>> = LPooled::take();
        let mut seen: LPooled<Vec<RefKey>> = LPooled::take();
        flatten_union_members(predicate, env, &mut preds, &mut seen)?;
        if preds.iter().all(|p| member_facts(p).exact) {
            return None;
        }
        let sfacts = self.facts(env)?;
        let mut out: LPooled<Vec<Type>> = LPooled::take();
        for p in preds.iter() {
            let pf = member_facts(p);
            if pf.exact {
                out.push(p.clone());
                continue;
            }
            // the shape decides `p` only where one member has it and `p` can
            // match that member (an instance's scrutinee may be another
            // member of `p`'s shape)
            let sole = |shaped: &dyn Fn(&MemberFacts) -> bool| {
                let mut it = sfacts.iter().filter(|(_, mf)| shaped(mf));
                match (it.next(), it.next()) {
                    (Some((m, _)), None) => p.could_pass(env, m).unwrap_or(false),
                    _ => false,
                }
            };
            if let Some(pa) = &pf.arr
                && !sole(&|mf| mf.arr.as_ref().is_some_and(|ma| arr_overlap(pa, ma)))
            {
                return None;
            }
            if pf.map && !sole(&|mf| mf.map) {
                return None;
            }
            if pf.error && !sole(&|mf| mf.error) {
                return None;
            }
            out.push(shallowify(p));
        }
        Some(if out.len() == 1 {
            out.pop().unwrap()
        } else {
            Type::Set(Arc::from_iter(out.drain(..)))
        })
    }
}

/// Whether `t` or a part of it is a union two of whose members share a
/// shape (`arr_overlap`). An application is walked once a search, and a
/// typedef once a path, whatever its parameters: a non-regular one's grow
/// at every level, and what they hold is walked at the first.
/// What a [`shapes_collide`] search has walked: the applications, and
/// the typedefs on the path.
#[derive(Default)]
struct ShapeSeen {
    walked: LPooled<AHashSet<RefKey>>,
    path: SmallVec<[usize; 8]>,
}

fn shapes_collide(t: &Type, env: &Env, seen: &mut ShapeSeen) -> bool {
    ensure_sufficient(|| match t {
        Type::Ref(tr) => {
            let Some(key) = ref_key(tr) else { return false };
            if seen.path.contains(&key.0) || !seen.walked.insert(key.clone()) {
                return false;
            }
            let Ok(x) = t.lookup_ref(env) else { return false };
            seen.path.push(key.0);
            let r = shapes_collide(&x, env, seen);
            seen.path.pop();
            r
        }
        Type::TVar(tv) => tv.binding().is_some_and(|b| shapes_collide(&b, env, seen)),
        Type::Set(ts) => {
            let shapes: LPooled<Vec<_>> = ts
                .iter()
                .filter_map(|m| m.with_deref(|m| m.and_then(|m| member_facts(m).arr)))
                .collect();
            let overlap = shapes
                .iter()
                .enumerate()
                .any(|(i, a)| shapes[i + 1..].iter().any(|b| arr_overlap(a, b)));
            overlap || ts.iter().any(|m| shapes_collide(m, env, seen))
        }
        t => t
            .try_for_each_child(&mut |c| match shapes_collide(c, env, seen) {
                true => ControlFlow::Break(()),
                false => ControlFlow::Continue(()),
            })
            .is_break(),
    })
}

/// A flattened union member's runtime footprint. Arrays, tuples,
/// structs, variants with a payload and lists all inhabit
/// `Value::Array`, each kind under its own marker; `arr` is the member's
/// constraint within that class. `exact`: the full `is_a` is already
/// O(1).
#[derive(Debug)]
struct MemberFacts {
    arr: Option<(Form, ArrCon)>,
    map: bool,
    error: bool,
    exact: bool,
}

/// What a member's array is: its kind, and a variant's tag.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Form {
    Array,
    Tuple,
    Struct,
    Variant(ArcStr),
    List,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ArrCon {
    Len(usize),
    AnyLen,
}

impl MemberFacts {
    const EXACT: Self = MemberFacts { arr: None, map: false, error: false, exact: true };

    fn arr(form: Form, con: ArrCon) -> Self {
        MemberFacts { arr: Some((form, con)), map: false, error: false, exact: false }
    }
}

fn member_facts(t: &Type) -> MemberFacts {
    match t {
        Type::Primitive(bits) => MemberFacts {
            arr: bits.contains(Typ::Array).then_some((Form::Array, ArrCon::AnyLen)),
            map: bits.contains(Typ::Map),
            error: bits.contains(Typ::Error),
            exact: true,
        },
        Type::Variant(_, ps, _) if ps.is_empty() => MemberFacts::EXACT,
        Type::Variant(tag, ps, _) => {
            MemberFacts::arr(Form::Variant(tag.clone()), ArrCon::Len(ps.len()))
        }
        Type::Tuple(ts) => MemberFacts::arr(Form::Tuple, ArrCon::Len(ts.len())),
        Type::Struct(fs) => MemberFacts::arr(Form::Struct, ArrCon::Len(fs.len())),
        Type::Array(_) => MemberFacts::arr(Form::Array, ArrCon::AnyLen),
        Type::List(_) => MemberFacts::arr(Form::List, ArrCon::AnyLen),
        Type::Map { .. } => MemberFacts { map: true, exact: false, ..MemberFacts::EXACT },
        Type::Error(_) => MemberFacts { error: true, exact: false, ..MemberFacts::EXACT },
        Type::Abstract { .. } | Type::Fn(_) | Type::ByRef(..) | Type::Bottom => {
            MemberFacts::EXACT
        }
        // `flatten_union_members` never yields these.
        Type::Any
        | Type::Set(_)
        | Type::Ref(_)
        | Type::TVar(_)
        | Type::App(..)
        | Type::Hole
        | Type::Bound(_) => MemberFacts::EXACT,
    }
}

fn arr_overlap((pf, pcon): &(Form, ArrCon), (mf, mcon): &(Form, ArrCon)) -> bool {
    pf == mf
        && match (pcon, mcon) {
            (ArrCon::AnyLen, _) | (_, ArrCon::AnyLen) => true,
            (ArrCon::Len(a), ArrCon::Len(b)) => a == b,
        }
}

fn shallowify(t: &Type) -> Type {
    match t {
        Type::Variant(tag, ps, at) => {
            Type::Variant(tag.clone(), Arc::from_iter(ps.iter().map(|_| Type::Any)), *at)
        }
        Type::Tuple(ts) => Type::Tuple(Arc::from_iter(ts.iter().map(|_| Type::Any))),
        Type::Struct(fs) => Type::Struct(Arc::from_iter(
            fs.iter().map(|(n, _, at)| (n.clone(), Type::Any, *at)),
        )),
        Type::Array(_) => Type::Array(Arc::new(Type::Any)),
        Type::List(_) => Type::List(Arc::new(Type::Any)),
        Type::Map { .. } => {
            Type::Map { key: Arc::new(Type::Any), value: Arc::new(Type::Any) }
        }
        Type::Error(_) => Type::Error(Arc::new(Type::Any)),
        t => t.clone(),
    }
}

/// `seen` is the path of definitions being expanded.
fn flatten_union_members(
    t: &Type,
    env: &Env,
    out: &mut LPooled<Vec<Type>>,
    seen: &mut LPooled<Vec<RefKey>>,
) -> Option<()> {
    ensure_sufficient(|| match t {
        Type::Set(ts) => {
            for t in ts.iter() {
                flatten_union_members(t, env, out, seen)?;
            }
            Some(())
        }
        Type::Ref(tr) => {
            let t = t.lookup_ref(env).ok()?;
            let key = ref_key(tr)?;
            if seen.contains(&key) {
                return None;
            }
            seen.push(key);
            let res = flatten_union_members(&t, env, out, seen);
            seen.pop();
            res
        }
        Type::TVar(tv) => match tv.binding() {
            Some(t) => flatten_union_members(&t, env, out, seen),
            None => None,
        },
        Type::App(c, a) => match Type::app_filled(c, a) {
            Some(t) => flatten_union_members(&t, env, out, seen),
            None => None,
        },
        Type::Any | Type::Hole => None,
        t => {
            out.push(t.clone());
            Some(())
        }
    })
}

/// What a runtime test sees of `t` at its head: cells, filled
/// applications and typedefs expanded; `None` for what it can't see into
/// (an open cell, `Any`, a constructor), which takes any value.
fn rep_head<'a>(env: &Env, t: &'a Type) -> Option<Cow<'a, Type>> {
    resolved_head(env, t).filter(|h| !matches!(&**h, Type::Any))
}

fn resolved_head<'a>(env: &Env, t: &'a Type) -> Option<Cow<'a, Type>> {
    let owned = |t: Type| resolved_head(env, &t).map(|h| Cow::Owned(h.into_owned()));
    ensure_sufficient(|| match t {
        Type::TVar(_) => t.deref_cloned().and_then(owned),
        Type::App(c, a) => Type::app_filled(c, a).and_then(owned),
        Type::Ref(_) => t.lookup_ref(env).ok().and_then(owned),
        Type::Hole | Type::Bound(_) => None,
        t => Some(Cow::Borrowed(t)),
    })
}

/// The primitive tags a value of another type may share.
const SHARED_PRIMS: BitFlags<Typ> = make_bitflags!(Typ::{Map | Error | Array});

/// Whether a value of `t` may also be a value of a type `t` doesn't hold:
/// a Rust-backed abstract's values carry only its id, so one with
/// parameters shares its form with its other instantiations.
fn shares_form(env: &Env, t: &Type) -> bool {
    ensure_sufficient(|| match rep_head(env, t).as_deref() {
        Some(Type::Abstract { id, params }) => {
            !params.is_empty() && !env.abstract_reps.contains_key(id)
        }
        None | Some(Type::Bottom) => false,
        Some(Type::Variant(_, ps, _)) if ps.is_empty() => false,
        Some(Type::Primitive(p)) => p.intersects(SHARED_PRIMS),
        Some(Type::Set(ts)) => ts.iter().any(|t| shares_form(env, t)),
        Some(_) => true,
    })
}

/// How a runtime-form judgment reads an open cell.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Open {
    /// As no type yet: nothing collides with it. Inference may still
    /// fill it.
    Benign,
    /// As any type it may yet bind: it collides with another open cell
    /// and with every type whose form another type shares.
    Unknown,
}

/// Why one of `a` and `b`, an open cell [`Open::Unknown`] reads as any
/// type it may yet bind, collides with the other: it may share the
/// other's form, or, `ordered`, be another numeric type. A bound that is
/// a type holds every value of the cell, so a cell with such bounds
/// collides only where each of them does. Under `tested`, `a`'s cells
/// collide with nothing ([`rep_collision`]), except against a type bound
/// of `b`'s cell: a value of the bound passes the test whole.
fn open_collision(
    env: &Env,
    seen: &mut RepSeen,
    tested: bool,
    ordered: bool,
    a: &Type,
    b: &Type,
) -> Option<Indiscernible> {
    let open = |t: &Type| match t {
        Type::TVar(tv) => tv.open_cell(),
        _ => None,
    };
    let pair = || Some(Indiscernible::Pair(a.clone(), b.clone()));
    let mut beside = |cell: &super::TVar, t: &Type| {
        // a `NoError` cell is never an error, so it shares no error's form
        if cell.cell_constraints().contains(&Type::Bound(Bound::NoError))
            && error_only(env, t)
        {
            return None;
        }
        let typed = cell.typed_bounds(env);
        if typed.is_empty() {
            return match shares_form(env, t) {
                true => pair(),
                false => (ordered && holds_number(env, t))
                    .then(|| Indiscernible::OpenNumber(t.clone())),
            };
        }
        let mut why = None;
        for b in typed.iter() {
            let found = match tested {
                // `seen` clears pairs by what the walk found under `tested`
                true => {
                    let fresh = &mut RepSeen::default();
                    rep_collision(env, fresh, false, Open::Unknown, ordered, t, b)
                }
                false => rep_collision(env, seen, false, Open::Unknown, ordered, t, b),
            };
            why = Some(found?);
        }
        match why? {
            Indiscernible::Numbers(_) => Some(Indiscernible::OpenNumber(t.clone())),
            _ => pair(),
        }
    };
    match (open(a), open(b)) {
        (Some(x), Some(y)) => (x.cell_addr() != y.cell_addr()).then(pair).flatten(),
        (Some(x), None) => beside(&x, b),
        (None, Some(y)) => beside(&y, a),
        (None, None) => None,
    }
}

/// Whether a value of `t` may be a number.
fn holds_number(env: &Env, t: &Type) -> bool {
    match rep_head(env, t).as_deref() {
        Some(Type::Primitive(p)) => p.intersects(Typ::number()),
        _ => false,
    }
}

impl TVar {
    /// The bounds of this cell that are types, each holding every value
    /// of it: its constraints less the marker bounds (`Concrete`, a
    /// trait) a runtime test can't see.
    pub fn typed_bounds(&self, env: &Env) -> SmallVec<[Type; 1]> {
        let mut bounds = self.cell_constraints();
        bounds.retain(|b| rep_head(env, b).is_some());
        bounds
    }
}

/// Whether every value of `t` is an error.
fn error_only(env: &Env, t: &Type) -> bool {
    ensure_sufficient(|| match rep_head(env, t).as_deref() {
        Some(Type::Error(_)) => true,
        Some(Type::Primitive(p)) => *p == BitFlags::from(Typ::Error),
        Some(Type::Set(ts)) => ts.iter().all(|t| error_only(env, t)),
        _ => false,
    })
}

/// The primitive tags a value of the (non-primitive) head `t` has; a
/// tuple, struct, variant or list has none, its marker sets it apart.
fn rep_prims(t: &Type) -> BitFlags<Typ> {
    match t {
        Type::Map { .. } => Typ::Map.into(),
        Type::Error(_) => Typ::Error.into(),
        Type::Array(_) => Typ::Array.into(),
        _ => BitFlags::empty(),
    }
}

/// The applications a walk has expanded, each once, and the path of
/// applications it is inside, each with its parameters' size.
struct Expanded<
    K: std::hash::Hash + Eq + 'static,
    N: std::hash::Hash + Eq + 'static = usize,
> {
    seen: LPooled<AHashSet<K>>,
    path: LPooled<Vec<((usize, usize), usize)>>,
    /// The shared nodes (or pairs of them) the walk found nothing under,
    /// so a type that shares them (a DAG) is walked once.
    clear: LPooled<AHashSet<N>>,
    /// The heads the walk built, alive until it ends: a node `clear`
    /// holds keeps its address.
    built: LPooled<Vec<Type>>,
}

impl<K: std::hash::Hash + Eq + 'static, N: std::hash::Hash + Eq + 'static> Default
    for Expanded<K, N>
{
    fn default() -> Self {
        Self {
            seen: LPooled::take(),
            path: LPooled::take(),
            clear: LPooled::take(),
            built: LPooled::take(),
        }
    }
}

impl<K: std::hash::Hash + Eq + 'static, N: std::hash::Hash + Eq + 'static>
    Expanded<K, N>
{
    /// `body` over the node `node`, unless the walk found nothing under
    /// it already. Whatever a node first found nothing under, it finds
    /// nothing under again: every application in it was expanded then,
    /// and is not again.
    fn once<R>(
        &mut self,
        node: Option<N>,
        body: impl FnOnce(&mut Self) -> Option<R>,
    ) -> Option<R> {
        if node.as_ref().is_some_and(|n| self.clear.contains(n)) {
            return None;
        }
        let r = body(self);
        if r.is_none()
            && let Some(n) = node
        {
            self.clear.insert(n);
        }
        r
    }

    /// Keep a head the walk built alive until it ends.
    fn keep(&mut self, head: Option<Cow<'_, Type>>) {
        if let Some(Cow::Owned(t)) = head {
            self.built.push(t)
        }
    }
}

/// An application a walk is about to expand: its key, its definition (or
/// pair of definitions) and its parameters' size.
type At<K> = (K, (usize, usize), usize);

fn def_of(t: &Type) -> usize {
    match t {
        Type::Ref(tr) => tr.def_key().unwrap_or(0),
        _ => usize::MAX,
    }
}

fn size_of(t: &Type) -> usize {
    match t {
        Type::Ref(tr) => params_size(&tr.params),
        _ => 0,
    }
}

/// How many applications of one definition with smaller parameters a
/// path may hold before the walk takes them for growing at every level:
/// a finite type may grow a few times (`type A<'a> = [null, A<Array<i64>>]`
/// from `A<i64>`), one that grows at every level never stops.
const MAX_GROWTH: usize = 4;

/// Whether an application of `def` with parameters of `size` grows at
/// every level: `path`, the applications it is inside, holds
/// [`MAX_GROWTH`] of `def` with smaller parameters.
pub(super) fn grows_on<D: PartialEq>(path: &[(D, usize)], def: &D, size: usize) -> bool {
    path.iter().filter(|(d, s)| d == def && *s < size).count() >= MAX_GROWTH
}

/// `body` inside the application `at`: `again` when the walk expanded it
/// already, `grows` when the path holds [`MAX_GROWTH`] applications of
/// its definition with smaller parameters, as one whose parameters grow
/// at every level does (`type N<'a> = [null, ('a, N<Array<'a>>)]`), which
/// no walk finishes. Parameters that stop growing on a path come from
/// finitely many types, so a walk ends.
fn walk<K: std::hash::Hash + Eq + 'static, N: std::hash::Hash + Eq + 'static, R>(
    seen: &mut Expanded<K, N>,
    at: Option<At<K>>,
    again: R,
    grows: impl FnOnce() -> R,
    body: impl FnOnce(&mut Expanded<K, N>) -> R,
) -> R {
    let Some((k, def, size)) = at else { return body(seen) };
    if !seen.seen.insert(k) {
        return again;
    }
    if grows_on(&seen.path, &def, size) {
        return grows();
    }
    seen.path.push((def, size));
    let r = body(seen);
    seen.path.pop();
    r
}

/// Pairs already on the walk; a finite witness never revisits one.
type RepSeen = Expanded<(Type, Type), (usize, usize)>;

fn pair_at(a: &Type, b: &Type) -> Option<At<(Type, Type)>> {
    (matches!(a, Type::Ref(_)) || matches!(b, Type::Ref(_))).then(|| {
        ((a.clone(), b.clone()), (def_of(a), def_of(b)), size_of(a) + size_of(b))
    })
}

fn ref_at(t: &Type) -> Option<At<Type>> {
    matches!(t, Type::Ref(_)).then(|| (t.clone(), (def_of(t), 0), size_of(t)))
}

/// An abstract type a walk is about to look through, keyed like a named
/// type: a representation may name its own type.
fn abstract_at(t: &Type) -> Option<At<Type>> {
    match t {
        Type::Abstract { id, params } => {
            Some((t.clone(), (id.inner() as usize, 1), params_size(params)))
        }
        _ => None,
    }
}

/// What an abstract type's values compare by without an implementation
/// of the corresponding comparison trait: its representation, its
/// parameters substituted (`design/tvar_constraints.md`). A Rust-backed
/// abstract has none to look into, and one with an implementation is
/// what the implementation says.
fn compared_rep(
    env: &Env,
    id: AbstractId,
    params: &[Type],
    kind: CoreTrait,
) -> Option<Type> {
    if env.implements(kind, id) {
        return None;
    }
    Some(env.abstract_reps.get(&id)?.instantiate_with(params))
}

/// Whether two heads [`rep_collision`]'s own arms did not decide share a
/// runtime form: a primitive and a head whose form holds it, two
/// references, or two function types.
fn rep_overlaps(a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::Primitive(p), t) | (t, Type::Primitive(p)) => p.intersects(rep_prims(t)),
        (Type::ByRef(..), Type::ByRef(..)) | (Type::Fn(_), Type::Fn(_)) => true,
        _ => false,
    }
}

/// Two parts of `a` and `b` a runtime test can't tell apart, or,
/// `ordered`, two numeric types at one position, whose values have no
/// order between them. `tested`: `a` is a select arm's test, whose cells,
/// bound or open, are parts the arm binds and does not test.
fn rep_collision(
    env: &Env,
    seen: &mut RepSeen,
    tested: bool,
    open: Open,
    ordered: bool,
    a: &Type,
    b: &Type,
) -> Option<Indiscernible> {
    if tested && matches!(a, Type::TVar(_)) {
        return None;
    }
    ensure_sufficient(|| {
        let grows = || match (a, b) {
            (Type::Ref(tr), _) | (_, Type::Ref(tr)) => {
                Some(Indiscernible::Grows(tr.name.clone()))
            }
            _ => unreachable!("only a reference expands"),
        };
        walk(seen, pair_at(a, b), None, grows, |seen| {
            let pairs =
                |seen: &mut RepSeen, xs: &mut dyn Iterator<Item = (&Type, &Type)>| {
                    xs.map(|(x, y)| rep_collision(env, seen, tested, open, ordered, x, y))
                        .find(Option::is_some)
                        .flatten()
                };
            let (ha, hb) = (rep_head(env, a), rep_head(env, b));
            let node = ha
                .as_deref()
                .and_then(super::node_addr)
                .zip(hb.as_deref().and_then(super::node_addr));
            let r = seen.once(node, |seen| {
                let (Some(ha), Some(hb)) = (ha.as_deref(), hb.as_deref()) else {
                    // an open side meets each member of a union on the other
                    return match (open, ha.as_deref(), hb.as_deref()) {
                        (Open::Benign, _, _) => None,
                        (Open::Unknown, _, Some(Type::Set(ts))) => {
                            pairs(seen, &mut ts.iter().map(|t| (a, t)))
                        }
                        (Open::Unknown, Some(Type::Set(ts)), _) => {
                            pairs(seen, &mut ts.iter().map(|t| (t, b)))
                        }
                        (Open::Unknown, _, _) => {
                            open_collision(env, seen, tested, ordered, a, b)
                        }
                    };
                };
                let (a, b) = (ha, hb);
                let mut pairs =
                    |xs: &mut dyn Iterator<Item = (&Type, &Type)>| pairs(seen, xs);
                match (a, b) {
                    (Type::Bottom, _) | (_, Type::Bottom) => None,
                    (Type::Set(ts), _) => pairs(&mut ts.iter().map(|t| (t, b))),
                    (_, Type::Set(ts)) => pairs(&mut ts.iter().map(|t| (a, t))),
                    (Type::Primitive(p), Type::Primitive(q)) => {
                        let numbers = (*p | *q) & Typ::number();
                        (ordered && numbers.len() > 1)
                            .then(|| Indiscernible::Numbers(Type::Primitive(numbers)))
                    }
                    (Type::Array(x), Type::Array(y))
                    | (Type::List(x), Type::List(y))
                    | (Type::Error(x), Type::Error(y)) => {
                        pairs(&mut std::iter::once((&**x, &**y)))
                    }
                    (
                        Type::Map { key: k0, value: v0 },
                        Type::Map { key: k1, value: v1 },
                    ) => pairs(&mut [(&**k0, &**k1), (&**v0, &**v1)].into_iter()),
                    (Type::Tuple(x), Type::Tuple(y)) if x.len() == y.len() => {
                        pairs(&mut x.iter().zip(y.iter()))
                    }
                    (Type::Struct(x), Type::Struct(y))
                        if x.len() == y.len()
                            && x.iter().zip(y.iter()).all(|(x, y)| x.0 == y.0) =>
                    {
                        pairs(&mut x.iter().zip(y.iter()).map(|(x, y)| (&x.1, &y.1)))
                    }
                    (Type::Variant(x, xa, _), Type::Variant(y, ya, _))
                        if x == y && xa.len() == ya.len() =>
                    {
                        pairs(&mut xa.iter().zip(ya.iter()))
                    }
                    // the tag tells them apart
                    (Type::Variant(x, _, _), Type::Variant(y, _, _)) if x != y => None,
                    // a Graphix-minted value carries its params and the test compares
                    // them; a Rust-backed one carries only its id, so two of its
                    // instantiations share one runtime form
                    (
                        a @ Type::Abstract { id: x, params: px },
                        b @ Type::Abstract { id: y, params: py },
                    ) if x == y => {
                        let rust_backed = env.abstract_reps.get(x).is_none();
                        let same = px.len() == py.len()
                            && px
                                .iter()
                                .zip(py.iter())
                                .all(|(p, q)| union_identical(p, q));
                        (rust_backed && !same)
                            .then(|| Indiscernible::Pair(a.clone(), b.clone()))
                    }
                    // a runtime test can't tell a reference's mutability
                    (Type::ByRef(m0, x), Type::ByRef(m1, y)) if m0 == m1 && x == y => {
                        None
                    }
                    (Type::Fn(x), Type::Fn(y)) if x == y => None,
                    (a, b) => rep_overlaps(a, b)
                        .then(|| Indiscernible::Pair(a.clone(), b.clone())),
                }
            });
            seen.keep(ha);
            seen.keep(hb);
            r
        })
    })
}

impl Type {
    /// Two parts, one of `self` and one of `t`, that are different types
    /// with one runtime form: a value of either passes a runtime test for
    /// the other (two references; two function types; a Rust-backed
    /// abstract at two instantiations; a primitive and a head whose values
    /// carry it, `array` and `Array<i64>`). A test that tells them apart
    /// can't tell such a value's type.
    pub fn rep_collision(&self, env: &Env, t: &Type) -> Option<Indiscernible> {
        rep_collision(env, &mut RepSeen::default(), false, Open::Benign, false, self, t)
    }

    /// [`Self::rep_collision`] with an open cell read as any type it may
    /// yet bind ([`Open::Unknown`]): what a definition must rule out.
    pub fn rep_collision_open(&self, env: &Env, t: &Type) -> Option<Indiscernible> {
        rep_collision(env, &mut RepSeen::default(), false, Open::Unknown, false, self, t)
    }

    /// [`Self::rep_collision_open`] for a select arm testing for `self`
    /// among `t`: a cell of the test's, bound or open, is a part the arm
    /// binds and does not test (a bind, a slice's rest), so it collides
    /// with nothing; an open one of `t`'s may be any type, even where the
    /// arm's capture shares it.
    pub fn rep_collision_tested(&self, env: &Env, t: &Type) -> Option<Indiscernible> {
        rep_collision(env, &mut RepSeen::default(), true, Open::Unknown, false, self, t)
    }
}

/// Why a type is not `Discernible` or not `Ordered`.
pub enum Indiscernible {
    /// Two members of one union with one runtime form.
    Pair(Type, Type),
    /// A reference or a function, under `Ordered`: neither has an order.
    Unordered(Type),
    /// A type holding two numeric types, under `Ordered`: their values
    /// have no order between them.
    Numbers(Type),
    /// A numeric type beside an open cell, under `Ordered`: the cell may
    /// yet be another numeric type.
    OpenNumber(Type),
    /// The name of a definition whose parameters grow as it recurses, so
    /// no walk decides it.
    Grows(ModPath),
    /// An abstract type without the requested comparison implementation, whose values
    /// compare by a representation that fails one of the above; its
    /// representation is its module's, so only its name is told.
    Abstract(Type, CoreTrait),
}

impl Indiscernible {
    /// The refusal of `what`, a runtime test for `test` among `rest`,
    /// which a [`rep_collision`] found can't tell this apart.
    pub fn untold(&self, what: &str, test: &Type, rest: &Type) -> anyhow::Error {
        match self {
            Indiscernible::Pair(a, b) => anyhow::anyhow!(
                "{what} can't tell {} from {}: both have the same runtime form; wrap \
                 them in distinct variants",
                a.resolve_tvars(),
                b.resolve_tvars()
            ),
            Indiscernible::Grows(name) => anyhow::anyhow!(
                "{what} tests for {} among {}, which hold {name}, whose parameters grow \
                 as it recurses, so whether their runtime forms differ can't be decided",
                test.resolve_tvars(),
                rest.resolve_tvars()
            ),
            Indiscernible::Unordered(_)
            | Indiscernible::Numbers(_)
            | Indiscernible::OpenNumber(_) => {
                unreachable!("a runtime test is no order")
            }
            Indiscernible::Abstract(..) => {
                unreachable!("a runtime test does not look into an abstract type")
            }
        }
    }

    /// Why an abstract type's values fail a comparison: they compare by
    /// a representation outside its module's sight.
    pub fn by_representation(a: &Type, kind: CoreTrait) -> CompactString {
        format_compact!(
            "{} has no {kind:?} implementation, so its values compare by its \
             representation, which can't tell them apart or order them here; \
             implement {kind:?} for it",
            a.resolve_tvars()
        )
    }

    /// Why a type holding two numeric types has no order.
    pub const NUMBERS: &str = "more than one numeric type: values of two numeric \
                               types have no order between them; cast them to one";

    /// Why a type holding a numeric type beside an open cell has no order.
    pub const OPEN_NUMBER: &str = "beside a type variable that isn't known here and \
                                   may be another numeric type, whose values have no \
                                   order with these: annotate it";

    /// What an unordered type is, and how `==` and `!=` compare it.
    pub fn unordered_kind(t: &Type) -> (&'static str, &'static str) {
        match t {
            Type::Fn(_) => ("function", "by identity"),
            _ => ("reference", "by what they point to"),
        }
    }
}

fn rep_ambiguity(
    env: &Env,
    seen: &mut Expanded<Type>,
    open: Open,
    keys_only: bool,
    refs: bool,
    t: &Type,
) -> Option<Indiscernible> {
    ensure_sufficient(|| {
        let grows = || match t {
            Type::Ref(tr) => Some(Indiscernible::Grows(tr.name.clone())),
            _ => unreachable!("only a reference expands"),
        };
        walk(seen, ref_at(t), None, grows, |seen| {
            let head = rep_head(env, t)?;
            let r = seen.once(super::node_addr(&head), |seen| {
            let t = &*head;
            let mut parts = |ts: &mut dyn Iterator<Item = &Type>| {
                ts.map(|t| rep_ambiguity(env, seen, open, keys_only, refs, t))
                    .find(Option::is_some)
                    .flatten()
            };
            let ordered = refs && !keys_only;
            match &t {
                Type::Primitive(p) if ordered && (*p & Typ::number()).len() > 1 => {
                    Some(Indiscernible::Numbers(t.clone()))
                }
                Type::Set(ts) => {
                    if !keys_only {
                        for (i, a) in ts.iter().enumerate() {
                            for b in &ts[i + 1..] {
                                let c = rep_collision(
                                    env,
                                    &mut RepSeen::default(),
                                    false,
                                    open,
                                    ordered,
                                    a,
                                    b,
                                );
                                if c.is_some() {
                                    return c;
                                }
                            }
                        }
                    }
                    parts(&mut ts.iter())
                }
                Type::Map { key, value } => {
                    let keys = match keys_only {
                        true => rep_ambiguity(
                            env,
                            &mut Expanded::default(),
                            open,
                            false,
                            true,
                            key,
                        ),
                        false => None,
                    };
                    keys.or_else(|| parts(&mut [&**key, &**value].into_iter()))
                }
                Type::Array(e) | Type::List(e) | Type::Error(e) => {
                    parts(&mut std::iter::once(&**e))
                }
                Type::Tuple(ts) | Type::Variant(_, ts, _) => parts(&mut ts.iter()),
                Type::Struct(fs) => parts(&mut fs.iter().map(|(_, t, _)| t)),
                Type::ByRef(..) | Type::Fn(_) if ordered => {
                    Some(Indiscernible::Unordered(t.clone()))
                }
                Type::Abstract { id, params } => {
                    let kind = if ordered { CoreTrait::Ord } else { CoreTrait::Eq };
                    let rep = compared_rep(env, *id, params, kind)?;
                    let inside = walk(
                        seen,
                        abstract_at(t),
                        None,
                        || None,
                        |seen| rep_ambiguity(env, seen, open, keys_only, refs, &rep),
                    );
                    seen.keep(Some(Cow::Owned(rep)));
                    let inside = inside?;
                    let open = |t: &Type| matches!(t, Type::TVar(tv) if tv.open_cell().is_some());
                    // an unknown part is judged as it would be outside
                    Some(match inside {
                        Indiscernible::Pair(a, b) if open(&a) || open(&b) => {
                            Indiscernible::Pair(a, b)
                        }
                        Indiscernible::OpenNumber(n) => Indiscernible::OpenNumber(n),
                        Indiscernible::Grows(n) => Indiscernible::Grows(n),
                        _ => Indiscernible::Abstract(t.clone(), kind),
                    })
                }
                _ => None,
            }
            });
            seen.keep(Some(head));
            r
        })
    })
}

impl Type {
    /// Why this type is not `Discernible`: two members of one union
    /// anywhere in it (not under a reference, a function or an abstract
    /// type) with one runtime form, which comparing its values can't tell
    /// apart; with `ordered`, why it is not `Ordered`: that, or a
    /// reference in the same places.
    pub fn indiscernible(
        &self,
        env: &Env,
        open: Open,
        ordered: bool,
    ) -> Option<Indiscernible> {
        rep_ambiguity(env, &mut Expanded::default(), open, false, ordered, self)
    }

    /// Why a map key type this type holds is not `Ordered`.
    pub fn map_key_failure(&self, env: &Env, open: Open) -> Option<Indiscernible> {
        rep_ambiguity(env, &mut Expanded::default(), open, true, false, self)
    }

    /// The refusal of `self` as a binding of `'name: bound` (`Discernible`
    /// or `Ordered`) for the reason `why`.
    pub fn not_discernible(
        &self,
        bound: &Type,
        name: &str,
        why: &Indiscernible,
    ) -> anyhow::Error {
        format_with_flags(PrintFlag::DerefTVars, || {
            let t = self.resolve_tvars();
            let what = match name.starts_with('_') {
                true => format_compact!("{t} must be {bound} here, but it"),
                false => format_compact!("'{name} must be {bound}, but {t}"),
            };
            match why {
                Indiscernible::Pair(a, b) => anyhow::anyhow!(
                    "{what} holds {} and {}, which have the same runtime form; wrap \
                     them in distinct variants",
                    a.resolve_tvars(),
                    b.resolve_tvars()
                ),
                Indiscernible::Grows(name) => anyhow::anyhow!(
                    "{what} holds {name}, whose parameters grow as it recurses, so \
                     whether its members have distinct runtime forms can't be decided"
                ),
                Indiscernible::Unordered(r) => {
                    let (kind, how) = Indiscernible::unordered_kind(r);
                    let r = r.resolve_tvars();
                    let holds = match r == t {
                        true => format_compact!("is a {kind}"),
                        false => format_compact!("holds the {kind} {r}"),
                    };
                    anyhow::anyhow!(
                        "{what} {holds}: {kind}s have no order, and only == and != \
                         compare them, {how}"
                    )
                }
                Indiscernible::Numbers(n) => anyhow::anyhow!(
                    "{what} holds {}, {}",
                    n.resolve_tvars(),
                    Indiscernible::NUMBERS
                ),
                Indiscernible::OpenNumber(n) => anyhow::anyhow!(
                    "{what} holds {}, {}",
                    n.resolve_tvars(),
                    Indiscernible::OPEN_NUMBER
                ),
                Indiscernible::Abstract(a, kind) => {
                    anyhow::anyhow!(
                        "{what} holds {}",
                        Indiscernible::by_representation(a, *kind)
                    )
                }
            }
        })
    }
}

fn reference_rep(
    env: &Env,
    id: AbstractId,
    params: &Arc<[Type]>,
    kind: CoreTrait,
) -> Option<Type> {
    let typ = Type::Abstract { id, params: params.clone() };
    if env.find_impl(kind.id(), &typ).is_ok_and(|im| im.is_some()) {
        return None;
    }
    Some(env.abstract_reps.get(&id)?.instantiate_with(params))
}

/// Whether a comparison of values of `t` meets a reference: one where
/// [`Type::indiscernible`] looks.
fn compared_ref(env: &Env, seen: &mut Expanded<Type>, t: &Type, kind: CoreTrait) -> bool {
    ensure_sufficient(|| {
        walk(
            seen,
            ref_at(t),
            false,
            || true,
            |seen| {
                let Some(t) = resolved_head(env, t) else { return false };
                let t = &*t;
                let mut any = |ts: &mut dyn Iterator<Item = &Type>| {
                    Iterator::any(&mut &mut *ts, |t| compared_ref(env, seen, t, kind))
                };
                match t {
                    Type::Any | Type::ByRef(..) => true,
                    Type::Set(ts) | Type::Tuple(ts) | Type::Variant(_, ts, _) => {
                        any(&mut ts.iter())
                    }
                    Type::Array(e) | Type::List(e) | Type::Error(e) => {
                        any(&mut std::iter::once(&**e))
                    }
                    Type::Struct(fs) => any(&mut fs.iter().map(|(_, t, _)| t)),
                    Type::Map { key, value } => any(&mut [&**key, &**value].into_iter()),
                    Type::Abstract { id, params } => {
                        match reference_rep(env, *id, params, kind) {
                            None => false,
                            Some(rep) => walk(
                                seen,
                                abstract_at(t),
                                false,
                                || true,
                                |seen| compared_ref(env, seen, &rep, kind),
                            ),
                        }
                    }
                    _ => false,
                }
            },
        )
    })
}

/// Why a `Cast` implementation refused: the message of its
/// `` `InvalidCast(why) ``, else the error as it prints.
fn refusal(e: &Value) -> Cow<'static, str> {
    match rep::variant(e) {
        Some((_, [Value::String(s)])) => Cow::Owned(s.to_string()),
        _ => Cow::Owned(format!("{}", NakedPrefix(e))),
    }
}

/// The data inside `v`, the object of one entry keyed `name` a text
/// format writes an abstract value of the type `name` as (read as its
/// `[key, value]` pairs).
fn unwrapped<'a>(name: &str, v: &'a Value) -> Option<&'a Value> {
    match &rep::array_elems(v)?[..] {
        [entry] => match &rep::array_elems(entry)?[..] {
            [Value::String(k), inside] if k == name => Some(inside),
            _ => None,
        },
        _ => None,
    }
}

/// Whether `v` is the variant `t` names: one held, or a payload
/// variant's data, its tag and then its payload (a data string is a
/// string, never a tag here).
fn carries_tag(env: &Env, t: &Type, v: &Value) -> bool {
    let Some(head) = rep_head(env, t) else { return false };
    let Type::Variant(tag, args, _) = &*head else { return false };
    match rep::read_variant(v) {
        Some((t, rep::Parts::Held(p))) => t == tag && p.len() == args.len(),
        Some((t, rep::Parts::Data(p))) => {
            !args.is_empty() && t == tag && p.len() == args.len()
        }
        None => false,
    }
}

/// The primitives `v` reads as without leaving its runtime class: every
/// number for a number, its own primitive for anything else.
fn own_class(v: &Value) -> BitFlags<Typ> {
    let t = Typ::get(v);
    if t.is_number() { Typ::number() } else { t.into() }
}

/// Push the members of the union member `t` a union tries one at a time:
/// a name or a variable naming a union is that union's members, and a
/// primitive set its parts in each of `classes` and the rest, so a union
/// reads each part with the members of its class. A name met again adds
/// nothing: `named` holds those met.
fn members_by_class<'a>(
    env: &Env,
    t: Cow<'a, Type>,
    classes: [BitFlags<Typ>; 2],
    out: &mut SmallVec<[Cow<'a, Type>; 8]>,
    named: &mut LPooled<Vec<RefKey>>,
    depth: usize,
) {
    if depth > MAX_UNION_NESTING {
        return out.push(t);
    }
    if let Type::Ref(tr) = &*t
        && let Some(k) = ref_key(tr)
    {
        if named.contains(&k) {
            return;
        }
        named.push(k);
    }
    let head = match &t {
        Cow::Borrowed(t) => rep_head(env, t),
        Cow::Owned(t) => rep_head(env, t).map(|h| Cow::Owned(h.into_owned())),
    };
    ensure_sufficient(|| match &head {
        Some(Cow::Borrowed(Type::Set(ms))) => ms.iter().for_each(|m| {
            members_by_class(env, Cow::Borrowed(m), classes, out, named, depth + 1)
        }),
        Some(Cow::Owned(Type::Set(ms))) => ms.iter().for_each(|m| {
            members_by_class(env, Cow::Owned(m.clone()), classes, out, named, depth + 1)
        }),
        Some(h) if let Type::Primitive(p) = &**h => {
            let [own, parsed] = classes;
            let parts = [*p & own, *p & parsed, *p & !own & !parsed];
            match parts.iter().filter(|p| !p.is_empty()).count() {
                0 | 1 => out.push(t),
                _ => parts
                    .into_iter()
                    .filter(|p| !p.is_empty())
                    .for_each(|p| out.push(Cow::Owned(Type::Primitive(p)))),
            }
        }
        _ => out.push(t),
    })
}

/// Whether the union member `t`, past [`members_by_class`], may read the
/// scalar a text parses as ([`in_own_class`] of it) where the text's own
/// class does not hold it.
fn takes_scalar(env: &Env, t: &Type) -> bool {
    match rep_head(env, t).as_deref() {
        Some(Type::Primitive(p)) => p.intersects(text_scalars()),
        Some(Type::Set(_)) => true,
        _ => false,
    }
}

/// The classes of the scalars a text parses as ([`parsed_scalar`]).
fn text_scalars() -> BitFlags<Typ> {
    Typ::number() | Typ::Bool | Typ::DateTime | Typ::Duration
}

/// How many unions deep [`members_by_class`] looks into a union's members;
/// a deeper one is tried whole.
const MAX_UNION_NESTING: usize = 64;

/// Whether `t` may read `v` without leaving `v`'s runtime class, which a
/// union tries before any conversion across classes.
fn in_own_class(env: &Env, t: &Type, v: &Value) -> bool {
    ensure_sufficient(|| match rep_head(env, t).as_deref() {
        None | Some(Type::Bottom) => true,
        Some(Type::Primitive(s)) => rep::kind(v).is_none() && s.intersects(own_class(v)),
        Some(Type::Set(ts)) => ts.iter().any(|t| in_own_class(env, t, v)),
        Some(Type::Array(e)) => {
            rep::array_elems(v).is_some_and(|a| a.iter().all(|x| in_own_class(env, e, x)))
        }
        Some(Type::List(e)) if list::is_list(v) => {
            list::Iter::new(v.clone()).all(|x| in_own_class(env, e, &x))
        }
        Some(Type::List(e)) => {
            rep::kind(v).is_none()
                && rep::array_elems(v)
                    .is_some_and(|a| a.iter().all(|x| in_own_class(env, e, x)))
        }
        Some(Type::Tuple(ts)) => rep::tuple_elems(v).is_some_and(|a| {
            a.len() == ts.len() && ts.iter().zip(a).all(|(t, x)| in_own_class(env, t, x))
        }),
        Some(Type::Struct(fs)) => rep::struct_fields(v).is_some_and(|a| {
            a.len() == fs.len()
                && fs.iter().zip(a).all(|((_, t, _), f)| {
                    rep::field(f).is_some_and(|(_, x)| in_own_class(env, t, x))
                })
        }),
        Some(Type::Variant(tag, ps, _)) => rep::variant(v).is_some_and(|(t, a)| {
            t == tag
                && a.len() == ps.len()
                && ps.iter().zip(a).all(|(p, x)| in_own_class(env, p, x))
        }),
        Some(Type::Map { key, value }) => match v {
            Value::Map(m) => m
                .into_iter()
                .all(|(k, x)| in_own_class(env, key, k) && in_own_class(env, value, x)),
            _ => false,
        },
        Some(Type::Error(_)) => matches!(v, Value::Error(_)),
        Some(_) => matches!(v, Value::Abstract(_)),
    })
}

/// Whether unmarked struct data, `[name, value]` pairs, reads into the
/// fields `fs` with each value in its own class.
fn fields_in_own_class(
    env: &Env,
    fs: &[(ArcStr, Type, crate::typ::WrittenAt)],
    a: &ValArray,
) -> bool {
    a.iter().all(|p| match p {
        Value::Array(kv) if kv.len() == 2 => match (&kv[0], &kv[1]) {
            (Value::String(n), x) => fs
                .iter()
                .find(|(f, _, _)| f == n)
                .is_some_and(|(_, t, _)| in_own_class(env, t, x)),
            _ => false,
        },
        _ => false,
    })
}

/// `v` as the primitive `t`, exactly or not at all. A number converts to
/// a number, and a bool (1 or 0), a datetime (Unix seconds) and a
/// duration (seconds) to an integer or a float, never a decimal; a number
/// to a datetime or a duration (an integer target holds it exactly, a
/// fraction is dropped), a string to what it parses as, a number, bool,
/// datetime or duration to its plain printed form, a decimal to the float
/// nearest it, a float to its decimal (a whole one with a point), and an
/// array of numbers to bytes. Nothing converts to null or to an error.
fn convert(v: Value, t: Typ) -> Option<Value> {
    convert_text(v, t, parsed_scalar)
}

/// [`convert`], `scalar` reading a text as the scalar it parses as
/// ([`parsed_scalar`]) where the conversion needs it.
fn convert_text(
    v: Value,
    t: Typ,
    scalar: impl FnOnce(&str) -> Option<Value>,
) -> Option<Value> {
    match (&v, t) {
        (_, Typ::Null | Typ::Error) => None,
        (v, t) if Typ::get(v) == t => Some(v.clone()),
        (Value::Array(a), Typ::Bytes) => {
            let mut bytes: LPooled<Vec<u8>> = LPooled::take();
            for el in a.iter() {
                match convert(el.clone(), Typ::U8)? {
                    Value::U8(b) => bytes.push(b),
                    _ => return None,
                }
            }
            Some(Value::Bytes(bytes::Bytes::copy_from_slice(&bytes).into()))
        }
        (Value::Array(_) | Value::Map(_), Typ::Map | Typ::Array) => v.cast(t),
        (Value::String(s), t) => parse(s, t, scalar),
        (_, Typ::String) => printed(&v),
        (_, Typ::DateTime | Typ::Duration) if Typ::get(&v).is_number() => v.cast(t),
        (_, t) if t.is_integer() && counts(&v) => integer(&v, t),
        (Value::Decimal(d), Typ::F64) => {
            format_compact!("{d}").parse().ok().map(Value::F64)
        }
        (Value::Decimal(d), Typ::F32) => {
            format_compact!("{d}").parse().ok().map(Value::F32)
        }
        (Value::F32(_) | Value::F64(_), Typ::Decimal) => float_decimal(&v),
        (_, t) if t.is_number() && counts(&v) => v.cast(t),
        _ => None,
    }
}

// CR claude for claude: [bug/low] A float whose shortest form has 17 significant
// digits loses digits on its way to a decimal: Value::cast(Typ::Decimal) rounds
// 0.30000000000000004 to 0.3. JSON reads a number in a decimal position as the f64
// wherever the f64 prints back as the text (graphix-package-json's number and
// reads_back), so an Array<decimal> holding decimal:0.30000000000000004 writes that
// text and reads back as 0.3 (main too); 16-digit decimals read back. Convert a float
// through its shortest text, as f32_as_written does, or have json keep the decimal
// for a decimal position. Probe: ~/ptmp/cr29/scratch/review-r6/B-r2-1/probes/dec16.gx
// (prints decimal:0.3 for the fourth element) (float-decimal-digits-01)
/// The float `v` as a decimal; a whole one keeps a point (scale 1) where
/// the decimal can hold it, so it prints as the float does (`3.0`).
fn float_decimal(v: &Value) -> Option<Value> {
    let Value::Decimal(d) = v.clone().cast(Typ::Decimal)? else { return None };
    if d.scale() > 0 {
        return Some(Value::Decimal(d));
    }
    let mut pointed = *d;
    pointed.rescale(1);
    Some(Value::Decimal(match pointed.scale() == 1 && pointed == *d {
        true => Arc::new(pointed),
        false => d,
    }))
}

/// The string `s` read as the primitive `t`, `scalar` reading it as the
/// scalar it parses as.
fn parse(
    s: &ArcStr,
    t: Typ,
    scalar: impl FnOnce(&str) -> Option<Value>,
) -> Option<Value> {
    if t == Typ::DateTime
        && let Ok(d) = chrono::DateTime::parse_from_rfc3339(s)
    {
        return Some(Value::DateTime(Arc::new(d.with_timezone(&chrono::Utc))));
    }
    if t.is_integer()
        && let Ok(n) = s.trim().parse::<i128>()
    {
        return of_int(n, t);
    }
    let plain = match t {
        Typ::Decimal => {
            let s = s.trim();
            s.parse::<rust_decimal::Decimal>().ok().map(|d| Value::Decimal(d.into()))
        }
        Typ::F64 => s.trim().parse::<f64>().ok().map(Value::F64),
        Typ::F32 => s.trim().parse::<f32>().ok().map(Value::F32),
        _ => None,
    };
    if plain.is_some() {
        return plain;
    }
    if text_scalars().contains(t) {
        return scalar(s).and_then(|p| convert(p, t));
    }
    if parses_to_collection(s) {
        return None;
    }
    match literal(s) {
        Ok(Value::String(_)) | Err(_) => None,
        Ok(parsed) => convert(parsed, t),
    }
}

/// A number, bool, datetime or duration printed without its type: a
/// datetime in RFC3339 and a duration in seconds, as json writes them,
/// and a whole float with its point.
fn printed(v: &Value) -> Option<Value> {
    let s = match v {
        Value::Bool(b) => format_compact!("{b}"),
        Value::DateTime(d) => format_compact!("{}", d.to_rfc3339()),
        Value::Duration(d) => {
            format_compact!("{}", PlainScalar(&Value::F64(d.as_secs_f64())))
        }
        Value::Decimal(d) => format_compact!("{d}"),
        v if Typ::get(v).is_number() => format_compact!("{}", PlainScalar(v)),
        _ => return None,
    };
    Some(Value::String(s.as_str().into()))
}

/// `v`, a number, bool, datetime or duration, as the integer type `t`, exactly
/// or not at all; a fraction, of a float or of a second, is dropped
/// first.
fn integer(v: &Value, t: Typ) -> Option<Value> {
    let n = match v {
        Value::I8(n) => *n as i128,
        Value::I16(n) => *n as i128,
        Value::I32(n) | Value::Z32(n) => *n as i128,
        Value::I64(n) | Value::Z64(n) => *n as i128,
        Value::U8(n) => *n as i128,
        Value::U16(n) => *n as i128,
        Value::U32(n) | Value::V32(n) => *n as i128,
        Value::U64(n) | Value::V64(n) => *n as i128,
        Value::F32(f) => float_int(*f as f64)?,
        Value::F64(f) => float_int(*f)?,
        Value::Bool(b) => *b as i128,
        Value::DateTime(d) => {
            let secs = d.timestamp() as i128;
            secs + (secs < 0 && d.timestamp_subsec_nanos() > 0) as i128
        }
        Value::Duration(d) => d.as_secs() as i128,
        v => return v.clone().cast(t),
    };
    of_int(n, t)
}

// XCR claude for claude: [design/medium] D11's round trip made a float's or decimal's TEXT
// held by no integer member, but a float VALUE is still held by one, so a value and its
// print now read differently: with x = 3.0, cast<[i64, decimal]>(x) is i64 3 and
// cast<[i64, decimal]>("[x]") is decimal 3.0 (main gave i64 3 for both); [u8, f32]
// likewise. Text also crosses classes now: cast<[i64, datetime]>("3.0") is a datetime,
// cast<[i64, duration]>("3.0") is 3.s, cast<[u8, f64]>("1e2") is f64 100.0 (main: u8
// 100). Inside a composite the value path still wins: str::parse<Array<[u8, f32]>>("[3.0,
// 4.5]") is [u8:3, f32:4.5]. Rule needed: (a) text carries its class and a value loses it
// where the target lacks it (as now), or (b) the same exactness for values: no integer
// member holds a float value when a real member of the target holds it. Recommend (b):
// one rule for text and value, and the composite path agrees with the scalar one. Probes:
// ~/tmp/cr29/scratch/review2/RV3/{value_vs_text,cast_cross,parse_vs_cast}.gx
// (holds-exactly-class-01)
// 2026-10-30 claude: Eric ruled (2026-10-30, cr-discuss round 4 quick picks): option (b),
// the same exactness for values: no integer member holds a float value when a real member
// of the target holds it, so a value and its text read alike.
// 2026-10-30 claude: Built Eric's option (b): an integer member holds a float or a
// decimal, value or text, only where no real member holds it exactly. holds_exactly holds
// a float's text by an integer again; a union's pick among its exact readings (Choice)
// takes the first unless it reads a real as an integer and a later reading keeps every
// real real; a primitive set picks the same way among its bits; RealReads counts at the
// leaves (through the union memo) so a union of composites is judged per leaf. Probes
// now: value_vs_text decimal/decimal and f32/f32; cast_cross [i64, datetime] and [i64,
// duration] "3.0" are i64 3, "1e2" f64 100; parse_vs_cast arrays [f32:3.0, f32:4.5].
// Pins, all failing on main's rule: lang::types::{cast_float_value_and_text_read_alike,
// cast_whole_float_without_a_real_member, cast_float_elements_read_as_the_scalar,
// cast_union_prefers_exact_composite},
// lib_tests::json::json_whole_floats_read_back_beside_an_integer_tuple.
// 2026-10-30 claude: Eric ruled (2026-10-30, round 4 follow-up): "Yes, we should allow
// rounding to match." Where no member holds a number exactly, a real member that holds it
// rounded to its precision (an f64 into an f32, a decimal or a long integer into an f64)
// comes before one that would drop part of it, as a value and as text, at every element
// and leaf by leaf in a union of tuples or structs; an exact integer member still comes
// first (cast<[i64, f32]>(1234567900.0) is i64). Built as Holds (Exactly, Rounded,
// Partly), the rounds a union walks, with holds_rounded (no infinity a finite value
// overflows to). An integer a real member rounds, cast<[f32, u8]>(16777217) and its text,
// is f32 16777216, as on main. Pin: lang::types::cast_rounding_before_a_dropped_part
// (fails on 16f7ced15).
// 2026-10-30 claude: Judged with the follow-up, as the same rule read the other way: an
// integer, value or text, goes to a real member only where no integer member holds it
// exactly. A flat primitive union already read so (integers first in canonical order); a
// union naming or ordering its members otherwise did not (type B = u8: cast<[f64, B]>(2)
// was f64 2.0 on main, and json::read of a written u8 2 into the JsonData-admitted [f64,
// B] gave f64 2.0). The leaf count is NumReads (numbers read across the integer/real line,
// numbers read as no number) and Choice passes over any crossing reading. With both kinds
// kept, JsonData/TomlData admit an f32 beside an integer no wider than 16 bits (textdata
// Shape::Int { wide }, Shape::F32); [f32, i32] and wider stay refused (1234567936 writes
// 1234568000.0, which the integer holds exactly). Pins:
// lang::types::cast_a_number_keeps_its_kind,
// lib_tests::json::json_f32_beside_a_narrow_integer_reads_back,
// lib_tests::toml::toml_f32_beside_a_narrow_integer_reads_back (all fail on 322a66957 or
// 16f7ced15's cast.rs).
// 2026-10-30 claude: Round 2 (review7 B): the JsonData/TomlData relaxation of 13b0e1a9a
// judged a tuple's or struct's positions apart while the reader judges a member's reading
// whole, so [(f32, u16), (i64, f32)], [(f32, u8), (f64, f32)] and the struct form were
// admitted and read back as the other member; [(f32, u8), (f64, f64)] was such a hole on
// main. textdata.rs now follows the union reader's pick over classes of written values
// (Token): for each, how the writer's own member and the other read it (rounded, outside
// the data's class, crossing the integer/real line, a number as no number), a composite's
// parts together, then the round, a payload variant's tag, the pass (tuple, struct, own
// class, the rest; a primitive set by class and bit), the union's order and Choice; a map
// key union is also judged from its text. That closes the review's holes and the
// cross-class ones main had ([f32, string], [(string, f32), (i64, f64)], [(i64, duration),
// (u8, string)], Map<[decimal, datetime], i64>), and admits unions main refused that read
// back ([{a: i64}, {a: i64, b: [i64, null]}] in JSON, [`B(u8), (datetime, f32)]).
// Validated against the cast with JsonData off over ~330 random and hand-made unions
// (~/tmp/cr29/scratch/laneB/r3/{fuzz,oracle}.py): no union admitted that fails to read
// back. Pins (each failing on 5fa76d1e2):
// lib_tests::json::{json_target_f32_tuple_beside_an_exact_reader_refused,
// json_target_f32_tuple_beside_a_wide_integer_refused,
// json_target_f32_struct_beside_an_exact_reader_refused,
// json_target_duration_tuple_beside_an_exact_reader_refused,
// json_target_f32_beside_a_string_refused,
// json_target_string_tuple_beside_a_number_tuple_refused,
// json_target_readings_outside_the_class_refused,
// json_target_key_text_read_as_another_member_refused,
// json_unions_read_back_by_their_pick},
// lib_tests::toml::{toml_target_f32_tuple_beside_an_exact_reader_refused,
// toml_target_string_tuple_beside_a_number_tuple_refused}, textdata's unit tests. The open
// semantics questions are holds-exactly-open-01.
// 2026-10-30 claude: Also: a nested union counts as in the data's class where
// any member is, as in_own_class does, so [(i64, f64), (u8, [i64, string])] is refused
// (the cast tries (u8, [i64, string]) first and reads (3, 3.5) back as (u8:3, "3.5")).
// Pin: lib_tests::json::json_target_union_part_in_the_class_refused.
// 2026-10-30 claude: Round 3 (review7 B r2): textdata's walk is now a least fixpoint
// (Shapes::least_outs over a Fixpoint memo, one entry per writer/key/reader pair a
// text_ambiguity call meets; each pair starts with no outcome, passes repeat until one
// changes nothing or rereads no pair it was computing). A pair met again along a walk
// claimed nothing, which Outs::all spread to the whole product, so members that recurse
// into each other were admitted: [TA, TB] over TA = [i64, `Cons(f32, TA)], TB = [f64,
// `Cons(f64, TB)] read `Cons(f32:0.1, 3) back as TB's (main too). The memo also ends the
// exponential check: L_k = [Array<L_{k-1}>, (L_{k-1}, string)] took 0.94/12.3/193 s at k =
// 10/12/14 (debug); now 0.15-0.17 s debug, against main's quick 0.12-0.13 s. Pins:
// lib_tests::json::json_target_recursive_members_refused and
// lib_tests::toml::toml_target_recursive_members_refused (fail in all four modes on the
// old walk), json_recursive_members_read_back and toml_recursive_members_read_back,
// textdata's a_recursive_pair_claims_what_its_values_reach and
// a_check_evaluates_each_pair_once (11,417,790 evaluations over 54 nodes at L10 on the old
// walk; 191 now, bound 2n^2). graphix-package-json's admitted_unions_read_back checks
// textdata against the real writer and cast over the unit tests' unions (fails when the
// cast's rounds or Choice change).
// 2026-10-30 claude: Round 3, second commit: a sweep of recursive unions against the cast
// (~/tmp/cr29/scratch/laneB/r4/rec.py) found two holes the round-2 model had without
// recursion, on main too. (1) The cast reads a bool into a real member as 1 or 0 in the
// Rounded round (holds_rounded) and textdata's real leaves read no bool: type A =
// Array<f64>; type P = [bool, (f32, bool)]; [A, P] read (f32:0.1, true) back as [0.1,
// 1.0]. Leaf::reads_in now has F32/F64/Decimal reading Bool rounded. (2) in_own_class
// holds no unmarked tuple or struct, so one is in the data's class only where the union's
// pick tries it as a member (the rank), never as a part of a composite or a member of a
// union nested in a member; textdata counted it by its parts everywhere: [(Array<string>,
// i64), ((u8, u8), u8)] read ((1, 2), 3) back as (["1", "2"], 3). Shapes::part and
// part_class mark such a tuple or struct outside the class. Pins:
// lib_tests::json::{json_target_bool_read_as_a_real_refused,
// json_target_nested_tuple_outside_the_class_refused} (both admitted by 6e2c8147f),
// textdata's a_bool_rounds_into_a_real_and_a_part_is_apart (fails without either change).
// Open: whether a bool should round into a real and only partly into an integer
// (cast<[f32, u8]>(true) is f32:1.0 here, u8:1 on main) is a cast question beside
// holds-exactly-open-01.
// 2026-10-30 claude: Round 4 (review7 B r3): textdata's Shapes::node keyed its typedef
// memo on its arguments' node ids, which a primitive argument pushes fresh at every
// expansion, so json::read or toml::read into a recursive typedef applied to arguments
// (R<u64> over type R<'a> = [`Nil, `Cons('a, R<'a>)]) expanded without end and grew memory
// until killed (main too, since 2bcdcec09). The memo is now keyed by the arguments' types,
// so a typedef expands once per list of arguments, and one whose arguments grow at every
// level (type N<'a> = [`Nil, `Cons('a, N<Array<'a>>)], reachable through a written 'a:
// JsonData alone) is refused by the representation walks' growth rule (cast::grows_on),
// with its own message (TextAmbiguity::Grows). Pins, each killed at a 3-4G cap without the
// fix: textdata's an_applied_typedef_expands_once_per_arguments,
// lib_tests::json::{json_applied_recursive_typedef_reads_back,
// json_data_refuses_a_typedef_growing_its_parameters} (the latter only without the growth
// rule), lib_tests::toml::toml_applied_recursive_typedef_reads_back.
// admitted_unions_read_back now also reads back the typedef cases of textdata's unit tests
// (named(..)), and the count pin covers recursive groups
// (a_check_evaluates_each_pair_a_few_times). typ::regular::key had the same memo; fixed in
// the next commit.
// 2026-10-30 claude: Round 5 (review7 B r3 v2): (1) textdata's walks over a node's members
// (writes_text, writes_tag, holds_string, key_forms, name_written, name_read, entry and
// the key unions) recursed through unions and abstracts with no cycle guard and no memo:
// an abstract holding itself (type N = Abstract<[N, `L(i64)]>) overflowed the stack in
// --check, and a map key whose unions share members (U_i = [U_{i-1}, V_{i-1}], V_i =
// [U_{i-1}, `T_i]) walked every path (48.8 s at n=36). Each is now a fold over
// Shapes::reach (the nodes a node stands for through unions and abstracts, each met once,
// a cycle adding nothing, memoized per node). (2) A member that is itself a union had no
// pass (Shapes::pass gave None), so takes refused it beside any member that reads its
// values: [Num, string] over type Num = [i64, f64] and {a: [Opt, string], b: Array<[Opt,
// bool]>} over Opt = [i64, null] were refused where main admits and they read back. It now
// takes a scalar's pass from its class bit (in_own_class: a union is in the class where
// any member is). (3) Found by the sweep: two members holding one primitive at a position
// read a value one way, but the leaf outcomes paired every reading of one with every
// reading of the other, so type A = (i32, f32) and type B = (datetime, f32) made [A, B]
// refused (the writer's f32 rounding where the other's read exactly), main admits it and
// it reads back; outs_int now pairs a primitive's readings with themselves. Pins (each
// fails on 1ab657e8c): lib_tests::json::{json_target_abstract_holding_itself_refused,
// json_target_key_abstract_holding_itself_refused,
// json_target_abstracts_holding_each_other_refused} (stack overflow),
// json_union_typedef_members_read_back, json_typedef_tuples_sharing_an_f32_read_back,
// lib_tests::toml::toml_union_typedef_members_read_back, textdata's
// a_walk_meets_each_node_once (2,885,914,760 steps over 167 nodes on the old walks,
// instrumented; bound 8 a node), a_union_member_is_tried_by_its_class and
// one_primitive_reads_a_value_one_way; json_target_union_typedef_beside_a_tag_refused pins
// that [Num, Txt] over Txt = [string, `None] stays refused.
// 2026-10-31 claude: Round 6 (review7 B r4, the review of round 5, which was brief r4B4's
// first pass): (1) A union member whose own members write values of two classes was
// refused where main admits and it reads back: [Shape, string] over type Shape = [{w: f64,
// h: f64}, f64], [Ev, i64] and [Ev, bool] over type Ev = [`Click(i64), string], [D, bool]
// over type D = [duration, string]. Shapes::outs classed every value of such a member by
// class_of over the member whole, which a struct beside an f64 makes both in and out of
// the class. A writer union is now judged node by node (Shapes::reach, past every union
// and abstract), each value by the node that writes it; a reader union reads node by node
// too. (2) class_of stored (true, true) for a pair met again and kept what it computed
// through that as final; it is now a least fixpoint through the same Fixpoint as outs,
// made generic over its value (Lattice): [B, R] over type B = [(string, string), null],
// type R = [Array<R>, i64] reads back. (3) A pair of nodes that refer to no other node is
// computed directly and kept in no memo: a 1000-tag enum as a map key took 428 MB, now 34
// MB; a chain of n nested union typedefs still keeps about n^2 class pairs (1000 levels:
// 549 MB, main 36 MB), noted at Fixpoint. Pins, each failing without its change:
// textdata's a_union_member_is_tried_by_its_class and
// a_recursive_pair_claims_what_its_values_reach,
// lib_tests::json::{json_union_typedef_members_of_two_classes_read_back,
// json_recursive_member_beside_a_tuple_reads_back},
// lib_tests::toml::toml_union_typedef_members_of_two_classes_read_back. The review's
// sweeps, rerun: 0 holes, and every refusal main does not make fails to read back on the
// oracle (~/tmp/cr29/scratch/laneB/r7/overm*.txt).
// 2026-10-31 claude: Round 7 (review7 B r4 r2, brief r4B5): a primitive set beside another
// member was given every reading of every one of its members, and a member the value read
// back already is (cast_member's Ok(None)) was not seen to take it at once: with type
// Amount = [f64, string], [Amount, i64] and [Amount, u32] were refused, as was [Label,
// f64] over type Label = [string, i64] (main admits them and they read back). textdata now
// reads a primitive set by its pick (Shapes::pick, which prim_takes uses too): the member
// the value read back already is, else the first that holds it exactly by class and bit,
// else the first real that rounds it, a member that may miss some value of a class passing
// it on. Each reading carries a level (held, built, a number as no number, crossed) and a
// held member takes the value within its pass whatever the union's order; u32 and i32 no
// longer write an integer past an f64's precision. [Id, Val] over Id = Abstract<i64> and
// Val = [f64, string] stays refused (a real hole at 2^53+1). Pins: textdata's
// a_primitive_set_reads_by_its_pick (fails with the held rule, the pick or the 32-bit
// writes reverted, each alone),
// lib_tests::json::json_primitive_set_beside_a_member_reads_back and
// lib_tests::toml::toml_primitive_set_beside_a_member_reads_back (both refused at
// 3cb1fc3d2), json_target_abstract_beside_a_primitive_set_refused. Needs a soak.
// 2026-10-31 claude: Round 7, second commit (brief r4B5 item 3): Fixpoint restarted each
// pass from the root, so a chain of typedefs whose facts cross one back link a pass took
// O(n) passes over O(n) pairs (fact_1600 of the review's gencost.py: 10.2 s debug, main
// 0.4 s quick). It is now a top-down solver: a pair is computed when first met and again
// only when a pair it read grew (its readers are kept in its entry until the check's least
// fixpoint ends), so fact_n costs about 1.3 evaluations a pair (fact_1600 0.55 s). Pair
// keys are packed into a u64, so a group of n mutually recursive unions keeps its n^2
// pairs in about the old memory (mutual_512 2.8 GB, 3cb1fc3d2 2.6 GB, main 38 MB; 6.7 s
// against 16.5 s). Pin: textdata's a_check_evaluates_each_pair_a_few_times now bounds
// mutual 32 and fact 50/200 at twice their pairs (the old solver: mutual 32 108,420
// evaluations of 36,864 pairs, fact 50 62,996 of 1,796).
// 2026-10-31 claude: Round 7, third commit: a pair whose computation read no pair short of
// final but itself is now final when that computation ends (unless another computation of
// it is running), and only pairs not yet final keep their readers, in a side map; with the
// u64 keys a class pair costs 16 bytes (3cb1fc3d2: 32) and an outcome pair 48 (64), and a
// node's reach span serves both ways unless an abstract that writes nothing known lies on
// it. Peaks against 3cb1fc3d2 (debug, the review's cost files): chainlit_2000 951 MB /
// 1737 MB, chaintag_2000 713 / 1045, mutual_512 2042 MB 5.7 s / 2624 MB 14.1 s, tree_512
// 252 / 315, fact_1600 42 MB 0.43 s / 68 MB 7.3 s (these replace the second commit's
// figures). Pin: textdata's a_pair_reading_only_final_pairs_is_final_at_once (fails with
// the finalization off). The OVER sweep of this round found one refusal no witness
// settles, a limit of judging members in pairs: textdata-pairwise-01.
// 2026-10-31 claude: Eric ruled (round 5 D2, the open questions of holds-exactly-open-01):
// 1 keep exactness before kind; 2 a decimal made from a whole float keeps scale 1; 3 the
// members that keep the value's class come first, across the rounds; 4 numeric text is
// ordered by the class of the value it parses as; 5 keep the canonical-order tie-break; 6
// confirmed, an integer stays an integer when union members are named; 7 a bool is held
// exactly by a number of 1 or 0 (cast<[f32, u8]>(true) is u8 1). JSON/TOML no longer
// follow the cast's pick: D6 refuses untagged unions there.
// 2026-10-31 claude: Round 6 (brief r6B): the D2 rulings are built
// (holds-exactly-open-01's note). JsonData and TomlData no longer follow the cast's pick:
// D6 replaced textdata's member-by-member model (Shapes, Fixpoint, the leaf readings) with
// the tag rule, Type::text_refusal, so the textdata pins named above are deleted with it;
// the cast pins named above stand (cast_union_prefers_exact_composite,
// cast_rounding_before_a_dropped_part, cast_a_number_keeps_its_kind,
// cast_float_elements_read_as_the_scalar).
/// Whether `c`, `v` converted, holds all of `v`: converted back to `v`'s
/// class it is `v` again. A string is what it parses as, so `"3.50"` is
/// held exactly by 3.5 and not by 3, and a bool is held by the number 1
/// or 0. `parsed` is the scalar `v` parses as when it is text.
fn holds_exactly(v: &Value, parsed: Option<&Value>, c: &Value) -> bool {
    match (v, c) {
        (Value::String(_), c) => parsed.is_none_or(|p| holds_exactly(p, None, c)),
        // CR claude for eric: [design/low] D2 ruling 7 (a bool is held exactly by the
        // number 1 or 0) with ruling 5 (the canonical tie-break) reads a bool into a
        // number before a string: cast<[i64, string]>(true) is i64 1 and cast<[f32,
        // string]>(true) f32 1.0, where main gives "true" for both, since the string
        // holds the bool exactly too and a bool's class holds neither member. Pinned as
        // built in lang::types::cast_union_order (i64:1). Options: keep it; or a bool
        // tries its text before a number (the string first). Probe:
        // ~/ptmp/cr29/scratch/r6-B/realord.gx (bool-union-number-01)
        (Value::Bool(b), c) if Typ::get(c).is_number() => {
            holds_exactly(&Value::U8(*b as u8), None, c)
        }
        (v, c) => convert(c.clone(), Typ::get(v)).as_ref() == Some(v),
    }
}

/// Whether the real `c`, `v` converted, holds `v` rounded to its
/// precision: any real but an infinity a finite value overflowed to.
/// `parsed` is the scalar `v` parses as when it is text.
fn holds_rounded(v: &Value, parsed: Option<&Value>, c: &Value) -> bool {
    let infinite = |v: &Value| match v {
        Value::F32(f) => f.is_infinite(),
        Value::F64(f) => f.is_infinite(),
        _ => false,
    };
    Typ::get(c).is_real()
        && (!infinite(c)
            || match v {
                Value::String(_) => parsed.is_some_and(infinite),
                v => infinite(v),
            })
}

/// The scalar the text `s` parses as: a number, a bool, a datetime or a
/// duration; `None` for anything else.
fn parsed_scalar(s: &str) -> Option<Value> {
    literal(s).ok().filter(|p| text_scalars().contains(Typ::get(p)))
}

#[cfg(test)]
thread_local! {
    static LITERALS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// `s` read as a value literal.
fn literal(s: &str) -> Result<Value> {
    #[cfg(test)]
    LITERALS.with(|n| n.set(n.get() + 1));
    s.parse()
}

/// Whether `v` reads as a number: a number, a bool (1 or 0), a datetime
/// (its Unix seconds) or a duration (its seconds).
fn counts(v: &Value) -> bool {
    matches!(v, Value::Bool(_) | Value::DateTime(_) | Value::Duration(_))
        || Typ::get(v).is_number()
}

fn of_int(n: i128, t: Typ) -> Option<Value> {
    match t {
        Typ::I8 => i8::try_from(n).ok().map(Value::I8),
        Typ::I16 => i16::try_from(n).ok().map(Value::I16),
        Typ::I32 => i32::try_from(n).ok().map(Value::I32),
        Typ::Z32 => i32::try_from(n).ok().map(Value::Z32),
        Typ::I64 => i64::try_from(n).ok().map(Value::I64),
        Typ::Z64 => i64::try_from(n).ok().map(Value::Z64),
        Typ::U8 => u8::try_from(n).ok().map(Value::U8),
        Typ::U16 => u16::try_from(n).ok().map(Value::U16),
        Typ::U32 => u32::try_from(n).ok().map(Value::U32),
        Typ::V32 => u32::try_from(n).ok().map(Value::V32),
        Typ::U64 => u64::try_from(n).ok().map(Value::U64),
        Typ::V64 => u64::try_from(n).ok().map(Value::V64),
        _ => unreachable!("an integer type"),
    }
}

/// Whether `s` reads as an array or a map, which no primitive but a
/// string holds.
fn parses_to_collection(s: &str) -> bool {
    matches!(s.trim_start().as_bytes().first(), Some(b'[' | b'{'))
        && matches!(literal(s), Ok(Value::Array(_) | Value::Map(_)))
}

/// A float's integer part, `None` for NaN or an infinity.
fn float_int(f: f64) -> Option<i128> {
    f.is_finite().then(|| f.trunc() as i128)
}

/// Whether a part of each type a [`map_refs`] walk meets can hold a
/// reference, decided once per type.
type HoldsRef = AHashMap<Type, bool>;

fn map_refs(
    env: &Env,
    holds: &mut HoldsRef,
    t: &Type,
    v: &Value,
    f: &mut dyn FnMut(&Value) -> Value,
) -> Value {
    ensure_sufficient(|| {
        let refs = match holds.get(t) {
            Some(r) => *r,
            None => {
                let r = compared_ref(env, &mut Expanded::default(), t, CoreTrait::Eq);
                holds.insert(t.clone(), r);
                r
            }
        };
        if !refs {
            return v.clone();
        }
        let Some(t) = resolved_head(env, t) else { return v.clone() };
        let t = &*t;
        let mut each = |ts: &mut dyn Iterator<Item = (&Type, &Value)>| {
            let vs: LPooled<Vec<Value>> =
                Iterator::map(&mut &mut *ts, |(t, v)| map_refs(env, holds, t, v, f))
                    .collect();
            vs
        };
        match (t, v) {
            (Type::Any, v) => map_any_refs(env, holds, v, f).unwrap_or_else(|| v.clone()),
            (Type::ByRef(..), v) => f(v),
            (Type::Set(ts), v) => match member_of(env, ts, v) {
                Some(i) => map_refs(env, holds, &ts[i], v, f),
                None => v.clone(),
            },
            (Type::Error(e), Value::Error(x)) => {
                Value::Error(map_refs(env, holds, e, x, f).into())
            }
            (Type::Array(e), v) if let Some(x) = rep::array_elems(v) => {
                let mut vs = each(&mut x.iter().map(|v| (&**e, v)));
                Value::Array(ValArray::from_iter_exact(vs.drain(..)))
            }
            (Type::List(e), v) => {
                let vs: LPooled<Vec<Value>> = list::Iter::new(v.clone())
                    .map(|v| map_refs(env, holds, e, &v, f))
                    .collect();
                list::from_iter(vs.iter().cloned())
            }
            (Type::Tuple(ts), v)
                if let Some(x) = rep::tuple_elems(v)
                    && x.len() == ts.len() =>
            {
                let mut vs = each(&mut ts.iter().zip(x.iter()));
                rep::tuple(vs.drain(..))
            }
            (Type::Variant(tag, ts, _), v)
                if let Some((_, x)) = rep::variant(v)
                    && !x.is_empty()
                    && x.len() == ts.len() =>
            {
                let mut vs = each(&mut ts.iter().zip(x.iter()));
                rep::make_variant(tag.clone(), vs.drain(..))
            }
            (Type::Struct(fs), v)
                if let Some(x) = rep::struct_fields(v)
                    && x.len() == fs.len() =>
            {
                let mut vs: LPooled<Vec<Value>> = fs
                    .iter()
                    .zip(x.iter())
                    .map(|((_, t, _), field)| match rep::field(field) {
                        Some((n, v)) => {
                            rep::make_field(n.clone(), map_refs(env, holds, t, v, f))
                        }
                        None => field.clone(),
                    })
                    .collect();
                rep::structure_of_fields(vs.drain(..))
            }
            (Type::Map { value, .. }, Value::Map(m)) => Value::Map(
                m.into_iter()
                    .map(|(k, v)| (k.clone(), map_refs(env, holds, value, v, f)))
                    .collect(),
            ),
            (Type::Abstract { id, params }, v)
                if let Some(g) = abstract_value::get(v)
                    && g.id == *id
                    && let Some(rep) = reference_rep(env, *id, params, CoreTrait::Eq) =>
            {
                let payload = map_refs(env, holds, &rep, &g.payload, f);
                abstract_value::wrap(*id, g.name.clone(), g.params.clone(), payload)
            }
            (_, v) => v.clone(),
        }
    })
}

fn map_any_refs(
    env: &Env,
    holds: &mut HoldsRef,
    v: &Value,
    f: &mut dyn FnMut(&Value) -> Value,
) -> Option<Value> {
    ensure_sufficient(|| {
        if crate::reference::cell(v).is_some() {
            return Some(f(v));
        }
        if let Some(g) = abstract_value::get(v) {
            let rep = reference_rep(env, g.id, &g.params, CoreTrait::Eq)?;
            if !compared_ref(env, &mut Expanded::default(), &rep, CoreTrait::Eq) {
                return None;
            }
            let payload = map_refs(env, holds, &rep, &g.payload, f);
            return Some(abstract_value::wrap(
                g.id,
                g.name.clone(),
                g.params.clone(),
                payload,
            ));
        }
        rep::map_parts(v, &mut |v| Ok::<_, Infallible>(map_any_refs(env, holds, v, f)))
            .unwrap_or_else(|never| match never {})
    })
}

impl Type {
    /// Whether comparing values of this type meets a reference: `==`
    /// compares such values by [`Self::map_refs`].
    pub fn compares_refs(&self, env: &Env) -> bool {
        compared_ref(env, &mut Expanded::default(), self, CoreTrait::Eq)
    }

    pub fn orders_refs(&self, env: &Env) -> bool {
        compared_ref(env, &mut Expanded::default(), self, CoreTrait::Ord)
    }

    /// `v`, a value of this type, with each reference a comparison meets
    /// replaced by `f` of it.
    pub fn map_refs(
        &self,
        env: &Env,
        v: &Value,
        f: &mut dyn FnMut(&Value) -> Value,
    ) -> Value {
        map_refs(env, &mut LPooled::take(), self, v, f)
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use crate::{
        env::Env,
        expr::{ModPath, parser::parse_type},
        typ::Type,
    };
    use netidx_value::Value;
    use std::{
        borrow::Cow,
        time::{Duration, Instant},
    };

    /// An env holding the typedefs `defs` at the root, and `t` there.
    pub(crate) fn typed(defs: &[(&str, &str)], t: &str) -> (Env, Type) {
        Env::typed_at_root(defs, t).unwrap()
    }

    // A select's shallow test over `type T0 = [null, (T1, T1)]`, .. walks
    // each typedef once, not once per path.
    #[test]
    fn shallow_discriminant_walks_a_typedef_dag_once() {
        let depth = 26;
        let names: Vec<String> = (0..=depth).map(|i| format!("T{i}")).collect();
        let bodies: Vec<String> = (0..=depth)
            .map(|i| match i {
                i if i == depth => "i64".into(),
                i => format!("[null, (T{}, T{})]", i + 1, i + 1),
            })
            .collect();
        let defs: Vec<(&str, &str)> =
            names.iter().zip(bodies.iter()).map(|(n, b)| (&**n, &**b)).collect();
        let (env, scrutinee) = typed(&defs, "T0");
        let pred = parse_type("(T1, T1)").unwrap().scope_refs(&ModPath::root());
        pred.seed_refs(&env);
        let started = Instant::now();
        pred.shallow_discriminant(&env, &scrutinee);
        assert!(started.elapsed() < Duration::from_secs(2), "{:?}", started.elapsed());
    }

    /// `type L1 = [`A, `B]; type M1 = [`A, `C]` and, to `n`, `type Li =
    /// [Li-1, Mi-1]; type Mi = [Mi-1, Li-1]`: each name holds the next two.
    pub(crate) fn union_dag(n: usize) -> Vec<(String, String)> {
        let mut defs =
            vec![("L1".into(), "[`A, `B]".into()), ("M1".into(), "[`A, `C]".into())];
        for i in 2..=n {
            defs.push((format!("L{i}"), format!("[L{}, M{}]", i - 1, i - 1)));
            defs.push((format!("M{i}"), format!("[M{}, L{}]", i - 1, i - 1)));
        }
        defs
    }

    // A union's members past the unions its names hold expand each name
    // once, however many paths reach it.
    #[test]
    fn union_members_expand_each_name_once() {
        let defs = union_dag(20);
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, "L20");
        let head = super::rep_head(&env, &t).map(|h| h.into_owned());
        let Some(Type::Set(ms)) = &head else { panic!("L20 is a union") };
        let mut out = smallvec::SmallVec::new();
        let mut named = poolshark::local::LPooled::take();
        for m in ms.iter() {
            let classes = [enumflags2::BitFlags::empty(); 2];
            super::members_by_class(
                &env,
                Cow::Borrowed(m),
                classes,
                &mut out,
                &mut named,
                0,
            )
        }
        let leaves: Vec<String> = out.iter().map(|m| m.to_string()).collect();
        assert_eq!(leaves, ["`A", "`B", "`A", "`C"]);
        let v = Value::String(arcstr::literal!("C"));
        let flat = parse_type("[`A, `B, `C]").unwrap();
        assert_eq!(t.cast_value(&env, v.clone()), flat.cast_value(&env, v));
    }

    // A text cast into a union parses the text as a literal once, and only
    // where a member may read the scalar it parses as.
    #[test]
    fn a_union_parses_its_text_once_and_only_for_a_scalar_member() {
        let parses = |t: &str, v: Value| {
            let (env, t) = typed(&[], t);
            super::LITERALS.with(|n| n.set(0));
            let r = t.cast_value(&env, v);
            (r, super::LITERALS.with(|n| n.get()))
        };
        let s = |s: &str| Value::String(s.into());
        let strs = |ss: &[&str]| Value::Array(ss.iter().map(|x| s(x)).collect());
        let a = crate::rep::make_variant(arcstr::literal!("A"), []);
        assert_eq!(parses("[`A, null]", s("A")), (a.clone(), 0));
        assert_eq!(parses("[`A, `B(i64)]", s("A")), (a.clone(), 0));
        assert_eq!(parses("[string, `Z]", s("w1")), (s("w1"), 0));
        assert_eq!(parses("Array<[`A, null]>", strs(&["A", "A", "A"])).1, 0);
        assert_eq!(parses("[i64, `A]", s("3")), (Value::I64(3), 1));
        assert_eq!(parses("[i64, `A]", s("A")), (a, 1));
        assert_eq!(parses("[f32, bool, `A]", s("true")), (Value::Bool(true), 1));
        let d = "3.0".parse::<rust_decimal::Decimal>().unwrap();
        let d = Value::Decimal(triomphe::Arc::new(d));
        assert_eq!(parses("[duration, decimal]", s("3.0")), (d, 1));
        assert_eq!(parses("Array<[i64, `A]>", strs(&["3", "A", "4"])).1, 3);
    }

    // A failing union read costs what the data's size does: each union
    // reads each value once, however many members try it.
    #[test]
    fn failing_union_read_is_linear() {
        let (env, j) = typed(&[("J", "[i64, (J, i64), Array<J>]")], "J");
        let mut v = Value::Array([Value::from("x"), Value::I64(1)].into_iter().collect());
        for _ in 0..22 {
            v = Value::Array([v, Value::I64(1)].into_iter().collect());
        }
        let started = Instant::now();
        let r = j.cast_value(&env, v);
        assert!(format!("{r}").contains("no member admits it"), "{r}");
        assert!(started.elapsed() < Duration::from_secs(2), "{:?}", started.elapsed());
    }
}
