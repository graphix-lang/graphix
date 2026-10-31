//! Which unions a text format cannot read back: two members such that a
//! JSON or TOML value one writes may read back as the other. A reader
//! picks one member of a union for a value as `cast.rs` does: a payload
//! variant whose tag the data names first, whatever its reading rounds;
//! then the round its reading falls in (exact, then rounded to a real's
//! precision, `Holds`); then the pass, the members keeping the data's
//! class before the others (`in_own_class`, a primitive set in bit
//! order); then the union's order, where a reading that crosses the line
//! between integers and reals gives way to a later one keeping every
//! number its kind (`Choice`). A tuple or a struct is in the data's class
//! only where the pick tries it as a member, never as a part of a
//! composite or a member of a union nested in a member (`in_own_class`
//! holds no unmarked one). Values are judged by class ([`Token`]), and a
//! member's reading of a composite whole: `[(f32, u8), (f64, f64)]`
//! reads `(f32:0.1, u8:1)` back as `(0.1, 1.0)`, exact where the f32's
//! own reading rounds; `[(string, f32), (i64, f64)]` reads `("1",
//! f32:0.1)` back as `(1, 0.1)`. A union two of whose members write
//! one value is refused even where reading it as either gives the same
//! value (`` `Nil `` in `[R<i64>, R<bool>]`), and a typedef whose
//! parameters grow as it recurses is refused: its shapes never end.

use crate::{
    env::Env,
    expr::ModPath,
    typ::{Typ, Type, cast::grows_on, params_size},
};
use ahash::{AHashMap, AHashSet};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::cell::RefCell;
use triomphe::Arc;

/// Why a type's text form may not read back what it wrote
/// ([`Type::text_ambiguity`]).
#[derive(Debug, Clone)]
pub enum TextAmbiguity {
    /// Two members of a union: a value one writes may read back as the
    /// other.
    Members(Type, Type),
    /// A typedef whose parameters grow as it recurses.
    Grows(ModPath),
}

/// The format a type is read from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    Json,
    Toml,
}

/// A number as data, by which types hold it: an integer every integer
/// type holds, one an f32 holds, one an f32 may only round, one an f64
/// may only round; a whole float alike; a fraction an f32 holds or only
/// rounds; a decimal no float holds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Num {
    IntS,
    IntM,
    IntL,
    IntH,
    WholeS,
    WholeM,
    WholeL,
    WholeH,
    FracF32,
    Frac,
    Dec,
}

impl Num {
    fn real(self) -> bool {
        !matches!(self, Num::IntS | Num::IntM | Num::IntL | Num::IntH)
    }
}

/// A class of values a reader is handed: data, or the text of data (a
/// string, an object's key), a variant's tag or any other text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    Null,
    Bool,
    Num(Num),
    DateTime,
    NumText(Num),
    BoolText,
    DateText,
    TagText,
    Text,
}

/// The runtime class of a value or a member (`in_own_class`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Class {
    Null,
    Bool,
    Number,
    DateTime,
    Duration,
    String,
}

impl Token {
    fn class(self) -> Class {
        match self {
            Token::Null => Class::Null,
            Token::Bool => Class::Bool,
            Token::Num(_) => Class::Number,
            Token::DateTime => Class::DateTime,
            _ => Class::String,
        }
    }

    /// The token as it is written, or as an object's key: its text, a
    /// whole number's with or without its point. A null key writes its map
    /// as pairs.
    fn written(self, key: bool) -> SmallVec<[Token; 2]> {
        use Num::*;
        let whole = |n| match n {
            WholeS => Some(IntS),
            WholeM => Some(IntM),
            WholeL => Some(IntL),
            WholeH => Some(IntH),
            _ => None,
        };
        let mut r = SmallVec::new();
        match (key, self) {
            (false, t) => r.push(t),
            (true, Token::Null) => (),
            (true, Token::Bool) => r.push(Token::BoolText),
            (true, Token::DateTime) => r.push(Token::DateText),
            (true, Token::Num(n)) => {
                r.push(Token::NumText(n));
                r.extend(whole(n).map(Token::NumText));
            }
            (true, t) => r.push(t),
        }
        r
    }

    /// The primitive the format's reader makes of it. An integer past
    /// i64's range reads as a u64; taking it as an i64 only refuses more
    /// (a u64 member is not seen to hold it, an i64 member is).
    fn read_back(self) -> Typ {
        match self {
            Token::Null => Typ::Null,
            Token::Bool => Typ::Bool,
            Token::Num(Num::Dec) => Typ::Decimal,
            Token::Num(n) if n.real() => Typ::F64,
            Token::Num(_) => Typ::I64,
            Token::DateTime => Typ::DateTime,
            _ => Typ::String,
        }
    }
}

const INTS: [Token; 4] = [
    Token::Num(Num::IntS),
    Token::Num(Num::IntM),
    Token::Num(Num::IntL),
    Token::Num(Num::IntH),
];
const FLOATS: [Token; 6] = [
    Token::Num(Num::WholeS),
    Token::Num(Num::WholeM),
    Token::Num(Num::WholeL),
    Token::Num(Num::WholeH),
    Token::Num(Num::FracF32),
    Token::Num(Num::Frac),
];
const NUMS: [Token; 11] = [
    INTS[0],
    INTS[1],
    INTS[2],
    INTS[3],
    FLOATS[0],
    FLOATS[1],
    FLOATS[2],
    FLOATS[3],
    FLOATS[4],
    FLOATS[5],
    Token::Num(Num::Dec),
];
const TEXTS: [Token; 15] = {
    let mut t = [Token::Text; 15];
    let mut i = 0;
    while i < NUMS.len() {
        if let Token::Num(n) = NUMS[i] {
            t[i] = Token::NumText(n);
        }
        i += 1;
    }
    t[11] = Token::BoolText;
    t[12] = Token::DateText;
    t[13] = Token::TagText;
    t
};

/// How a member reads a value, the low nibble of an outcome for the
/// writer's own member and the high one for the other: two flags, only
/// rounded (a later round) and outside the value's class (a later pass),
/// and a level, the highest of its parts': the value as it is
/// (`cast_member`'s `Ok(None)`), built, with a number read as no number,
/// with a number read across the line between integers and reals
/// (`NumReads`).
const ROUNDS: u8 = 1;
const FOREIGN: u8 = 2;
const LEVEL: u8 = 12;
const HELD: u8 = 0;
const BUILT: u8 = 4;
const OTHER: u8 = 8;
const CROSSED: u8 = 12;

/// The outcome of two parts read together: each side's flags of either,
/// and the higher level.
fn join(x: u8, y: u8) -> u8 {
    let side = |x: u8, y: u8| (x | y) & (ROUNDS | FOREIGN) | (x & LEVEL).max(y & LEVEL);
    side(x & 15, y & 15) | side(x >> 4, y >> 4) << 4
}

/// Both sides' bits for whether each reading keeps the data's class.
fn own(a: bool, b: bool) -> u8 {
    (!a as u8) * FOREIGN | ((!b as u8) * FOREIGN) << 4
}

/// A primitive as data of the format.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Leaf {
    Null,
    Bool,
    Int(Typ),
    F32,
    F64,
    Decimal,
    /// Written as its seconds.
    Duration,
    Str,
    /// A datetime written as its RFC3339 text (JSON's).
    DateText,
    /// A native datetime (TOML's).
    DateTime,
}

impl Leaf {
    fn class(self) -> Class {
        match self {
            Leaf::Null => Class::Null,
            Leaf::Bool => Class::Bool,
            Leaf::Int(_) | Leaf::F32 | Leaf::F64 | Leaf::Decimal => Class::Number,
            Leaf::Duration => Class::Duration,
            Leaf::Str => Class::String,
            Leaf::DateText | Leaf::DateTime => Class::DateTime,
        }
    }

    /// The class of the data it writes, or of its text as a key.
    fn data_class(self, key: bool) -> Class {
        match self {
            _ if key => Class::String,
            Leaf::Null => Class::Null,
            Leaf::Bool => Class::Bool,
            Leaf::Int(_) | Leaf::F32 | Leaf::F64 | Leaf::Decimal | Leaf::Duration => {
                Class::Number
            }
            Leaf::Str | Leaf::DateText => Class::String,
            Leaf::DateTime => Class::DateTime,
        }
    }

    fn typ(self) -> Typ {
        match self {
            Leaf::Null => Typ::Null,
            Leaf::Bool => Typ::Bool,
            Leaf::Int(t) => t,
            Leaf::F32 => Typ::F32,
            Leaf::F64 => Typ::F64,
            Leaf::Decimal => Typ::Decimal,
            Leaf::Duration => Typ::Duration,
            Leaf::Str => Typ::String,
            Leaf::DateText | Leaf::DateTime => Typ::DateTime,
        }
    }

    /// The values it writes.
    fn writes(self) -> &'static [Token] {
        match self {
            Leaf::Null => &[Token::Null],
            Leaf::Bool => &[Token::Bool],
            Leaf::Int(Typ::I8) => &INTS[..1],
            Leaf::Int(Typ::U8 | Typ::U16 | Typ::I16) => &INTS[..2],
            Leaf::Int(Typ::U32 | Typ::V32 | Typ::I32 | Typ::Z32) => &INTS[..3],
            Leaf::Int(_) => &INTS,
            Leaf::F32 | Leaf::F64 | Leaf::Duration => &FLOATS,
            Leaf::Decimal => &NUMS,
            Leaf::Str => &TEXTS,
            Leaf::DateText => &[Token::DateText],
            Leaf::DateTime => &[Token::DateTime],
        }
    }

    /// Each way it may read a value of `t` exactly or rounded (a bool into
    /// a real, as `holds_rounded` has it); a reading that drops part of a
    /// value never beats the writer's own, which is one of these.
    fn reads(self, t: Token) -> impl Iterator<Item = u8> {
        let foreign = if self.class() == t.class() { 0 } else { FOREIGN };
        let level = if self.typ() == t.read_back() { HELD } else { BUILT };
        self.reads_in(t).iter().map(move |r| join(r | foreign, level))
    }

    /// Whether it reads every value `w` (a tag where `None`) writes of `t`,
    /// one of the ways [`Self::reads`] gives, none overflowing it.
    fn total(self, w: Option<Leaf>, t: Token) -> bool {
        let int_range = |t: Typ| match t {
            Typ::U8 => (0, u8::MAX as i128),
            Typ::I8 => (i8::MIN as i128, i8::MAX as i128),
            Typ::U16 => (0, u16::MAX as i128),
            Typ::I16 => (i16::MIN as i128, i16::MAX as i128),
            Typ::U32 | Typ::V32 => (0, u32::MAX as i128),
            Typ::I32 | Typ::Z32 => (i32::MIN as i128, i32::MAX as i128),
            Typ::U64 | Typ::V64 => (0, u64::MAX as i128),
            _ => (i64::MIN as i128, i64::MAX as i128),
        };
        let reads = !self.reads_in(t).is_empty();
        match (self, w) {
            (r, Some(w)) if r == w => true,
            (Leaf::Str, _) => reads,
            (Leaf::Null | Leaf::Bool | Leaf::DateText | Leaf::DateTime, _) => {
                reads && !matches!(t, Token::Num(_) | Token::NumText(_))
            }
            (Leaf::Int(r), Some(Leaf::Int(w))) => {
                let ((rl, rh), (wl, wh)) = (int_range(r), int_range(w));
                rl <= wl && wh <= rh
            }
            (
                Leaf::F64,
                Some(
                    Leaf::Int(_)
                    | Leaf::F32
                    | Leaf::F64
                    | Leaf::Decimal
                    | Leaf::Duration
                    | Leaf::Bool
                    | Leaf::DateTime,
                ),
            )
            | (
                Leaf::F32,
                Some(
                    Leaf::Int(_)
                    | Leaf::F32
                    | Leaf::Decimal
                    | Leaf::Bool
                    | Leaf::DateTime,
                ),
            )
            | (Leaf::Decimal, Some(Leaf::Int(_) | Leaf::Decimal | Leaf::Bool)) => reads,
            _ => false,
        }
    }

    fn reads_in(self, t: Token) -> &'static [u8] {
        use Num::*;
        const NO: &[u8] = &[];
        const YES: &[u8] = &[0];
        let num = match t {
            Token::Num(n) | Token::NumText(n) => Some(n),
            _ => None,
        };
        match self {
            Leaf::Null if t == Token::Null => YES,
            Leaf::Bool if matches!(t, Token::Bool | Token::BoolText) => YES,
            Leaf::Null | Leaf::Bool => NO,
            Leaf::Str => match t {
                Token::Null => NO,
                Token::Num(_) => &[OTHER],
                _ => YES,
            },
            Leaf::Duration if num.is_some() => &[OTHER],
            Leaf::Duration => NO,
            Leaf::DateText | Leaf::DateTime => match t {
                Token::DateText | Token::DateTime => YES,
                _ if num.is_some() => &[OTHER],
                _ => NO,
            },
            Leaf::F32 | Leaf::F64 | Leaf::Decimal if t == Token::Bool => &[ROUNDS],
            _ if t == Token::DateTime => YES,
            Leaf::Int(ty) => {
                let wide = !matches!(ty, Typ::U8 | Typ::I8 | Typ::U16 | Typ::I16);
                let byte = ty == Typ::I8;
                match num {
                    Some(IntS) => YES,
                    Some(IntM) if !byte => YES,
                    Some(IntL | IntH) if wide => YES,
                    Some(WholeS) => &[CROSSED],
                    Some(WholeM) if !byte => &[CROSSED],
                    Some(WholeL | WholeH) if wide => &[CROSSED],
                    _ => NO,
                }
            }
            Leaf::F32 => match num {
                Some(IntS | IntM) => &[CROSSED],
                Some(IntL | IntH) => &[CROSSED, CROSSED | ROUNDS],
                Some(WholeS | WholeM | FracF32) => YES,
                Some(WholeL | WholeH) => &[0, ROUNDS],
                Some(Frac | Dec) => &[ROUNDS],
                None => NO,
            },
            Leaf::F64 => match num {
                Some(IntS | IntM | IntL) => &[CROSSED],
                Some(IntH) => &[CROSSED, CROSSED | ROUNDS],
                Some(Dec) => &[ROUNDS],
                Some(_) => YES,
                None => NO,
            },
            Leaf::Decimal => match num {
                Some(n) if n.real() => YES,
                Some(_) => &[CROSSED],
                None => NO,
            },
        }
    }
}

/// The ways a value one member writes may be read by its own member and
/// by another, each an outcome of two nibbles.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Outs([u64; 4]);

impl Outs {
    const NONE: Outs = Outs([0; 4]);

    fn one(o: u8) -> Outs {
        let mut s = Outs::NONE;
        s.insert(o);
        s
    }

    fn insert(&mut self, o: u8) {
        self.0[(o >> 6) as usize] |= 1 << (o & 63)
    }

    fn iter(self) -> impl Iterator<Item = u8> {
        (0..4usize).flat_map(move |w| {
            let mut x = self.0[w];
            std::iter::from_fn(move || {
                (x != 0).then(|| {
                    let b = x.trailing_zeros();
                    x &= x - 1;
                    (w as u32 * 64 + b) as u8
                })
            })
        })
    }

    /// A value read one of the ways of either.
    fn or(self, o: Outs) -> Outs {
        Outs(std::array::from_fn(|i| self.0[i] | o.0[i]))
    }

    /// A value of a part read one of the ways of `self` beside a part read
    /// one of the ways of `o`.
    fn and(self, o: Outs) -> Outs {
        let mut r = Outs::NONE;
        for x in self.iter() {
            for y in o.iter() {
                r.insert(join(x, y))
            }
        }
        r
    }

    /// Any number of parts, each read one of the ways of `self`; none is
    /// an array held as it is.
    fn star(self) -> Outs {
        let mut r = Outs::one(0);
        loop {
            let n = r.or(r.and(self));
            if n == r {
                return r;
            }
            r = n
        }
    }

    /// Each outcome with the side `shift` names in the class or out of it,
    /// as `own` and `foreign` allow, whatever its reading said.
    fn classed(self, shift: u8, own: bool, foreign: bool) -> Outs {
        let mut r = Outs::NONE;
        for x in self.iter() {
            let x = x & !(FOREIGN << shift);
            if own {
                r.insert(x)
            }
            if foreign {
                r.insert(x | FOREIGN << shift)
            }
        }
        r
    }

    /// Each outcome read together with a part read as `bits` says.
    fn with(self, bits: u8) -> Outs {
        let mut r = Outs::NONE;
        for x in self.iter() {
            r.insert(join(x, bits))
        }
        r
    }

    fn any(parts: impl Iterator<Item = Outs>) -> Outs {
        parts.fold(Outs::NONE, Outs::or)
    }

    fn all(parts: impl Iterator<Item = Outs>) -> Outs {
        let mut r = Outs::one(0);
        for p in parts {
            r = r.and(p);
            if r == Outs::NONE {
                break;
            }
        }
        r
    }
}

/// What a type writes, and so what its reader takes, as data of one
/// format; a node of a [`Shapes`] arena.
enum Shape {
    /// Nothing the format can write (a function, an error, TOML's null).
    None,
    /// Not known here (an open cell, `Any`, an abstract whose `Cast`
    /// implementation writes both methods): shares a form with nothing.
    Unknown,
    /// A primitive, and its bit.
    Scalar(Leaf, Typ),
    /// A bare variant, written as its tag.
    Lit(ArcStr),
    /// Any number of the element, written as an array.
    Seq(usize, SeqKind),
    /// A tuple, or, `tagged`, a variant's tag (a `Lit`) and its payload.
    Fixed(SmallVec<[usize; 4]>, bool),
    /// A map, written as an object where its keys are scalars, else as an
    /// array of `[key, value]` pairs; an object reads as its sorted pairs.
    Pairs(usize, usize),
    /// A struct: its fields by name, each with whether its type admits
    /// null (the reader takes it left out).
    Struct(SmallVec<[(ArcStr, usize, bool); 4]>),
    /// `prim` where the members are a primitive set's, tried in bit order.
    Union(SmallVec<[usize; 4]>, bool),
    /// An abstract type whose `Cast` implementation leaves a method out,
    /// and its representation: written as it where `to_value` is left out
    /// (else as nothing known here), read as it where `from_value` is (else
    /// by `from_value`, which may read any value it is tried on). A reader
    /// builds it from the data, never in the data's class.
    Foreign { rep: usize, writes: bool, reads_rep: bool },
}

/// What reads an array of data: an array, held as it is where every
/// element is; a list, built; bytes, built and outside an array's class.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SeqKind {
    Array,
    List,
    Bytes,
}

impl SeqKind {
    /// Whether its reader keeps an array's class.
    fn own(self) -> bool {
        self != SeqKind::Bytes
    }
}

/// Which of a node's sides a walk over it follows: what it writes, or what
/// reads into it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Way {
    Writes,
    Reads,
}

type Side<'s> = &'s dyn Fn(&Shapes, usize) -> Outs;

/// Two nodes and whether the first writes an object's key, a
/// [`Fixpoint`]'s key: a writer and a reader, or a reader and a writer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Pair(u64);

impl Pair {
    fn new(a: usize, key: bool, b: usize) -> Pair {
        debug_assert!(a < 1 << 31 && b < 1 << 32);
        Pair((a as u64) << 33 | (key as u64) << 32 | b as u64)
    }

    fn parts(self) -> (usize, bool, usize) {
        ((self.0 >> 33) as usize, self.0 >> 32 & 1 == 1, self.0 as u32 as usize)
    }
}

/// What a [`Fixpoint`] solves for: it starts at `BOTTOM` and only grows.
trait Lattice: Copy + Eq {
    const BOTTOM: Self;
    fn join(self, o: Self) -> Self;
}

impl Lattice for Outs {
    const BOTTOM: Outs = Outs::NONE;

    fn join(self, o: Outs) -> Outs {
        self.or(o)
    }
}

impl Lattice for (bool, bool) {
    const BOTTOM: (bool, bool) = (false, false);

    fn join(self, o: (bool, bool)) -> (bool, bool) {
        (self.0 || o.0, self.1 || o.1)
    }
}

/// A pair's value so far, whether it was computed from the values it read
/// as they stand, whether it is final, and the pairs that read it while
/// it was not.
struct Fix<V> {
    value: V,
    stable: bool,
    done: bool,
    readers: SmallVec<[Pair; 1]>,
}

/// A function of pairs computed as the least fixpoint over the pairs a
/// walk meets, by a top-down solver: a pair is computed when first met,
/// reading a pair it is still computing as it stands, and when a pair's
/// value grows the pairs that read it are computed again. A value only
/// grows (each is joined with the last) and a pair is computed again only
/// when one it read grew, so this ends, each pair computed once and again
/// per growth of a pair it reads; the function is monotone, so no value
/// passes the least fixpoint. Every pair a check meets stays: a group of
/// `n` unions recursive in each other holds about `n^2` of them.
struct Fixpoint<V> {
    pairs: AHashMap<Pair, Fix<V>>,
    /// The pairs being computed, innermost last.
    computing: Vec<Pair>,
    /// The pairs the running [`Self::least`] met.
    met: Vec<Pair>,
    #[cfg(test)]
    evaluated: usize,
}

impl<V> Default for Fixpoint<V> {
    fn default() -> Self {
        Fixpoint {
            pairs: AHashMap::default(),
            computing: Vec::new(),
            met: Vec::new(),
            #[cfg(test)]
            evaluated: 0,
        }
    }
}

impl<V: Lattice> Fixpoint<V> {
    /// `k`'s value, every pair it reaches final.
    fn least(cell: &RefCell<Self>, k: Pair, f: &dyn Fn(Pair) -> V) -> V {
        Self::solve(cell, k, f);
        let mut s = cell.borrow_mut();
        let Fixpoint { pairs, met, .. } = &mut *s;
        for p in met.drain(..) {
            let e = pairs.get_mut(&p).expect("met");
            e.done = true;
            e.readers = SmallVec::new();
        }
        pairs[&k].value
    }

    /// `k`'s value as the running [`Self::least`] has it, read by the pair
    /// being computed.
    fn get(cell: &RefCell<Self>, k: Pair, f: &dyn Fn(Pair) -> V) -> V {
        Self::solve(cell, k, f);
        let mut s = cell.borrow_mut();
        let r = s.computing.last().copied();
        let e = s.pairs.get_mut(&k).expect("solved");
        if !e.done
            && let Some(r) = r
            && e.readers.last() != Some(&r)
        {
            e.readers.push(r)
        }
        e.value
    }

    fn solve(cell: &RefCell<Self>, k: Pair, f: &dyn Fn(Pair) -> V) {
        {
            let mut s = cell.borrow_mut();
            match s.pairs.get_mut(&k) {
                Some(e) if e.done || e.stable => return,
                Some(e) => e.stable = true,
                None => {
                    let e = Fix {
                        value: V::BOTTOM,
                        stable: true,
                        done: false,
                        readers: SmallVec::new(),
                    };
                    s.pairs.insert(k, e);
                    s.met.push(k);
                }
            }
            s.computing.push(k);
            #[cfg(test)]
            {
                s.evaluated += 1;
            }
        }
        let r = crate::stack::ensure_sufficient(|| f(k));
        let readers = {
            let mut s = cell.borrow_mut();
            s.computing.pop();
            let e = s.pairs.get_mut(&k).expect("solved");
            let v = e.value.join(r);
            if v == e.value {
                return;
            }
            e.value = v;
            let readers = std::mem::take(&mut e.readers);
            for p in readers.iter() {
                s.pairs.get_mut(p).expect("a reader").stable = false
            }
            readers
        };
        for p in readers {
            Self::solve(cell, p, f)
        }
    }
}

/// [`Shapes::reach`] of each node and way asked, as a span of `found`.
#[derive(Default)]
struct Reach {
    spans: AHashMap<(usize, Way), (usize, usize)>,
    found: Vec<(usize, bool)>,
    #[cfg(test)]
    walked: usize,
}

struct Shapes<'a> {
    env: &'a Env,
    format: Format,
    nodes: Vec<(Shape, Type)>,
    /// Each reference expanded, by its definition and its arguments, so a
    /// recursive type is a cycle.
    refs: AHashMap<(usize, Arc<[Type]>), usize>,
    /// The definitions being expanded, each with its arguments' size.
    path: Vec<(usize, usize)>,
    /// A definition met growing its arguments as it recurses.
    grows: Option<ModPath>,
    /// [`Shapes::class_of`] by reader, writer and key.
    classes: RefCell<Fixpoint<(bool, bool)>>,
    fix: RefCell<Fixpoint<Outs>>,
    reach: RefCell<Reach>,
}

impl<'a> Shapes<'a> {
    fn new(env: &'a Env, format: Format, t: &Type) -> Self {
        let mut shapes = Shapes {
            env,
            format,
            nodes: Vec::new(),
            refs: AHashMap::default(),
            path: Vec::new(),
            grows: None,
            classes: RefCell::default(),
            fix: RefCell::default(),
            reach: RefCell::default(),
        };
        shapes.node(t);
        shapes
    }

    /// [`Type::text_ambiguity`] of the type the shapes were built from.
    fn ambiguity(&self) -> Option<TextAmbiguity> {
        if let Some(name) = &self.grows {
            return Some(TextAmbiguity::Grows(name.clone()));
        }
        let mut keys: LPooled<Vec<usize>> = LPooled::take();
        for (s, _) in self.nodes.iter() {
            if let Shape::Pairs(k, _) = s
                && self.key_forms(*k).0
            {
                let unions = self.reach(*k, Way::Writes).map(|(m, _)| m);
                keys.extend(
                    unions.filter(|m| matches!(self.nodes[*m].0, Shape::Union(..))),
                )
            }
        }
        keys.sort_unstable();
        keys.dedup();
        let unions = (0..self.nodes.len()).map(|n| (n, false));
        for (u, key) in unions.chain(keys.iter().map(|k| (*k, true))) {
            let Shape::Union(ms, prim) = &self.nodes[u].0 else { continue };
            for (i, a) in ms.iter().enumerate() {
                for b in ms[i + 1..].iter() {
                    let hit = match prim {
                        true => {
                            self.prim_takes(ms, *a, *b, key)
                                || self.prim_takes(ms, *b, *a, key)
                        }
                        false => {
                            self.takes(*a, *b, key, true)
                                || self.takes(*b, *a, key, false)
                        }
                    };
                    if hit {
                        return Some(TextAmbiguity::Members(
                            self.nodes[*a].1.clone(),
                            self.nodes[*b].1.clone(),
                        ));
                    }
                }
            }
        }
        None
    }

    fn push(&mut self, s: Shape, t: &Type) -> usize {
        self.nodes.push((s, t.clone()));
        self.nodes.len() - 1
    }

    fn prim(&self, p: Typ) -> Shape {
        let json = self.format == Format::Json;
        let leaf = match p {
            Typ::Null if json => Leaf::Null,
            Typ::Bool => Leaf::Bool,
            Typ::F64 => Leaf::F64,
            Typ::F32 => Leaf::F32,
            Typ::Duration if json => Leaf::Duration,
            Typ::Decimal if json => Leaf::Decimal,
            Typ::String => Leaf::Str,
            Typ::DateTime if json => Leaf::DateText,
            Typ::DateTime => Leaf::DateTime,
            Typ::Null | Typ::Duration | Typ::Decimal | Typ::Error => return Shape::None,
            Typ::Bytes | Typ::Array | Typ::Map | Typ::Abstract => return Shape::Unknown,
            p => Leaf::Int(p),
        };
        Shape::Scalar(leaf, p)
    }

    fn node(&mut self, t: &Type) -> usize {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => self.node(&b),
                None => self.push(Shape::Unknown, t),
            },
            Type::Ref(tr) => {
                let Some(def) = tr.resolve_peek(self.env) else {
                    return self.push(Shape::Unknown, t);
                };
                let Some(known) = def.bindings(&tr.params) else {
                    return self.push(Shape::Unknown, t);
                };
                let key = (def.def_key(), tr.params.clone());
                if let Some(id) = self.refs.get(&key) {
                    return *id;
                }
                let id = self.push(Shape::Unknown, t);
                let size = params_size(&tr.params);
                if grows_on(&self.path, &key.0, size) {
                    self.grows.get_or_insert_with(|| tr.name.clone());
                    return id;
                }
                self.path.push((key.0, size));
                self.refs.insert(key, id);
                let body = self.node(&def.typ.replace_tvars(&known));
                self.path.pop();
                self.nodes[id].0 = Shape::Union(SmallVec::from_iter([body]), false);
                id
            }
            Type::Primitive(p) => match p.len() {
                1 => {
                    let p = p.iter().next().expect("one");
                    match p {
                        Typ::Bytes => {
                            let u8 = Type::Primitive(Typ::U8.into());
                            let int = self
                                .push(Shape::Scalar(Leaf::Int(Typ::U8), Typ::U8), &u8);
                            self.push(Shape::Seq(int, SeqKind::Bytes), t)
                        }
                        p => {
                            let s = self.prim(p);
                            self.push(s, t)
                        }
                    }
                }
                _ => {
                    let ms =
                        p.iter().map(|b| self.node(&Type::Primitive(b.into()))).collect();
                    self.push(Shape::Union(ms, true), t)
                }
            },
            Type::Set(ms) => {
                let ms = ms.iter().map(|m| self.node(m)).collect();
                self.push(Shape::Union(ms, false), t)
            }
            Type::Bottom | Type::Error(_) | Type::Fn(_) | Type::ByRef(..) => {
                self.push(Shape::None, t)
            }
            Type::Any | Type::App(..) | Type::Hole | Type::Bound(_) => {
                self.push(Shape::Unknown, t)
            }
            Type::Array(e) => {
                let e = self.node(e);
                self.push(Shape::Seq(e, SeqKind::Array), t)
            }
            Type::List(e) => {
                let e = self.node(e);
                self.push(Shape::Seq(e, SeqKind::List), t)
            }
            Type::Tuple(ts) => {
                let ts = ts.iter().map(|t| self.node(t)).collect();
                self.push(Shape::Fixed(ts, false), t)
            }
            Type::Variant(tag, ps, _) if ps.is_empty() => {
                self.push(Shape::Lit(tag.clone()), t)
            }
            Type::Variant(tag, ps, _) => {
                let tag = self.push(Shape::Lit(tag.clone()), t);
                let ps =
                    std::iter::once(tag).chain(ps.iter().map(|p| self.node(p))).collect();
                self.push(Shape::Fixed(ps, true), t)
            }
            Type::Struct(fs) => {
                let null = Type::Primitive(Typ::Null.into());
                let fs = fs
                    .iter()
                    .map(|(n, ft, _)| {
                        let omittable = ft
                            .contains_with_flags(BitFlags::empty(), self.env, &null)
                            .unwrap_or(false);
                        (n.clone(), self.node(ft), omittable)
                    })
                    .collect();
                self.push(Shape::Struct(fs), t)
            }
            Type::Map { key, value } => {
                let (k, v) = (self.node(key), self.node(value));
                self.push(Shape::Pairs(k, v), t)
            }
            // XCR claude for claude: [bug/medium] An abstract type written as its
            // representation takes that representation's shape here, but a union's
            // reader tries it only after every member of the value's own class and never
            // keeps the data as it is, so any primitive member that converts the written
            // data wins. With type N = Abstract<i64> and type S = Abstract<string> (each
            // with impl Cast), [N, f64] writes N(2) as 2 and json::read gives back f64
            // 2.0; [S, bool] reads S("true") back as true; [S, i64] reads S("1") back as
            // 1, a map key alike ({"1":1}). JsonData accepts all three. Either model such
            // a member as overlapping every member the reader converts its data into, or
            // have the union reader try it in its representation's class. probes:
            // ~/tmp/cr29/scratch/laneTA/idnum.gx, idkey.gx (textdata-abstract-01)
            // 2026-10-30 claude: Fixed with the round-2 textdata model
            // (holds-exactly-class-01): an abstract type written as its representation is
            // Shape::Foreign, its readings and its own reading outside the data's class,
            // so the union's pick tries it after every member that reads the data in its
            // class, in the union's order. [S, bool] and Map<[Id, i64], i64> are refused;
            // [N, f64] is admitted and reads back (the lane's Choice lets N's kept
            // reading replace f64's crossing one). Pins, the first two failing without
            // Shape::Foreign (main and 5fa76d1e2 admit them):
            // lib_tests::json::{json_target_abstract_string_beside_a_bool_refused,
            // json_target_abstract_key_beside_an_integer_refused,
            // json_abstract_number_beside_a_float_reads_back}.
            // 2026-10-30 claude: Round 5: an abstract written as its representation that
            // holds itself (type N = Abstract<[N, `L(i64)]>), or two that hold each
            // other, made a cycle of Shape::Foreign nodes, which textdata's member walks
            // followed without end (a stack overflow in --check, json::read into N, [N,
            // `B(string)] or Map<N, i64>). The walks now fold over Shapes::reach, each
            // node met once. Verdicts are main's: refused, since N(N(x)) writes what N(x)
            // does. Pins: lib_tests::json::{json_target_abstract_holding_itself_refused,
            // json_target_key_abstract_holding_itself_refused,
            // json_target_abstracts_holding_each_other_refused} (each overflows the stack
            // on 1ab657e8c).
            // 2026-10-31 claude: Round 6 (review7 B r4): an abstract written as its
            // representation whose Cast implementation writes from_value was modelled as
            // reading data as its representation, but the cast calls from_value, which
            // may read anything: with a from_value reading a string s as Id(str::len(s)),
            // [Id, `A] read `A back as Id(1) (main too). Shape::Foreign now carries
            // whether from_value is written (Env::read_as_representation); such a reader
            // reads every value it is tried on, outside the class. An impl writing both
            // methods is still Shape::Unknown, trusted: textdata-cast-trust-01 asks Eric
            // which rule holds. Pins:
            // lib_tests::json::json_target_abstract_reading_by_from_value_beside_a_tag_refused
            // (admitted without the change),
            // json_abstract_reading_by_from_value_beside_a_payload_variant_reads_back.
            // 2026-10-31 claude: Round 7 (brief r4B5): an abstract whose Cast implementation
            // writes to_value and leaves from_value out was Shape::Unknown, reading nothing,
            // while the cast reads it as its representation: [S, `A] read "A" back as S("A")
            // and [N, decimal] read 3 back as N(3) (main too). Shape::Foreign now keeps the
            // two sides apart (writes: to_value left out; reads_rep: from_value left out), so
            // such an abstract writes nothing known here and reads as its representation. Pins
            // (admitted at 3cb1fc3d2):
            // lib_tests::json::{json_target_abstract_reading_as_its_representation_beside_a_tag_refused,
            // json_target_abstract_reading_as_its_representation_beside_a_decimal_refused};
            // json_target_abstract_beside_a_decimal_refused (impl Cast {}) stays refused.
            // CR claude for eric: [design/low] textdata trusts an abstract type whose Cast
            // implementation writes to_value (Shape::Unknown shares a form with nothing),
            // whatever its from_value does, while one that writes only from_value is judged
            // to read every value it is tried on (Shape::Foreign(_, true)). With a
            // from_value that reads a string s as Id(str::len(s)), [Id, `A] is refused when
            // the impl leaves to_value out and admitted when it writes one, and in both the
            // cast reads `A back as Id(1). Rule needed: (a) a written from_value reads any
            // value wherever it stands, so the second is refused too; or (b) what a written
            // method reads is the impl's duty, as from_value(to_value(x)) == x is
            // (design/abstract_cast.md), so neither is judged and Shape::Foreign's flag
            // goes. Recommend (a): JsonData then holds whatever the impl writes; a
            // from_value is tried after every member keeping the data's class, so only
            // members outside it are refused beside one. Probes:
            // ~/tmp/cr29/scratch/laneB/r7/fv.txt, fv1 and fv3 (run with hand.py there)
            // (textdata-cast-trust-01)
            // 2026-10-31 claude: The to_value-only case is now judged: an implementation
            // that writes to_value and leaves from_value out reads as its representation
            // (Shape::Foreign { writes: false, reads_rep: true }), so [S, `A] over type S =
            // Abstract<string> is refused (textdata-abstract-01's round 7 note). This CR's
            // question is now only about an implementation that writes both methods, still
            // Shape::Unknown.
            Type::Abstract { id, params } => {
                let writes = self.env.written_as_representation(*id);
                let reads_rep = self.env.read_as_representation(*id);
                match self.env.abstract_reps.get(id) {
                    Some(r) if writes || reads_rep => {
                        let rep = self.node(&r.instantiate_with(params));
                        self.push(Shape::Foreign { rep, writes, reads_rep }, t)
                    }
                    _ => self.push(Shape::Unknown, t),
                }
            }
        })
    }

    /// The nodes `n` stands for past unions and the representations of
    /// abstracts that `way` meets as their representation, `n` among them,
    /// each with whether the way to it crosses such an abstract. A walk
    /// over a node's members folds over these, so it meets each node once
    /// however many ways lead to it, and a cycle among them adds nothing
    /// (the least fixpoint).
    fn reach(&self, n: usize, way: Way) -> impl Iterator<Item = (usize, bool)> + '_ {
        let (start, end) = self.reach_span(n, way);
        (start..end).map(move |i| {
            #[cfg(test)]
            {
                self.reach.borrow_mut().walked += 1;
            }
            self.reach.borrow().found[i]
        })
    }

    fn reach_span(&self, n: usize, way: Way) -> (usize, usize) {
        let mut r = self.reach.borrow_mut();
        if let Some(span) = r.spans.get(&(n, way)) {
            return *span;
        }
        let mut seen: LPooled<AHashSet<(usize, bool)>> = LPooled::take();
        let mut next: LPooled<Vec<(usize, bool)>> = LPooled::take();
        let start = r.found.len();
        next.push((n, false));
        while let Some((m, crossed)) = next.pop() {
            if !seen.insert((m, crossed)) {
                continue;
            }
            #[cfg(test)]
            {
                r.walked += 1;
            }
            r.found.push((m, crossed));
            match &self.nodes[m].0 {
                Shape::Union(ms, _) => next.extend(ms.iter().map(|m| (*m, crossed))),
                Shape::Foreign { rep, writes, .. } if *writes || way == Way::Reads => {
                    next.push((*rep, true))
                }
                _ => (),
            }
        }
        let span = (start, r.found.len());
        r.spans.insert((n, way), span);
        span
    }

    /// The node `n` stands for, past a reference's one-member union.
    fn head(&self, mut n: usize) -> usize {
        for _ in 0..self.nodes.len() {
            match &self.nodes[n].0 {
                Shape::Union(ms, _) if ms.len() == 1 => n = ms[0],
                _ => break,
            }
        }
        n
    }

    /// The tag and length of the variant with a payload `n` is.
    fn tagged(&self, n: usize) -> Option<(&ArcStr, usize)> {
        match &self.nodes[self.head(n)].0 {
            Shape::Fixed(ts, true) => match &self.nodes[ts[0]].0 {
                Shape::Lit(tag) => Some((tag, ts.len())),
                _ => None,
            },
            _ => None,
        }
    }

    /// Whether `x` may write the string `tag`.
    fn writes_text(&self, x: usize, tag: &ArcStr) -> bool {
        self.reach(x, Way::Writes).any(|(m, _)| match &self.nodes[m].0 {
            Shape::Scalar(Leaf::Str, _) => true,
            Shape::Lit(t) => t == tag,
            _ => false,
        })
    }

    /// Whether `w` may write an array of `len` values led by the string
    /// `tag`, which a union holding the variant it tags reads as that
    /// variant or not at all.
    fn writes_tag(&self, w: usize, tag: &ArcStr, len: usize) -> bool {
        self.reach(w, Way::Writes).any(|(m, _)| match &self.nodes[m].0 {
            Shape::Seq(e, _) => self.writes_text(*e, tag),
            Shape::Fixed(ts, _) => ts.len() == len && self.writes_text(ts[0], tag),
            _ => false,
        })
    }

    /// Whether `k` may write a string.
    fn holds_string(&self, k: usize) -> bool {
        self.reach(k, Way::Writes).any(|(m, _)| {
            matches!(
                self.nodes[m].0,
                Shape::Scalar(Leaf::Str, _)
                    | Shape::Unknown
                    | Shape::Foreign { writes: false, .. }
            )
        })
    }

    /// Whether a map with keys `k` may be written as an object, its keys'
    /// texts, and as an array of pairs, its keys' data (`map_shape`): a key
    /// type that may hold a string writes the strings' object as their
    /// pairs, a null or composite key writes pairs.
    fn key_forms(&self, k: usize) -> (bool, bool) {
        if self.holds_string(k) {
            return (false, true);
        }
        self.reach(k, Way::Writes).fold((false, false), |(o, p), (m, _)| {
            match &self.nodes[m].0 {
                Shape::Union(..)
                | Shape::Foreign { .. }
                | Shape::None
                | Shape::Unknown => (o, p),
                Shape::Scalar(Leaf::Null, _) => (o, true),
                Shape::Scalar(..) | Shape::Lit(_) => (true, p),
                _ => (o, true),
            }
        })
    }

    /// How `x` writes a struct's field name `n`, read as that name.
    fn name_written(&self, x: usize, n: &ArcStr) -> Outs {
        Outs::any(self.reach(x, Way::Writes).map(|(m, crossed)| {
            let o = match &self.nodes[m].0 {
                Shape::Scalar(Leaf::Str, _) => Outs::one(HELD),
                Shape::Lit(t) if t == n => Outs::one(FOREIGN | BUILT),
                _ => Outs::NONE,
            };
            if crossed { o.with(FOREIGN | BUILT) } else { o }
        }))
    }

    /// How `x` reads a struct's field name `n`.
    fn name_read(&self, x: usize, n: &ArcStr) -> Outs {
        Outs::any(self.reach(x, Way::Reads).map(|(m, crossed)| {
            let o = match &self.nodes[m].0 {
                Shape::Scalar(l, _) => {
                    let mut o = Outs::NONE;
                    for r in l.reads(Token::Text) {
                        o.insert(r << 4)
                    }
                    o
                }
                Shape::Lit(t) if t == n => Outs::one((FOREIGN | BUILT) << 4),
                Shape::Foreign { reads_rep: false, .. } => {
                    Outs::one((FOREIGN | BUILT) << 4)
                }
                _ => Outs::NONE,
            };
            if crossed { o.with((FOREIGN | BUILT) << 4) } else { o }
        }))
    }

    /// How a two-element array is written by `t` (`writer`) or read by it,
    /// `first` and `second` judging each element against the node of `t`
    /// that writes or reads it; `t` is a part of a composite.
    fn entry(&self, t: usize, writer: bool, first: Side, second: Side) -> Outs {
        let side = |bits: u8| if writer { bits } else { bits << 4 };
        let way = if writer { Way::Writes } else { Way::Reads };
        Outs::any(self.reach(t, way).map(|(m, crossed)| {
            let o = match &self.nodes[m].0 {
                Shape::Seq(e, k) => {
                    let bits = side((!k.own() as u8) * FOREIGN | self.builds(m));
                    first(self, *e).and(second(self, *e)).with(bits)
                }
                Shape::Fixed(ts, _) if ts.len() == 2 => first(self, ts[0])
                    .and(second(self, ts[1]))
                    .with(side(FOREIGN | BUILT)),
                Shape::Foreign { reads_rep: false, .. } if !writer => {
                    first(self, m).and(second(self, m)).with(side(FOREIGN | BUILT))
                }
                _ => Outs::NONE,
            };
            if crossed { o.with(side(FOREIGN | BUILT)) } else { o }
        }))
    }

    /// Whether a value `w` writes (as an object's key where `key`) may be
    /// in the reader `r`'s class and whether it may be out of it
    /// (`in_own_class`, which decides a union member's pass): a union's
    /// class is its members', a composite's all of its parts', an empty
    /// array is in any array's; a recursive type's, its finite values'.
    fn class_of(&self, r: usize, w: usize, key: bool) -> (bool, bool) {
        if self.leaf(r) && self.leaf(w) {
            return self.class_of_int(r, w, key);
        }
        let f = |p: Pair| {
            let (r, key, w) = p.parts();
            self.class_of_int(r, w, key)
        };
        Fixpoint::least(&self.classes, Pair::new(r, key, w), &f)
    }

    /// [`Self::class_of`] as the running solve of its [`Fixpoint`] has it.
    fn class_in(&self, r: usize, w: usize, key: bool) -> (bool, bool) {
        if self.leaf(r) && self.leaf(w) {
            return self.class_of_int(r, w, key);
        }
        let f = |p: Pair| {
            let (r, key, w) = p.parts();
            self.class_of_int(r, w, key)
        };
        Fixpoint::get(&self.classes, Pair::new(r, key, w), &f)
    }

    /// Whether `n` refers to no other node: a pair of such nodes is
    /// computed directly and kept in no memo.
    fn leaf(&self, n: usize) -> bool {
        matches!(
            self.nodes[n].0,
            Shape::Scalar(..) | Shape::Lit(_) | Shape::None | Shape::Unknown
        )
    }

    /// [`Self::class_of`] where `r` is a part of a composite or a member of
    /// a union: a tuple or a struct there is never in the data's class
    /// (`in_own_class` holds only marked ones of data); only the union's
    /// pick looks inside one it tries itself.
    fn part_class(&self, r: usize, w: usize, key: bool) -> (bool, bool) {
        match self.apart(r) {
            0 => self.class_in(r, w, key),
            _ => (false, true),
        }
    }

    /// `FOREIGN` where `n` is a tuple or a struct.
    fn apart(&self, n: usize) -> u8 {
        match self.nodes[self.head(n)].0 {
            Shape::Fixed(_, false) | Shape::Struct(_) => FOREIGN,
            _ => 0,
        }
    }

    /// [`Self::outs`] where `a` and `b` are parts of a composite
    /// ([`Self::part_class`]).
    fn part(&self, a: usize, key: bool, b: usize) -> Outs {
        self.outs(a, key, b).with(self.apart(a) | self.apart(b) << 4)
    }

    fn class_of_int(&self, r: usize, w: usize, key: bool) -> (bool, bool) {
        use Shape::*;
        let parts = |ps: &mut dyn Iterator<Item = (usize, usize)>| {
            ps.fold((true, false), |(o, f), (r, w)| {
                let (o1, f1) = self.part_class(r, w, key);
                (o && o1, f || f1)
            })
        };
        match (&self.nodes[w].0, &self.nodes[r].0) {
            (Union(ms, _), _) => ms.iter().fold((false, false), |(o, f), m| {
                let (o1, f1) = self.class_in(r, *m, key);
                (o || o1, f || f1)
            }),
            (Foreign { rep, writes: true, .. }, _) => self.class_in(r, *rep, key),
            (_, Union(ms, _)) => ms.iter().fold((false, true), |(o, f), m| {
                let (o1, f1) = match ms.len() {
                    1 => self.class_in(*m, w, key),
                    _ => self.part_class(*m, w, key),
                };
                (o || o1, f && f1)
            }),
            (None | Unknown | Foreign { .. }, _) | (_, Unknown) => (true, true),
            (_, None | Foreign { .. } | Lit(_) | Pairs(..) | Fixed(_, true)) => {
                (false, true)
            }
            (Scalar(x, _), Scalar(y, _)) => {
                let c = x.data_class(key) == y.class();
                (c, !c)
            }
            (Lit(_), Scalar(y, _)) => {
                let c = y.class() == Class::String;
                (c, !c)
            }
            (Scalar(..) | Lit(_), _) | (_, Scalar(..)) => (false, true),
            (Seq(e, _), Seq(f, k)) => {
                (k.own(), !k.own() || self.part_class(*f, *e, key).1)
            }
            (Fixed(ts, _), Seq(f, k)) if k.own() => {
                parts(&mut ts.iter().map(|t| (*f, *t)))
            }
            (Seq(e, _), Fixed(us, false)) => parts(&mut us.iter().map(|u| (*u, *e))),
            (Fixed(ts, _), Fixed(us, false)) if ts.len() == us.len() => {
                parts(&mut us.iter().copied().zip(ts.iter().copied()))
            }
            (Struct(fs), Struct(gs)) => {
                let field = |n: &ArcStr| gs.iter().find(|(m, _, _)| m == n);
                match fs.iter().all(|(n, _, _)| field(n).is_some()) {
                    true => parts(
                        &mut fs
                            .iter()
                            .filter_map(|(n, f, _)| field(n).map(|(_, g, _)| (*g, *f))),
                    ),
                    false => (false, true),
                }
            }
            _ => (true, true),
        }
    }

    /// How a value `a` writes (as an object's key where `key`) is read by
    /// `a`'s own member and by `b`, every pair it reaches final.
    fn least_outs(&self, a: usize, key: bool, b: usize) -> Outs {
        if self.leaf(a) && self.leaf(b) {
            return self.outs_int(a, key, b);
        }
        let f = |p: Pair| {
            let (a, key, b) = p.parts();
            self.outs_int(a, key, b)
        };
        Fixpoint::least(&self.fix, Pair::new(a, key, b), &f)
    }

    /// [`Self::least_outs`] as the running solve of its [`Fixpoint`] has
    /// it.
    fn outs(&self, a: usize, key: bool, b: usize) -> Outs {
        if self.leaf(a) && self.leaf(b) {
            return self.outs_int(a, key, b);
        }
        let f = |p: Pair| {
            let (a, key, b) = p.parts();
            self.outs_int(a, key, b)
        };
        Fixpoint::get(&self.fix, Pair::new(a, key, b), &f)
    }

    fn outs_int(&self, a: usize, key: bool, b: usize) -> Outs {
        use Shape::*;
        // a value is in its member's class as the node writing it is, past
        // every union and abstract
        match (&self.nodes[a].0, &self.nodes[b].0) {
            (Union(..), _) => Outs::any(
                self.reach(a, Way::Writes)
                    .filter(|(m, _)| {
                        !matches!(self.nodes[*m].0, Union(..) | Foreign { .. })
                    })
                    .map(|(m, crossed)| {
                        let (own, foreign) = self.class_of(a, m, key);
                        let o = self.outs(m, key, b).classed(0, own, foreign);
                        if crossed { o.with(BUILT) } else { o }
                    }),
            ),
            (Foreign { rep, writes: true, .. }, _) => {
                self.outs(*rep, key, b).with(FOREIGN | BUILT)
            }
            (Foreign { .. }, _) => Outs::NONE,
            (_, Union(..)) => self.union_read(a, key, b),
            (_, Foreign { reads_rep: false, .. }) => {
                let read = |o: u8| Outs::one(o & 15 | (FOREIGN | BUILT) << 4);
                Outs::any(self.outs(a, key, a).iter().map(read))
            }
            (_, Foreign { rep, .. }) => {
                self.outs(a, key, *rep).with((FOREIGN | BUILT) << 4)
            }
            (None | Unknown, _) | (_, None | Unknown) => Outs::NONE,
            _ => self.direct(a, key, b).with(self.builds(a) | self.builds(b) << 4),
        }
    }

    /// The level a reading by `n` has whatever its parts: built, but for
    /// an array (held where every element is) and a primitive.
    fn builds(&self, n: usize) -> u8 {
        match &self.nodes[n].0 {
            Shape::Seq(_, SeqKind::Array) | Shape::Scalar(..) => HELD,
            _ => BUILT,
        }
    }

    /// The values the primitive or tag `w` writes (as an object's key where
    /// `key`), each with each way `w` reads it back.
    fn written(&self, w: usize, key: bool) -> SmallVec<[(Token, u8); 8]> {
        match &self.nodes[w].0 {
            Shape::Scalar(x, _) => x
                .writes()
                .iter()
                .flat_map(|t| t.written(key))
                .flat_map(|t| x.reads(t).map(move |ra| (t, ra)))
                .collect(),
            Shape::Lit(_) => SmallVec::from_slice(&[(Token::TagText, FOREIGN | BUILT)]),
            _ => SmallVec::new(),
        }
    }

    /// Each way the primitive or tag `r` reads a value of `t` that the
    /// primitive or tag `w` writes and reads back `ra`: one primitive
    /// reads one value one way.
    fn token_read(&self, r: usize, w: usize, t: Token, ra: u8) -> SmallVec<[u8; 2]> {
        match (&self.nodes[w].0, &self.nodes[r].0) {
            (Shape::Scalar(x, _), Shape::Scalar(y, _)) if x == y => {
                SmallVec::from_slice(&[ra])
            }
            (_, Shape::Scalar(y, _)) => y.reads(t).collect(),
            (Shape::Lit(u), Shape::Lit(v)) if u != v => SmallVec::new(),
            (_, Shape::Lit(_)) if t == Token::TagText => {
                SmallVec::from_slice(&[FOREIGN | BUILT])
            }
            _ => SmallVec::new(),
        }
    }

    /// How the union `b` reads what `a` writes: as each node it stands for
    /// reads it, a primitive set by its pick ([`Self::pick`]).
    fn union_read(&self, a: usize, key: bool, b: usize) -> Outs {
        let (own, foreign) = self.class_of(b, a, key);
        let w = match &self.nodes[a].0 {
            Shape::Scalar(x, _) => Some(Some(*x)),
            Shape::Lit(_) => Some(None),
            _ => None,
        };
        let mut picked: LPooled<AHashSet<usize>> = LPooled::take();
        let mut o = Outs::NONE;
        if let Some(w) = w {
            for (m, crossed) in self.reach(b, Way::Reads) {
                if let (false, Shape::Union(ms, true)) = (crossed, &self.nodes[m].0) {
                    picked.extend(ms.iter().copied());
                    for (t, ra) in self.written(a, key) {
                        for (_, rb) in self.pick(ms, w, t, ra) {
                            o.insert(ra | rb << 4)
                        }
                    }
                }
            }
        }
        let read = self.reach(b, Way::Reads).filter(|(m, crossed)| {
            !crossed
                && !matches!(self.nodes[*m].0, Shape::Union(..))
                && !picked.contains(m)
        });
        o.or(Outs::any(read.map(|(m, _)| self.outs(a, key, m)))).classed(4, own, foreign)
    }

    /// How a value `a` writes is read by `b`, neither a union nor an
    /// abstract, its levels as their parts' alone.
    fn direct(&self, a: usize, key: bool, b: usize) -> Outs {
        use Shape::*;
        match (&self.nodes[a].0, &self.nodes[b].0) {
            (Scalar(..) | Lit(_), Scalar(..) | Lit(_)) => {
                let mut o = Outs::NONE;
                for (t, ra) in self.written(a, key) {
                    for rb in self.token_read(b, a, t, ra) {
                        o.insert(ra | rb << 4)
                    }
                }
                o
            }
            _ if key => Outs::NONE,
            (Seq(e, ka), Seq(f, kb)) => {
                self.part(*e, false, *f).star().with(own(ka.own(), kb.own()))
            }
            (Seq(e, ka), Fixed(ts, _)) => {
                Outs::all(ts.iter().map(|t| self.part(*e, false, *t)))
                    .with(own(ka.own(), true))
            }
            (Fixed(ts, _), Seq(f, kb)) => {
                Outs::all(ts.iter().map(|t| self.part(*t, false, *f)))
                    .with(own(true, kb.own()))
            }
            (Fixed(ts, _), Fixed(us, _)) if ts.len() == us.len() => {
                Outs::all(ts.iter().zip(us.iter()).map(|(t, u)| self.part(*t, false, *u)))
            }
            (Pairs(k, v), _) => self.map_read(*k, *v, b),
            (Struct(fs), _) => self.struct_read(fs, b),
            (Seq(e, ka), Pairs(j, u)) => {
                let k: Side = &|s, x| s.part(x, false, *j);
                let v: Side = &|s, y| s.part(y, false, *u);
                self.entry(*e, true, k, v).star().with(own(ka.own(), false))
            }
            (Fixed(ts, _), Pairs(j, u)) => {
                let k: Side = &|s, x| s.part(x, false, *j);
                let v: Side = &|s, y| s.part(y, false, *u);
                Outs::all(ts.iter().map(|t| self.entry(*t, true, k, v)))
                    .with(own(true, false))
            }
            (Seq(e, ka), Struct(gs)) => Outs::all(gs.iter().map(|(n, g, og)| {
                let k: Side = &|s, x| s.name_written(x, n);
                let v: Side = &|s, y| s.part(y, false, *g);
                let m = self.entry(*e, true, k, v);
                if *og { m.or(Outs::one(HELD)) } else { m }
            }))
            .with(own(ka.own(), true)),
            (Fixed(ts, _), Struct(gs)) => {
                let required = gs.iter().filter(|(_, _, o)| !o).count();
                if ts.len() < required || ts.len() > gs.len() {
                    return Outs::NONE;
                }
                Outs::all(ts.iter().map(|t| {
                    Outs::any(gs.iter().map(|(n, g, _)| {
                        let k: Side = &|s, x| s.name_written(x, n);
                        let v: Side = &|s, y| s.part(y, false, *g);
                        self.entry(*t, true, k, v)
                    }))
                }))
            }
            _ => Outs::NONE,
        }
    }

    /// How a map's entries are read by its own member and by `b`.
    fn map_read(&self, k: usize, v: usize, b: usize) -> Outs {
        let (text, data) = self.key_forms(k);
        let forms = [text.then_some(true), data.then_some(false)];
        Outs::any(forms.into_iter().flatten().map(|key| {
            let kw: Side = &|s, x| s.part(k, key, x);
            let vw: Side = &|s, y| s.part(v, false, y);
            match &self.nodes[b].0 {
                Shape::Pairs(j, u) => {
                    let e = self.outs(k, key, *j).and(self.outs(v, false, *u));
                    e.star().with(own(false, false))
                }
                Shape::Seq(f, kb) => {
                    self.entry(*f, false, kw, vw).star().with(own(false, kb.own()))
                }
                Shape::Fixed(us, _) => {
                    Outs::all(us.iter().map(|t| self.entry(*t, false, kw, vw)))
                        .with(own(false, true))
                }
                Shape::Struct(gs) => Outs::all(gs.iter().map(|(n, g, og)| {
                    let m = self.name_written(k, n).and(self.part(v, false, *g));
                    if *og { m.or(Outs::one(HELD)) } else { m }
                }))
                .with(own(false, true)),
                _ => Outs::NONE,
            }
        }))
    }

    /// How a struct's pairs, sorted by name, are read by its own member and
    /// by `b`. JSON writes every field, TOML leaves a null one out.
    fn struct_read(&self, fs: &[(ArcStr, usize, bool)], b: usize) -> Outs {
        let toml = self.format == Format::Toml;
        let left_out =
            |m: Outs, o: bool| if toml && o { m.or(Outs::one(HELD)) } else { m };
        let field = |gs: &[(ArcStr, usize, bool)], n: &ArcStr| {
            gs.iter().find(|(m, _, _)| m == n).map(|(_, g, o)| (*g, *o))
        };
        match &self.nodes[b].0 {
            Shape::Struct(gs) => {
                if gs.iter().any(|(n, _, o)| !o && field(fs, n).is_none()) {
                    return Outs::NONE;
                }
                let mut r = Outs::one(0);
                for (n, f, of) in fs.iter() {
                    match field(gs, n) {
                        Some((g, og)) => {
                            r = r.and(left_out(self.part(*f, false, g), *of && og))
                        }
                        None if toml && *of => (),
                        None => return Outs::NONE,
                    }
                }
                r
            }
            Shape::Pairs(j, u) => Outs::all(fs.iter().map(|(n, f, o)| {
                let m = self.name_read(*j, n).and(self.part(*f, false, *u));
                left_out(m, *o)
            }))
            .with(own(true, false)),
            Shape::Seq(g, kb) => Outs::all(fs.iter().map(|(n, f, o)| {
                let k: Side = &|s, x| s.name_read(x, n);
                let v: Side = &|s, y| s.part(*f, false, y);
                left_out(self.entry(*g, false, k, v), *o)
            }))
            .with(own(true, kb.own())),
            Shape::Fixed(us, _) if us.len() == fs.len() => {
                Outs::all(us.iter().zip(fs.iter()).map(|(t, (n, f, _))| {
                    let k: Side = &|s, x| s.name_read(x, n);
                    let v: Side = &|s, y| s.part(*f, false, y);
                    self.entry(*t, false, k, v)
                }))
            }
            Shape::Fixed(us, _)
                if toml && us.len() >= fs.iter().filter(|(_, _, o)| !o).count() =>
            {
                Outs::all(us.iter().map(|t| {
                    Outs::any(fs.iter().map(|(n, f, _)| {
                        let k: Side = &|s, x| s.name_read(x, n);
                        let v: Side = &|s, y| s.part(*f, false, y);
                        self.entry(*t, false, k, v)
                    }))
                }))
            }
            _ => Outs::NONE,
        }
    }

    /// The pass of a union's pick that tries the member `n` reading a value
    /// with `bits` (`cast_member`): a tuple, then a struct, keeping the
    /// data's class, then any other member keeping it (a union where one of
    /// its members does, `in_own_class`), then the rest.
    fn pass(&self, n: usize, bits: u8) -> Option<u8> {
        let inside = bits & FOREIGN == 0;
        Some(match &self.nodes[self.head(n)].0 {
            Shape::None | Shape::Unknown => return None,
            Shape::Fixed(_, false) if inside => 0,
            Shape::Struct(_) if inside => 1,
            Shape::Scalar(..) | Shape::Seq(..) | Shape::Union(..) if inside => 2,
            _ => 3,
        })
    }

    /// Whether a value the member `w` of a union writes (as an object's key
    /// where `key`) may read back as its member `r`, `w_first` where `w`
    /// comes first in the union.
    fn takes(&self, w: usize, r: usize, key: bool, w_first: bool) -> bool {
        let (tw, tr) = (self.tagged(w), self.tagged(r));
        if let Some((tag, len)) = tr
            && tw != tr
            && self.writes_tag(w, tag, len)
        {
            return true;
        }
        if tw.is_some() && tw != tr {
            return false;
        }
        self.least_outs(w, key, r).iter().any(|o| {
            let (a, b) = (o & 15, o >> 4);
            if (a ^ b) & ROUNDS != 0 {
                return a & ROUNDS != 0;
            }
            let crossed = |x: u8| x & LEVEL == CROSSED;
            let kept = |x: u8| x & LEVEL <= BUILT;
            let (first, same) = match (tw.is_some(), self.pass(w, a), self.pass(r, b)) {
                (true, _, _) => (Some(w_first), true),
                (false, Some(x), Some(y)) if x != y => (Some(x < y), false),
                (false, Some(_), Some(_)) => (Some(w_first), true),
                _ => (None, false),
            };
            // a member the value already is takes it at once within its pass
            let held = |x: u8| same && x & LEVEL == HELD;
            match first {
                Some(true) => !held(a) && (held(b) || crossed(a) && kept(b)),
                _ => held(b) || !held(a) && !(crossed(b) && kept(a)),
            }
        })
    }

    /// [`Self::takes`] for the members `w` and `r` of the primitive set
    /// `ms`: a value `w` writes reads back as `r` where the set's pick may
    /// take `r`.
    fn prim_takes(&self, ms: &[usize], w: usize, r: usize, key: bool) -> bool {
        let Shape::Scalar(x, _) = self.nodes[w].0 else { return false };
        self.written(w, key)
            .into_iter()
            .any(|(t, ra)| self.pick(ms, Some(x), t, ra).iter().any(|(m, _)| *m == r))
    }

    /// The primitive set `ms`'s reading of a value of `t` that `w` (a tag
    /// where `None`) writes and reads back `ra` (`cast_inner`'s
    /// `Type::Primitive` arm), each way it may go, with the member taking
    /// it: the member the value read back already is; else the first that
    /// holds it exactly, by class and then bit, real text an integer holds
    /// going to the first later real that holds it too; else the first real
    /// that rounds it. A member that may miss some value of `t` lets the
    /// next one read it; `w`'s own member reads it `ra`.
    fn pick(
        &self,
        ms: &[usize],
        w: Option<Leaf>,
        t: Token,
        ra: u8,
    ) -> SmallVec<[(usize, u8); 4]> {
        let mut order: SmallVec<[(usize, Leaf, Typ); 8]> = ms
            .iter()
            .filter_map(|m| match &self.nodes[*m].0 {
                Shape::Scalar(l, ty) => Some((*m, *l, *ty)),
                _ => None,
            })
            .collect();
        let ways = |l: Leaf| -> SmallVec<[u8; 2]> {
            let ws: SmallVec<[u8; 2]> = match Some(l) == w {
                true => SmallVec::from_slice(&[ra]),
                false => l.reads(t).collect(),
            };
            ws.into_iter().map(|o| o & !FOREIGN).collect()
        };
        let total = |l: Leaf| Some(l) == w || l.total(w, t);
        let exactly = |l: Leaf| {
            let ws = ways(l);
            total(l) && !ws.is_empty() && ws.iter().all(|o| o & ROUNDS == 0)
        };
        let mut r = SmallVec::new();
        let v = t.read_back();
        if let Some((m, l, _)) = order.iter().find(|(_, _, ty)| *ty == v) {
            r.extend(ways(*l).into_iter().map(|o| (*m, o)));
            return r;
        }
        let own = |ty: Typ| if v.is_number() { ty.is_number() } else { ty == v };
        order.sort_by_key(|(_, _, ty)| {
            (!(v.is_real() && ty.is_real()), !own(*ty), *ty as u64)
        });
        let real_text = matches!(t, Token::NumText(n) if n.real());
        let mut reals_only = false;
        for (i, (m, l, ty)) in order.iter().enumerate() {
            if reals_only && !ty.is_real() {
                continue;
            }
            let exact = ways(*l).into_iter().filter(|o| o & ROUNDS == 0);
            if real_text && ty.is_integer() {
                let later =
                    order[i + 1..].iter().any(|(_, l, ty)| ty.is_real() && exactly(*l));
                if !later {
                    r.extend(exact.map(|o| (*m, o)))
                }
                reals_only |= exactly(*l);
                continue;
            }
            r.extend(exact.map(|o| (*m, o)));
            if exactly(*l) {
                return r;
            }
        }
        if reals_only {
            return r;
        }
        for (m, l, _) in order.iter().filter(|(_, _, ty)| ty.is_real()) {
            r.extend(ways(*l).into_iter().filter(|o| o & ROUNDS != 0).map(|o| (*m, o)));
            if total(*l) {
                break;
            }
        }
        r
    }
}

impl Type {
    /// Why a value of this type written as `format` data may read back as
    /// another value; `None` when every union reads back what each member
    /// writes.
    pub fn text_ambiguity(&self, env: &Env, format: Format) -> Option<TextAmbiguity> {
        Shapes::new(env, format, self).ambiguity()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{expr::parser::parse_type, typ::cast::tests::typed};

    fn ambiguous(t: &str, format: Format) -> bool {
        let env = Env::default();
        parse_type(t).unwrap().text_ambiguity(&env, format).is_some()
    }

    /// [`ambiguous`] over the typedefs `defs`; graphix-package-json's
    /// cross-check reads these calls from this file.
    fn named(defs: &[(&str, &str)], t: &str, format: Format) -> bool {
        let (env, t) = typed(defs, t);
        t.text_ambiguity(&env, format).is_some()
    }

    #[test]
    fn a_recursive_pair_claims_what_its_values_reach() {
        assert!(!named(
            &[("TA", "[{a: [u64, null]}, (TB, duration)]"), ("TB", "[`Nil, (TB, TA)]")],
            "[TA, TB]",
            Format::Json
        ));
        assert!(!named(
            &[("TA", "[u64, {a: i32, b: TA}]"), ("TB", "[`B(TB, i8), List<TB>]")],
            "[TA, TB]",
            Format::Json
        ));
        assert!(!named(
            &[
                ("TA", "[`A(TB, TA), Map<i64, TB>]"),
                ("TB", "[{a: TA, x: [TA, null]}, {x: decimal}]")
            ],
            "[TA, TB]",
            Format::Json
        ));
        assert!(!named(
            &[("TA", "[{a: string, x: [(f64, TB), null]}, i64]"), ("TB", "Map<f64, TA>")],
            "[TA, TB]",
            Format::Json
        ));
        assert!(!named(
            &[("B", "[(string, string), null]"), ("R", "[Array<R>, i64]")],
            "[B, R]",
            Format::Json
        ));
        for f in [Format::Json, Format::Toml] {
            assert!(named(
                &[("TA", "[i64, `Cons(f32, TA)]"), ("TB", "[f64, `Cons(f64, TB)]")],
                "[TA, TB]",
                f
            ));
            assert!(!named(
                &[("TA", "[`NA, (f32, TA)]"), ("TB", "[`NB, (f64, TB)]")],
                "[TA, TB]",
                f
            ));
        }
    }

    // A typedef expands once for each list of arguments it is applied to,
    // so a recursion that passes its parameters on is a cycle; one that
    // grows them at every level never ends, and is refused.
    #[test]
    fn an_applied_typedef_expands_once_per_arguments() {
        for f in [Format::Json, Format::Toml] {
            assert!(!named(&[("R<'a>", "[`Nil, `Cons('a, R<'a>)]")], "{k: R<i64>}", f));
            assert!(!named(
                &[("R<'a>", "[`Nil, `Cons('a, R<'a>)]"), ("I", "i64")],
                "{k: R<I>}",
                f
            ));
            assert!(!named(
                &[("P<'a, 'b>", "[`N, `C('a, P<'b, 'a>)]")],
                "{k: P<i64, string>}",
                f
            ));
            assert!(named(
                &[("R<'a>", "[`Nil, `Cons('a, R<'a>)]")],
                "{k: [R<i64>, R<string>]}",
                f
            ));
        }
        assert!(!named(
            &[("A<'a>", "['a, `Next(A<Array<i64>>)]")],
            "A<i64>",
            Format::Json
        ));
        let defs = [
            ("R<'a>", "[`Nil, `Cons('a, R<'a>)]"),
            ("P<'a, 'b>", "[`N, `C('a, P<'b, 'a>)]"),
        ];
        let (env, t) = typed(&defs, "[R<u64>, P<i64, f32>]");
        let s = Shapes::new(&env, Format::Json, &t);
        assert!(s.nodes.len() < 40, "{} nodes", s.nodes.len());
        let (env, t) = typed(&[("N<'a>", "[`Nil, `Cons('a, N<Array<'a>>)]")], "N<i64>");
        assert!(matches!(
            t.text_ambiguity(&env, Format::Json),
            Some(TextAmbiguity::Grows(n)) if n.to_string() == "N"
        ));
    }

    // A bool rounds into a real member, and a tuple or a struct is in the
    // data's class only where the union's pick tries it itself, never as a
    // part or a member of a union inside a member.
    #[test]
    fn a_bool_rounds_into_a_real_and_a_part_is_apart() {
        let json = |t| ambiguous(t, Format::Json);
        assert!(named(
            &[("A", "Array<f64>"), ("P", "[bool, (f32, bool)]")],
            "[A, P]",
            Format::Json
        ));
        assert!(named(
            &[("TA", "[f64, Array<TA>]"), ("TB", "[bool, (f32, TB)]")],
            "[TA, TB]",
            Format::Json
        ));
        assert!(!json("[Array<f64>, (f32, bool)]"));
        assert!(json("[(Array<string>, i64), ((u8, u8), u8)]"));
        assert!(!json("[((u8, u8), u8), (string, string)]"));
        assert!(!json("[f64, bool]"));
    }

    /// `L0 = [i64, null]`, each `L(k+1)` a union of `width` members over
    /// `Lk`, the outermost `L(depth)` the type.
    fn levels(depth: usize, width: usize) -> (Env, Type) {
        let tails = [", string)", ", bool, string)", ", null, string)"];
        let mut defs = vec![("L0".to_string(), "[i64, null]".to_string())];
        for k in 1..=depth {
            let below = format!("L{}", k - 1);
            let members = std::iter::once(format!("Array<{below}>"))
                .chain(tails[..width - 1].iter().map(|t| format!("({below}{t}")));
            let body = format!("[{}]", members.collect::<Vec<_>>().join(", "));
            defs.push((format!("L{k}"), body));
        }
        let defs: Vec<(&str, &str)> =
            defs.iter().map(|(n, b)| (n.as_str(), b.as_str())).collect();
        typed(&defs, &format!("L{depth}"))
    }

    // A check meets at most two pairs (a value and a key) per pair of
    // nodes, however the unions nest, and evaluates each once, and again
    // only when a pair it read grew: a type with no recursion once each, a
    // recursive group or a chain whose facts cross back link by link under
    // twice each. The wall limit is only a hang guard; the counts are the
    // pin.
    #[test]
    fn a_check_evaluates_each_pair_a_few_times() {
        type Shape = (&'static str, fn() -> (Env, Type), bool, usize);
        let shapes: [Shape; 9] = [
            ("L10 x2", || levels(10, 2), false, 1),
            ("L14 x2", || levels(14, 2), false, 1),
            ("L6 x4", || levels(6, 4), false, 1),
            ("L8 x4", || levels(8, 4), false, 1),
            ("mutual 32", || mutual(32), true, 2),
            ("ring 32", || ring(32), false, 1),
            ("tree 32", || tree(32), false, 1),
            ("fact 50", || fact(50), false, 2),
            ("fact 200", || fact(200), false, 2),
        ];
        let (tx, rx) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            for (_, make, _, _) in shapes {
                let (env, t) = make();
                let s = Shapes::new(&env, Format::Json, &t);
                let ambiguous = s.ambiguity().is_some();
                let f = s.fix.borrow();
                tx.send((ambiguous, f.evaluated, f.pairs.len(), s.nodes.len())).unwrap();
            }
        });
        for (name, _, refused, passes) in shapes {
            let (ambiguous, evaluated, pairs, nodes) =
                rx.recv_timeout(std::time::Duration::from_secs(300)).expect("hung");
            assert_eq!(ambiguous, refused, "{name}");
            assert!(pairs <= 2 * nodes * nodes, "{name}: {pairs} pairs of {nodes} nodes");
            assert!(
                evaluated <= passes * pairs,
                "{name}: {evaluated} evaluations of {pairs}"
            );
        }
    }

    /// `U0 = [i64, bool]`, ``V0 = [`T0, datetime]``, each `U(i)` the union of
    /// `U(i-1)` and `V(i-1)` and each `V(i)` of `U(i-1)` and `` `T(i) ``,
    /// and a map keyed by `U(n)`: the ways from the key to its members grow
    /// as the Fibonacci numbers do.
    fn dag(n: usize) -> (Env, Type) {
        let mut defs = vec![
            ("U0".to_string(), "[i64, bool]".to_string()),
            ("V0".to_string(), "[`T0, datetime]".to_string()),
        ];
        for i in 1..=n {
            defs.push((format!("U{i}"), format!("[U{}, V{}]", i - 1, i - 1)));
            defs.push((format!("V{i}"), format!("[U{}, `T{i}]", i - 1)));
        }
        generated(defs, &format!("Map<U{n}, i64>"))
    }

    // A walk over a node's members meets each node it reaches once,
    // however many ways lead there: the key's few walks cost a few steps a
    // node (its unions share members, so the check refuses it). The wall
    // limit is only a hang guard; the count is the pin.
    #[test]
    fn a_walk_meets_each_node_once() {
        let (tx, rx) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            let (env, t) = dag(32);
            let s = Shapes::new(&env, Format::Json, &t);
            let ambiguous = s.ambiguity().is_some();
            tx.send((ambiguous, s.reach.borrow().walked, s.nodes.len())).unwrap()
        });
        let (ambiguous, walked, nodes) =
            rx.recv_timeout(std::time::Duration::from_secs(300)).expect("hung");
        assert!(ambiguous);
        assert!(walked <= 8 * nodes, "{walked} steps over {nodes} nodes");
    }

    // A union member of a union is tried in the pass of the members keeping
    // the data's class where any of its members keeps it, else after them,
    // in the union's order, as a scalar is.
    #[test]
    fn a_union_member_is_tried_by_its_class() {
        for f in [Format::Json, Format::Toml] {
            assert!(!named(&[("Num", "[i64, f64]")], "{k: [Num, string]}", f));
            assert!(!named(&[("Num", "[i64, f64]")], "{k: [Num, bool]}", f));
            assert!(!named(&[("Txt", "[string, bool]")], "{k: [Txt, i64]}", f));
        }
        assert!(!named(
            &[("Opt", "[i64, null]")],
            "{a: [Opt, string], b: Array<[Opt, bool]>}",
            Format::Json
        ));
        assert!(!named(&[("Opt", "[i64, null]")], "[Opt, Array<Opt>]", Format::Json));
        assert!(named(
            &[("Num", "[i64, f64]"), ("Txt", "[string, `None]")],
            "[Num, Txt]",
            Format::Json
        ));
        assert!(named(&[("Num", "[i64, f64]")], "[Num, f32]", Format::Json));
        assert!(named(&[("Num", "[u8, f64]")], "[Num, i64]", Format::Json));
        assert!(named(&[("Txt", "[datetime, bool]")], "[Txt, string]", Format::Json));
        for f in [Format::Json, Format::Toml] {
            assert!(!named(
                &[("Shape", "[{w: f64, h: f64}, f64]")],
                "{k: [Shape, string]}",
                f
            ));
            assert!(!named(&[("Ev", "[`Click(i64), string]")], "{k: [Ev, i64]}", f));
        }
        assert!(!named(&[("Ev", "[`Click(i64), string]")], "[Ev, bool]", Format::Json));
        assert!(!named(&[("Ev", "[`Click(i64), string]")], "[Ev, f64]", Format::Json));
        assert!(!named(&[("D", "[duration, string]")], "[D, bool]", Format::Json));
        assert!(!named(&[("W", "[string, (i64, i64)]")], "[W, bool]", Format::Json));
        assert!(!named(&[("V", "[Map<string, f64>, f64]")], "[V, string]", Format::Json));
        assert!(!named(
            &[("Shape", "[{w: f64, h: f64}, f64]")],
            "{a: [Shape, string], b: Array<[Shape, null]>}",
            Format::Json
        ));
        assert!(named(&[("D", "[duration, string]")], "[D, f64]", Format::Json));
    }

    // One primitive reads one value one way: where both members hold an
    // f32 at a position, the writer's own f32 rounds the value exactly
    // when the other's does. TOML's native datetime an integer reads.
    #[test]
    fn one_primitive_reads_a_value_one_way() {
        for f in [Format::Json, Format::Toml] {
            assert!(!named(
                &[("A", "(i32, f32)"), ("B", "(datetime, f32)")],
                "{k: [A, B]}",
                f
            ));
        }
        assert!(!named(
            &[("R<'a>", "[`Nil('a), `X('a, f32)]")],
            "{k: [R<i32>, R<datetime>]}",
            Format::Json
        ));
        assert!(named(
            &[("R<'a>", "[`Nil('a), `X('a, f32)]")],
            "{k: [R<i32>, R<datetime>]}",
            Format::Toml
        ));
        assert!(named(
            &[("A", "(i64, f32)"), ("B", "(i64, f64)")],
            "[A, B]",
            Format::Json
        ));
    }

    // A primitive set reads a value as one member, the first holding it
    // exactly by class and then bit, and a member the value read back
    // already is takes it within its pass, whatever the union's order.
    #[test]
    fn a_primitive_set_reads_by_its_pick() {
        assert!(!named(
            &[("Amount", "[f64, string]"), ("Qty", "i64")],
            "Array<{amount: [Amount, Qty]}>",
            Format::Json
        ));
        assert!(!named(
            &[("Amount", "[f64, string]"), ("Age", "i64")],
            "Array<{amount: [Amount, Age]}>",
            Format::Json
        ));
        for f in [Format::Json, Format::Toml] {
            assert!(!named(
                &[("Amount", "[f64, string]"), ("Count", "u32")],
                "{total: [Amount, Count]}",
                f
            ));
            assert!(!named(
                &[("Label", "[string, i64]"), ("Score", "f64")],
                "{v: [Label, Score]}",
                f
            ));
        }
        assert!(named(&[("V", "[f32, string]"), ("W", "i64")], "[V, W]", Format::Json));
        assert!(named(
            &[("V", "[datetime, string]"), ("W", "i64")],
            "[V, W]",
            Format::Json
        ));
    }

    /// The typedefs `defs`, each `(name, body)`, and `t` over them.
    fn generated(defs: Vec<(String, String)>, t: &str) -> (Env, Type) {
        let defs: Vec<(&str, &str)> =
            defs.iter().map(|(n, b)| (n.as_str(), b.as_str())).collect();
        typed(&defs, t)
    }

    /// `n` typedefs, each a union over the next two, wrapping round: a
    /// recursive group whose members a check refuses.
    fn mutual(n: usize) -> (Env, Type) {
        let defs = (0..n).map(|i| {
            let (a, b) = ((i + 1) % n, (i + 2) % n);
            let body = format!("[`N{i}, `A{i}(M{a}, f64), (M{b}, string), Array<M{a}>]");
            (format!("M{i}"), body)
        });
        generated(defs.collect(), "M0")
    }

    /// `n` typedefs in a ring, each a struct, a tuple and an array: a
    /// recursive group whose members a check admits, every pair judged.
    fn ring(n: usize) -> (Env, Type) {
        let defs = (0..n).map(|i| {
            let j = (i + 1) % n;
            let body =
                format!("[{{f{i}: T{j}, g: [T0, null]}}, (T{j}, bool), Array<string>]");
            (format!("T{i}"), body)
        });
        generated(defs.collect(), "T0")
    }

    /// Two chains of `n` typedefs linked both ways through tuples, a
    /// primitive at one end of each: what the far end writes is known only
    /// once a fact has crossed every link back.
    fn fact(n: usize) -> (Env, Type) {
        let mut defs = vec![];
        for (p, base, end) in [("A", "i64", "`EA"), ("B", "f64", "`EB")] {
            for i in 0..=n {
                let mut ms = vec![];
                if i == 0 {
                    ms.push(base.to_string())
                }
                if i < n {
                    ms.push(format!("(`F, {p}{})", i + 1))
                }
                if i > 0 {
                    ms.push(format!("(`G, {p}{})", i - 1));
                    ms.push(end.to_string())
                }
                defs.push((format!("{p}{i}"), format!("[{}]", ms.join(", "))))
            }
        }
        generated(defs, "[A0, B0]")
    }

    /// `n` struct kinds, each holding the union of them all.
    fn tree(n: usize) -> (Env, Type) {
        let mut defs: Vec<(String, String)> = (0..n)
            .map(|i| {
                (
                    format!("S{i}"),
                    format!("{{k{i}: string, kids: Array<U>, opt: [U, null]}}"),
                )
            })
            .collect();
        let ms: Vec<String> = (0..n).map(|i| format!("S{i}")).collect();
        defs.push(("U".into(), format!("[{}, (U, U, bool)]", ms.join(", "))));
        generated(defs, "U")
    }

    #[test]
    fn unions_whose_members_write_alike() {
        let json = |t| ambiguous(t, Format::Json);
        let toml = |t| ambiguous(t, Format::Toml);
        assert!(json("[u8, i64]"));
        assert!(!json("[i64, f64]"));
        assert!(!json("[f64, u8]"));
        assert!(json("[f32, i64]"));
        assert!(toml("[f32, i64]"));
        assert!(json("[f32, i32]"));
        assert!(!json("[f32, u8]"));
        assert!(!json("[f32, i16]"));
        assert!(!toml("[f32, u16]"));
        assert!(!json("[(f32, i64), (u8, i64)]"));
        assert!(json("[(f32, i64), (u32, i64)]"));
        assert!(!json("[(f32, f32), (u8, u8)]"));
        assert!(!json("[Array<f32>, (u8, u8)]"));
        assert!(!json("[(i64, i64), (f64, f64)]"));
        assert!(!json("[(i64, i64), Array<f64>]"));
        assert!(json("[(f32, u16), (i64, f32)]"));
        assert!(json("[(f32, u8), (u32, f32)]"));
        assert!(json("[(f32, u8), (f64, f32)]"));
        assert!(json("[(f32, u8), (f64, f64)]"));
        assert!(json("[(f32, f32), (u8, f64)]"));
        assert!(json("[Array<f32>, (f64, u8)]"));
        assert!(json("[{a: f32, b: u8}, {a: f64, b: f32}]"));
        assert!(json("[{a: f32, b: [u8, null]}, {a: f64, b: [f64, null]}]"));
        assert!(toml("{k: [(f32, u8), (f64, f32)]}"));
        assert!(toml("{k: [(f32, u16), (i64, f32)]}"));
        assert!(json("[(duration, u8), (f64, f64)]"));
        assert!(json("[(decimal, u8), (i64, f64)]"));
        assert!(json("[duration, u8]"));
        assert!(json("[f32, f64]"));
        assert!(json("[duration, i64]"));
        assert!(!toml("[duration, i64]"));
        assert!(json("Map<[duration, i64], i64>"));
        assert!(!json("[duration, bool]"));
        assert!(!json("[i64, null]"));
        assert!(json("[string, datetime]"));
        assert!(!toml("[string, datetime]"));
        assert!(!json("[decimal, string]"));
        assert!(json("[decimal, f64]"));
        assert!(json("[duration, f64]"));
        assert!(json("[duration, f32]"));
        assert!(json("[Array<string>, Array<i64>]"));
        assert!(!json("[`A(i64), `B(string), `C]"));
        assert!(json("[`A, string]"));
        assert!(json("[(string, i64), `B(i64)]"));
        assert!(json("[(string, string), `B(i64)]"));
        assert!(!json("[{a: i64}, {b: i64}]"));
        assert!(!json("[{a: i64}, {a: i64, b: [i64, null]}]"));
        assert!(toml("[{a: i64}, {a: i64, b: [i64, null]}]"));
        assert!(json("[{b: i64}, {a: [i64, null], b: i64}]"));
        assert!(json("[Map<string, i64>, {a: i64}]"));
        assert!(!json("Array<{name: string, n: [i64, null]}>"));
        assert!(json("Array<[u8, i64]>"));
    }

    #[test]
    fn a_reading_across_classes_wins_where_its_own_rounds() {
        let json = |t| ambiguous(t, Format::Json);
        let toml = |t| ambiguous(t, Format::Toml);
        assert!(!json("[string, i64]"));
        assert!(!json("[bool, string]"));
        assert!(!json("[duration, string]"));
        assert!(!json("[duration, datetime]"));
        assert!(!json("[i64, datetime]"));
        assert!(!json("[f64, datetime]"));
        assert!(json("[f32, string]"));
        assert!(json("[f32, datetime]"));
        assert!(toml("[f32, string]"));
        assert!(toml("[f32, datetime]"));
        assert!(!toml("[i64, datetime]"));
        assert!(json("[(string, f32), (i64, f64)]"));
        assert!(toml("[(string, f32), (i64, f64)]"));
        assert!(json("[(bool, f32), (string, f64)]"));
        assert!(json("[(i64, f32), (string, f64)]"));
        assert!(json("[(f32, string), (f64, f64)]"));
        assert!(json("[Array<f32>, (string, f64)]"));
        assert!(json("[(datetime, f64), (i64, f32)]"));
        assert!(json("[(Map<u8, f32>, u8), (Array<(string, f64)>, f64)]"));
        assert!(!json("[(string, u8), (i64, u8)]"));
        assert!(!json("[(decimal, string), (f64, i64)]"));
        assert!(!json("[(null, f32), (string, f64)]"));
    }

    #[test]
    fn readings_outside_the_class_go_by_the_union_order() {
        let json = |t| ambiguous(t, Format::Json);
        assert!(json("[(i64, f64), (u8, [i64, string])]"));
        assert!(json("[(i64, f64), (u8, [bool, i64, string])]"));
        assert!(json("[(duration, string), (f64, bool)]"));
        assert!(json("[(duration, string), (f64, i64)]"));
        assert!(json("[(i64, duration), (u8, string)]"));
        assert!(json("[(i64, duration), (u8, datetime)]"));
        assert!(json("[(`A, string), (string, i64)]"));
        assert!(!json("[(duration, u8), (string, u8)]"));
        assert!(!json("[(u8, duration), (i64, string)]"));
        assert!(!json("[(u8, duration), (i64, datetime)]"));
        assert!(!json("[(u8, bool), (i64, string)]"));
        assert!(!json("[Map<u8, i64>, {a: i64}]"));
    }

    #[test]
    fn an_object_key_reads_back_from_its_text() {
        let json = |t| ambiguous(t, Format::Json);
        let toml = |t| ambiguous(t, Format::Toml);
        assert!(json("Map<[decimal, datetime], i64>"));
        assert!(!json("[decimal, datetime]"));
        assert!(!json("Map<[duration, datetime], i64>"));
        assert!(!json("Map<[`A, i64], i64>"));
        assert!(!json("Map<[i64, bool], i64>"));
        assert!(!json("Map<[i64, datetime], i64>"));
        assert!(!json("Map<[duration, bool], i64>"));
        assert!(!json("Map<[i64, string], i64>"));
        assert!(json("Map<[`A, string], i64>"));
        assert!(!toml("Map<[i64, datetime], i64>"));
    }
}
