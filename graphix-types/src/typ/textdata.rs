//! Which unions a text format cannot read back: two members such that a
//! JSON or TOML value one writes may read back as the other. A reader
//! picks one member of a union for a value as `cast.rs` does: the round
//! its reading falls in first (exact, then rounded to a real's precision,
//! `Holds`), a payload variant whose tag the data names before anything,
//! the members keeping the data's class before the others (`in_own_class`,
//! a primitive set in bit order), then the union's order, where a reading
//! that crosses the line between integers and reals gives way to a later
//! one keeping every number its kind (`Choice`). Values are judged by
//! class ([`Token`]), and a member's reading of a composite whole:
//! `[(f32, u8), (f64, f64)]` reads `(f32:0.1, u8:1)` back as `(0.1, 1.0)`,
//! exact where the f32's own reading rounds; `[(string, f32), (i64, f64)]`
//! reads `("1", f32:0.1)` back as `(1, 0.1)`.

use crate::{
    env::Env,
    typ::{Typ, Type},
};
use ahash::{AHashMap, AHashSet};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use smallvec::SmallVec;

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
/// writer's own member and the high one for the other: only rounded (a
/// later round), outside the value's class (a later pass), with a number
/// read across the line between integers and reals, with a number read as
/// no number (`NumReads`).
const ROUNDS: u8 = 1;
const FOREIGN: u8 = 2;
const CROSSED: u8 = 4;
const OTHER: u8 = 8;

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

    /// The values it writes.
    fn writes(self) -> &'static [Token] {
        match self {
            Leaf::Null => &[Token::Null],
            Leaf::Bool => &[Token::Bool],
            Leaf::Int(Typ::I8) => &INTS[..1],
            Leaf::Int(Typ::U8 | Typ::U16 | Typ::I16) => &INTS[..2],
            Leaf::Int(_) => &INTS,
            Leaf::F32 | Leaf::F64 | Leaf::Duration => &FLOATS,
            Leaf::Decimal => &NUMS,
            Leaf::Str => &TEXTS,
            Leaf::DateText => &[Token::DateText],
            Leaf::DateTime => &[Token::DateTime],
        }
    }

    /// Each way it may read a value of `t` exactly or rounded.
    fn reads(self, t: Token) -> impl Iterator<Item = u8> {
        let foreign = if self.class() == t.class() { 0 } else { FOREIGN };
        self.reads_in(t).iter().map(move |r| r | foreign)
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
                r.insert(x | y)
            }
        }
        r
    }

    /// Any number of parts, each read one of the ways of `self`.
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

    fn with(self, bits: u8) -> Outs {
        let mut r = Outs::NONE;
        for x in self.iter() {
            r.insert(x | bits)
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
    /// Not known here (an open cell, `Any`, an abstract written by its
    /// `Cast` implementation): shares a form with nothing.
    Unknown,
    /// A primitive, and its bit.
    Scalar(Leaf, Typ),
    /// A bare variant, written as its tag.
    Lit(ArcStr),
    /// An array of any length of the element; `own` where its reader keeps
    /// an array's class (bytes do not).
    Seq(usize, bool),
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
    /// An abstract type written as its representation, which a reader
    /// builds from the data and so never reads in the data's class.
    Foreign(usize),
}

type Seen = AHashSet<(usize, bool, usize)>;
type Side<'s> = &'s dyn Fn(&Shapes, usize, &mut Seen) -> Outs;

struct Shapes<'a> {
    env: &'a Env,
    format: Format,
    nodes: Vec<(Shape, Type)>,
    /// Each reference expanded, by its definition and its arguments'
    /// nodes, so a recursive type is a cycle.
    refs: AHashMap<(usize, SmallVec<[usize; 4]>), usize>,
}

impl<'a> Shapes<'a> {
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
                let expanded = tr.resolved().and_then(|r| {
                    let known = r.bindings(&tr.params)?;
                    Some((r.def_key(), r.typ.replace_tvars(&known)))
                });
                let Some((def, body)) = expanded else {
                    return match t.lookup_ref(self.env) {
                        Ok(body) => self.node(&body),
                        Err(_) => self.push(Shape::Unknown, t),
                    };
                };
                let args: SmallVec<[usize; 4]> =
                    tr.params.iter().map(|p| self.node(p)).collect();
                if let Some(id) = self.refs.get(&(def, args.clone())) {
                    return *id;
                }
                let id = self.push(Shape::Unknown, t);
                self.refs.insert((def, args), id);
                let body = self.node(&body);
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
                            self.push(Shape::Seq(int, false), t)
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
            Type::Array(e) | Type::List(e) => {
                let e = self.node(e);
                self.push(Shape::Seq(e, true), t)
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
            Type::Abstract { id, params } if self.env.written_as_representation(*id) => {
                match self.env.abstract_reps.get(id) {
                    Some(r) => {
                        let rep = self.node(&r.instantiate_with(params));
                        self.push(Shape::Foreign(rep), t)
                    }
                    None => self.push(Shape::Unknown, t),
                }
            }
            Type::Abstract { .. } => self.push(Shape::Unknown, t),
        })
    }

    /// The unions a map key `k` is read through from its text.
    fn key_unions(&self, k: usize, out: &mut SmallVec<[(usize, bool); 4]>) {
        match &self.nodes[k].0 {
            Shape::Union(ms, _) => {
                if !out.contains(&(k, true)) {
                    out.push((k, true));
                    ms.iter().for_each(|m| self.key_unions(*m, out))
                }
            }
            Shape::Foreign(k) => self.key_unions(*k, out),
            _ => (),
        }
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
        match &self.nodes[x].0 {
            Shape::Union(ms, _) => ms.iter().any(|m| self.writes_text(*m, tag)),
            Shape::Foreign(x) => self.writes_text(*x, tag),
            Shape::Scalar(Leaf::Str, _) => true,
            Shape::Lit(t) => t == tag,
            _ => false,
        }
    }

    /// Whether `w` may write an array of `len` values led by the string
    /// `tag`, which a union holding the variant it tags reads as that
    /// variant or not at all.
    fn writes_tag(&self, w: usize, tag: &ArcStr, len: usize) -> bool {
        match &self.nodes[w].0 {
            Shape::Union(ms, _) => ms.iter().any(|m| self.writes_tag(*m, tag, len)),
            Shape::Foreign(w) => self.writes_tag(*w, tag, len),
            Shape::Seq(e, _) => self.writes_text(*e, tag),
            Shape::Fixed(ts, _) => ts.len() == len && self.writes_text(ts[0], tag),
            _ => false,
        }
    }

    /// Whether `k` may hold a string.
    fn holds_string(&self, k: usize) -> bool {
        match &self.nodes[k].0 {
            Shape::Union(ms, _) => ms.iter().any(|m| self.holds_string(*m)),
            Shape::Foreign(k) => self.holds_string(*k),
            Shape::Scalar(Leaf::Str, _) | Shape::Unknown => true,
            _ => false,
        }
    }

    /// Whether a map with keys `k` may be written as an object, its keys'
    /// texts, and as an array of pairs, its keys' data (`map_shape`): a key
    /// type that may hold a string writes the strings' object as their
    /// pairs, a null or composite key writes pairs.
    fn key_forms(&self, k: usize) -> (bool, bool) {
        if self.holds_string(k) {
            return (false, true);
        }
        match &self.nodes[k].0 {
            Shape::Union(ms, _) => ms.iter().fold((false, false), |(o, p), m| {
                let (o1, p1) = self.key_forms(*m);
                (o || o1, p || p1)
            }),
            Shape::Foreign(k) => self.key_forms(*k),
            Shape::Scalar(Leaf::Null, _) => (false, true),
            Shape::Scalar(..) | Shape::Lit(_) => (true, false),
            Shape::None | Shape::Unknown => (false, false),
            _ => (false, true),
        }
    }

    /// How `x` writes a struct's field name `n`, read as that name.
    fn name_written(&self, x: usize, n: &ArcStr) -> Outs {
        match &self.nodes[x].0 {
            Shape::Union(ms, _) => Outs::any(ms.iter().map(|m| self.name_written(*m, n))),
            Shape::Foreign(x) => self.name_written(*x, n).with(FOREIGN),
            Shape::Scalar(Leaf::Str, _) => Outs::one(0),
            Shape::Lit(t) if t == n => Outs::one(FOREIGN),
            _ => Outs::NONE,
        }
    }

    /// How `x` reads a struct's field name `n`.
    fn name_read(&self, x: usize, n: &ArcStr) -> Outs {
        match &self.nodes[x].0 {
            Shape::Union(ms, _) => Outs::any(ms.iter().map(|m| self.name_read(*m, n))),
            Shape::Foreign(x) => self.name_read(*x, n).with(FOREIGN << 4),
            Shape::Scalar(l, _) => {
                let mut o = Outs::NONE;
                for r in l.reads(Token::Text) {
                    o.insert(r << 4)
                }
                o
            }
            Shape::Lit(t) if t == n => Outs::one(FOREIGN << 4),
            _ => Outs::NONE,
        }
    }

    /// How a two-element array is written by `t` (`writer`) or read by it,
    /// `first` and `second` judging each element against the node of `t`
    /// that writes or reads it.
    fn entry(
        &self,
        t: usize,
        writer: bool,
        first: Side,
        second: Side,
        seen: &mut Seen,
    ) -> Outs {
        match &self.nodes[t].0 {
            Shape::Union(ms, _) => {
                Outs::any(ms.iter().map(|m| self.entry(*m, writer, first, second, seen)))
            }
            Shape::Foreign(x) => {
                let bits = if writer { own(false, true) } else { own(true, false) };
                self.entry(*x, writer, first, second, seen).with(bits)
            }
            Shape::Seq(e, o) => {
                let bits = if writer { own(*o, true) } else { own(true, *o) };
                first(self, *e, seen).and(second(self, *e, seen)).with(bits)
            }
            Shape::Fixed(ts, _) if ts.len() == 2 => {
                first(self, ts[0], seen).and(second(self, ts[1], seen))
            }
            _ => Outs::NONE,
        }
    }

    /// How a value `a` writes (as an object's key where `key`) is read by
    /// `a`'s own member and by `b`. A pair met again along one walk claims
    /// nothing: a written value is finite.
    fn outs(&self, a: usize, key: bool, b: usize, seen: &mut Seen) -> Outs {
        if !seen.insert((a, key, b)) {
            return Outs::NONE;
        }
        let r = crate::stack::ensure_sufficient(|| self.outs_int(a, key, b, seen));
        seen.remove(&(a, key, b));
        r
    }

    fn outs_int(&self, a: usize, key: bool, b: usize, seen: &mut Seen) -> Outs {
        use Shape::*;
        let leaf = |x: Leaf, read: &dyn Fn(Token) -> SmallVec<[u8; 2]>| {
            let mut o = Outs::NONE;
            for t in x.writes().iter().flat_map(|t| t.written(key)) {
                for ra in x.reads(t) {
                    for rb in read(t) {
                        o.insert(ra | rb << 4)
                    }
                }
            }
            o
        };
        match (&self.nodes[a].0, &self.nodes[b].0) {
            (Union(ms, _), _) => {
                Outs::any(ms.iter().map(|m| self.outs(*m, key, b, seen)))
            }
            (_, Union(ms, _)) => {
                Outs::any(ms.iter().map(|m| self.outs(a, key, *m, seen)))
            }
            (Foreign(x), _) => self.outs(*x, key, b, seen).with(FOREIGN),
            (_, Foreign(y)) => self.outs(a, key, *y, seen).with(FOREIGN << 4),
            (None | Unknown, _) | (_, None | Unknown) => Outs::NONE,
            (Scalar(x, _), Scalar(y, _)) => leaf(*x, &|t| y.reads(t).collect()),
            (Scalar(x, _), Lit(_)) => leaf(*x, &|t| match t {
                Token::TagText => SmallVec::from_slice(&[FOREIGN]),
                _ => SmallVec::new(),
            }),
            (Lit(_), Scalar(y, _)) => {
                let mut o = Outs::NONE;
                for rb in y.reads(Token::TagText) {
                    o.insert(FOREIGN | rb << 4)
                }
                o
            }
            (Lit(t), Lit(u)) if t == u => Outs::one(FOREIGN | FOREIGN << 4),
            _ if key => Outs::NONE,
            (Seq(e, oa), Seq(f, ob)) => {
                self.outs(*e, false, *f, seen).star().with(own(*oa, *ob))
            }
            (Seq(e, oa), Fixed(ts, _)) => {
                Outs::all(ts.iter().map(|t| self.outs(*e, false, *t, seen)))
                    .with(own(*oa, true))
            }
            (Fixed(ts, _), Seq(f, ob)) => {
                Outs::all(ts.iter().map(|t| self.outs(*t, false, *f, seen)))
                    .with(own(true, *ob))
            }
            (Fixed(ts, _), Fixed(us, _)) if ts.len() == us.len() => Outs::all(
                ts.iter().zip(us.iter()).map(|(t, u)| self.outs(*t, false, *u, seen)),
            ),
            (Pairs(k, v), _) => self.map_read(*k, *v, b, seen),
            (Struct(fs), _) => self.struct_read(fs, b, seen),
            (Seq(e, oa), Pairs(j, u)) => {
                let k: Side = &|s, x, seen| s.outs(x, false, *j, seen);
                let v: Side = &|s, y, seen| s.outs(y, false, *u, seen);
                self.entry(*e, true, k, v, seen).star().with(own(*oa, false))
            }
            (Fixed(ts, _), Pairs(j, u)) => {
                let k: Side = &|s, x, seen| s.outs(x, false, *j, seen);
                let v: Side = &|s, y, seen| s.outs(y, false, *u, seen);
                Outs::all(ts.iter().map(|t| self.entry(*t, true, k, v, seen)))
                    .with(own(true, false))
            }
            (Seq(e, oa), Struct(gs)) => Outs::all(gs.iter().map(|(n, g, og)| {
                let k: Side = &|s, x, _| s.name_written(x, n);
                let v: Side = &|s, y, seen| s.outs(y, false, *g, seen);
                let m = self.entry(*e, true, k, v, seen);
                if *og { m.or(Outs::one(0)) } else { m }
            }))
            .with(own(*oa, true)),
            (Fixed(ts, _), Struct(gs)) => {
                let required = gs.iter().filter(|(_, _, o)| !o).count();
                if ts.len() < required || ts.len() > gs.len() {
                    return Outs::NONE;
                }
                Outs::all(ts.iter().map(|t| {
                    Outs::any(gs.iter().map(|(n, g, _)| {
                        let k: Side = &|s, x, _| s.name_written(x, n);
                        let v: Side = &|s, y, seen| s.outs(y, false, *g, seen);
                        self.entry(*t, true, k, v, seen)
                    }))
                }))
            }
            _ => Outs::NONE,
        }
    }

    /// How a map's entries are read by its own member and by `b`.
    fn map_read(&self, k: usize, v: usize, b: usize, seen: &mut Seen) -> Outs {
        let (text, data) = self.key_forms(k);
        let forms = [text.then_some(true), data.then_some(false)];
        Outs::any(forms.into_iter().flatten().map(|key| {
            let kw: Side = &|s, x, seen| s.outs(k, key, x, seen);
            let vw: Side = &|s, y, seen| s.outs(v, false, y, seen);
            match &self.nodes[b].0 {
                Shape::Pairs(j, u) => {
                    let e =
                        self.outs(k, key, *j, seen).and(self.outs(v, false, *u, seen));
                    e.star().with(own(false, false))
                }
                Shape::Seq(f, ob) => {
                    self.entry(*f, false, kw, vw, seen).star().with(own(false, *ob))
                }
                Shape::Fixed(us, _) => {
                    Outs::all(us.iter().map(|t| self.entry(*t, false, kw, vw, seen)))
                        .with(own(false, true))
                }
                Shape::Struct(gs) => Outs::all(gs.iter().map(|(n, g, og)| {
                    let m = self.name_written(k, n).and(self.outs(v, false, *g, seen));
                    if *og { m.or(Outs::one(0)) } else { m }
                }))
                .with(own(false, true)),
                _ => Outs::NONE,
            }
        }))
    }

    /// How a struct's pairs, sorted by name, are read by its own member and
    /// by `b`. JSON writes every field, TOML leaves a null one out.
    fn struct_read(
        &self,
        fs: &[(ArcStr, usize, bool)],
        b: usize,
        seen: &mut Seen,
    ) -> Outs {
        let toml = self.format == Format::Toml;
        let left_out = |m: Outs, o: bool| if toml && o { m.or(Outs::one(0)) } else { m };
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
                            r = r.and(left_out(self.outs(*f, false, g, seen), *of && og))
                        }
                        None if toml && *of => (),
                        None => return Outs::NONE,
                    }
                }
                r
            }
            Shape::Pairs(j, u) => Outs::all(fs.iter().map(|(n, f, o)| {
                let m = self.name_read(*j, n).and(self.outs(*f, false, *u, seen));
                left_out(m, *o)
            }))
            .with(own(true, false)),
            Shape::Seq(g, ob) => Outs::all(fs.iter().map(|(n, f, o)| {
                let k: Side = &|s, x, _| s.name_read(x, n);
                let v: Side = &|s, y, seen| s.outs(*f, false, y, seen);
                left_out(self.entry(*g, false, k, v, seen), *o)
            }))
            .with(own(true, *ob)),
            Shape::Fixed(us, _) if us.len() == fs.len() => {
                Outs::all(us.iter().zip(fs.iter()).map(|(t, (n, f, _))| {
                    let k: Side = &|s, x, _| s.name_read(x, n);
                    let v: Side = &|s, y, seen| s.outs(*f, false, y, seen);
                    self.entry(*t, false, k, v, seen)
                }))
            }
            Shape::Fixed(us, _)
                if toml && us.len() >= fs.iter().filter(|(_, _, o)| !o).count() =>
            {
                Outs::all(us.iter().map(|t| {
                    Outs::any(fs.iter().map(|(n, f, _)| {
                        let k: Side = &|s, x, _| s.name_read(x, n);
                        let v: Side = &|s, y, seen| s.outs(*f, false, y, seen);
                        self.entry(*t, false, k, v, seen)
                    }))
                }))
            }
            _ => Outs::NONE,
        }
    }

    /// The pass of a union's pick that tries the member `n` reading a value
    /// with `bits` (`cast_member`): a tuple, then a struct, keeping the
    /// data's class, then any member keeping it, then the rest; `None` for
    /// a union, whose members decide.
    fn pass(&self, n: usize, bits: u8) -> Option<u8> {
        let inside = bits & FOREIGN == 0;
        Some(match &self.nodes[self.head(n)].0 {
            Shape::Union(..) | Shape::None | Shape::Unknown => return None,
            Shape::Fixed(_, false) if inside => 0,
            Shape::Struct(_) if inside => 1,
            Shape::Scalar(..) | Shape::Seq(..) if inside => 2,
            _ => 3,
        })
    }

    /// Whether a value the member `w` of a union writes (as an object's key
    /// where `key`) may read back as its member `r`, `w_first` where `w`
    /// comes first in the union.
    fn takes(
        &self,
        w: usize,
        r: usize,
        key: bool,
        w_first: bool,
        seen: &mut Seen,
    ) -> bool {
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
        self.outs(w, key, r, seen).iter().any(|o| {
            let (a, b) = (o & 15, o >> 4);
            if (a ^ b) & ROUNDS != 0 {
                return a & ROUNDS != 0;
            }
            let kept = |x: u8| x & (CROSSED | OTHER) == 0;
            let w_first = match (tw.is_some(), self.pass(w, a), self.pass(r, b)) {
                (true, _, _) => Some(w_first),
                (false, Some(x), Some(y)) if x != y => Some(x < y),
                (false, Some(_), Some(_)) => Some(w_first),
                _ => None,
            };
            match w_first {
                Some(true) => a & CROSSED != 0 && kept(b),
                _ => !(b & CROSSED != 0 && kept(a)),
            }
        })
    }

    /// [`Self::takes`] for members of one primitive set, which reads a
    /// value as its first member that holds it exactly, by class and then
    /// bit, real text an integer holds going to a real that holds it too,
    /// else as its first real that rounds it.
    fn prim_takes(&self, w: usize, r: usize, key: bool) -> bool {
        let (Shape::Scalar(x, xt), Shape::Scalar(y, yt)) =
            (&self.nodes[w].0, &self.nodes[r].0)
        else {
            return false;
        };
        let pass = |t: Token, ty: Typ| -> u8 {
            match t {
                Token::Num(n) if n.real() => match (ty.is_real(), ty.is_number()) {
                    (true, _) => 0,
                    (_, true) => 1,
                    _ => 2,
                },
                Token::Num(_) => !ty.is_number() as u8,
                Token::Null => (ty != Typ::Null) as u8,
                Token::Bool => (ty != Typ::Bool) as u8,
                Token::DateTime => (ty != Typ::DateTime) as u8,
                _ => (ty != Typ::String) as u8,
            }
        };
        x.writes().iter().flat_map(|t| t.written(key)).any(|t| {
            x.reads(t).any(|ra| {
                y.reads(t).any(|rb| {
                    let (wr, rr) = (ra & ROUNDS != 0, rb & ROUNDS != 0);
                    if wr != rr {
                        return wr;
                    }
                    if !wr
                        && let Token::NumText(n) = t
                        && n.real()
                        && (xt.is_integer() && yt.is_real()
                            || xt.is_real() && yt.is_integer())
                    {
                        return yt.is_real();
                    }
                    let rank = |ty: Typ| (if wr { 0 } else { pass(t, ty) }, ty as u64);
                    rank(*yt) < rank(*xt)
                })
            })
        })
    }
}

impl Type {
    /// Two members of a union in this type such that a value of `format`
    /// one writes may read back as the other; `None` when every union reads
    /// back what each member writes.
    pub fn text_ambiguity(&self, env: &Env, format: Format) -> Option<(Type, Type)> {
        let mut shapes =
            Shapes { env, format, nodes: Vec::new(), refs: AHashMap::default() };
        shapes.node(self);
        let mut seen = Seen::default();
        let unions = (0..shapes.nodes.len()).map(|n| (n, false));
        let mut keys: SmallVec<[(usize, bool); 4]> = SmallVec::new();
        for (s, _) in shapes.nodes.iter() {
            if let Shape::Pairs(k, _) = s
                && shapes.key_forms(*k).0
            {
                shapes.key_unions(*k, &mut keys)
            }
        }
        for (u, key) in unions.chain(keys) {
            let Shape::Union(ms, prim) = &shapes.nodes[u].0 else { continue };
            for (i, a) in ms.iter().enumerate() {
                for b in ms[i + 1..].iter() {
                    let hit = match prim {
                        true => {
                            shapes.prim_takes(*a, *b, key)
                                || shapes.prim_takes(*b, *a, key)
                        }
                        false => {
                            shapes.takes(*a, *b, key, true, &mut seen)
                                || shapes.takes(*b, *a, key, false, &mut seen)
                        }
                    };
                    if hit {
                        return Some((
                            shapes.nodes[*a].1.clone(),
                            shapes.nodes[*b].1.clone(),
                        ));
                    }
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::expr::parser::parse_type;

    fn ambiguous(t: &str, format: Format) -> bool {
        let env = Env::default();
        parse_type(t).unwrap().text_ambiguity(&env, format).is_some()
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
