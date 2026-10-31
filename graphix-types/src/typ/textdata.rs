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
use ahash::AHashMap;
use arcstr::ArcStr;
use enumflags2::BitFlags;
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

    /// Each way it may read a value of `t` exactly or rounded (a bool into
    /// a real, as `holds_rounded` has it); a reading that drops part of a
    /// value never beats the writer's own, which is one of these.
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

type Side<'s> = &'s dyn Fn(&Shapes, usize) -> Outs;

/// A writer, whether it writes an object's key, and a reader.
type Pair = (usize, bool, usize);

/// A pair's outcomes so far, the pass that last computed them, whether
/// that pass is still computing them, and whether they are final.
#[derive(Clone, Copy)]
struct Fix {
    outs: Outs,
    pass: u32,
    open: bool,
    done: bool,
}

/// [`Shapes::outs`] as the least fixpoint over the pairs a walk meets.
/// Every pair starts at `Outs::NONE`; a pass computes each pair it meets
/// once and reads a pair it is still computing as it stood, and passes
/// repeat until one changes nothing or reads no pair it was computing.
/// A pair's outcomes only grow (`outs` joins them), so the passes end;
/// every operation on outcomes maps each outcome or pair of outcomes to
/// outcomes, so none passes the least fixpoint.
#[derive(Default)]
struct Fixpoint {
    pairs: AHashMap<Pair, Fix>,
    pass: u32,
    reread: bool,
    changed: bool,
    computed: Vec<Pair>,
    #[cfg(test)]
    evaluated: usize,
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
    classes: RefCell<AHashMap<(usize, usize, bool), (bool, bool)>>,
    fix: RefCell<Fixpoint>,
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
        };
        shapes.node(t);
        shapes
    }

    /// [`Type::text_ambiguity`] of the type the shapes were built from.
    fn ambiguity(&self) -> Option<TextAmbiguity> {
        if let Some(name) = &self.grows {
            return Some(TextAmbiguity::Grows(name.clone()));
        }
        let unions = (0..self.nodes.len()).map(|n| (n, false));
        let mut keys: SmallVec<[(usize, bool); 4]> = SmallVec::new();
        for (s, _) in self.nodes.iter() {
            if let Shape::Pairs(k, _) = s
                && self.key_forms(*k).0
            {
                self.key_unions(*k, &mut keys)
            }
        }
        for (u, key) in unions.chain(keys) {
            let Shape::Union(ms, prim) = &self.nodes[u].0 else { continue };
            for (i, a) in ms.iter().enumerate() {
                for b in ms[i + 1..].iter() {
                    let hit = match prim {
                        true => {
                            self.prim_takes(*a, *b, key) || self.prim_takes(*b, *a, key)
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
    /// that writes or reads it; `t` is a part of a composite.
    fn entry(&self, t: usize, writer: bool, first: Side, second: Side) -> Outs {
        match &self.nodes[t].0 {
            Shape::Union(ms, _) => {
                Outs::any(ms.iter().map(|m| self.entry(*m, writer, first, second)))
            }
            Shape::Foreign(x) => {
                let bits = if writer { own(false, true) } else { own(true, false) };
                self.entry(*x, writer, first, second).with(bits)
            }
            Shape::Seq(e, o) => {
                let bits = if writer { own(*o, true) } else { own(true, *o) };
                first(self, *e).and(second(self, *e)).with(bits)
            }
            Shape::Fixed(ts, _) if ts.len() == 2 => {
                let apart = if writer { FOREIGN } else { FOREIGN << 4 };
                first(self, ts[0]).and(second(self, ts[1])).with(apart)
            }
            _ => Outs::NONE,
        }
    }

    /// Whether a value `w` writes (as an object's key where `key`) may be
    /// in the reader `r`'s class and whether it may be out of it
    /// (`in_own_class`, which decides a union member's pass): a union's
    /// class is its members', a composite's all of its parts', an empty
    /// array is in any array's; a pair met again claims both.
    fn class_of(&self, r: usize, w: usize, key: bool) -> (bool, bool) {
        if let Some(c) = self.classes.borrow().get(&(r, w, key)) {
            return *c;
        }
        self.classes.borrow_mut().insert((r, w, key), (true, true));
        let c = crate::stack::ensure_sufficient(|| self.class_of_int(r, w, key));
        self.classes.borrow_mut().insert((r, w, key), c);
        c
    }

    /// [`Self::class_of`] where `r` is a part of a composite or a member of
    /// a union: a tuple or a struct there is never in the data's class
    /// (`in_own_class` holds only marked ones of data); only the union's
    /// pick looks inside one it tries itself.
    fn part_class(&self, r: usize, w: usize, key: bool) -> (bool, bool) {
        match self.apart(r) {
            0 => self.class_of(r, w, key),
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
                let (o1, f1) = self.class_of(r, *m, key);
                (o || o1, f || f1)
            }),
            (Foreign(w), _) => self.class_of(r, *w, key),
            (_, Union(ms, _)) => ms.iter().fold((false, true), |(o, f), m| {
                let (o1, f1) = match ms.len() {
                    1 => self.class_of(*m, w, key),
                    _ => self.part_class(*m, w, key),
                };
                (o || o1, f && f1)
            }),
            (None | Unknown, _) | (_, Unknown) => (true, true),
            (_, None | Foreign(_) | Lit(_) | Pairs(..) | Fixed(_, true)) => (false, true),
            (Scalar(x, _), Scalar(y, _)) => {
                let c = x.data_class(key) == y.class();
                (c, !c)
            }
            (Lit(_), Scalar(y, _)) => {
                let c = y.class() == Class::String;
                (c, !c)
            }
            (Scalar(..) | Lit(_), _) | (_, Scalar(..)) => (false, true),
            (Seq(e, _), Seq(f, own)) => (*own, !own || self.part_class(*f, *e, key).1),
            (Fixed(ts, _), Seq(f, true)) => parts(&mut ts.iter().map(|t| (*f, *t))),
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
        loop {
            {
                let mut f = self.fix.borrow_mut();
                f.pass += 1;
                f.reread = false;
                f.changed = false;
                f.computed.clear();
            }
            let r = self.outs(a, key, b);
            let mut f = self.fix.borrow_mut();
            if !(f.reread && f.changed) {
                let Fixpoint { pairs, computed, .. } = &mut *f;
                for p in computed.drain(..) {
                    pairs.get_mut(&p).expect("computed").done = true
                }
                return r;
            }
        }
    }

    /// [`Self::least_outs`] as this pass of [`Fixpoint`] has it.
    fn outs(&self, a: usize, key: bool, b: usize) -> Outs {
        let p = (a, key, b);
        let prev = {
            let mut f = self.fix.borrow_mut();
            let pass = f.pass;
            let prev = match f.pairs.get(&p).copied() {
                Some(e) if e.done => return e.outs,
                Some(e) if e.pass == pass => {
                    f.reread |= e.open;
                    return e.outs;
                }
                Some(e) => e.outs,
                None => Outs::NONE,
            };
            f.pairs.insert(p, Fix { outs: prev, pass, open: true, done: false });
            f.computed.push(p);
            #[cfg(test)]
            {
                f.evaluated += 1;
            }
            prev
        };
        let r = prev.or(crate::stack::ensure_sufficient(|| self.outs_int(a, key, b)));
        let mut f = self.fix.borrow_mut();
        f.changed |= r != prev;
        let pass = f.pass;
        f.pairs.insert(p, Fix { outs: r, pass, open: false, done: false });
        r
    }

    fn outs_int(&self, a: usize, key: bool, b: usize) -> Outs {
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
            (Union(ms, _), _) => Outs::any(ms.iter().map(|m| {
                let (own, foreign) = self.class_of(a, *m, key);
                self.outs(*m, key, b).classed(0, own, foreign)
            })),
            (_, Union(ms, _)) => {
                let (own, foreign) = self.class_of(b, a, key);
                Outs::any(ms.iter().map(|m| self.outs(a, key, *m)))
                    .classed(4, own, foreign)
            }
            (Foreign(x), _) => self.outs(*x, key, b).with(FOREIGN),
            (_, Foreign(y)) => self.outs(a, key, *y).with(FOREIGN << 4),
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
                self.part(*e, false, *f).star().with(own(*oa, *ob))
            }
            (Seq(e, oa), Fixed(ts, _)) => {
                Outs::all(ts.iter().map(|t| self.part(*e, false, *t)))
                    .with(own(*oa, true))
            }
            (Fixed(ts, _), Seq(f, ob)) => {
                Outs::all(ts.iter().map(|t| self.part(*t, false, *f)))
                    .with(own(true, *ob))
            }
            (Fixed(ts, _), Fixed(us, _)) if ts.len() == us.len() => {
                Outs::all(ts.iter().zip(us.iter()).map(|(t, u)| self.part(*t, false, *u)))
            }
            (Pairs(k, v), _) => self.map_read(*k, *v, b),
            (Struct(fs), _) => self.struct_read(fs, b),
            (Seq(e, oa), Pairs(j, u)) => {
                let k: Side = &|s, x| s.part(x, false, *j);
                let v: Side = &|s, y| s.part(y, false, *u);
                self.entry(*e, true, k, v).star().with(own(*oa, false))
            }
            (Fixed(ts, _), Pairs(j, u)) => {
                let k: Side = &|s, x| s.part(x, false, *j);
                let v: Side = &|s, y| s.part(y, false, *u);
                Outs::all(ts.iter().map(|t| self.entry(*t, true, k, v)))
                    .with(own(true, false))
            }
            (Seq(e, oa), Struct(gs)) => Outs::all(gs.iter().map(|(n, g, og)| {
                let k: Side = &|s, x| s.name_written(x, n);
                let v: Side = &|s, y| s.part(y, false, *g);
                let m = self.entry(*e, true, k, v);
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
                Shape::Seq(f, ob) => {
                    self.entry(*f, false, kw, vw).star().with(own(false, *ob))
                }
                Shape::Fixed(us, _) => {
                    Outs::all(us.iter().map(|t| self.entry(*t, false, kw, vw)))
                        .with(own(false, true))
                }
                Shape::Struct(gs) => Outs::all(gs.iter().map(|(n, g, og)| {
                    let m = self.name_written(k, n).and(self.part(v, false, *g));
                    if *og { m.or(Outs::one(0)) } else { m }
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
            Shape::Seq(g, ob) => Outs::all(fs.iter().map(|(n, f, o)| {
                let k: Side = &|s, x| s.name_read(x, n);
                let v: Side = &|s, y| s.part(*f, false, y);
                left_out(self.entry(*g, false, k, v), *o)
            }))
            .with(own(true, *ob)),
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
    // nodes, however the unions nest, and evaluates each once a pass: a
    // type with no recursion takes one pass, a recursive group a few. The
    // wall limit is only a hang guard; the counts are the pin.
    #[test]
    fn a_check_evaluates_each_pair_a_few_times() {
        type Shape = (&'static str, fn() -> (Env, Type), bool, usize);
        let shapes: [Shape; 7] = [
            ("L10 x2", || levels(10, 2), false, 1),
            ("L14 x2", || levels(14, 2), false, 1),
            ("L6 x4", || levels(6, 4), false, 1),
            ("L8 x4", || levels(8, 4), false, 1),
            ("mutual 32", || mutual(32), true, 3),
            ("ring 32", || ring(32), false, 1),
            ("tree 32", || tree(32), false, 1),
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
