//! Which unions a text format cannot read back: two members one JSON or
//! TOML value could be written by. A reader picks one member of a union
//! for a value; where two could have written it, a value written as one
//! may read back as the other (`[u8, i64]`, `[string, datetime]` in JSON,
//! `[Array<string>, Array<i64>]` over `[]`).

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

/// What a type writes, and so what its reader takes, as data of one
/// format; a node of a [`Shapes`] arena.
enum Shape {
    /// Nothing the format can write (a function, an error, TOML's null).
    None,
    /// Not known here (an open cell, `Any`, an abstract written by its
    /// `Cast` implementation): shares a form with nothing.
    Unknown,
    Null,
    Bool,
    Int,
    Float,
    /// Any number, integral or not (a decimal written exactly).
    Number,
    Str,
    /// A native datetime (TOML's).
    DateTime,
    /// One string: a bare variant's tag.
    Lit(ArcStr),
    /// An array of any length, of the element.
    Seq(usize),
    Fixed(SmallVec<[usize; 4]>),
    /// An array of any length of `[key, value]` pairs: a map, which an
    /// object reads as.
    Pairs(usize, usize),
    /// A struct: its fields by name, each with whether the reader takes
    /// it left out (its type admits null).
    Struct(SmallVec<[(ArcStr, usize, bool); 4]>),
    Union(SmallVec<[usize; 4]>),
}

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
        match p {
            Typ::Null if json => Shape::Null,
            Typ::Bool => Shape::Bool,
            Typ::F32 | Typ::F64 => Shape::Float,
            Typ::Duration if json => Shape::Float,
            Typ::Decimal if json => Shape::Number,
            Typ::String => Shape::Str,
            Typ::DateTime if json => Shape::Str,
            Typ::DateTime => Shape::DateTime,
            Typ::Null | Typ::Duration | Typ::Decimal | Typ::Error => Shape::None,
            Typ::Bytes | Typ::Array | Typ::Map | Typ::Abstract => Shape::Unknown,
            _ => Shape::Int,
        }
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
                self.nodes[id].0 = Shape::Union(SmallVec::from_iter([body]));
                id
            }
            Type::Primitive(p) => match p.len() {
                1 => {
                    let p = p.iter().next().expect("one");
                    match p {
                        Typ::Bytes => {
                            let int =
                                self.push(Shape::Int, &Type::Primitive(Typ::U8.into()));
                            self.push(Shape::Seq(int), t)
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
                    self.push(Shape::Union(ms), t)
                }
            },
            Type::Set(ms) => {
                let ms = ms.iter().map(|m| self.node(m)).collect();
                self.push(Shape::Union(ms), t)
            }
            Type::Bottom | Type::Error(_) | Type::Fn(_) | Type::ByRef(..) => {
                self.push(Shape::None, t)
            }
            Type::Any | Type::App(..) | Type::Hole | Type::Bound(_) => {
                self.push(Shape::Unknown, t)
            }
            Type::Array(e) | Type::List(e) => {
                let e = self.node(e);
                self.push(Shape::Seq(e), t)
            }
            Type::Tuple(ts) => {
                let ts = ts.iter().map(|t| self.node(t)).collect();
                self.push(Shape::Fixed(ts), t)
            }
            Type::Variant(tag, ps, _) if ps.is_empty() => {
                self.push(Shape::Lit(tag.clone()), t)
            }
            Type::Variant(tag, ps, _) => {
                let tag = self.push(Shape::Lit(tag.clone()), t);
                let ps =
                    std::iter::once(tag).chain(ps.iter().map(|p| self.node(p))).collect();
                self.push(Shape::Fixed(ps), t)
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
            Type::Abstract { id, params } if self.env.written_as_representation(*id) => {
                match self.env.abstract_reps.get(id) {
                    Some(r) => {
                        let rep = r.instantiate_with(params);
                        self.node(&rep)
                    }
                    None => self.push(Shape::Unknown, t),
                }
            }
            Type::Abstract { .. } => self.push(Shape::Unknown, t),
        })
    }

    /// Whether some value of the format is written by `a` and read by `b`,
    /// or the other way round. A pair met again along one walk claims
    /// nothing: a shared value is finite.
    fn overlap(&self, a: usize, b: usize, seen: &mut AHashSet<(usize, usize)>) -> bool {
        if !seen.insert((a, b)) {
            return false;
        }
        let r = crate::stack::ensure_sufficient(|| self.overlap_int(a, b, seen));
        seen.remove(&(a, b));
        r
    }

    fn overlap_int(
        &self,
        a: usize,
        b: usize,
        seen: &mut AHashSet<(usize, usize)>,
    ) -> bool {
        use Shape::*;
        let all_omittable = |fs: &[(ArcStr, usize, bool)]| fs.iter().all(|(_, _, o)| *o);
        match (&self.nodes[a].0, &self.nodes[b].0) {
            (Union(ms), _) => ms.iter().any(|m| self.overlap(*m, b, seen)),
            (_, Union(ms)) => ms.iter().any(|m| self.overlap(a, *m, seen)),
            (None | Unknown, _) | (_, None | Unknown) => false,
            (Null, Null) | (Bool, Bool) | (Str, Str) | (DateTime, DateTime) => true,
            (Int | Float | Number, Number) | (Number, Int | Float) => true,
            (Int, Int) | (Float, Float) => true,
            (Str, Lit(_)) | (Lit(_), Str) => true,
            (Lit(x), Lit(y)) => x == y,
            (Seq(_), Seq(_) | Pairs(..)) | (Pairs(..), Seq(_) | Pairs(..)) => true,
            (Seq(e), Fixed(ts)) | (Fixed(ts), Seq(e)) => {
                ts.iter().all(|t| self.overlap(*e, *t, seen))
            }
            (Fixed(ts), Fixed(us)) => {
                ts.len() == us.len()
                    && ts.iter().zip(us.iter()).all(|(t, u)| self.overlap(*t, *u, seen))
            }
            (Fixed(ts), Pairs(k, v)) | (Pairs(k, v), Fixed(ts)) => ts
                .iter()
                .all(|t| self.pair(*t, &|s, x, seen| s.overlap(x, *k, seen), *v, seen)),
            (Seq(e), Struct(fs)) | (Struct(fs), Seq(e)) => {
                all_omittable(fs)
                    || fs.iter().filter(|(_, _, o)| !o).all(|(n, f, _)| {
                        self.pair(*e, &|s, x, _| s.is_lit(x, n), *f, seen)
                    })
            }
            (Pairs(k, v), Struct(fs)) | (Struct(fs), Pairs(k, v)) => {
                all_omittable(fs)
                    || fs
                        .iter()
                        .filter(|(_, _, o)| !o)
                        .all(|(n, f, _)| self.is_lit(*k, n) && self.overlap(*v, *f, seen))
            }
            (Fixed(ts), Struct(fs)) | (Struct(fs), Fixed(ts)) => {
                let present: SmallVec<[&(ArcStr, usize, bool); 4]> = match ts.len() {
                    n if n == fs.len() => fs.iter().collect(),
                    n if n == fs.iter().filter(|(_, _, o)| !o).count() => {
                        fs.iter().filter(|(_, _, o)| !o).collect()
                    }
                    _ => return false,
                };
                ts.iter().zip(present).all(|(t, (n, f, _))| {
                    self.pair(*t, &|s, x, _| s.is_lit(x, n), *f, seen)
                })
            }
            (Struct(xs), Struct(ys)) => {
                let field = |fs: &[(ArcStr, usize, bool)], n: &ArcStr| {
                    fs.iter().find(|(m, _, _)| m == n).map(|(_, f, o)| (*f, *o))
                };
                let covers = |a: &[(ArcStr, usize, bool)],
                              b: &[(ArcStr, usize, bool)]| {
                    a.iter().filter(|(_, _, o)| !o).all(|(n, _, _)| field(b, n).is_some())
                };
                covers(xs, ys)
                    && covers(ys, xs)
                    && xs.iter().all(|(n, x, ox)| match field(ys, n) {
                        Some((y, oy)) if !ox || !oy => self.overlap(*x, y, seen),
                        _ => true,
                    })
            }
            _ => false,
        }
    }

    /// Whether `t` takes a two-element array whose first element meets
    /// `first` and whose second meets the node `second`.
    fn pair(
        &self,
        t: usize,
        first: &dyn Fn(&Self, usize, &mut AHashSet<(usize, usize)>) -> bool,
        second: usize,
        seen: &mut AHashSet<(usize, usize)>,
    ) -> bool {
        match &self.nodes[t].0 {
            Shape::Union(ms) => ms.iter().any(|m| self.pair(*m, first, second, seen)),
            Shape::Seq(e) => first(self, *e, seen) && self.overlap(*e, second, seen),
            Shape::Fixed(ts) if ts.len() == 2 => {
                first(self, ts[0], seen) && self.overlap(ts[1], second, seen)
            }
            _ => false,
        }
    }

    /// Whether the node takes the string `n`.
    fn is_lit(&self, x: usize, n: &ArcStr) -> bool {
        match &self.nodes[x].0 {
            Shape::Union(ms) => ms.iter().any(|m| self.is_lit(*m, n)),
            Shape::Str => true,
            Shape::Lit(l) => l == n,
            _ => false,
        }
    }
}

impl Type {
    /// Two members of a union in this type that one value of `format`
    /// could be written by, so a read of it may give back the other; `None`
    /// when every union's members write apart.
    pub fn text_ambiguity(&self, env: &Env, format: Format) -> Option<(Type, Type)> {
        let mut shapes =
            Shapes { env, format, nodes: Vec::new(), refs: AHashMap::default() };
        shapes.node(self);
        let mut seen: AHashSet<(usize, usize)> = AHashSet::default();
        for (s, _) in shapes.nodes.iter() {
            let Shape::Union(ms) = s else { continue };
            for (i, a) in ms.iter().enumerate() {
                for b in ms[i + 1..].iter() {
                    if shapes.overlap(*a, *b, &mut seen) {
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
        assert!(!json("[i64, null]"));
        assert!(json("[string, datetime]"));
        assert!(!toml("[string, datetime]"));
        assert!(!json("[decimal, string]"));
        assert!(json("[decimal, f64]"));
        assert!(json("[duration, f64]"));
        assert!(json("[Array<string>, Array<i64>]"));
        assert!(!json("[`A(i64), `B(string), `C]"));
        assert!(json("[`A, string]"));
        assert!(json("[(string, i64), `B(i64)]"));
        assert!(!json("[{a: i64}, {b: i64}]"));
        assert!(json("[{a: i64}, {a: i64, b: [i64, null]}]"));
        assert!(json("[Map<string, i64>, {a: i64}]"));
        assert!(!json("Array<{name: string, n: [i64, null]}>"));
        assert!(json("Array<[u8, i64]>"));
    }
}
