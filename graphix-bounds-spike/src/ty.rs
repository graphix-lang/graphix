use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_value::Typ;
use std::{fmt, rc::Rc};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct Var(pub u32);

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct FnArg {
    pub label: Option<ArcStr>,
    pub optional: bool,
    pub ty: Ty,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct FnTy {
    pub args: Vec<FnArg>,
    pub vargs: Option<Ty>,
    pub ret: Ty,
    pub throws: Ty,
}

/// A union is flat, holds no bottom and at most one `Prim`, and has two or
/// more members; build one with [`union`].
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Ty {
    Bot,
    Any,
    Prim(BitFlags<Typ>),
    Array(Rc<Ty>),
    List(Rc<Ty>),
    Map(Rc<Ty>, Rc<Ty>),
    Error(Rc<Ty>),
    Ref(bool, Rc<Ty>),
    Tuple(Rc<[Ty]>),
    Struct(Rc<[(ArcStr, Ty)]>),
    Variant(ArcStr, Rc<[Ty]>),
    Fn(Rc<FnTy>),
    Union(Rc<[Ty]>),
    Var(Var),
    /// A declared variable inside its own definition: equal only to itself.
    Rigid(Var),
    /// An application of a recursive typedef, unfolded when it is met.
    Named(u32, Rc<[Ty]>),
}

/// What tells two members of a union apart without looking at a variable.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Head {
    Prim,
    Array,
    List,
    Map,
    Error,
    Ref,
    Tuple(usize),
    Struct(Vec<ArcStr>),
    Variant(ArcStr, usize),
    Fn,
    Open,
    Other,
}

impl Ty {
    pub fn prim(t: Typ) -> Ty {
        Ty::Prim(BitFlags::from_flag(t))
    }

    pub fn head(&self) -> Head {
        match self {
            Ty::Prim(_) => Head::Prim,
            Ty::Array(_) => Head::Array,
            Ty::List(_) => Head::List,
            Ty::Map(..) => Head::Map,
            Ty::Error(_) => Head::Error,
            Ty::Ref(..) => Head::Ref,
            Ty::Tuple(ts) => Head::Tuple(ts.len()),
            Ty::Struct(fs) => Head::Struct(fs.iter().map(|(n, _)| n.clone()).collect()),
            Ty::Variant(tag, ts) => Head::Variant(tag.clone(), ts.len()),
            Ty::Fn(_) => Head::Fn,
            Ty::Var(_) | Ty::Rigid(_) => Head::Open,
            Ty::Bot | Ty::Any | Ty::Union(_) | Ty::Named(..) => Head::Other,
        }
    }

    pub fn map(&self, f: &mut impl FnMut(&Ty) -> Option<Ty>) -> Ty {
        if let Some(t) = f(self) {
            return t;
        }
        let all = |ts: &[Ty], f: &mut dyn FnMut(&Ty) -> Ty| -> Rc<[Ty]> {
            ts.iter().map(|t| f(t)).collect()
        };
        match self {
            Ty::Bot | Ty::Any | Ty::Prim(_) | Ty::Var(_) | Ty::Rigid(_) => self.clone(),
            Ty::Array(t) => Ty::Array(Rc::new(t.map(f))),
            Ty::List(t) => Ty::List(Rc::new(t.map(f))),
            Ty::Error(t) => Ty::Error(Rc::new(t.map(f))),
            Ty::Ref(m, t) => Ty::Ref(*m, Rc::new(t.map(f))),
            Ty::Map(k, v) => Ty::Map(Rc::new(k.map(f)), Rc::new(v.map(f))),
            Ty::Tuple(ts) => Ty::Tuple(all(ts, &mut |t| t.map(f))),
            Ty::Variant(tag, ts) => Ty::Variant(tag.clone(), all(ts, &mut |t| t.map(f))),
            Ty::Named(id, ts) => Ty::Named(*id, all(ts, &mut |t| t.map(f))),
            Ty::Union(ts) => union(ts.iter().map(|t| t.map(f))),
            Ty::Struct(fs) => {
                Ty::Struct(fs.iter().map(|(n, t)| (n.clone(), t.map(f))).collect())
            }
            Ty::Fn(ft) => Ty::Fn(Rc::new(FnTy {
                args: ft
                    .args
                    .iter()
                    .map(|a| FnArg { ty: a.ty.map(f), ..a.clone() })
                    .collect(),
                vargs: ft.vargs.as_ref().map(|t| t.map(f)),
                ret: ft.ret.map(f),
                throws: ft.throws.map(f),
            })),
        }
    }

    pub fn each(&self, f: &mut impl FnMut(&Ty)) {
        f(self);
        match self {
            Ty::Bot | Ty::Any | Ty::Prim(_) | Ty::Var(_) | Ty::Rigid(_) => (),
            Ty::Array(t) | Ty::List(t) | Ty::Error(t) | Ty::Ref(_, t) => t.each(f),
            Ty::Map(k, v) => {
                k.each(f);
                v.each(f)
            }
            Ty::Tuple(ts) | Ty::Variant(_, ts) | Ty::Named(_, ts) | Ty::Union(ts) => {
                ts.iter().for_each(|t| t.each(f))
            }
            Ty::Struct(fs) => fs.iter().for_each(|(_, t)| t.each(f)),
            Ty::Fn(ft) => {
                ft.args.iter().for_each(|a| a.ty.each(f));
                ft.vargs.iter().for_each(|t| t.each(f));
                ft.ret.each(f);
                ft.throws.each(f)
            }
        }
    }
}

pub fn union(ts: impl IntoIterator<Item = Ty>) -> Ty {
    fn add(out: &mut Vec<Ty>, prims: &mut BitFlags<Typ>, any: &mut bool, t: Ty) {
        match t {
            Ty::Bot => (),
            Ty::Any => *any = true,
            Ty::Prim(p) => *prims |= p,
            Ty::Union(ts) => ts.iter().for_each(|t| add(out, prims, any, t.clone())),
            t if out.contains(&t) => (),
            t => out.push(t),
        }
    }
    let (mut out, mut prims, mut any) = (vec![], BitFlags::empty(), false);
    ts.into_iter().for_each(|t| add(&mut out, &mut prims, &mut any, t));
    if any {
        return Ty::Any;
    }
    if !prims.is_empty() {
        out.insert(0, Ty::Prim(prims));
    }
    match out.len() {
        0 => Ty::Bot,
        1 => out.pop().unwrap(),
        _ => Ty::Union(out.into()),
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let list = |f: &mut fmt::Formatter<'_>, ts: &[Ty]| -> fmt::Result {
            for (i, t) in ts.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?
                }
                write!(f, "{t}")?
            }
            Ok(())
        };
        match self {
            Ty::Bot => write!(f, "_"),
            Ty::Any => write!(f, "Any"),
            Ty::Prim(p) if p.exactly_one().is_some() => {
                write!(f, "{}", format!("{:?}", p.exactly_one().unwrap()).to_lowercase())
            }
            Ty::Prim(p) => {
                write!(f, "[")?;
                for (i, t) in p.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?
                    }
                    write!(f, "{}", format!("{t:?}").to_lowercase())?
                }
                write!(f, "]")
            }
            Ty::Array(t) => write!(f, "Array<{t}>"),
            Ty::List(t) => write!(f, "List<{t}>"),
            Ty::Map(k, v) => write!(f, "Map<{k}, {v}>"),
            Ty::Error(t) => write!(f, "Error<{t}>"),
            Ty::Ref(true, t) => write!(f, "&mut {t}"),
            Ty::Ref(false, t) => write!(f, "&{t}"),
            Ty::Tuple(ts) => {
                write!(f, "(")?;
                list(f, ts)?;
                write!(f, ")")
            }
            Ty::Struct(fs) => {
                write!(f, "{{")?;
                for (i, (n, t)) in fs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?
                    }
                    write!(f, "{n}: {t}")?
                }
                write!(f, "}}")
            }
            Ty::Variant(tag, ts) if ts.is_empty() => write!(f, "`{tag}"),
            Ty::Variant(tag, ts) => {
                write!(f, "`{tag}(")?;
                list(f, ts)?;
                write!(f, ")")
            }
            Ty::Fn(ft) => {
                write!(f, "fn(")?;
                for (i, a) in ft.args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?
                    }
                    match &a.label {
                        Some(l) if a.optional => write!(f, "?#{l}: {}", a.ty)?,
                        Some(l) => write!(f, "#{l}: {}", a.ty)?,
                        None => write!(f, "{}", a.ty)?,
                    }
                }
                if let Some(v) = &ft.vargs {
                    write!(f, ", @args: {v}")?
                }
                write!(f, ") -> {}", ft.ret)?;
                match &ft.throws {
                    Ty::Bot => Ok(()),
                    t => write!(f, " throws {t}"),
                }
            }
            Ty::Union(ts) => {
                write!(f, "[")?;
                list(f, ts)?;
                write!(f, "]")
            }
            Ty::Var(v) => write!(f, "'_{}", v.0),
            Ty::Rigid(v) => write!(f, "'r{}", v.0),
            Ty::Named(id, ts) if ts.is_empty() => write!(f, "#{id}"),
            Ty::Named(id, ts) => {
                write!(f, "#{id}<")?;
                list(f, ts)?;
                write!(f, ">")
            }
        }
    }
}
