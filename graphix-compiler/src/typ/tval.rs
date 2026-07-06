use super::{PrintFlag, Type, TypeRef, cast::IsAFlags};
use crate::{env::Env, typ::format_with_flags};
use ahash::AHashSet;
use netidx::publisher::Value;
use netidx_value::NakedValue;
use poolshark::local::LPooled;
use std::fmt;

/// A value with it's type, used for formatting
pub struct TVal<'a> {
    pub env: &'a Env,
    pub typ: &'a Type,
    pub v: &'a Value,
}

/// The type-blind fallback formatter: recurse composite VALUES,
/// printing every leaf naked. `NakedValue` alone is not recursive — a
/// composite falls through to netidx's TYPED `Value` display, so its
/// nested elements printed `i64:0`-style whenever the static type
/// didn't guide the walk (an `Any`/⊥/unbound-tvar slot, a union member
/// the value matched imprecisely). Interpolated output never
/// type-prefixes default int/float, nested included (Eric's ruling,
/// soak-jul06c B4) — and the JIT's formatting agrees.
fn fmt_naked_rec(f: &mut fmt::Formatter<'_>, v: &Value) -> fmt::Result {
    match v {
        Value::Array(a) => {
            write!(f, "[")?;
            for (i, v) in a.iter().enumerate() {
                fmt_naked_rec(f, v)?;
                if i < a.len() - 1 {
                    write!(f, ", ")?
                }
            }
            write!(f, "]")
        }
        Value::Map(m) => {
            write!(f, "{{")?;
            for (i, (k, v)) in m.into_iter().enumerate() {
                fmt_naked_rec(f, k)?;
                write!(f, " => ")?;
                fmt_naked_rec(f, v)?;
                if i < m.len() - 1 {
                    write!(f, ", ")?
                }
            }
            write!(f, "}}")
        }
        v => write!(f, "{}", NakedValue(v)),
    }
}

impl<'a> TVal<'a> {
    fn fmt_int(
        &self,
        f: &mut fmt::Formatter<'_>,
        hist: &mut AHashSet<(usize, usize)>,
    ) -> fmt::Result {
        if !self.typ.is_a_with(&self.env, IsAFlags::MatchAbstract.into(), &self.v) {
            return format_with_flags(PrintFlag::DerefTVars, || {
                eprintln!("error, type {} does not match value {:?}", self.typ, self.v);
                fmt_naked_rec(f, self.v)
            });
        }
        match (&self.typ, &self.v) {
            (
                Type::Primitive(_)
                | Type::Abstract { .. }
                | Type::Bottom
                | Type::Any
                | Type::Error(_),
                v,
            ) => fmt_naked_rec(f, v),
            (Type::Fn(_), Value::Abstract(v)) => write!(f, "{v:?}"),
            (Type::Fn(_), v) => fmt_naked_rec(f, v),
            (Type::Ref(TypeRef { .. }), v) => {
                let typ = match self.typ.lookup_ref(&self.env) {
                    Err(e) => return write!(f, "error, {e:?}"),
                    Ok(typ) => typ,
                };
                let typ_addr = (&typ as *const Type).addr();
                let v_addr = (self.v as *const Value).addr();
                if !hist.contains(&(typ_addr, v_addr)) {
                    hist.insert((typ_addr, v_addr));
                    TVal { typ: &typ, env: self.env, v }.fmt_int(f, hist)?
                }
                Ok(())
            }
            (Type::Array(et), Value::Array(a)) => {
                write!(f, "[")?;
                for (i, v) in a.iter().enumerate() {
                    Self { typ: et, env: self.env, v }.fmt_int(f, hist)?;
                    if i < a.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "]")
            }
            (Type::Array(_), v) => fmt_naked_rec(f, v),
            (Type::Map { key, value }, Value::Map(m)) => {
                write!(f, "{{")?;
                for (i, (k, v)) in m.into_iter().enumerate() {
                    Self { typ: key, env: self.env, v: k }.fmt_int(f, hist)?;
                    write!(f, " => ")?;
                    Self { typ: value, env: self.env, v: v }.fmt_int(f, hist)?;
                    if i < m.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            (Type::Map { .. }, v) => fmt_naked_rec(f, v),
            (Type::ByRef(_), v) => fmt_naked_rec(f, v),
            (Type::Struct(flds), Value::Array(a)) => {
                write!(f, "{{")?;
                for (i, ((n, et), v)) in flds.iter().zip(a.iter()).enumerate() {
                    write!(f, "{n}: ")?;
                    match v {
                        Value::Array(a) if a.len() == 2 => {
                            Self { typ: et, env: self.env, v: &a[1] }.fmt_int(f, hist)?
                        }
                        _ => write!(f, "err")?,
                    }
                    if i < flds.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            (Type::Struct(_), v) => fmt_naked_rec(f, v),
            (Type::Tuple(flds), Value::Array(a)) => {
                write!(f, "(")?;
                for (i, (t, v)) in flds.iter().zip(a.iter()).enumerate() {
                    Self { typ: t, env: self.env, v }.fmt_int(f, hist)?;
                    if i < flds.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, ")")
            }
            (Type::Tuple(_), v) => fmt_naked_rec(f, v),
            (Type::TVar(tv), v) => match &tv.read().typ.read().typ {
                None => fmt_naked_rec(f, v),
                Some(typ) => TVal { env: self.env, typ, v }.fmt_int(f, hist),
            },
            (Type::Variant(n, flds), Value::Array(a)) if a.len() >= 2 => {
                write!(f, "`{n}(")?;
                for (i, (t, v)) in flds.iter().zip(a[1..].iter()).enumerate() {
                    Self { typ: t, env: self.env, v }.fmt_int(f, hist)?;
                    if i < flds.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, ")")
            }
            (Type::Variant(_, _), Value::String(s)) => write!(f, "`{s}"),
            (Type::Variant(_, _), v) => fmt_naked_rec(f, v),
            (Type::Set(ts), v) => match ts.iter().find(|t| t.is_a(&self.env, v)) {
                None => fmt_naked_rec(f, v),
                Some(t) => Self { typ: t, env: self.env, v }.fmt_int(f, hist),
            },
        }
    }
}

impl<'a> fmt::Display for TVal<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_int(f, &mut LPooled::take())
    }
}
