use super::{PrintFlag, Type, TypeRef, cast::IsAFlags};
use crate::{env::Env, typ::format_with_flags};
use ahash::AHashSet;
use netidx::publisher::Value;
use netidx_value::NakedValue;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::fmt;

/// A value with it's type, used for formatting
pub struct TVal<'a> {
    pub env: &'a Env,
    pub typ: &'a Type,
    pub v: &'a Value,
}

/// The type-blind fallback formatter: walk composite VALUES, printing
/// every leaf naked. `NakedValue` alone is not recursive — a
/// composite falls through to netidx's TYPED `Value` display, so its
/// nested elements printed `i64:0`-style whenever the static type
/// didn't guide the walk (an `Any`/⊥/unbound-tvar slot, a union member
/// the value matched imprecisely). Interpolated output never
/// type-prefixes default int/float, nested included (Eric's ruling,
/// soak-jul06c B4) — and the JIT's formatting agrees.
///
/// Iterative on an explicit stack: value nesting depth is
/// USER-CONTROLLED (a cons list nests one level per element — printing
/// a ~2k-element `list::init` overflowed the stack, jul17a
/// crash_000003), so the walk must not recurse.
fn fmt_naked(f: &mut fmt::Formatter<'_>, v: &Value) -> fmt::Result {
    fmt_naked_capped(f, v, usize::MAX)
}

/// `cap` bounds the number of VALUES written before the walk stops
/// with a `…` (unbalanced by design — it is a truncated dump, used by
/// the type-mismatch diagnostic so a huge mismatched value can't spam
/// unbounded stderr). The full printer passes `usize::MAX`.
fn fmt_naked_capped(
    f: &mut fmt::Formatter<'_>,
    v: &Value,
    mut cap: usize,
) -> fmt::Result {
    enum W<'a> {
        V(&'a Value),
        S(&'static str),
    }
    let mut stack: SmallVec<[W; 64]> = SmallVec::new();
    stack.push(W::V(v));
    while let Some(w) = stack.pop() {
        match w {
            W::S(s) => write!(f, "{s}")?,
            W::V(v) => {
                if cap == 0 {
                    return write!(f, "…");
                }
                cap -= 1;
                match v {
                    Value::Array(a) => {
                        write!(f, "[")?;
                        stack.push(W::S("]"));
                        for i in (0..a.len()).rev() {
                            stack.push(W::V(&a[i]));
                            if i > 0 {
                                stack.push(W::S(", "));
                            }
                        }
                    }
                    Value::Map(m) => {
                        write!(f, "{{")?;
                        stack.push(W::S("}"));
                        let pairs: SmallVec<[(&Value, &Value); 16]> =
                            m.into_iter().collect();
                        for (i, (k, v)) in pairs.iter().enumerate().rev() {
                            stack.push(W::V(v));
                            stack.push(W::S(" => "));
                            stack.push(W::V(k));
                            if i > 0 {
                                stack.push(W::S(", "));
                            }
                        }
                    }
                    v => write!(f, "{}", NakedValue(v))?,
                }
            }
        }
    }
    Ok(())
}

/// Bounded-prefix Display of a value, for diagnostics.
struct NakedPrefix<'a>(&'a Value);

impl fmt::Display for NakedPrefix<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_naked_capped(f, self.0, 128)
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
                eprintln!(
                    "error, type {} does not match value {}",
                    self.typ,
                    NakedPrefix(self.v)
                );
                fmt_naked(f, self.v)
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
            ) => fmt_naked(f, v),
            (Type::Fn(_), Value::Abstract(v)) => write!(f, "{v:?}"),
            (Type::Fn(_), v) => fmt_naked(f, v),
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
            (Type::Array(_), v) => fmt_naked(f, v),
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
            (Type::Map { .. }, v) => fmt_naked(f, v),
            (Type::ByRef(_), v) => fmt_naked(f, v),
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
            (Type::Struct(_), v) => fmt_naked(f, v),
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
            (Type::Tuple(_), v) => fmt_naked(f, v),
            (Type::TVar(tv), v) => match &tv.read().typ.read().typ {
                None => fmt_naked(f, v),
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
            (Type::Variant(_, _), v) => fmt_naked(f, v),
            (Type::Set(ts), v) => match ts.iter().find(|t| t.is_a(&self.env, v)) {
                None => fmt_naked(f, v),
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

#[cfg(test)]
mod test {
    use super::*;
    use compact_str::format_compact;

    struct Naked<'a>(&'a Value);
    impl fmt::Display for Naked<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt_naked(f, self.0)
        }
    }

    // A cons-style chain nests one VALUE level per element; the old
    // recursive walker overflowed the stack at ~2k levels (jul17a
    // crash_000003). The iterative walker must not care about depth.
    #[test]
    fn deep_value_prints_iteratively() {
        let mut v = Value::I64(0);
        for i in 0..100_000i64 {
            v = Value::Array([Value::I64(i), v].into_iter().collect());
        }
        let s = format_compact!("{}", Naked(&v));
        assert!(s.starts_with("[99999, [99998, "));
        assert!(s.ends_with(", 0]]") || s.ends_with("]"));
    }

    #[test]
    fn naked_prefix_caps() {
        let mut v = Value::I64(0);
        for i in 0..1000i64 {
            v = Value::Array([Value::I64(i), v].into_iter().collect());
        }
        let s = format_compact!("{}", NakedPrefix(&v));
        assert!(s.ends_with("…"));
        assert!(s.len() < 1024);
    }
}
