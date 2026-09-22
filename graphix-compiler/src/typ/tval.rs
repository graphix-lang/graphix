use super::{PrintFlag, Type, TypeRef, cast::IsAFlags};
use crate::{env::Env, typ::format_with_flags};
use ahash::AHashSet;
use netidx_value::NakedValue;
use netidx_value::Value;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::fmt;

/// A value with its type, used for formatting.
pub struct TVal<'a> {
    pub env: &'a Env,
    pub typ: &'a Type,
    pub v: &'a Value,
}

/// The type-blind formatter: walks composite values printing every
/// leaf naked (no `i64:` prefixes at any depth). Iterative on an
/// explicit stack because value nesting depth is user-controlled.
pub(crate) fn fmt_naked(f: &mut dyn fmt::Write, v: &Value) -> fmt::Result {
    fmt_naked_capped(f, v, usize::MAX)
}

/// `cap` bounds the number of values written before the walk stops
/// with a `…` (unbalanced by design: a truncated diagnostic dump).
fn fmt_naked_capped(f: &mut dyn fmt::Write, v: &Value, mut cap: usize) -> fmt::Result {
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
                    // Debug consults a user Display impl when the hooks are armed.
                    v @ Value::Abstract(_) if crate::abstract_value::get(v).is_some() => {
                        let g = crate::abstract_value::get(v).unwrap();
                        write!(f, "{g:?}")?
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

/// Does `t`'s outer shape alone exclude `v`: a variant by tag and
/// arity, a primitive by class. Anything else may hold it.
fn shape_excludes(t: &Type, v: &Value) -> bool {
    match (t, v) {
        (Type::Variant(name, flds, _), Value::Array(a)) => {
            a.len() != flds.len() + 1 || !matches!(&a[0], Value::String(s) if s == name)
        }
        (Type::Variant(name, flds, _), Value::String(s)) => !flds.is_empty() || s != name,
        (Type::Variant(..), _) => true,
        (Type::Primitive(p), v) => !p.contains(netidx_value::Typ::get(v)),
        _ => false,
    }
}

/// The member of `ts` a value known to belong to one of them belongs
/// to: the one whose shape admits it when that is one member, else
/// `coretraits::union_member`'s choice.
fn member_of(env: &Env, ts: &[Type], v: &Value) -> Option<usize> {
    let mut admits = ts.iter().enumerate().filter(|(_, t)| !shape_excludes(t, v));
    match (admits.next(), admits.next()) {
        (Some((i, _)), None) => Some(i),
        (None, _) => None,
        (Some(_), Some(_)) => crate::node::coretraits::union_member(env, ts, v),
    }
}

impl<'a> TVal<'a> {
    // XCR codex for eric: [CR14, P1] done: the value is checked against the
    // type once at the root, every level grows the stack as it needs, and
    // a union level walks only when two members share the value's shape.
    fn fmt_int(
        &self,
        f: &mut fmt::Formatter<'_>,
        hist: &mut AHashSet<(usize, usize)>,
    ) -> fmt::Result {
        crate::stack::ensure_sufficient(|| self.fmt_inner(f, hist))
    }

    fn fmt_inner(
        &self,
        f: &mut fmt::Formatter<'_>,
        hist: &mut AHashSet<(usize, usize)>,
    ) -> fmt::Result {
        if crate::dbgenv::graphix_dbg_tval() {
            format_with_flags(PrintFlag::DerefTVars, || {
                eprintln!("TVAL typ={} v={}", self.typ, NakedPrefix(self.v));
            });
        }
        match (&self.typ, &self.v) {
            (Type::Abstract { .. }, v) if crate::abstract_value::get(v).is_some() => {
                let g = crate::abstract_value::get(v).unwrap();
                write!(f, "{g:?}")
            }
            (
                Type::Primitive(_)
                | Type::Abstract { .. }
                | Type::Hole
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
            // Prints `[<a, b, c>]`; a non-list-shaped value falls
            // back to the naked print.
            (Type::List(et), v) => {
                use crate::node::collection::list;
                if !list::is_list(v) {
                    return fmt_naked(f, v);
                }
                write!(f, "[<")?;
                let mut cur: Value = (*v).clone();
                let mut first = true;
                loop {
                    let Some((h, t)) = list::split(&cur) else { break };
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    TVal { typ: et, env: self.env, v: h }.fmt_int(f, hist)?;
                    let t = t.clone();
                    cur = t;
                }
                write!(f, ">]")
            }
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
                for (i, ((n, et, _), v)) in flds.iter().zip(a.iter()).enumerate() {
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
            (Type::App(c, a), v) => match Type::app_filled(c, a) {
                Some(typ) => TVal { env: self.env, typ: &typ, v }.fmt_int(f, hist),
                None => fmt_naked(f, v),
            },
            (Type::Variant(n, flds, _), Value::Array(a)) if a.len() >= 2 => {
                write!(f, "`{n}(")?;
                for (i, (t, v)) in flds.iter().zip(a[1..].iter()).enumerate() {
                    Self { typ: t, env: self.env, v }.fmt_int(f, hist)?;
                    if i < flds.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, ")")
            }
            (Type::Variant(_, _, _), Value::String(s)) => write!(f, "`{s}"),
            (Type::Variant(_, _, _), v) => fmt_naked(f, v),
            // Member selection is `coretraits::union_member`: the first
            // strict match (blind leaves match nothing), else plain is_a.
            (Type::Set(ts), v) => match member_of(self.env, ts, v) {
                None => fmt_naked(f, v),
                Some(i) => Self { typ: &ts[i], env: self.env, v }.fmt_int(f, hist),
            },
        }
    }
}

impl<'a> fmt::Display for TVal<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

    // Printing must not recurse on value depth.
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

    // Typed printing grows the stack as the value's depth needs and
    // checks each level once: deep and wide values both print.
    #[test]
    fn deep_typed_value_prints() {
        use crate::{expr::ModPath, expr::parser::parse_type, typ::Type};
        use triomphe::Arc;
        let env = Env::default();
        let mut v = Value::I64(0);
        let mut typ = Type::Primitive(netidx_value::Typ::I64.into());
        for _ in 0..1000 {
            v = Value::Array([v].into_iter().collect());
            typ = Type::Array(Arc::new(typ));
        }
        assert!(typ.is_a(&env, &v));
        let s = format_compact!("{}", TVal { env: &env, typ: &typ, v: &v });
        assert!(s.starts_with("[[[") && s.ends_with("]]]"), "{}", &s[s.len() - 20..]);
        assert!(s.contains("0]]"), "{}", &s[s.len() - 40..]);
        // a recursive union: each level picks its member by shape
        let mut env = Env::default();
        env.deftype(
            &ModPath::root(),
            "L",
            Arc::from_iter([]),
            &crate::expr::TypeDefBody::Alias(
                parse_type("[`Cons(i64, L), `Nil]").unwrap(),
            ),
            true,
            None,
            Default::default(),
            Arc::new(Default::default()),
        )
        .unwrap();
        let typ = parse_type("L").unwrap().scope_refs(&ModPath::root());
        let mut v = Value::String("Nil".into());
        for i in 0..20_000i64 {
            v = Value::Array(
                [Value::String("Cons".into()), Value::I64(i), v].into_iter().collect(),
            );
        }
        let started = std::time::Instant::now();
        let s = format_compact!("{}", TVal { env: &env, typ: &typ, v: &v });
        assert!(started.elapsed() < std::time::Duration::from_secs(2));
        assert!(s.starts_with("`Cons(19999, "));
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
