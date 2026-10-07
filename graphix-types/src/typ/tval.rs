use super::{PrintFlag, Type, cast::IsAFlags};
use crate::{abstract_value, env::Env, typ::format_with_flags};
use ahash::AHashMap;
use netidx_value::{NakedValue, Value};
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

/// The bytes of a string leaf a capped walk writes.
const MAX_LEAF: usize = 256;

/// `cap` bounds the number of values written before the walk stops
/// with a `…` (unbalanced by design: a truncated diagnostic dump); a
/// capped walk also cuts a long string leaf. Each open array or map is a
/// cursor, so the work is bounded by what is written.
fn fmt_naked_capped(f: &mut dyn fmt::Write, v: &Value, mut cap: usize) -> fmt::Result {
    type MapIter<'a> = <&'a netidx_value::Map as IntoIterator>::IntoIter;
    enum Open<'a> {
        Array(&'a [Value], usize),
        Map { pairs: MapIter<'a>, first: bool, value: Option<&'a Value> },
    }
    let capped = cap != usize::MAX;
    let mut stack: SmallVec<[Open; 16]> = SmallVec::new();
    let mut next = Some(v);
    loop {
        if let Some(v) = next.take() {
            if cap == 0 {
                return write!(f, "…");
            }
            cap -= 1;
            match v {
                Value::Array(a) => {
                    write!(f, "[")?;
                    stack.push(Open::Array(&a[..], 0));
                }
                Value::Map(m) => {
                    write!(f, "{{")?;
                    stack.push(Open::Map {
                        pairs: m.into_iter(),
                        first: true,
                        value: None,
                    });
                }
                Value::String(s) if capped && s.len() > MAX_LEAF => {
                    let mut end = MAX_LEAF;
                    while !s.is_char_boundary(end) {
                        end -= 1;
                    }
                    let cut = Value::String(s[..end].into());
                    write!(f, "{}…", NakedValue(&cut))?
                }
                // Debug consults a user Display impl when the hooks are armed.
                v @ Value::Abstract(_) => match abstract_value::get(v) {
                    Some(g) => write!(f, "{g:?}")?,
                    None => write!(f, "{}", NakedValue(v))?,
                },
                v => write!(f, "{}", NakedValue(v))?,
            }
            continue;
        }
        match stack.last_mut() {
            None => return Ok(()),
            Some(Open::Array(a, i)) => match a.get(*i) {
                Some(v) => {
                    if *i > 0 {
                        write!(f, ", ")?
                    }
                    *i += 1;
                    next = Some(v)
                }
                None => {
                    write!(f, "]")?;
                    stack.pop();
                }
            },
            Some(Open::Map { pairs, first, value }) => match value.take() {
                Some(v) => {
                    write!(f, " => ")?;
                    next = Some(v)
                }
                None => match pairs.next() {
                    Some((k, v)) => {
                        if !*first {
                            write!(f, ", ")?
                        }
                        *first = false;
                        *value = Some(v);
                        next = Some(k)
                    }
                    None => {
                        write!(f, "}}")?;
                        stack.pop();
                    }
                },
            },
        }
    }
}

/// Bounded-prefix Display of a value, for diagnostics.
pub(super) struct NakedPrefix<'a>(pub(super) &'a Value);

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
/// to: the one whose shape admits it when that is one member, else the
/// first strict match, else the first structured plain match, else the
/// first plain match.
fn member_of(env: &Env, ts: &[Type], v: &Value) -> Option<usize> {
    let mut admits = ts.iter().enumerate().filter(|(_, t)| !shape_excludes(t, v));
    match (admits.next(), admits.next()) {
        (Some((i, _)), None) => return Some(i),
        (None, _) => return None,
        (Some(_), Some(_)) => (),
    }
    let blind = |t: &Type| {
        t.with_deref(|t| matches!(t, None | Some(Type::Any) | Some(Type::Bottom)))
    };
    ts.iter()
        .position(|t| t.is_a_with(env, IsAFlags::Strict.into(), v))
        .or_else(|| ts.iter().position(|t| !blind(t) && t.is_a(env, v)))
        .or_else(|| ts.iter().position(|t| t.is_a(env, v)))
}

impl<'a> TVal<'a> {
    fn fmt_int(
        &self,
        f: &mut fmt::Formatter<'_>,
        hist: &mut AHashMap<(usize, usize), usize>,
    ) -> fmt::Result {
        crate::stack::ensure_sufficient(|| self.fmt_inner(f, hist))
    }

    fn fmt_inner(
        &self,
        f: &mut fmt::Formatter<'_>,
        hist: &mut AHashMap<(usize, usize), usize>,
    ) -> fmt::Result {
        if crate::dbgenv::graphix_dbg_tval() {
            format_with_flags(PrintFlag::DerefTVars, || {
                eprintln!("TVAL typ={} v={}", self.typ, NakedPrefix(self.v));
            });
        }
        // an abstract value carries its type: the payload prints as its
        // representation at the value's params, whatever the static type
        if let Some(g) = abstract_value::get(self.v)
            && let Some(rep) = self.env.abstract_reps.get(&g.id)
        {
            if let Some(s) = g.displayed() {
                return f.write_str(&s);
            }
            let typ = rep.instantiate_with(&g.params);
            write!(f, "{}(", g.name)?;
            TVal { typ: &typ, env: self.env, v: &g.payload }.fmt_int(f, hist)?;
            return write!(f, ")");
        }
        match (&self.typ, &self.v) {
            (
                Type::Primitive(_)
                | Type::Abstract { .. }
                | Type::Hole
                | Type::Concrete
                | Type::Function
                | Type::Singleton
                | Type::OneNumber
                | Type::Discernible
                | Type::Ordered
                | Type::Bottom
                | Type::Any
                | Type::Error(_),
                v,
            ) => fmt_naked(f, v),
            (Type::Fn(_), Value::Abstract(v)) => write!(f, "{v:?}"),
            (Type::Fn(_), v) => fmt_naked(f, v),
            // `hist` is the path: a definition met again on the same value
            // with params no smaller expands without consuming it (or grows
            // at every level), so it prints naked; with smaller ones it is
            // nested (`O<O<X>>`).
            (Type::Ref(tr), v) => {
                let typ = match self.typ.lookup_ref(&self.env) {
                    Err(e) => return write!(f, "error, {e:?}"),
                    Ok(typ) => typ,
                };
                let key = (tr.def_key().unwrap_or(0), (*v as *const Value).addr());
                let size = super::params_size(&tr.params);
                if hist.get(&key).is_some_and(|prev| size >= *prev) {
                    return fmt_naked(f, v);
                }
                let outer = hist.insert(key, size);
                let r = TVal { typ: &typ, env: self.env, v }.fmt_int(f, hist);
                match outer {
                    Some(o) => hist.insert(key, o),
                    None => hist.remove(&key),
                };
                r
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
                use crate::list;
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
            // a reference's id is the session's, never the program's
            (Type::ByRef(..), _) => write!(f, "&ref"),
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
            (Type::TVar(tv), v) => match tv.binding() {
                None => fmt_naked(f, v),
                Some(typ) => TVal { env: self.env, typ: &typ, v }.fmt_int(f, hist),
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
                log::warn!(
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
        let head = s.trim_end_matches(']');
        assert!(head.ends_with("[0, 0") && s.len() - head.len() == 100_000);
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

    #[test]
    fn naked_prefix_is_bounded() {
        let wide = Value::Array((0..100_000).map(Value::I64).collect());
        let s = NakedPrefix(&wide).to_string();
        assert!(s.starts_with("[0, 1, 2") && s.ends_with("…") && s.len() < 1024, "{s}");
        let long = Value::String("x".repeat(100_000).into());
        let s = NakedPrefix(&long).to_string();
        assert!(s.len() < MAX_LEAF + 16 && s.ends_with("…"), "{s}");
        let m: netidx_value::Map =
            [(Value::I64(1), Value::from("a")), (Value::I64(2), wide.clone())]
                .into_iter()
                .collect();
        let v = Value::Array([Value::Map(m), Value::Null].into_iter().collect());
        let mut whole = String::new();
        fmt_naked(&mut whole, &v).unwrap();
        assert!(
            whole.starts_with(r#"[{1 => "a", 2 => [0, 1, "#)
                && whole.ends_with("99999]}, null]")
        );
    }
}
