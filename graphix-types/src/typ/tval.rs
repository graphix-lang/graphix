use super::{PrintFlag, Type, cast::IsAFlags};
use crate::{abstract_value, env::Env, typ::format_with_flags};
use ahash::AHashSet;
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
                        // CR claude for eric: [perf] The cap counts values written, but
                        // each visited array pushes all of its elements here, and each
                        // map collects and pushes all of its pairs (54-63), before the
                        // cap applies. So NakedPrefix walks and allocates in proportion
                        // to a value's width: a failed cast<(i64, i64)> of an
                        // 8M-element array costs about 130 MB of transient stack to
                        // build a 456-byte InvalidCast message (cast.rs:46). A string
                        // leaf is written whole, so a failed cast of a large string
                        // puts the whole string in the message. Keep a cursor per open
                        // array or map, and cut long string leaves, so the prefix is
                        // bounded in work and size. (t-cast-setops-15)
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
                    v @ Value::Abstract(_) => match abstract_value::get(v) {
                        Some(g) => write!(f, "{g:?}")?,
                        None => write!(f, "{}", NakedValue(v))?,
                    },
                    v => write!(f, "{}", NakedValue(v))?,
                }
            }
        }
    }
    Ok(())
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
            (
                Type::Primitive(_)
                // CR claude for eric: [bug] Every typed print of an abstract value
                // loses the payload's type here. GxAbstract's Debug prints the payload
                // with fmt_naked, so a struct payload prints as its pair array, a tuple
                // as an array, a variant as [tag, args] (a nullary one as a quoted
                // string) and a List as its private nested-array rep. So `"[P({x: 1, y:
                // 2})]"` is `P([["x", 1], ["y", 2]])` while `"[p.0]"` is `{x: 1, y:
                // 2}`. Interpolation, println, dbg, the structural default of
                // Display::fmt and the shell echo all print this, where
                // design/traits.md and the book promise the type-directed structural
                // case in Graphix syntax. The env here holds the rep and its formals
                // (`Env::abstract_reps`), so this arm can print the payload as a TVal
                // of the rep at the box's params when the Display hook declines. A
                // process-global rep table keyed by AbstractId would be wrong, because
                // the id is the path's alone and two contexts may give one path
                // different reps. probe:
                // design/review-2026-10-05/repro/t-expr-core-05.gx (t-expr-core-05)
                | Type::Abstract { .. }
                | Type::Hole
                | Type::Concrete
                | Type::Function
                | Type::Singleton
                | Type::OneNumber
                | Type::Discernible
                | Type::Bottom
                | Type::Any
                | Type::Error(_),
                v,
            ) => fmt_naked(f, v),
            (Type::Fn(_), Value::Abstract(v)) => write!(f, "{v:?}"),
            (Type::Fn(_), v) => fmt_naked(f, v),
            // `hist` is the path: a name met again on the same value
            // expands without consuming it, so it prints naked.
            (Type::Ref(tr), v) => {
                let typ = match self.typ.lookup_ref(&self.env) {
                    Err(e) => return write!(f, "error, {e:?}"),
                    Ok(typ) => typ,
                };
                // CR claude for eric: [bug] The path key is the definition alone, while
                // cast_int and is_a_int key on ref_key(tr), the definition plus its
                // parameters. Under O<O<X>> with `type O<'a> = ['a, null]` (core's
                // Option included), the inner O<X> meets the same value under the same
                // definition. The guard takes it for a name expanding without consuming
                // structure, and the value prints naked: a List as its private cons rep
                // [1, [2, []]], a variant as ["Foo", 3], a struct as [["x", 1]]. String
                // interpolation, print/println/dbg and the shell share this printer, so
                // programs see the wrong string; A<B<X>> over two definitions prints
                // right. Key on ref_key(tr) as the cast walks do, ideally through one
                // path-key type the three walks share. probe:
                // design/review-2026-10-05/repro/t-cast-setops-10.gx (t-cast-setops-10)
                let key = (tr.def_key().unwrap_or(0), (*v as *const Value).addr());
                if !hist.insert(key) {
                    return fmt_naked(f, v);
                }
                let r = TVal { typ: &typ, env: self.env, v }.fmt_int(f, hist);
                hist.remove(&key);
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
