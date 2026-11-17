//! Inference by bounds. While a definition is checked no variable is
//! bound: what flows into a variable is a lower bound of its cell, what a
//! use demands of it an upper bound (a constraint), and each new bound is
//! met with the bounds already on the other side, so the bounds are always
//! consistent with each other.

use crate::{
    env::Env,
    stack::ensure_sufficient,
    typ::{FnArgKind, FnType, Mutability, TVar, Type, setops::union_identical},
};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use smallvec::SmallVec;

/// The relation's own state for one check.
#[derive(Default)]
pub struct Solving {
    /// The pairs holding a typedef's name that are being related: a pair
    /// met again inside itself holds.
    unfolding: Vec<(Type, Type)>,
    /// The bounds added since the outermost attempt began, in order, each
    /// the last of its cell's list when added: a failed attempt takes its
    /// own back.
    added: Vec<(TVar, Side)>,
    /// How many attempts are in progress.
    attempts: u32,
}

#[derive(Clone, Copy)]
enum Side {
    Lower,
    Upper,
}

/// `t` past the cells that are bound: an open cell, or a type that is no
/// cell.
fn shallow(t: &Type) -> Type {
    match t {
        Type::TVar(tv) => match tv.binding() {
            Some(b) => ensure_sufficient(|| shallow(&b)),
            None => t.clone(),
        },
        t => t.clone(),
    }
}

/// What tells two members of a union apart without looking inside them.
fn alike(a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::Primitive(p), Type::Primitive(q)) => p.intersects(*q),
        (Type::Array(_), Type::Array(_))
        | (Type::List(_), Type::List(_))
        | (Type::Error(_), Type::Error(_))
        | (Type::Map(_), Type::Map(_))
        | (Type::Fn(_), Type::Fn(_)) => true,
        (Type::ByRef(m, _), Type::ByRef(n, _)) => m == n,
        (Type::Tuple(xs), Type::Tuple(ys)) => xs.len() == ys.len(),
        (Type::Variant(t, xs, _), Type::Variant(u, ys, _)) => {
            t == u && xs.len() == ys.len()
        }
        (Type::Struct(xs), Type::Struct(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| x.0 == y.0)
        }
        (Type::Abstract { id: x, .. }, Type::Abstract { id: y, .. }) => x == y,
        _ => false,
    }
}

fn mismatch<T>(wanted: &Type, got: &Type) -> Result<T> {
    bail!("type mismatch {wanted} does not contain {got}")
}

/// Add `t` to `bounds` unless it is there; false if it was.
fn record(bounds: &mut SmallVec<[Type; 1]>, t: &Type) -> bool {
    match bounds.iter().any(|b| union_identical(b, t)) {
        true => false,
        false => {
            bounds.push(t.clone());
            true
        }
    }
}

impl Solving {
    /// `a` fits where `b` is wanted. An open cell on either side takes the
    /// other as a bound, and the bound meets every bound the cell has on
    /// its other side.
    pub fn constrain(&mut self, env: &Env, a: &Type, b: &Type) -> Result<()> {
        ensure_sufficient(|| self.constrain_in(env, a, b))
    }

    /// [`Self::constrain`] as a question: where `a` does not fit, every
    /// bound the try added is taken back and nothing is changed.
    pub fn attempt(&mut self, env: &Env, a: &Type, b: &Type) -> bool {
        let mark = self.added.len();
        self.attempts += 1;
        let fits = self.constrain(env, a, b).is_ok();
        self.attempts -= 1;
        if !fits {
            for (v, side) in self.added.drain(mark..).rev() {
                let cell = v.cell();
                let mut c = cell.write();
                match side {
                    Side::Lower => c.lower.pop(),
                    Side::Upper => c.constraints.pop(),
                };
            }
        }
        if self.attempts == 0 {
            self.added.clear()
        }
        fits
    }

    fn constrain_in(&mut self, env: &Env, a: &Type, b: &Type) -> Result<()> {
        let (a, b) = (shallow(a), shallow(b));
        match (&a, &b) {
            (Type::Bottom, _) | (_, Type::Any) => Ok(()),
            (Type::TVar(v), Type::TVar(w)) if v.same_cell(w) => Ok(()),
            // with no open cell on either side this is the relation over
            // decided types
            _ if !a.has_unbound() && !b.has_unbound() => match b.contains(env, &a)? {
                true => Ok(()),
                false => mismatch(&b, &a),
            },
            (Type::TVar(v), _) => self.bound_above(env, v, &b),
            (_, Type::TVar(w)) => self.bound_below(env, w, &a),
            (Type::Set(ms), _) => ms.iter().try_for_each(|m| self.constrain(env, m, &b)),
            (Type::Ref(_), _) | (_, Type::Ref(_)) => self.unfolded(env, &a, &b),
            (_, Type::Set(ms)) => {
                let mut takers = ms.iter().filter(|m| alike(&shallow(m), &a));
                match (takers.next(), takers.next()) {
                    (Some(m), None) => self.constrain(env, &a, m),
                    (None, _) => mismatch(&b, &a),
                    (Some(_), Some(_)) => {
                        bail!("{a} could go to more than one member of {b}")
                    }
                }
            }
            (Type::Array(x), Type::Array(y))
            | (Type::List(x), Type::List(y))
            | (Type::Error(x), Type::Error(y))
            | (Type::ByRef(_, x), Type::ByRef(Mutability::Shared, y)) => {
                self.constrain(env, x, y)
            }
            (Type::ByRef(Mutability::Mut, x), Type::ByRef(Mutability::Mut, y)) => {
                self.equal(env, x, y)
            }
            (Type::Map(m), Type::Map(n)) => {
                self.constrain(env, &m.key, &n.key)?;
                self.constrain(env, &m.value, &n.value)
            }
            (Type::Tuple(xs), Type::Tuple(ys)) if alike(&a, &b) => {
                xs.iter().zip(ys.iter()).try_for_each(|(x, y)| self.constrain(env, x, y))
            }
            (Type::Variant(_, xs, _), Type::Variant(_, ys, _)) if alike(&a, &b) => {
                xs.iter().zip(ys.iter()).try_for_each(|(x, y)| self.constrain(env, x, y))
            }
            (Type::Struct(xs), Type::Struct(ys)) if alike(&a, &b) => xs
                .iter()
                .zip(ys.iter())
                .try_for_each(|(x, y)| self.constrain(env, &x.1, &y.1)),
            (Type::Abstract { params: xs, .. }, Type::Abstract { params: ys, .. })
                if alike(&a, &b) && xs.len() == ys.len() =>
            {
                xs.iter().zip(ys.iter()).try_for_each(|(x, y)| self.equal(env, x, y))
            }
            (Type::Fn(f), Type::Fn(g)) => self.fn_below(env, f, g),
            _ => mismatch(&b, &a),
        }
    }

    /// Each fits where the other is wanted.
    fn equal(&mut self, env: &Env, a: &Type, b: &Type) -> Result<()> {
        self.constrain(env, a, b)?;
        self.constrain(env, b, a)
    }

    /// `b` is a new upper bound of the open cell `v`: whatever has flowed
    /// into `v` must fit it.
    fn bound_above(&mut self, env: &Env, v: &TVar, b: &Type) -> Result<()> {
        let cell = v.cell();
        let lower = {
            let mut c = cell.write();
            match record(&mut c.constraints, b) {
                true => c.lower.clone(),
                false => return Ok(()),
            }
        };
        if self.attempts > 0 {
            self.added.push((v.clone(), Side::Upper))
        }
        lower.iter().try_for_each(|l| self.constrain(env, l, b))
    }

    /// `a` is a new lower bound of the open cell `w`: it must fit whatever
    /// is demanded of `w`.
    fn bound_below(&mut self, env: &Env, w: &TVar, a: &Type) -> Result<()> {
        let cell = w.cell();
        let upper = {
            let mut c = cell.write();
            match record(&mut c.lower, a) {
                true => c.constraints.clone(),
                false => return Ok(()),
            }
        };
        if self.attempts > 0 {
            self.added.push((w.clone(), Side::Lower))
        }
        upper.iter().try_for_each(|u| self.constrain(env, a, u))
    }

    /// A pair with a typedef's name on a side, by what the name stands for.
    fn unfolded(&mut self, env: &Env, a: &Type, b: &Type) -> Result<()> {
        let met = |(x, y): &(Type, Type)| union_identical(x, a) && union_identical(y, b);
        if self.unfolding.iter().any(met) {
            return Ok(());
        }
        let expand = |t: &Type| match t {
            Type::Ref(_) => t.lookup_ref(env),
            t => Ok(t.clone()),
        };
        let (x, y) = (expand(a)?, expand(b)?);
        self.unfolding.push((a.clone(), b.clone()));
        let r = self.constrain(env, &x, &y);
        self.unfolding.pop();
        r
    }

    /// `f` may be called wherever `g` may be.
    fn fn_below(&mut self, env: &Env, f: &FnType, g: &FnType) -> Result<()> {
        let positional = |ft: &FnType| -> SmallVec<[Type; 4]> {
            let args = ft.args.iter();
            args.filter(|a| matches!(a.kind, FnArgKind::Positional { .. }))
                .map(|a| a.typ.clone())
                .collect()
        };
        let labeled = |ft: &FnType| -> SmallVec<[(ArcStr, bool, Type); 4]> {
            let args = ft.args.iter().filter_map(|a| match &a.kind {
                FnArgKind::Labeled { name, has_default } => {
                    Some((name.clone(), *has_default, a.typ.clone()))
                }
                FnArgKind::Positional { .. } => None,
            });
            args.collect()
        };
        let (fp, gp) = (positional(f), positional(g));
        for (i, ga) in gp.iter().enumerate() {
            match (fp.get(i), &f.vargs) {
                (Some(fa), _) => self.constrain(env, ga, fa)?,
                (None, Some(v)) => self.constrain(env, ga, v)?,
                (None, None) => bail!(
                    "type mismatch: a function of {} arguments where one of {} is wanted",
                    fp.len(),
                    gp.len()
                ),
            }
        }
        if fp.len() > gp.len() {
            bail!(
                "type mismatch: a function of {} arguments where one of {} is wanted",
                fp.len(),
                gp.len()
            )
        }
        if let (Some(fv), Some(gv)) = (&f.vargs, &g.vargs) {
            self.constrain(env, gv, fv)?
        }
        let (fl, gl) = (labeled(f), labeled(g));
        for (name, optional, ga) in gl.iter() {
            match fl.iter().find(|(n, ..)| n == name) {
                Some((_, false, _)) if *optional => {
                    bail!("type mismatch: #{name} is required")
                }
                Some((_, _, fa)) => self.constrain(env, ga, fa)?,
                None => bail!("type mismatch: no argument #{name}"),
            }
        }
        if let Some((name, ..)) =
            fl.iter().find(|(n, opt, _)| !opt && !gl.iter().any(|(m, ..)| m == n))
        {
            bail!("type mismatch: #{name} is required")
        }
        self.constrain(env, &f.rtype, &g.rtype)?;
        self.constrain(env, &f.throws, &g.throws)
    }
}

#[cfg(test)]
mod tests {
    use super::Solving;
    use crate::{
        env::Env,
        typ::{
            TVar, Type,
            cast::tests::{parsed, typed},
        },
    };
    use ahash::AHashMap;
    use arcstr::ArcStr;

    fn var(name: &str) -> (TVar, Type) {
        let tv = TVar::empty_named(ArcStr::from(name));
        (tv.clone(), Type::TVar(tv))
    }

    /// `t` written at the root of `env`, each variable it names standing
    /// for the type `vars` gives it.
    fn over(env: &Env, t: &str, vars: &[(&str, &Type)]) -> Type {
        let known = vars.iter().map(|(n, t)| (ArcStr::from(*n), (*t).clone()));
        parsed(env, t).replace_tvars(&AHashMap::from_iter(known))
    }

    fn shown(ts: &[Type]) -> Vec<String> {
        ts.iter().map(|t| t.to_string()).collect()
    }

    #[test]
    fn decided_types_are_related_as_they_are() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let t = |t: &str| parsed(&env, t);
        assert!(s.constrain(&env, &t("i64"), &t("[i64, string]")).is_ok());
        assert!(!s.attempt(&env, &t("string"), &t("i64")));
        assert!(s.constrain(&env, &t("Array<i64>"), &t("Array<[i64, null]>")).is_ok());
    }

    #[test]
    fn a_lower_bound_meets_a_later_upper_bound() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let (a, ta) = var("a");
        assert!(s.constrain(&env, &parsed(&env, "Array<i64>"), &ta).is_ok());
        assert!(s.constrain(&env, &ta, &parsed(&env, "Array<[i64, string]>")).is_ok());
        assert!(!s.attempt(&env, &ta, &parsed(&env, "Array<string>")));
        assert!(!a.is_bound());
        assert_eq!(shown(&a.cell_lower()), ["Array<i64>"]);
    }

    #[test]
    fn an_upper_bound_meets_a_later_lower_bound() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let (_, ta) = var("a");
        assert!(s.constrain(&env, &ta, &parsed(&env, "i64")).is_ok());
        assert!(!s.attempt(&env, &parsed(&env, "string"), &ta));
        assert!(s.constrain(&env, &parsed(&env, "i64"), &ta).is_ok());
    }

    #[test]
    fn a_bound_passes_along_an_edge_between_two_variables() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let ((_, ta), (b, tb)) = (var("a"), var("b"));
        assert!(s.constrain(&env, &parsed(&env, "i64"), &ta).is_ok());
        assert!(s.constrain(&env, &ta, &tb).is_ok());
        assert!(shown(&b.cell_lower()).contains(&"i64".to_string()));
        assert!(!s.attempt(&env, &tb, &parsed(&env, "string")));
        assert!(b.cell_constraints().is_empty());
        assert!(s.constrain(&env, &tb, &parsed(&env, "[i64, null]")).is_ok());
        // what arrives later passes too
        assert!(!s.attempt(&env, &parsed(&env, "string"), &ta));
        assert!(s.constrain(&env, &parsed(&env, "null"), &ta).is_ok());
    }

    #[test]
    fn a_function_takes_its_parameters_the_other_way() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let (a, ta) = var("a");
        let f = over(&env, "fn(x: 'a) -> 'a", &[("a", &ta)]);
        let g = parsed(&env, "fn(x: i64) -> [i64, string]");
        assert!(s.constrain(&env, &f, &g).is_ok());
        assert_eq!(shown(&a.cell_lower()), ["i64"]);
        assert_eq!(shown(&a.cell_constraints()), ["[i64, string]"]);
        let h = parsed(&env, "fn(x: string) -> string");
        assert!(!s.attempt(&env, &f, &h));
    }

    #[test]
    fn a_recursive_typedef_is_unfolded_to_its_variable() {
        let (env, _) = typed(&[("L<'a>", "[`C('a, L<'a>), `N]")], "null");
        let mut s = Solving::default();
        let (x, tx) = var("x");
        let list = over(&env, "L<'x>", &[("x", &tx)]);
        assert!(s.constrain(&env, &parsed(&env, "`C(i64, `C(i64, `N))"), &list).is_ok());
        assert_eq!(shown(&x.cell_lower()), ["i64"]);
        assert!(!s.attempt(&env, &tx, &parsed(&env, "string")));
        // and a list of the variable is a list of what holds it
        assert!(s.constrain(&env, &list, &parsed(&env, "L<[i64, null]>")).is_ok());
    }

    #[test]
    fn a_writable_reference_holds_one_type() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let (a, ta) = var("a");
        let r = over(&env, "&mut 'a", &[("a", &ta)]);
        assert!(s.constrain(&env, &r, &parsed(&env, "&mut i64")).is_ok());
        assert_eq!(shown(&a.cell_lower()), ["i64"]);
        assert_eq!(shown(&a.cell_constraints()), ["i64"]);
        let (_, tb) = var("b");
        let shared = over(&env, "&'b", &[("b", &tb)]);
        assert!(s.constrain(&env, &shared, &parsed(&env, "&[i64, string]")).is_ok());
        assert!(s.constrain(&env, &parsed(&env, "&i64"), &shared).is_ok());
    }
}
