//! How a typedef's body meets each of its parameters: what containment
//! of one application in another asks of the two arguments there. A
//! parameter under constructors alone is covariant (`T<a> ⊇ T<b>` asks `a
//! ⊇ b`), one under a function's argument contravariant, one under a
//! writable reference or an abstract type's parameter both ways, and one
//! the body never reaches asks nothing. Two applications of one
//! definition are then compared by their arguments, in a step each,
//! where their expansions would be walked once per application they
//! reach.
//!
//! The arguments stand for the expansions only where the walk over them
//! has no choice to make: a parameter inside a union two of whose members
//! may take one value (a bare parameter, two members of one constructor)
//! is left to the walk ([`Meets::walk`]), since which member takes a
//! value decides what its cells become, and so are one passed to a
//! bounded parameter, whose bound each expansion holds, and the payload
//! of core's `ErrChain`, where a chain of a chain is that chain. A
//! parameter left to the walk for a union's choice alone keeps its
//! questions ([`Meets::asked`]): arguments that answer them make the
//! application hold, though the walk may ask less.

use crate::{
    env::Env,
    stack::ensure_sufficient,
    typ::{
        Mutability, ResolvedRef, Type, TypeRef,
        expansive::{Named, definition, holds_formal},
    },
};
use ahash::AHashMap;
use arcstr::ArcStr;
use netidx_value::Typ;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use triomphe::Arc;

type DefRef = std::sync::Arc<ResolvedRef>;

/// What `T<a> ⊇ T<b>` asks of `a` and `b` at one parameter: any of the
/// four questions, or the walk.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub(crate) struct Meets(u8);

impl Meets {
    /// `a ⊇ b`: a covariant position.
    const HOLDS: u8 = 1;
    /// `a ⊇ b` with the sides swapped an odd number of times: the
    /// second half of an invariant position under a function's argument.
    const HOLDS_SWAPPED: u8 = 2;
    /// `b ⊇ a`: the second half of an invariant position.
    const HELD: u8 = 4;
    /// `b ⊇ a` with the sides swapped: a contravariant position.
    const HELD_SWAPPED: u8 = 8;
    const QUESTIONS: u8 = 15;
    /// Its positions are not all known.
    const WALK: u8 = 16;
    /// A position stands in a union with a choice.
    const CHOICE: u8 = 32;
    const TO_EXPANSIONS: u8 = Self::WALK | Self::CHOICE;

    const UNUSED: Self = Self(0);
    const COVARIANT: Self = Self(Self::HOLDS);
    const TO_WALK: Self = Self(Self::WALK);

    /// Only the expansions tell.
    pub(crate) fn walk(self) -> bool {
        self.0 & Self::TO_EXPANSIONS != 0
    }

    /// Every question a position asks, where every position is known:
    /// the arguments that answer them make the application hold.
    pub(crate) fn asked(self) -> Option<Self> {
        (self.0 & Self::WALK == 0).then_some(Self(self.0 & Self::QUESTIONS))
    }

    fn unknown(self) -> bool {
        self.0 & Self::WALK != 0
    }

    fn choice(self) -> Self {
        Self(self.0 | Self::CHOICE)
    }

    pub(crate) fn holds(self) -> bool {
        self.0 & Self::HOLDS != 0
    }

    pub(crate) fn held(self) -> bool {
        self.0 & Self::HELD != 0
    }

    pub(crate) fn holds_swapped(self) -> bool {
        self.0 & Self::HOLDS_SWAPPED != 0
    }

    pub(crate) fn held_swapped(self) -> bool {
        self.0 & Self::HELD_SWAPPED != 0
    }

    /// Each question `q` of `self` as question `f(q)`: a question is its
    /// bit's index, bit 0 the swap and bit 1 the direction.
    fn each(self, f: impl Fn(u8) -> u8) -> Self {
        let mut out = self.0 & Self::TO_EXPANSIONS;
        for q in (0..4).filter(|q| self.0 & (1 << q) != 0) {
            out |= 1 << f(q)
        }
        Self(out)
    }

    /// Under a function's argument.
    fn argument(self) -> Self {
        self.each(|q| q ^ 3)
    }

    /// Under a position held both ways.
    fn invariant(self) -> Self {
        Self(self.0 | self.each(|q| q ^ 2).0)
    }

    /// At a parameter its definition meets as `inner`.
    fn through(self, inner: Self) -> Self {
        if inner == Self::UNUSED {
            return inner;
        }
        let mut out = (self.0 | inner.0) & Self::TO_EXPANSIONS;
        for q in (0..4).filter(|q| inner.0 & (1 << q) != 0) {
            out |= self.each(|p| p ^ q).0 & Self::QUESTIONS
        }
        Self(out)
    }
}

/// The walk steps one definition's scan may take before every parameter
/// it reached is left to the walk.
const BUDGET: usize = 1 << 16;

struct Def {
    def: DefRef,
    formals: SmallVec<[ArcStr; 4]>,
    meets: SmallVec<[Meets; 4]>,
}

struct Scan<'a> {
    env: &'a Env,
    index: LPooled<AHashMap<usize, usize>>,
    defs: LPooled<Vec<Def>>,
    /// The definitions looked through on the way to here.
    active: LPooled<Vec<usize>>,
    unknown: bool,
    changed: bool,
    steps: usize,
}

impl<'a> Scan<'a> {
    fn intern(&mut self, def: &DefRef) -> usize {
        let key = def.def_key();
        if let Some(i) = self.index.get(&key) {
            return *i;
        }
        let i = self.defs.len();
        self.defs.push(Def {
            def: def.clone(),
            formals: def.params().iter().map(|(tv, _)| tv.name.clone()).collect(),
            meets: def.params().iter().map(|_| Meets::default()).collect(),
        });
        self.index.insert(key, i);
        self.changed = true;
        i
    }

    /// How `c` meets its parameters, as far as is known.
    fn meets_of(&mut self, c: &DefRef) -> SmallVec<[Meets; 4]> {
        match c.meets.get() {
            Some(m) => SmallVec::from_slice(m),
            None => {
                let i = self.intern(c);
                self.defs[i].meets.clone()
            }
        }
    }

    fn record(&mut self, d: usize, name: &ArcStr, at: Meets) {
        let Def { formals, meets, .. } = &mut self.defs[d];
        if let Some(i) = formals.iter().position(|f| f == name) {
            let joined = Meets(meets[i].0 | at.0);
            self.changed |= joined != meets[i];
            meets[i] = joined
        }
    }

    fn holds(&self, d: usize, t: &Type) -> bool {
        self.defs[d].formals.iter().any(|f| holds_formal(t, f))
    }

    /// `c`'s body over `args`, to look through; `None` where it is being
    /// looked through already.
    fn expansion(&mut self, c: &DefRef, args: &[Type]) -> Option<Type> {
        if self.active.contains(&c.def_key()) {
            return None;
        }
        let known = c.bindings(args)?;
        Some(c.typ().replace_tvars(&known))
    }

    fn walk(&mut self, d: usize, t: &Type, at: Meets) {
        self.steps += 1;
        if self.steps > BUDGET {
            return;
        }
        ensure_sufficient(|| match t {
            Type::Bottom
            | Type::Any
            | Type::Primitive(_)
            | Type::Hole
            | Type::Bound(_) => (),
            Type::TVar(tv) => match tv.binding() {
                Some(b) => self.walk(d, &b, at),
                None => self.record(d, &tv.name, at),
            },
            Type::Ref(tr) => self.apply(d, tr, at),
            Type::Set(ms) => {
                let at = match at.walk() || self.apart(d, ms) {
                    true => at,
                    false => at.choice(),
                };
                for m in ms.iter() {
                    self.walk(d, m, at)
                }
            }
            Type::Fn(f) => {
                let at = match f.quantifiers.is_empty() {
                    true => at,
                    false => Meets::TO_WALK,
                };
                for arg in f.args.iter() {
                    self.walk(d, &arg.typ, at.argument())
                }
                if let Some(t) = &f.vargs {
                    self.walk(d, t, at.argument())
                }
                self.walk(d, &f.rtype, at);
                self.walk(d, &f.throws, at)
            }
            Type::ByRef(Mutability::Mut, t) => self.walk(d, t, at.invariant()),
            Type::Abstract { id: _, params } => {
                for p in params.iter() {
                    self.walk(d, p, at.invariant())
                }
            }
            Type::App(..) => t.for_each_child(&mut |c| self.walk(d, c, Meets::TO_WALK)),
            Type::ByRef(Mutability::Shared, _)
            | Type::Error(_)
            | Type::Array(_)
            | Type::List(_)
            | Type::Tuple(_)
            | Type::Struct(_)
            | Type::Variant(..)
            | Type::Map(_) => t.for_each_child(&mut |c| self.walk(d, c, at)),
        })
    }

    /// An application in `d`'s body: each argument is met as the
    /// definition applied meets its parameter, and not at all where that
    /// body never reaches it. One it leaves to the walk is looked
    /// through, since the argument may be what the walk needed: `['x,
    /// null]` over a variant has two members apart; one being looked
    /// through already stands in a choice.
    fn apply(&mut self, d: usize, tr: &TypeRef, at: Meets) {
        if !tr.params.iter().any(|a| self.holds(d, a)) {
            return;
        }
        let to_walk = |scan: &mut Self| {
            tr.params.iter().for_each(|a| scan.walk(d, a, Meets::TO_WALK))
        };
        let c = match definition(tr, self.env) {
            Named::Def(c) if c.params().len() == tr.params.len() => c,
            Named::Def(_) | Named::Trait => return to_walk(self),
            Named::Unknown => {
                self.unknown = true;
                return to_walk(self);
            }
        };
        if self.env.is_core_errchain(&c) {
            return to_walk(self);
        }
        let inner = self.meets_of(&c);
        let passed = |scan: &Self, j: usize| scan.holds(d, &tr.params[j]);
        let bounded =
            (0..inner.len()).any(|j| c.params()[j].1.is_some() && passed(self, j));
        if bounded {
            return to_walk(self);
        }
        if at.unknown() {
            let reached =
                tr.params.iter().zip(inner.iter()).filter(|(_, m)| **m != Meets::UNUSED);
            return reached.for_each(|(a, _)| self.walk(d, a, Meets::TO_WALK));
        }
        let at = match (0..inner.len()).any(|j| inner[j].walk() && passed(self, j)) {
            false => at,
            true => match self.expansion(&c, &tr.params) {
                None => at.choice(),
                Some(body) => {
                    self.active.push(c.def_key());
                    self.walk(d, &body, at);
                    self.active.pop();
                    return;
                }
            },
        };
        for (arg, inner) in tr.params.iter().zip(inner.iter()) {
            self.walk(d, arg, at.through(*inner))
        }
    }

    /// Whether no value is taken by two members of the union `ms`, one of
    /// them holding a parameter of `d`: then each member of one
    /// application is met by the same member of the other and no other.
    fn apart(&mut self, d: usize, ms: &[Type]) -> bool {
        let mut heads: SmallVec<[(Type, bool); 8]> = SmallVec::new();
        for m in ms {
            if !self.heads(d, m, &mut heads) {
                return false;
            }
        }
        if !heads.iter().any(|(_, holds)| *holds) {
            return true;
        }
        heads.iter().enumerate().all(|(i, (a, holds_a))| {
            heads[i + 1..]
                .iter()
                .all(|(b, holds_b)| !(*holds_a || *holds_b) || !share(a, b))
        })
    }

    /// The members `m` stands for in a union, each with whether it holds
    /// a parameter of `d`: a name is its body's members. False where one
    /// is not known.
    fn heads(
        &mut self,
        d: usize,
        m: &Type,
        out: &mut SmallVec<[(Type, bool); 8]>,
    ) -> bool {
        self.steps += 1;
        if self.steps > BUDGET {
            return false;
        }
        ensure_sufficient(|| match m {
            Type::Set(ms) => ms.iter().all(|m| self.heads(d, m, out)),
            Type::TVar(tv) => match tv.binding() {
                Some(b) => self.heads(d, &b, out),
                None => false,
            },
            Type::Ref(tr) => {
                let c = match definition(tr, self.env) {
                    Named::Def(c) => c,
                    Named::Trait => return false,
                    Named::Unknown => {
                        self.unknown = true;
                        return false;
                    }
                };
                let holds = tr.params.iter().any(|a| self.holds(d, a));
                match c.typ() {
                    Type::Set(_) | Type::Ref(_) | Type::TVar(_) => {
                        let Some(body) = self.expansion(&c, &tr.params) else {
                            return false;
                        };
                        self.active.push(c.def_key());
                        let known = self.heads(d, &body, out);
                        self.active.pop();
                        known
                    }
                    body => {
                        out.push((body.clone(), holds));
                        true
                    }
                }
            }
            m => {
                out.push((m.clone(), self.holds(d, m)));
                true
            }
        })
    }
}

/// Whether one value may be of both `a` and `b`, union members that are
/// neither unions, names nor variables, by their outermost constructors.
fn share(a: &Type, b: &Type) -> bool {
    let bit = |t: &Type| match t {
        Type::Array(_) => Some(Typ::Array),
        Type::Map(_) => Some(Typ::Map),
        Type::Error(_) => Some(Typ::Error),
        _ => None,
    };
    match (a, b) {
        (Type::Bottom, _) | (_, Type::Bottom) => false,
        (Type::Any | Type::Hole | Type::Bound(_) | Type::App(..), _)
        | (_, Type::Any | Type::Hole | Type::Bound(_) | Type::App(..)) => true,
        (Type::Primitive(p), Type::Primitive(q)) => p.intersects(*q),
        (Type::Primitive(p), t) | (t, Type::Primitive(p)) => {
            bit(t).is_some_and(|b| p.contains(b))
        }
        (a, b) => a.shares_constructor(b),
    }
}

/// How `def`'s body meets each of its parameters; `None` while a name it
/// reaches is not defined. Kept by the definition, and by each one its
/// scan reached.
pub(crate) fn meets(env: &Env, def: &DefRef) -> Option<Arc<[Meets]>> {
    if let Some(m) = def.meets.get() {
        return Some(m.clone());
    }
    let mut scan = Scan {
        env,
        index: LPooled::take(),
        defs: LPooled::take(),
        active: LPooled::take(),
        unknown: false,
        changed: false,
        steps: 0,
    };
    scan.intern(def);
    while scan.changed && scan.steps <= BUDGET {
        scan.changed = false;
        let mut d = 0;
        while d < scan.defs.len() {
            let Def { def, .. } = &scan.defs[d];
            let (body, at) = match env.is_core_errchain(def) {
                true => (def.typ().clone(), Meets::TO_WALK),
                false => (def.typ().clone(), Meets::COVARIANT),
            };
            scan.walk(d, &body, at);
            d += 1;
        }
    }
    if scan.unknown {
        return None;
    }
    if scan.steps > BUDGET {
        let all = Arc::from_iter(def.params().iter().map(|_| Meets::TO_WALK));
        return Some(def.meets.get_or_init(|| all).clone());
    }
    for d in scan.defs.iter() {
        let _ = d.def.meets.set(Arc::from_iter(d.meets.iter().copied()));
    }
    def.meets.get().cloned()
}

#[cfg(test)]
mod test {
    use super::{Meets, meets};
    use crate::{env::Env, typ::Type};

    fn of(defs: &[(&str, &str)], name: &str) -> Vec<Meets> {
        let (env, t) = Env::typed_at_root(defs, name).expect("defined");
        let Type::Ref(tr) = &t else { panic!("{name} is no name") };
        let def = tr.resolve_peek(&env).expect("resolved");
        meets(&env, &def).expect("known").to_vec()
    }

    const CO: Meets = Meets::COVARIANT;
    const CONTRA: Meets = Meets(Meets::HELD_SWAPPED);
    const UNUSED: Meets = Meets::UNUSED;

    #[test]
    fn a_parameter_is_met_as_its_positions_are() {
        let one = |body: &str| of(&[("T<'a>", body)], "T<i64>")[0];
        assert_eq!(one("[`Leaf('a), `Node(Array<T<'a>>)]"), CO);
        assert_eq!(one("{v: 'a, next: [T<'a>, null]}"), CO);
        assert_eq!(one("[`Nil, `Cons(fn(x: 'a) -> i64, T<'a>)]"), CONTRA);
        assert_eq!(one("fn(k: fn(x: 'a) -> i64) -> i64"), CO);
        assert_eq!(one("[`N, `C(T<'a>)]"), UNUSED);
        assert_eq!(
            one("(fn(x: 'a) -> i64, 'a)"),
            Meets(Meets::HOLDS | Meets::HELD_SWAPPED)
        );
        assert_eq!(one("&mut 'a"), Meets(Meets::HOLDS | Meets::HELD));
        assert_eq!(
            one("fn(r: &mut 'a) -> i64"),
            Meets(Meets::HELD_SWAPPED | Meets::HOLDS_SWAPPED)
        );
        assert_eq!(one("Map<'a, Array<'a>>"), CO);
    }

    #[test]
    fn a_union_with_a_choice_is_left_to_the_walk() {
        let one = |body: &str| of(&[("T<'a>", body)], "T<i64>")[0];
        assert!(one("['a, null]").walk());
        assert!(one("[Array<'a>, Array<i64>]").walk());
        assert_eq!(one("[`A('a), `A('a, i64)]"), CO);
        assert!(one("[`A('a), `B(['a, null])]").walk());
        assert_eq!(one("[('a, i64), ('a, i64, i64), i64]"), CO);
        assert_eq!(one("[{a: 'a}, {b: 'a}]"), CO);
        assert_eq!(one("[Array<i64>, array, `A('a)]"), CO);
    }

    #[test]
    fn a_parameter_in_a_choice_keeps_its_questions() {
        let one = |body: &str| of(&[("T<'a>", body)], "T<i64>")[0];
        let tree = one("['a, `Node(Array<T<'a>>)]");
        assert!(tree.walk());
        assert_eq!(tree.asked(), Some(CO));
        assert_eq!(one("['a, null]").asked(), Some(CO));
        assert_eq!(
            one("[fn(x: 'a) -> i64, `N(T<'a>), fn(x: i64) -> i64]").asked(),
            Some(CONTRA)
        );
        assert_eq!(
            one("[&mut 'a, `N(Array<T<'a>>), &mut i64]").asked(),
            Some(Meets(Meets::HOLDS | Meets::HELD))
        );
        assert_eq!(one("[`A('a), `N(T<'a>)]").asked(), Some(CO));
        let defs = [("D<'a0, 'a1>", "[`U0(D<'a1, 'a0>), 'a1, `Uz(fn(x: 'a0) -> i64)]")];
        let both = Some(Meets(Meets::HOLDS | Meets::HELD_SWAPPED));
        assert_eq!(
            of(&defs, "D<i64, i64>").iter().map(|m| m.asked()).collect::<Vec<_>>(),
            [both; 2]
        );
    }

    #[test]
    fn a_parameter_only_the_recursion_passes_on_is_unused() {
        let defs = [(
            "D<'a0, 'a1, 'a2>",
            "[Error<D<'a1, 'a2, 'a0>>, Error<D<'a1, 'a0, 'a2>>, `Y(D<'a1, 'a2, 'a0>)]",
        )];
        assert_eq!(of(&defs, "D<i64, i64, i64>"), [UNUSED; 3]);
        let defs = [("D<'a0, 'a1>", "[Error<D<'a1, 'a0>>, Error<D<'a0, 'a0>>, `P('a0)]")];
        let d = of(&defs, "D<i64, i64>");
        assert!(d[0].walk() && d[1].walk());
        let defs = [("D<'a0, 'a1>", "[Error<D<'a0, 'a1>>, Error<D<'a0, 'a0>>, `P('a0)]")];
        let d = of(&defs, "D<i64, i64>");
        assert!(d[0].walk());
        assert_eq!(d[1], UNUSED);
    }

    #[test]
    fn a_permuting_recursion_meets_every_parameter() {
        let defs = [(
            "D<'a0, 'a1, 'a2>",
            "[`N, `X(fn(x: 'a0) -> i64, D<'a1, 'a0, 'a2>), `Y(D<'a1, 'a2, 'a0>)]",
        )];
        assert_eq!(of(&defs, "D<i64, i64, i64>"), [CONTRA; 3]);
        let defs = [("D<'a0, 'a1>", "[`R(D<'a1, 'a0>), `U('a0)]")];
        assert_eq!(of(&defs, "D<i64, i64>"), [CO; 2]);
    }

    #[test]
    fn an_application_is_met_through_its_definition() {
        let defs = [
            ("T<'p>", "[`Nil, `Cons(fn(x: 'p) -> i64, T<'p>)]"),
            ("W<'p>", "(T<'p>, fn(k: T<'p>) -> i64)"),
        ];
        assert_eq!(of(&defs, "W<i64>"), [Meets(Meets::HELD_SWAPPED | Meets::HOLDS)]);
        let defs = [
            ("A<'p>", "[`Nil, `A(fn(x: 'p) -> i64, B<'p>)]"),
            ("B<'p>", "[`Nil, `B(A<'p>)]"),
        ];
        assert_eq!(of(&defs, "B<i64>"), [CONTRA]);
        assert_eq!(of(&defs, "A<i64>"), [CONTRA]);
    }

    #[test]
    fn an_alias_left_to_the_walk_is_looked_through() {
        let defs = [("O<'t>", "['t, null]"), ("W<'a>", "{v: 'a, next: O<W<'a>>}")];
        assert_eq!(of(&defs, "W<i64>"), [CO]);
        assert!(of(&defs, "O<i64>")[0].walk());
        let defs = [("O<'t>", "['t, null]"), ("W<'a>", "{v: O<'a>, next: O<W<'a>>}")];
        assert!(of(&defs, "W<i64>")[0].walk());
        let defs = [("O<'t>", "['t, null]"), ("L<'a>", "[`Cons('a, L<'a>), O<i64>]")];
        assert_eq!(of(&defs, "L<i64>"), [CO]);
        let defs =
            [("O<'t>", "['t, null]"), ("L<'a>", "[`Cons('a, L<'a>), O<`Cons(i64, 'a)>]")];
        assert!(of(&defs, "L<i64>")[0].walk());
    }
}
