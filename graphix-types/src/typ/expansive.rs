//! Whether a typedef is expansive (Kennedy and Pierce): whether it
//! unfolds to infinitely many distinct applications. A parameter of one
//! definition flows into a parameter of a definition its body applies,
//! plainly (`'a` in `C<'a>`) or under a constructor (`C<Array<'a>>`).
//! A definition's unfolding is finite unless a parameter's flow returns
//! to it through a flow under a constructor (`type N<'a> = [`Nil,
//! `Cons('a, N<Array<'a>>)]`). An abstract type with a representation
//! is a definition too: a walk may look into it.
//!
//! An alias applied to a parameter in an argument (`T<Opt<'a>>` over
//! `type Opt<'a> = ['a, null]`) wraps nothing, yet as written it nests
//! at every unfolding. So a definition stores such an argument as the
//! alias's expansion ([`through_aliases`]), and every walk unfolds the
//! union written out. An alias that cannot be looked through where it
//! is applied stays as written and counts as a constructor.

use crate::{
    env::Env,
    expr::ModPath,
    stack::ensure_sufficient,
    typ::{AbstractId, AppKey, ResolvedRef, TVar, Type, TypeRef},
};
use ahash::AHashMap;
use arcstr::ArcStr;
use poolshark::local::LPooled;
use triomphe::Arc;

/// What [`expansive`] found of a definition.
pub(crate) enum Expansive {
    Regular,
    /// The parameter whose flow returns to it under a constructor.
    Grows(TVar),
    /// The parameter whose flow returns to it through nothing but
    /// applications of the alias named, which no walk looks through.
    Through(TVar, ModPath, Blind),
    /// A name the body reaches is not defined yet.
    Unknown,
}

/// Why an alias applied to a parameter is not looked through.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Blind {
    /// It is not defined above the definition that applies it.
    Late,
    /// A parameter of it has a bound, which only its application holds.
    Bounded,
}

/// What a flow passes through.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Wrap {
    Bare,
    Ctor,
    /// The alias at this index of [`Flow::blind`].
    Alias(usize),
}

impl Wrap {
    /// What a cycle through `self` and `other` passes through.
    fn and(self, other: Self) -> Self {
        match (self, other) {
            (Wrap::Ctor, _) | (_, Wrap::Ctor) => Wrap::Ctor,
            (Wrap::Bare, w) | (w, _) => w,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Key {
    Def(usize),
    Abstract(AbstractId),
}

type Params = Arc<[(TVar, Option<Type>)]>;
type DefRef = std::sync::Arc<ResolvedRef>;

/// A definition a walk reached. Its nodes are its parameters, from
/// `first`.
struct Def {
    first: usize,
    params: Params,
    body: Type,
    /// Where its verdict is kept; an abstract type's representation has
    /// none.
    named: Option<DefRef>,
}

struct Flow<'a> {
    env: &'a Env,
    index: LPooled<AHashMap<Key, usize>>,
    defs: LPooled<Vec<Def>>,
    /// Per node, `(to, through what)`.
    flows: LPooled<Vec<LPooled<Vec<(usize, Wrap)>>>>,
    blind: LPooled<Vec<(ModPath, Blind)>>,
    unknown: bool,
}

#[cfg(test)]
thread_local! {
    /// Definitions scanned, for the tests' count of a check's work.
    static SCANS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    /// Alias bodies looked through.
    static LOOKS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

impl<'a> Flow<'a> {
    /// The walk from `def`, whatever is known of it already.
    fn from(env: &'a Env, def: &DefRef) -> Self {
        let mut flow = Flow {
            env,
            index: LPooled::take(),
            defs: LPooled::take(),
            flows: LPooled::take(),
            blind: LPooled::take(),
            unknown: false,
        };
        flow.intern(Key::Def(def.def_key()), def.params(), def.typ(), Some(def));
        let mut d = 0;
        while d < flow.defs.len() {
            flow.scan(d);
            d += 1;
        }
        flow
    }

    fn intern(
        &mut self,
        key: Key,
        params: &Params,
        body: &Type,
        named: Option<&DefRef>,
    ) -> usize {
        if let Some(i) = self.index.get(&key) {
            return *i;
        }
        let i = self.defs.len();
        self.defs.push(Def {
            first: self.flows.len(),
            params: params.clone(),
            body: body.clone(),
            named: named.cloned(),
        });
        self.flows.extend(params.iter().map(|_| LPooled::take()));
        self.index.insert(key, i);
        i
    }

    fn scan(&mut self, d: usize) {
        #[cfg(test)]
        SCANS.with(|n| n.set(n.get() + 1));
        let Def { params, body, .. } = &self.defs[d];
        let (params, body) = (params.clone(), body.clone());
        let formals: LPooled<Vec<ArcStr>> =
            params.iter().map(|(f, _)| f.name.clone()).collect();
        self.walk(d, &formals, &body);
    }

    /// Record the flows of `d`'s formals into the parameters of `c`,
    /// applied to `args`. A formal standing bare as a union's member flows
    /// plainly: a union flattens, so `T<['a, string]>` applied to itself
    /// is `T<['a, string]>` again.
    fn apply(&mut self, d: usize, formals: &[ArcStr], c: usize, args: &[Type]) {
        let first = self.defs[c].first;
        for (j, arg) in args.iter().enumerate() {
            let members = match arg {
                Type::Set(ms) => &ms[..],
                arg => std::slice::from_ref(arg),
            };
            for m in members {
                let bare = bare_formal(m);
                let mut wrap = None;
                for (i, f) in formals.iter().enumerate() {
                    let through = match &bare {
                        Some(b) if b == f => Wrap::Bare,
                        _ if holds_formal(m, f) => {
                            *wrap.get_or_insert_with(|| self.wrap(formals, m))
                        }
                        _ => continue,
                    };
                    let from = self.defs[d].first + i;
                    self.flows[from].push((first + j, through));
                }
            }
        }
    }

    /// What the argument member `m`, which holds a formal and is none
    /// bare, wraps it in. A definition stores an alias it looked through
    /// as its expansion, so one that stands as written and wraps nothing
    /// was not defined where it was applied.
    fn wrap(&mut self, formals: &[ArcStr], m: &Type) -> Wrap {
        let Type::Ref(tr) = m else { return Wrap::Ctor };
        let (name, why) = match Aliases::new(self.env, formals).look(m) {
            Look::Wraps => return Wrap::Ctor,
            Look::Bare(_) => (tr.name.clone(), Blind::Late),
            Look::Blind(name, why) => (name, why),
        };
        self.blind.push((name, why));
        Wrap::Alias(self.blind.len() - 1)
    }

    fn walk(&mut self, d: usize, formals: &[ArcStr], t: &Type) {
        ensure_sufficient(|| match t {
            Type::TVar(tv) => {
                if let Some(b) = tv.binding() {
                    self.walk(d, formals, &b)
                }
            }
            Type::Ref(tr) => {
                if passes_formal(&tr.params, formals) {
                    match definition(tr, self.env) {
                        Named::Def(c) if c.regular.get().is_some() => (),
                        Named::Def(c) => {
                            let key = Key::Def(c.def_key());
                            let c = self.intern(key, c.params(), c.typ(), Some(&c));
                            self.apply(d, formals, c, &tr.params)
                        }
                        Named::Trait => (),
                        Named::Unknown => self.unknown = true,
                    }
                }
                for arg in tr.params.iter() {
                    self.walk(d, formals, arg)
                }
            }
            Type::Abstract { id, params } => {
                if passes_formal(params, formals)
                    && let Some(r) = self.env.abstract_reps.get(id)
                {
                    let (rp, rep) = (r.params.clone(), r.rep.clone());
                    let c = self.intern(Key::Abstract(*id), &rp, &rep, None);
                    self.apply(d, formals, c, params)
                }
                for p in params.iter() {
                    self.walk(d, formals, p)
                }
            }
            t => t.for_each_child(&mut |c| self.walk(d, formals, c)),
        })
    }

    /// Per node, what the cycles of flows it lies on pass through
    /// (Tarjan's strongly connected components).
    fn growing(&self) -> LPooled<Vec<Wrap>> {
        let n = self.flows.len();
        let mut order: LPooled<Vec<usize>> = LPooled::take();
        order.resize(n, usize::MAX);
        let mut low: LPooled<Vec<usize>> = LPooled::take();
        low.resize(n, 0);
        let mut comp: LPooled<Vec<usize>> = LPooled::take();
        comp.resize(n, usize::MAX);
        let mut stack: LPooled<Vec<usize>> = LPooled::take();
        let mut work: LPooled<Vec<(usize, usize)>> = LPooled::take();
        let mut next = 0;
        for root in 0..n {
            if order[root] != usize::MAX {
                continue;
            }
            work.push((root, 0));
            while let Some((v, k)) = work.pop() {
                if k == 0 {
                    order[v] = next;
                    low[v] = next;
                    next += 1;
                    stack.push(v);
                }
                if let Some((w, _)) = self.flows[v].get(k).copied() {
                    work.push((v, k + 1));
                    if order[w] == usize::MAX {
                        work.push((w, 0));
                    } else if comp[w] == usize::MAX {
                        low[v] = low[v].min(order[w]);
                    }
                    continue;
                }
                if low[v] == order[v] {
                    while let Some(w) = stack.pop() {
                        comp[w] = v;
                        if w == v {
                            break;
                        }
                    }
                }
                if let Some((u, _)) = work.last() {
                    low[*u] = low[*u].min(low[v]);
                }
            }
        }
        let mut grows: LPooled<Vec<Wrap>> = LPooled::take();
        grows.resize(n, Wrap::Bare);
        for v in 0..n {
            for (w, through) in self.flows[v].iter() {
                if comp[*w] == comp[v] {
                    grows[comp[v]] = grows[comp[v]].and(*through)
                }
            }
        }
        (0..n).map(|v| grows[comp[v]]).collect()
    }
}

/// What a typedef applied to a parameter, standing as an argument or a
/// member of one, is to the flow of that parameter.
#[derive(Clone)]
enum Look {
    /// Its expansion, where every parameter stands bare in a union or not
    /// at all: the application adds members and wraps nothing.
    Bare(Type),
    /// Its expansion holds a parameter under a constructor.
    Wraps,
    /// The name that hides its expansion, and why.
    Blind(ModPath, Blind),
}

/// The alias applications over `formals` looked through so far.
struct Aliases<'a> {
    env: &'a Env,
    formals: &'a [ArcStr],
    /// The definitions being expanded: one met again is recursive.
    active: LPooled<Vec<usize>>,
    /// Each application looked through, held so its key stays its own.
    seen: LPooled<AHashMap<AppKey, (Type, Look)>>,
}

impl<'a> Aliases<'a> {
    fn new(env: &'a Env, formals: &'a [ArcStr]) -> Self {
        Aliases { env, formals, active: LPooled::take(), seen: LPooled::take() }
    }

    fn holds(&self, t: &Type) -> bool {
        self.formals.iter().any(|f| holds_formal(t, f))
    }

    fn look(&mut self, t: &Type) -> Look {
        ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => self.look(&b),
                None => Look::Bare(t.clone()),
            },
            Type::Set(ms) => {
                let mut members: LPooled<Vec<Type>> = LPooled::take();
                let mut blind = None;
                for m in ms.iter() {
                    match self.look(m) {
                        Look::Bare(t) => members.push(t),
                        Look::Wraps => return Look::Wraps,
                        hidden => blind = blind.or(Some(hidden)),
                    }
                }
                blind.unwrap_or_else(|| Look::Bare(Type::flatten_set(members.drain(..))))
            }
            t if !self.holds(t) => Look::Bare(t.clone()),
            Type::Ref(tr) => self.expand(t, tr),
            _ => Look::Wraps,
        })
    }

    fn expand(&mut self, t: &Type, tr: &TypeRef) -> Look {
        let def = match definition(tr, self.env) {
            Named::Def(def) => def,
            Named::Trait => return Look::Wraps,
            Named::Unknown => return Look::Blind(tr.name.clone(), Blind::Late),
        };
        let key = def.def_key();
        if self.active.contains(&key) {
            return Look::Wraps;
        }
        if def.params().iter().any(|(_, bound)| bound.is_some()) {
            return Look::Blind(tr.name.clone(), Blind::Bounded);
        }
        let app = tr.app_key(key);
        if let Some((_, found)) = self.seen.get(&app) {
            return found.clone();
        }
        let found = self.expansion(&def, &tr.params);
        self.seen.insert(app, (t.clone(), found.clone()));
        found
    }

    /// `def`'s body over `args`, each looked through before the body is:
    /// an alias in an argument is not the body naming itself.
    fn expansion(&mut self, def: &ResolvedRef, args: &[Type]) -> Look {
        #[cfg(test)]
        LOOKS.with(|n| n.set(n.get() + 1));
        let mut bare: LPooled<Vec<Type>> = LPooled::take();
        for arg in args {
            match self.look(arg) {
                Look::Bare(t) => bare.push(t),
                hidden => return hidden,
            }
        }
        let Some(known) = def.bindings(&bare) else { return Look::Wraps };
        self.active.push(def.def_key());
        let found = self.look(&def.typ().replace_tvars(&known));
        self.active.pop();
        found
    }

    /// Whether an application of `tr` in the body of the typedef `name`
    /// may unfold back to that body: `tr` names it, or something not
    /// defined yet, or a definition no walk has closed.
    fn may_return(&self, name: &str, tr: &TypeRef) -> bool {
        match definition(tr, self.env) {
            Named::Def(c) => c.regular.get().is_none() || tr.name == [name],
            Named::Trait => false,
            Named::Unknown => true,
        }
    }

    fn arg(&mut self, name: &str, arg: &Type) -> Option<Type> {
        let mut member = |m: &Type| match m {
            Type::Ref(_) if self.holds(m) => match self.look(m) {
                Look::Bare(t) => Some(t),
                Look::Wraps | Look::Blind(..) => self.stored(name, m),
            },
            m => self.stored(name, m),
        };
        match arg {
            Type::Set(ms) => Type::cow_slice(ms, member)
                .map(|ms| Type::flatten_set(ms.iter().cloned())),
            arg => member(arg),
        }
    }

    fn stored(&mut self, name: &str, t: &Type) -> Option<Type> {
        ensure_sufficient(|| match t {
            Type::Ref(tr)
                if passes_formal(&tr.params, self.formals)
                    && self.may_return(name, tr) =>
            {
                Type::cow_slice(&tr.params, |a| self.arg(name, a))
                    .map(|params| Type::Ref(Arc::new(tr.with_params(params))))
            }
            Type::Abstract { id, params }
                if passes_formal(params, self.formals)
                    && self.env.abstract_reps.get(id).is_some() =>
            {
                Type::cow_slice(params, |a| self.arg(name, a))
                    .map(|params| Type::Abstract { id: *id, params })
            }
            t => t.cow_children(&mut |c| self.stored(name, c)),
        })
    }
}

/// `body`, the body or the representation of the typedef `name` over
/// `params`, as its definition stores it, when that is not as written:
/// in each application that may unfold back to the body, an argument
/// that applies an alias to a parameter is the alias's expansion, where
/// that leaves every parameter bare in a union. The alias is one defined
/// already, so the definition's own unfolding never nests it.
pub(crate) fn through_aliases(
    env: &Env,
    name: &str,
    params: &Params,
    body: &Type,
) -> Option<Type> {
    if params.is_empty() {
        return None;
    }
    let formals: LPooled<Vec<ArcStr>> =
        params.iter().map(|(f, _)| f.name.clone()).collect();
    Aliases::new(env, &formals).stored(name, body)
}

enum Named {
    Def(DefRef),
    Trait,
    Unknown,
}

// CR claude for claude: [bug/medium] A module's typedef that shadows an import
// which an earlier typedef of the module already named escapes this check, and a
// walk over the type then never ends. With `type M<'a> = Array<'a>` in the parent,
// `use super::M; type N<'a> = [`Nil, `Cons('a, M<'a>)]; type M<'a> = N<Array<'a>>`
// is accepted: N's walk peeks M as the import, finds no cycle and settles N
// regular, the local M's walk skips the settled N, and N<i64> unfolds to
// N<Array<i64>> and on (`--check` killed at 30 s, on base-r20 too; without the
// `use` M is refused). The alias look-through reads the same peek:
// `type T<'a> = [`N, `C(Opt<'a>, T<Opt<'a>>)]` above a local Opt that shadows an
// imported one stores T's argument as the imported Opt's body, while the Opt
// beside it means the local one (T<i64> then holds what neither Opt alone
// allows). A block refuses a name its later statement declares (Unseen::Later);
// a module scope has no such rule for a name it takes from an import and then
// declares. Either refuse the later typedef (or the earlier use), or resolve a
// module's typedef names once all of them are declared. probes:
// ~/ptmp/cr29/scratch/r20/20c/ag/shadow-hang/, shadow-alias/
// (typedef-import-shadow-01)
fn definition(tr: &TypeRef, env: &Env) -> Named {
    if let Some(d) = tr.resolve_peek(env) {
        return Named::Def(d);
    }
    match tr.trait_id().is_some()
        || matches!(
            env.resolve_type_name(&tr.scope, &tr.name),
            Ok(Some(crate::env::TypeName::Trait(_)))
        ) {
        true => Named::Trait,
        false => Named::Unknown,
    }
}

/// The formal `arg` is, when it is one bare.
fn bare_formal(arg: &Type) -> Option<ArcStr> {
    match arg {
        Type::TVar(tv) => match tv.binding() {
            Some(b) => bare_formal(&b),
            None => Some(tv.name.clone()),
        },
        _ => None,
    }
}

/// Whether `args` hold one of `formals`: only then does the application
/// take a flow, and only then does what it names matter.
fn passes_formal(args: &[Type], formals: &[ArcStr]) -> bool {
    args.iter().any(|a| formals.iter().any(|f| holds_formal(a, f)))
}

fn holds_formal(t: &Type, f: &ArcStr) -> bool {
    ensure_sufficient(|| match t {
        Type::TVar(tv) => match tv.binding() {
            Some(b) => holds_formal(&b, f),
            None => &tv.name == f,
        },
        t => {
            let mut found = false;
            t.for_each_child(&mut |c| found = found || holds_formal(c, f));
            found
        }
    })
}

/// Whether `def` is expansive: one of its parameters flows back into
/// itself through a flow under a constructor, in its own body or through
/// the definitions it reaches. A walk that meets no undefined name
/// settles every definition it reached whose parameters do not grow: each
/// one's own walk is a part of this one.
// XCR claude for claude: [bug/medium] An alias applied to a recursive type's parameter
// is judged a growth though it grows nothing. `type Opt<'a> = ['a, null]; type T<'a> =
// [`N, `C('a, T<Opt<'a>>)]` is refused ("grows its parameter 'a .. wrapped in a
// constructor") while `T<['a, null]>` written out is accepted, and `T<I<'a>>` over
// `type I<'b> = 'b` is refused too. The flow test counts any application as a
// constructor, and the checker's walks compare applications as written: expanding
// aliases here alone let `A<I<'a>>` through, and the check then hung (lane 19f, D24).
// Eric ruled (2026-11-10, D29) (b): a typedef names a common or a large union, and
// writing a 50-member union out to pass this is ugly and error prone, so the checker
// takes the cost. The flow test looks through an alias application to its body (the
// bare parameter, or a union adding members to it, is a plain flow; a constructor in
// the body still grows), and the walks that unfold a recursive type compare alias
// applications expanded, so `T<Opt<Opt<i64>>>` is `T<Opt<i64>>` and the unfolding
// ends. The refusal stays for real growth (`N<Array<'a>>`). Flip the skill's
// `U<I<'a>>` case in ide/skills/graphix-lang/generics.md and its prose, and the book
// (udt/recursive.md). probes: ~/ptmp/cr29/d29/{alias,written,ident}.gx
// (alias-growth-01)
// 2026-11-10 claude: Built Eric's (b) at the definition, so no walk changed:
// Env::deftype stores an argument that applies an alias to a parameter as the alias's
// expansion (expansive::through_aliases) in every application that may unfold back to
// the definition, where the expansion leaves each parameter bare in a union or absent;
// T<Opt<'a>> is stored as T<['a, null]>, which every walk already ends on. An alias
// with a constructor around the parameter stays as written and grows. One defined
// below the type that applies it, or one that bounds a parameter, is not looked
// through, counts as a constructor, and a cycle through it is refused naming the alias
// (Expansive::Through) and the fix. An application of a closed (regular) definition
// keeps its arguments as written, so no unfolding accepted before prints differently.
// Pins that fail without it:
// typ::expansive::test::{an_alias_applied_to_a_parameter_is_its_expansion,
// parameters_passed_on_are_regular, an_alias_met_again_is_looked_through_once (8191
// looks at depth 12 without the memo)},
// lang::types::{alias_over_a_recursive_parameter, aliases_over_a_recursive_parameter
// (a 50-member alias, an alias of an alias, the identity alias, a second definition on
// the way back), alias_over_an_abstract_parameter}; refusals
// lang::types::{alias_below_its_recursive_application_refused,
// bounded_alias_over_a_recursive_parameter_refused} (twin bounded_alias_written_out)
// and alias_over_a_constructor_grows; generics.md cases; the book's udt/recursive.md
// fences. Judged here: the order rule (define the alias first) in place of a recheck
// when the alias arrives, since a use between the two definitions would walk a nesting
// unfolding. Found and not fixed: typedef-import-shadow-01. probes:
// ~/ptmp/cr29/scratch/r20/20c/ag/
pub(crate) fn expansive(env: &Env, def: &DefRef) -> Expansive {
    if def.params().is_empty() || def.regular.get().is_some() {
        return Expansive::Regular;
    }
    let flow = Flow::from(env, def);
    let growing = flow.growing();
    let grows = |d: &Def| {
        growing[d.first..d.first + d.params.len()].iter().any(|g| *g != Wrap::Bare)
    };
    let own = def.params().iter().zip(&growing[..]);
    if let Some(((tv, _), _)) = own.clone().find(|(_, g)| **g == Wrap::Ctor) {
        return Expansive::Grows(tv.clone());
    }
    for ((tv, _), g) in own {
        if let Wrap::Alias(i) = g {
            let (alias, why) = &flow.blind[*i];
            return Expansive::Through(tv.clone(), alias.clone(), *why);
        }
    }
    if flow.unknown {
        return Expansive::Unknown;
    }
    for d in flow.defs.iter().filter(|d| !grows(d)) {
        if let Some(named) = &d.named {
            let _ = named.regular.set(());
        }
    }
    Expansive::Regular
}

#[cfg(test)]
mod test {
    use crate::env::Env;

    /// The refusal of the typedefs `defs`, defined in order, if any.
    fn refusal(defs: &[(&str, &str)]) -> Option<String> {
        Env::typed_at_root(defs, "i64").err().map(|e| e.to_string())
    }

    /// A forward chain of parametric typedefs, each naming the next
    /// before it is defined, or a cycle of them, checked again in order
    /// as the end of a check does, scans each definition a bounded number
    /// of times: the first walk settles every definition it reached.
    #[test]
    fn a_walk_settles_every_definition_it_finds_regular() {
        use crate::{expr::ModPath, typ::Unguarded};
        const TYPES: usize = 200;
        for last in ["[`End, `C('a)]", "[`End, `C('a, A0<'a>)]"] {
            let defs: Vec<(String, String)> = (0..TYPES)
                .map(|i| (format!("A{i}<'a>"), format!("[`N, `C('a, A{}<'a>)]", i + 1)))
                .chain([(format!("A{TYPES}<'a>"), last.to_string())])
                .collect();
            let defs: Vec<(&str, &str)> =
                defs.iter().map(|(n, b)| (n.as_str(), b.as_str())).collect();
            let (env, _) = Env::typed_at_root(&defs, "i64").expect("regular");
            super::SCANS.with(|n| n.set(0));
            for i in 0..=TYPES {
                let u = env.check_typedef(&ModPath::root(), &format!("A{i}"));
                assert_eq!(u.expect("regular"), Unguarded::Guarded, "{last}");
            }
            let scans = super::SCANS.with(|n| n.get());
            assert!(scans <= TYPES + 1, "{last}: {scans} scans of {TYPES} definitions");
        }
    }

    #[test]
    fn a_parameter_wrapped_on_its_way_back_is_refused() {
        let grows = |defs: &[(&str, &str)], name: &str, param: &str| {
            let e = refusal(defs).expect("refused");
            let want = format!("recursive type {name} grows its parameter {param}");
            assert!(e.starts_with(&want), "{e}");
        };
        grows(&[("N<'a>", "[`Nil, `Cons('a, N<Array<'a>>)]")], "N", "'a");
        grows(&[("P<'a>", "[`Leaf('a), `Node(P<('a, 'a)>)]")], "P", "'a");
        grows(&[("T<'a>", "[`N, `C(T<T<'a>>)]")], "T", "'a");
        grows(&[("F<'a>", "[`N, `C(F<fn(x: 'a) -> i64>)]")], "F", "'a");
        grows(&[("Q<'a, 'b>", "[`N, `C('a, Q<'b, Array<'a>>)]")], "Q", "'a");
        grows(&[("A<'a>", "[`N, `C(B<'a>)]"), ("B<'b>", "A<Array<'b>>")], "B", "'b");
        grows(&[("W<'a>", "Abstract<[`N, `C(W<Array<'a>>)]>")], "W", "'a");
        grows(&[("U<'a>", "[`N, `C(U<['a, Array<'a>]>)]")], "U", "'a");
        grows(&[("R<'b>", "Array<'b>"), ("A<'a>", "[`N('a), `C(A<R<'a>>)]")], "A", "'a");
        grows(
            &[("M<'b>", "['b, Array<'b>]"), ("A<'a>", "[`N, `C(A<M<'a>>)]")],
            "A",
            "'a",
        );
        grows(
            &[("O<'b>", "['b, null]"), ("A<'a>", "[`N, `C(A<O<Array<'a>>>)]")],
            "A",
            "'a",
        );
        grows(
            &[("L<'b>", "[`Nil, `Cons('b, L<'b>)]"), ("A<'a>", "[`N, `C(A<L<'a>>)]")],
            "A",
            "'a",
        );
        grows(
            &[("O<'b>", "['b, null]"), ("W<'a>", "Abstract<[`N, `C(W<Array<O<'a>>>)]>")],
            "W",
            "'a",
        );
    }

    /// The body the typedef `name` of `env` stores.
    fn stored(env: &Env, name: &str) -> String {
        use crate::expr::ModPath;
        let defs = env.lexical.typedefs.get(&ModPath::root()).expect("root typedefs");
        defs.get(name).expect("defined").typ().to_string()
    }

    const OPT: (&str, &str) = ("O<'b>", "['b, null]");

    #[test]
    fn an_alias_applied_to_a_parameter_is_its_expansion() {
        let t = |defs: &[(&str, &str)], name: &str| {
            let (env, _) = Env::typed_at_root(defs, "i64").expect("regular");
            stored(&env, name)
        };
        assert_eq!(t(&[OPT, ("T<'a>", "[`N, `C('a, T<O<'a>>)]")], "T"), {
            t(&[("T<'a>", "[`N, `C('a, T<['a, null]>)]")], "T")
        });
        let id =
            [("I<'b>", "J<'b>"), ("J<'c>", "'c"), ("T<'a>", "[`N('a), `C(T<I<'a>>)]")];
        assert_eq!(t(&id, "T"), t(&[("T<'a>", "[`N('a), `C(T<'a>)]")], "T"));
        let nested =
            [OPT, ("P<'b>", "O<O<'b>>"), ("T<'a>", "[`N, `C(T<P<[O<'a>, string]>>)]")];
        assert_eq!(
            t(&nested, "T"),
            t(&[("T<'a>", "[`N, `C(T<['a, null, string]>)]")], "T")
        );
        let two = [OPT, ("V<'a, 'b>", "[`N('a), `C(V<O<['a, 'b]>, 'a>)]")];
        assert_eq!(
            t(&two, "V"),
            t(&[("V<'a, 'b>", "[`N('a), `C(V<['a, 'b, null], 'a>)]")], "V")
        );
        let alias = [OPT, ("A<'a>", "[`N('a), `C(B<'a>)]"), ("B<'b>", "A<O<'b>>")];
        let written = [("A<'a>", "[`N('a), `C(B<'a>)]"), ("B<'b>", "A<['b, null]>")];
        assert_eq!(t(&alias, "B"), t(&written, "B"));
        let later =
            [OPT, ("A<'a>", "[`N('a), `C(B<O<'a>>)]"), ("B<'b>", "[`M, `D(A<'b>)]")];
        assert_eq!(t(&later, "A"), t(&[("A<'a>", "[`N('a), `C(B<['a, null]>)]")], "A"));
        let rep = [OPT, ("W<'a>", "Abstract<[`N('a), `C(W<O<'a>>)]>")];
        let (env, _) = Env::typed_at_root(&rep, "i64").expect("regular");
        let rep = env.abstract_reps.into_iter().next().expect("a representation").1;
        assert_eq!(rep.rep.to_string(), "[`C(W<[null, 'a]>), `N('a)]");
    }

    /// An application that cannot unfold back to the definition stores
    /// its arguments as written: what its unfolding prints is unchanged.
    #[test]
    fn an_alias_argument_of_a_closed_definition_stays_as_written() {
        let defs = [
            OPT,
            ("L<'a>", "[`Nil, `Cons('a, L<'a>)]"),
            ("F<'a>", "[`F(L<O<'a>>), `G('a)]"),
        ];
        let (env, _) = Env::typed_at_root(&defs, "i64").expect("regular");
        assert_eq!(stored(&env, "F"), "[`F(L<O<'a>>), `G('a)]");
    }

    #[test]
    fn an_alias_defined_below_its_application_is_named() {
        let late = |defs: &[(&str, &str)], name: &str, param: &str, alias: &str| {
            let e = refusal(defs).expect("refused");
            let want = format!(
                "recursive type {name} passes its parameter {param} back to itself \
                 through {alias}, which is not defined above"
            );
            assert!(e.starts_with(&want), "{e}");
        };
        late(&[("T<'a>", "[`N, `C('a, T<O<'a>>)]"), OPT], "T", "'a", "O");
        let nested =
            [("I<'b>", "J<'b>"), ("T<'a>", "[`N, `C(T<I<'a>>)]"), ("J<'c>", "'c")];
        late(&nested, "T", "'a", "J");
        let cycle =
            [("A<'a>", "[`N('a), `C(B<O<'a>>)]"), OPT, ("B<'b>", "[`M, `D(A<'b>)]")];
        late(&cycle, "B", "'b", "O");
    }

    /// A DAG of aliases, each naming the two below it, is looked through
    /// once per alias, not once per path.
    #[test]
    fn an_alias_met_again_is_looked_through_once() {
        const DEPTH: usize = 40;
        let mut defs = vec![
            ("A0<'a>".to_string(), "['a, null]".to_string()),
            ("B0<'a>".to_string(), "['a, string]".to_string()),
        ];
        for i in 1..=DEPTH {
            defs.push((format!("A{i}<'a>"), format!("[A{}<'a>, B{}<'a>]", i - 1, i - 1)));
            defs.push((format!("B{i}<'a>"), format!("[B{}<'a>, A{}<'a>]", i - 1, i - 1)));
        }
        defs.push(("T<'a>".to_string(), format!("[`N, `C('a, T<A{DEPTH}<'a>>)]")));
        let defs: Vec<(&str, &str)> =
            defs.iter().map(|(n, b)| (n.as_str(), b.as_str())).collect();
        super::LOOKS.with(|n| n.set(0));
        let (env, _) = Env::typed_at_root(&defs, "i64").expect("regular");
        let looks = super::LOOKS.with(|n| n.get());
        assert!(looks <= 2 * (DEPTH + 1), "{looks} looks through {DEPTH} levels");
        let written = [("T<'a>", "[`N, `C('a, T<['a, null, string]>)]")];
        let (twin, _) = Env::typed_at_root(&written, "i64").expect("regular");
        assert_eq!(stored(&env, "T"), stored(&twin, "T"));
    }

    #[test]
    fn parameters_passed_on_are_regular() {
        for defs in [
            &[("L<'a>", "[`Nil, `Cons('a, L<'a>)]")][..],
            &[("S<'a, 'b>", "[`Nil, `C('a, S<'b, 'a>)]")],
            &[("K<'a>", "[`Nil, `C('a, K<Array<i64>>)]")],
            &[("Q<'a, 'b, 'c>", "[`A('a), `R(Q<'b, 'c, 'c>)]")],
            &[
                ("L<'a>", "[`Nil, `Cons('a, L<'a>)]"),
                ("W<'a>", "[`N, `C(L<Array<'a>>, W<'a>)]"),
            ],
            &[
                ("O<'a>", "['a, null]"),
                ("A", "O<Array<B>>"),
                ("B", "[`X, `Y(O<Array<A>>)]"),
            ],
            &[("R<'a>", "Abstract<[`N, `C('a, R<'a>)]>")],
            &[("U<'a>", "{n: [U<['a, string]>, null], v: 'a}")],
            &[("V<'a, 'b>", "[`N('a), `C(V<['a, 'b], 'a>)]")],
            &[("I<'b>", "'b"), ("A<'a>", "[`N('a), `C(A<I<'a>>)]")],
            &[OPT, ("T<'a>", "[`N, `C('a, T<O<'a>>)]")],
            &[OPT, ("P<'b>", "O<O<'b>>"), ("T<'a>", "[`N, `C('a, T<P<'a>>)]")],
            &[OPT, ("A<'a>", "[`N, `C(B<'a>)]"), ("B<'b>", "A<O<'b>>")],
            &[OPT, ("B<'b>", "A<O<'b>>"), ("A<'a>", "[`N, `C(B<'a>)]")],
            &[OPT, ("A<'a>", "[`N('a), `C(B<O<'a>>)]"), ("B<'b>", "[`M, `D(A<'b>)]")],
            &[OPT, ("W<'a>", "Abstract<[`N('a), `C(W<O<'a>>)]>")],
        ] {
            assert_eq!(refusal(defs), None, "{defs:?}");
        }
    }
}
