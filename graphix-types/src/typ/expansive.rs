//! Whether a typedef is expansive (Kennedy and Pierce): whether it
//! unfolds to infinitely many distinct applications. A parameter of one
//! definition flows into a parameter of a definition its body applies,
//! plainly (`'a` in `C<'a>`) or under a constructor (`C<Array<'a>>`).
//! A definition's unfolding is finite unless a parameter's flow returns
//! to it through a flow under a constructor (`type N<'a> = [`Nil,
//! `Cons('a, N<Array<'a>>)]`). An abstract type with a representation
//! is a definition too: a walk may look into it.

use crate::{
    env::Env,
    stack::ensure_sufficient,
    typ::{AbstractId, ResolvedRef, TVar, Type, TypeRef},
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
    /// A name the body reaches is not defined yet.
    Unknown,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Key {
    Def(usize),
    Abstract(AbstractId),
}

type Params = Arc<[(TVar, Option<Type>)]>;

struct Flow<'a> {
    env: &'a Env,
    index: LPooled<AHashMap<Key, usize>>,
    /// Per definition: its first node, its formals and its body. A node
    /// is a parameter of a definition.
    defs: LPooled<Vec<(usize, Params, Type)>>,
    /// Per node, `(to, under a constructor)`.
    flows: LPooled<Vec<LPooled<Vec<(usize, bool)>>>>,
    unknown: bool,
}

impl<'a> Flow<'a> {
    fn intern(&mut self, key: Key, params: &Params, body: &Type) -> usize {
        if let Some(i) = self.index.get(&key) {
            return *i;
        }
        let i = self.defs.len();
        self.defs.push((self.flows.len(), params.clone(), body.clone()));
        self.flows.extend(params.iter().map(|_| LPooled::take()));
        self.index.insert(key, i);
        i
    }

    fn scan(&mut self, d: usize) {
        let (_, params, body) = self.defs[d].clone();
        let formals: LPooled<Vec<ArcStr>> =
            params.iter().map(|(f, _)| f.name.clone()).collect();
        self.walk(d, &formals, &body);
    }

    /// Record the flows of `d`'s formals into the parameters of `c`,
    /// applied to `args`. A formal standing bare as a union's member flows
    /// plainly: a union flattens, so `T<['a, string]>` applied to itself
    /// is `T<['a, string]>` again.
    fn apply(&mut self, d: usize, formals: &[ArcStr], c: usize, args: &[Type]) {
        let first = self.defs[c].0;
        for (j, arg) in args.iter().enumerate() {
            let members = match arg {
                Type::Set(ms) => &ms[..],
                arg => std::slice::from_ref(arg),
            };
            for m in members {
                let bare = bare_formal(m);
                for (i, f) in formals.iter().enumerate() {
                    let under = match &bare {
                        Some(b) if b == f => false,
                        _ if holds_formal(m, f) => true,
                        _ => continue,
                    };
                    let from = self.defs[d].0 + i;
                    self.flows[from].push((first + j, under));
                }
            }
        }
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
                            let c = self.intern(key, c.params(), c.typ());
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
                    let c = self.intern(Key::Abstract(*id), &rp, &rep);
                    self.apply(d, formals, c, params)
                }
                for p in params.iter() {
                    self.walk(d, formals, p)
                }
            }
            t => t.for_each_child(&mut |c| self.walk(d, formals, c)),
        })
    }

    /// Per node, whether it lies on a cycle of flows through a flow under
    /// a constructor (Tarjan's strongly connected components).
    fn growing(&self) -> LPooled<Vec<bool>> {
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
        let mut grows: LPooled<Vec<bool>> = LPooled::take();
        grows.resize(n, false);
        for v in 0..n {
            for (w, under) in self.flows[v].iter() {
                if *under && comp[*w] == comp[v] {
                    grows[comp[v]] = true
                }
            }
        }
        (0..n).map(|v| grows[comp[v]]).collect()
    }
}

enum Named {
    Def(std::sync::Arc<ResolvedRef>),
    Trait,
    Unknown,
}

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
/// the definitions it reaches.
pub(crate) fn expansive(env: &Env, def: &ResolvedRef) -> Expansive {
    if def.params().is_empty() || def.regular.get().is_some() {
        return Expansive::Regular;
    }
    let mut flow = Flow {
        env,
        index: LPooled::take(),
        defs: LPooled::take(),
        flows: LPooled::take(),
        unknown: false,
    };
    flow.intern(Key::Def(def.def_key()), def.params(), def.typ());
    let mut d = 0;
    while d < flow.defs.len() {
        flow.scan(d);
        d += 1;
    }
    let growing = flow.growing();
    match def.params().iter().zip(growing.iter()).find(|(_, g)| **g) {
        Some(((tv, _), _)) => Expansive::Grows(tv.clone()),
        None if flow.unknown => Expansive::Unknown,
        None => {
            let _ = def.regular.set(());
            Expansive::Regular
        }
    }
}

#[cfg(test)]
mod test {
    use crate::env::Env;

    /// The refusal of the typedefs `defs`, defined in order, if any.
    fn refusal(defs: &[(&str, &str)]) -> Option<String> {
        Env::typed_at_root(defs, "i64").err().map(|e| e.to_string())
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
        grows(&[("I<'b>", "'b"), ("A<'a>", "[`N('a), `C(A<I<'a>>)]")], "A", "'a");
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
        ] {
            assert_eq!(refusal(defs), None, "{defs:?}");
        }
    }
}
