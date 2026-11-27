//! The on twin: an `on` block beside the form its rule says it stands for
//! (`design/on_blocks.md`), written out by hand over a second copy of the
//! state, with an in-program verdict ([`crate::TWIN_TAG`]).
//!
//! `on` is lowered by a rewrite of the syntax tree that both engines share,
//! so they run the same lowered program and no comparison of them sees a
//! bug in the rewrite. This generator states the rule a second time, over
//! a small tree of its own that says which reads are the block's own and
//! which are taken at the event, and the program compares the two: each
//! state variable's writes and each block's value, cycle by cycle.
//!
//! Not drawn: a glob `use` in the block (CR `t-seq-03`), `catch` and
//! dynamic modules (refused), a nested `seq`.

use super::chance;
use crate::{mutate::Rng, schedule::Schedule};
use std::{collections::BTreeSet, fmt::Write};

const STATES: usize = 3;

/// A way the hand-written form is made wrong, to measure what the twin
/// sees.
#[derive(Clone, Copy, PartialEq, Debug)]
#[cfg_attr(not(test), allow(dead_code))]
pub(crate) enum Fault {
    None,
    /// A read of a value bound outside the block follows it, as a read
    /// the lowering missed would.
    LiveOuter,
    /// Nothing is fired by the event.
    NoRoot,
}

#[derive(Clone)]
enum E {
    Lit(i64),
    /// A binding made outside the block: read as it stood at the event.
    Outer(String),
    /// State variable `i`, which the block may write: likewise.
    State(usize),
    /// A name the block makes, a parameter, or the event's own name.
    Own(String),
    /// `live(v)`: `v` where it stands.
    Live(String),
    /// `*r`, where `r` is a reference to `x` made outside the block.
    Deref,
    Bin(Box<E>, &'static str, Box<E>),
    /// A call by name: of `h`, defined outside, or a lambda the block made.
    Call(String, Box<E>),
    Sel(Box<E>, Vec<(Pat, Option<E>, E)>),
    /// `{ let n = v; e }`
    Blk(String, Box<E>, Box<E>),
}

#[derive(Clone)]
enum Pat {
    Lit(i64),
    Bind(String),
    Wild,
}

enum S {
    Write(usize, E),
    Let(String, E),
    /// `let g = |q: i64| e`
    Lam(String, String, E),
    /// `let w = |q: i64| s <- e`
    Writer(String, String, usize, E),
    /// `w(e)`, for its write.
    Do(String, E),
    Sel(E, Vec<(Pat, Option<E>, Vec<S>)>),
    /// `on .. { .. }` in a block, bound to a name or not.
    On(Option<String>, Box<Block>),
}

enum Clock {
    /// `on k`: the block reads the event's name as it fires.
    Name(String),
    /// `on e`
    Expr(String),
    /// `on let pat = e`
    Let(String, String),
}

struct Block {
    clock: Clock,
    body: Vec<S>,
    tail: E,
}

/// What a point in a block's text can name.
#[derive(Clone)]
struct Scope {
    own: Vec<String>,
    outer: Vec<String>,
    lams: Vec<String>,
    writers: Vec<String>,
}

impl Scope {
    fn with_own(&self, n: &str) -> Scope {
        let mut sc = self.clone();
        sc.outer.retain(|o| o != n);
        if !sc.own.iter().any(|o| o == n) {
            sc.own.push(n.to_string())
        }
        sc
    }
}

struct Gen<'a> {
    rng: &'a mut Rng,
    next: usize,
}

impl Gen<'_> {
    fn fresh(&mut self, prefix: &str) -> String {
        self.next += 1;
        format!("{prefix}{}", self.next)
    }

    fn pick(&mut self, of: &[String]) -> String {
        of[self.rng.below(of.len())].clone()
    }

    fn lit(&mut self) -> E {
        E::Lit(self.rng.below(6) as i64)
    }

    fn leaf(&mut self, sc: &Scope) -> E {
        match self.rng.below(12) {
            0 | 1 => self.lit(),
            2..=4 => E::State(self.rng.below(STATES)),
            5..=7 if !sc.outer.is_empty() => E::Outer(self.pick(&sc.outer)),
            8 | 9 if !sc.own.is_empty() => E::Own(self.pick(&sc.own)),
            10 => {
                let v = ["x", "y"][self.rng.below(2)];
                match sc.outer.iter().any(|o| o == v) {
                    true => E::Live(v.to_string()),
                    false => self.lit(),
                }
            }
            11 => E::Deref,
            _ => self.lit(),
        }
    }

    fn bin(&mut self, a: E, b: E) -> E {
        E::Bin(Box::new(a), ["+", "-", "*"][self.rng.below(3)], Box::new(b))
    }

    fn expr(&mut self, sc: &Scope, depth: usize) -> E {
        if depth == 0 || chance(self.rng, 0.3) {
            return self.leaf(sc);
        }
        match self.rng.below(10) {
            0..=4 => {
                let (a, b) = (self.expr(sc, depth - 1), self.expr(sc, depth - 1));
                self.bin(a, b)
            }
            5 | 6 => {
                let f = match sc.lams.is_empty() || chance(self.rng, 0.5) {
                    true => "h".to_string(),
                    false => self.pick(&sc.lams),
                };
                E::Call(f, Box::new(self.value(sc, depth - 1)))
            }
            7 | 8 => {
                let scrutinee = self.scrutinee(sc, depth - 1);
                let arms = self.arms(sc, |g, sc, _| g.expr(sc, depth - 1));
                E::Sel(Box::new(scrutinee), arms)
            }
            _ => {
                let n = self.fresh("t");
                let v = self.expr(sc, depth - 1);
                let x = self.expr(&sc.with_own(&n), depth - 1);
                E::Blk(n, Box::new(v), Box::new(x))
            }
        }
    }

    fn scrutinee(&mut self, sc: &Scope, depth: usize) -> E {
        E::Bin(Box::new(self.expr(sc, depth)), "%", Box::new(E::Lit(3)))
    }

    /// A binder: a new name, or one that shadows a value outside the block.
    fn binder(&mut self, sc: &Scope, prefix: &str) -> String {
        match !sc.outer.is_empty() && chance(self.rng, 0.25) {
            true => self.pick(&sc.outer),
            false => self.fresh(prefix),
        }
    }

    /// The arms of a select over an i64: literals, a guarded binder now
    /// and then, and a last arm that takes the rest. `body` is told
    /// whether its arm is the last.
    fn arms<T>(
        &mut self,
        sc: &Scope,
        mut body: impl FnMut(&mut Self, &Scope, bool) -> T,
    ) -> Vec<(Pat, Option<E>, T)> {
        let mut arms = Vec::new();
        let first = self.rng.below(3);
        for i in 0..self.rng.below(3) {
            arms.push((Pat::Lit(((first + i) % 3) as i64), None, body(self, sc, false)))
        }
        if chance(self.rng, 0.4) {
            let n = self.binder(sc, "n");
            let inner = sc.with_own(&n);
            let than = self.leaf(&inner);
            let guard = E::Bin(Box::new(E::Own(n.clone())), ">", Box::new(than));
            arms.push((Pat::Bind(n), Some(guard), body(self, &inner, false)))
        }
        if chance(self.rng, 0.5) {
            arms.push((Pat::Wild, None, body(self, sc, true)))
        } else {
            let n = self.binder(sc, "n");
            let inner = sc.with_own(&n);
            arms.push((Pat::Bind(n), None, body(self, &inner, true)))
        }
        arms
    }

    /// A value, a constant now and then: what the event alone fires.
    fn value(&mut self, sc: &Scope, depth: usize) -> E {
        match chance(self.rng, 0.2) {
            true => self.lit(),
            false => self.expr(sc, depth),
        }
    }

    fn write(&mut self, sc: &Scope) -> S {
        match !sc.writers.is_empty() && chance(self.rng, 0.25) {
            true => S::Do(self.pick(&sc.writers), self.value(sc, 1)),
            false => S::Write(self.rng.below(STATES), self.value(sc, 2)),
        }
    }

    /// A lambda's body: reads and arithmetic, which the event fires
    /// nothing in.
    fn lambda_body(&mut self, sc: &Scope, q: &str, depth: usize) -> E {
        if depth == 0 || chance(self.rng, 0.3) {
            return match self.rng.below(8) {
                0 | 1 => E::Own(q.to_string()),
                2 => self.lit(),
                3 | 4 => E::State(self.rng.below(STATES)),
                5 | 6 if !sc.outer.is_empty() => E::Outer(self.pick(&sc.outer)),
                _ => E::Deref,
            };
        }
        let a = self.lambda_body(sc, q, depth - 1);
        match self.rng.below(4) {
            0 => E::Call("h".to_string(), Box::new(a)),
            _ => {
                let b = self.lambda_body(sc, q, depth - 1);
                self.bin(a, b)
            }
        }
    }

    fn stmt(&mut self, sc: &mut Scope, depth: usize, out: &mut Vec<S>) {
        match self.rng.below(14) {
            5 | 6 => {
                let n = self.binder(sc, "m");
                out.push(S::Let(n.clone(), self.value(sc, 2)));
                *sc = sc.with_own(&n)
            }
            7 => {
                let (g, q) = (self.fresh("g"), self.fresh("q"));
                let read = self.lambda_body(sc, &q, 1);
                let body = self.bin(E::Own(q.clone()), read);
                out.push(S::Lam(g.clone(), q, body));
                sc.lams.push(g)
            }
            8 => {
                let (w, q) = (self.fresh("w"), self.fresh("q"));
                let read = self.lambda_body(sc, &q, 1);
                let body = E::Bin(Box::new(E::Own(q.clone())), "+", Box::new(read));
                out.push(S::Writer(w.clone(), q, self.rng.below(STATES), body));
                out.push(S::Do(w.clone(), self.value(sc, 1)));
                sc.writers.push(w)
            }
            9..=11 => {
                let scrutinee = self.scrutinee(sc, 1);
                let arms = self.arms(sc, |g, sc, last| {
                    let n = match last && chance(g.rng, 0.4) {
                        true => 0,
                        false => 1 + g.rng.below(2),
                    };
                    (0..n).map(|_| g.write(sc)).collect::<Vec<S>>()
                });
                out.push(S::Sel(scrutinee, arms))
            }
            12 | 13 if depth > 0 => {
                let inner = self.block(Some(sc), depth - 1);
                let bind = chance(self.rng, 0.4).then(|| self.fresh("v"));
                if let Some(n) = &bind {
                    sc.own.push(n.clone())
                }
                out.push(S::On(bind, Box::new(inner)))
            }
            _ => out.push(self.write(sc)),
        }
    }

    /// A block clocked by `k` at the top, and inside another by `j` or by
    /// a name the outer block can read as its own.
    fn block(&mut self, parent: Option<&Scope>, depth: usize) -> Block {
        let ev = match parent {
            Some(p) if !p.own.is_empty() && chance(self.rng, 0.3) => self.pick(&p.own),
            Some(_) => "j".to_string(),
            None => "k".to_string(),
        };
        let (clock, own) = match self.rng.below(10) {
            0..=5 => (Clock::Name(ev.clone()), vec![ev.clone()]),
            6 | 7 => (Clock::Expr(format!("({ev} + 1)")), vec![]),
            8 => {
                let p = self.fresh("p");
                (Clock::Let(p.clone(), ev.clone()), vec![p])
            }
            _ => {
                let (p, q) = (self.fresh("p"), self.fresh("p"));
                (Clock::Let(format!("({p}, {q})"), format!("({ev}, c)")), vec![p, q])
            }
        };
        let mut outer: Vec<String> = match parent {
            Some(p) => p.outer.iter().chain(p.own.iter()).cloned().collect(),
            None => ["x", "y", "late", "c"].iter().map(|s| s.to_string()).collect(),
        };
        if !outer.contains(&ev) {
            outer.push(ev)
        }
        outer.retain(|o| !own.contains(o));
        let mut sc = Scope {
            own,
            outer,
            lams: parent.map(|p| p.lams.clone()).unwrap_or_default(),
            writers: parent.map(|p| p.writers.clone()).unwrap_or_default(),
        };
        let mut body = Vec::new();
        for _ in 0..2 + self.rng.below(4) {
            self.stmt(&mut sc, depth, &mut body)
        }
        let tail = self.value(&sc, 2);
        Block { clock, body, tail }
    }
}

fn pat(p: &Pat) -> String {
    match p {
        Pat::Lit(n) => n.to_string(),
        Pat::Bind(n) => n.clone(),
        Pat::Wild => "_".to_string(),
    }
}

fn arm(p: &Pat, guard: Option<String>, body: String) -> String {
    match guard {
        Some(g) => format!("{} if {g} => {body}", pat(p)),
        None => format!("{} => {body}", pat(p)),
    }
}

fn arm_body(stmts: Vec<String>) -> String {
    match stmts.len() {
        0 => "never()".to_string(),
        1 => stmts.into_iter().next().unwrap(),
        _ => format!("{{ {} }}", stmts.join("; ")),
    }
}

/// The block as written.
fn on_e(e: &E) -> String {
    match e {
        E::Lit(n) => n.to_string(),
        E::Outer(v) | E::Own(v) => v.clone(),
        E::State(i) => format!("a{i}"),
        E::Live(v) => format!("live({v})"),
        E::Deref => "*r".to_string(),
        E::Bin(a, op, b) => format!("({} {op} {})", on_e(a), on_e(b)),
        E::Call(f, a) => format!("{f}({})", on_e(a)),
        E::Sel(s, arms) => {
            let arms: Vec<String> = arms
                .iter()
                .map(|(p, g, x)| arm(p, g.as_ref().map(on_e), on_e(x)))
                .collect();
            format!("select {} {{ {} }}", on_e(s), arms.join(", "))
        }
        E::Blk(n, v, x) => format!("{{ let {n} = {}; {} }}", on_e(v), on_e(x)),
    }
}

fn on_s(s: &S) -> String {
    match s {
        S::Write(i, e) => format!("a{i} <- {}", on_e(e)),
        S::Let(n, e) => format!("let {n} = {}", on_e(e)),
        S::Lam(g, q, e) => format!("let {g} = |{q}: i64| {}", on_e(e)),
        S::Writer(w, q, i, e) => format!("let {w} = |{q}: i64| a{i} <- {}", on_e(e)),
        S::Do(w, e) => format!("{w}({})", on_e(e)),
        S::Sel(sc, arms) => {
            let arms: Vec<String> = arms
                .iter()
                .map(|(p, g, body)| {
                    let body = arm_body(body.iter().map(on_s).collect());
                    arm(p, g.as_ref().map(on_e), body)
                })
                .collect();
            format!("select {} {{ {} }}", on_e(sc), arms.join(", "))
        }
        S::On(None, b) => on_block(b),
        S::On(Some(n), b) => format!("let {n} = {}", on_block(b)),
    }
}

fn on_block(b: &Block) -> String {
    let head = match &b.clock {
        Clock::Name(n) => n.clone(),
        Clock::Expr(e) => e.clone(),
        Clock::Let(p, e) => format!("let {p} = {e}"),
    };
    let body: Vec<String> = b.body.iter().map(on_s).collect();
    format!("on {head} {{ {}; {} }}", body.join("; "), on_e(&b.tail))
}

/// Whether anything in `e` fires once the event's rule is applied to it: a
/// call, a select and a block always hold something the event fires.
fn fires(e: &E) -> bool {
    match e {
        E::Lit(_) => false,
        E::Bin(a, _, b) => fires(a) || fires(b),
        _ => true,
    }
}

/// One block as its rule says it stands: a clock, a strict sample of
/// every outer binding its text reads, and the body over those.
struct Hand<'a> {
    clock: String,
    id: usize,
    samples: Vec<(String, String)>,
    fault: Fault,
    ids: &'a mut usize,
}

impl Hand<'_> {
    fn sample(&mut self, name: String, of: String) -> String {
        if !self.samples.iter().any(|(n, _)| *n == name) {
            self.samples.push((name.clone(), of))
        }
        name
    }

    fn at(&mut self, v: &str) -> String {
        self.sample(format!("{v}_at{}", self.id), v.to_string())
    }

    fn fired(&self, e: &E, s: String) -> String {
        match fires(e) || self.fault == Fault::NoRoot {
            true => s,
            false => format!("({} ~! {s})", self.clock),
        }
    }

    fn root(&mut self, e: &E) -> String {
        let s = self.e(e, false);
        self.fired(e, s)
    }

    fn call(&mut self, f: &str, arg: &E, lambda: bool) -> String {
        let s = self.e(arg, lambda);
        match lambda {
            true => format!("{f}({s})"),
            false => format!("{f}({})", self.fired(arg, s)),
        }
    }

    fn e(&mut self, e: &E, lambda: bool) -> String {
        match e {
            E::Lit(n) => n.to_string(),
            E::Outer(v) if self.fault == Fault::LiveOuter => v.clone(),
            E::Outer(v) => self.at(v),
            E::State(i) => self.at(&format!("b{i}")),
            E::Own(v) | E::Live(v) => v.clone(),
            E::Deref if self.fault == Fault::LiveOuter => "*r".to_string(),
            E::Deref => {
                let r = self.at("r");
                self.sample(format!("rd_at{}", self.id), format!("*{r}"))
            }
            E::Bin(a, op, b) => {
                format!("({} {op} {})", self.e(a, lambda), self.e(b, lambda))
            }
            E::Call(f, a) => self.call(f, a, lambda),
            E::Sel(s, arms) => {
                let s = self.root(s);
                let arms: Vec<String> = arms
                    .iter()
                    .map(|(p, g, x)| {
                        let g = g.as_ref().map(|g| self.e(g, false));
                        arm(p, g, self.e(x, false))
                    })
                    .collect();
                format!("select {s} {{ {} }}", arms.join(", "))
            }
            E::Blk(n, v, x) => {
                format!("{{ let {n} = {}; {} }}", self.root(v), self.root(x))
            }
        }
    }

    fn s(&mut self, s: &S) -> String {
        match s {
            S::Write(i, e) => format!("b{i} <- {}", self.root(e)),
            S::Let(n, e) => format!("let {n} = {}", self.root(e)),
            S::Lam(g, q, e) => format!("let {g} = |{q}: i64| {}", self.e(e, true)),
            S::Writer(w, q, i, e) => {
                format!("let {w} = |{q}: i64| b{i} <- {}", self.e(e, true))
            }
            S::Do(w, e) => self.call(w, e, false),
            S::Sel(sc, arms) => {
                let sc = self.root(sc);
                let arms: Vec<String> = arms
                    .iter()
                    .map(|(p, g, body)| {
                        let g = g.as_ref().map(|g| self.e(g, false));
                        arm(p, g, arm_body(body.iter().map(|s| self.s(s)).collect()))
                    })
                    .collect();
                format!("select {sc} {{ {} }}", arms.join(", "))
            }
            S::On(None, b) => hand_block(b, self.fault, self.ids),
            S::On(Some(n), b) => {
                format!("let {n} = {}", hand_block(b, self.fault, self.ids))
            }
        }
    }
}

fn hand_block(b: &Block, fault: Fault, ids: &mut usize) -> String {
    *ids += 1;
    let id = *ids;
    let clock = format!("ev{id}");
    let mut h = Hand { clock: clock.clone(), id, samples: Vec::new(), fault, ids };
    let body: Vec<String> = b.body.iter().map(|s| h.s(s)).collect();
    let tail = h.root(&b.tail);
    let mut out = String::from("{ ");
    let _ = match &b.clock {
        Clock::Name(e) | Clock::Expr(e) => write!(out, "let {clock} = {e}; "),
        Clock::Let(p, e) => write!(out, "let {clock} = {e}; let {p} = {clock}; "),
    };
    for (name, of) in &h.samples {
        let _ = write!(out, "let {name} = {clock} ~! {of}; ");
    }
    for s in body {
        let _ = write!(out, "{s}; ");
    }
    let _ = write!(out, "{tail} }}");
    out
}

/// An event: the cycles of the clock `c` it fires in, by period or by
/// name, two in a row among them.
fn event(rng: &mut Rng, end: usize) -> String {
    if chance(rng, 0.5) {
        let p = 2 + rng.below(4);
        return format!("select c % {p} {{ {} => c, _ => never() }}", rng.below(p));
    }
    let mut at = BTreeSet::new();
    for _ in 0..3 + rng.below(4) {
        let s = 1 + rng.below(end - 3);
        at.insert(s);
        if chance(rng, 0.3) {
            at.insert(s + 1);
        }
    }
    let arms: String = at.iter().map(|s| format!("{s} => c, ")).collect();
    format!("select c {{ {arms}_ => never() }}")
}

pub(crate) fn gen_on_twin_faulty(rng: &mut Rng, fault: Fault) -> String {
    let end = 20 + rng.below(20);
    let k = event(rng, end);
    let j = event(rng, end);
    let late = 2 + rng.below(end / 2);
    let block = Gen { rng, next: 0 }.block(None, 1);
    let on = on_block(&block);
    let hand = hand_block(&block, fault, &mut 0);
    let mut code = format!(
        "{{
  let c = 0;
  c <- select c < {end} {{ true => c + 1, false => never() }};
  let k = {k};
  let j = {j};
  let x = c * 3 - 1;
  let y = 0;
  y <- select c % 4 {{ 0 => c, _ => never() }};
  let late = select c >= {late} {{ true => c - {late}, false => never() }};
  let r = &x;
  let h = |q: i64| q * 2 + y;
"
    );
    for side in ["a", "b"] {
        for i in 0..STATES {
            let _ = writeln!(code, "  let {side}{i} = 0;");
        }
    }
    let _ = writeln!(code, "  let va = {on};\n  let vb = {hand};");
    let mut held = Vec::new();
    for v in (0..STATES).map(|i| i.to_string()).chain(["v".to_string()]) {
        for side in ["a", "b"] {
            let (t, s) = (
                format!("t{side}{v}"),
                match v.as_str() {
                    "v" => format!("v{side}"),
                    i => format!("{side}{i}"),
                },
            );
            let _ = writeln!(
                code,
                "  let {t} = 0;\n  {t} <- {s} ~ ({t} * 31 + c * 7 + {s});"
            );
        }
        held.push(format!("ta{v} == tb{v}"));
    }
    let _ = write!(
        code,
        "  let held = {};
  select c {{
    {end} => select held {{ true => `Held, false => `{tag} }},
    _ => never()
  }}
}}",
        held.join(" && "),
        tag = crate::TWIN_TAG,
    );
    let sched =
        Schedule { max_cycles: end as u64 + 64, max_events: 4096, ..Schedule::default() };
    sched.render(&code)
}

/// An on twin: it settles on the tag where the block and the form its
/// rule gives differ in what they write, what they answer, or when.
pub fn gen_on_twin(rng: &mut Rng) -> String {
    gen_on_twin_faulty(rng, Fault::None)
}

#[cfg(test)]
mod test {
    use super::*;

    /// The shapes the rule turns on are all drawn: a shadowed read, a
    /// lambda and a writer made in the block, a nested block, a constant
    /// the event fires, and every clock form.
    #[test]
    fn on_twin_shapes_are_drawn() {
        let mut rng = Rng::new(7);
        const N: usize = 400;
        let mut seen = [0usize; 7];
        for _ in 0..N {
            let p = gen_on_twin(&mut rng);
            let on = p.lines().find(|l| l.starts_with("  let va = ")).unwrap();
            let hand = p.lines().find(|l| l.starts_with("  let vb = ")).unwrap();
            let marks = [
                on.contains("|q"),
                on.matches("on ").count() > 1,
                on.contains("on let "),
                on.contains("live("),
                on.contains("*r"),
                hand.contains("(ev1 ~! "),
                ["x", "y", "late", "c"]
                    .iter()
                    .any(|v| on.contains(&format!("let {v} ="))),
            ];
            for (n, m) in seen.iter_mut().zip(marks) {
                *n += m as usize
            }
        }
        for (i, n) in seen.iter().enumerate() {
            assert!(n * 100 / N >= 10, "shape {i} in only {n}/{N}")
        }
    }
}
