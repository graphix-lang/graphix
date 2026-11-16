// Review 2026-11-16, t-setops. Three defects of graphix-types/src/typ/setops.rs:
// 1, 2. setops::Identity pairs the open cells of two function types
//    inconsistently in two cases, so `union_identical` (read here through
//    `Type::diff`, whose function arm subtracts a function type only from
//    one `setops::same_type` finds identical) calls two function types
//    identical that are no renaming of each other.
// 3. `union_ref_params` and the variant arm of `union_inner` judge members
//    of one alternative with a `Welds` that holds no alternatives.
// 4. `Type::matching_type`'s memo answers one function part with another's.
//
// Command (from the repo root, with this file copied to
// graphix-types/tests/review_t_setops_1.rs):
//   cargo test -p graphix-types --test review_t_setops_1 -- --nocapture
//
// Expected: every test passes.
// Observed (HEAD e1be2fbf2): 0 passed, 4 failed. The verdicts printed:
//   fn('a) -> 'a vs fn('a) -> 'b: identical = true, expected false
//   fn('a) -> 'b vs fn('a) -> 'a: identical = true, expected false
//   fn('a) -> 'a vs fn('c) -> 'b: identical = false, expected false
//   fn('a) -> 'a vs fn('c) -> 'c: identical = true, expected true
//   fn(Array<'a>, 'a) vs fn('c := Array<'b>, 'e): identical = true, expected false
//   fn('c := Array<'b>, 'e) vs fn(Array<'a>, 'a): identical = false, expected false
//   fn(Array<'a>, 'a) vs fn(Array<'b>, 'e): identical = false, expected false
//   bare: fn(q: 'c) -> i64 | fn(q: 'd) -> i64 = [fn(q: 'c) -> i64, fn(q: 'd) -> i64]
//   applied: R<fn(q: 'c) -> i64> | R<fn(q: 'd) -> i64> = R<fn(q: 'c) -> i64>
//   variant: `P(fn(q: 'c) -> i64, i64) | `P(fn(q: 'd) -> i64, string)
//            = `P([fn(q: 'c) -> i64, fn(q: 'd) -> i64], [i64, string])
//   (fn(x: 'a) -> 'a, fn(x: 'a) -> 'a, i64): part 0 returns 'a: true,
//            part 1 returns 'b: false, part 1 returns 'a: true
use ahash::AHashMap;
use arcstr::ArcStr;
use graphix_types::{
    env::Env,
    expr::{ModPath, TypeDefBody, parser::parse_type},
    typ::{TVar, Type},
};
use triomphe::Arc;

fn cell(name: &str) -> Type {
    Type::TVar(TVar::empty_named(ArcStr::from(name)))
}

/// `src` with each variable it names replaced by the cell given for it.
fn written(src: &str, cells: &[(&str, &Type)]) -> Type {
    let known: AHashMap<ArcStr, Type> =
        cells.iter().map(|(n, t)| (ArcStr::from(*n), (*t).clone())).collect();
    parse_type(src).unwrap().replace_tvars(&known)
}

/// Whether the difference found `t0` identical to `t1`.
fn identical(t0: &Type, t1: &Type) -> bool {
    t0.diff(&Env::default(), t1).unwrap().is_uninhabited()
}

// The doc of `union_identical`: "inside a function type the cells pair
// consistently (`fn('a) -> 'a` is `fn('b) -> 'b`, not `fn('a) -> 'b`)".
// A cell that stands beside itself is answered by the same-node shortcut
// at the top of `Identity::same`, which records no pair, so the cell is
// then free to pair with another one.
#[test]
fn a_cell_met_beside_itself_pairs_with_no_other() {
    let (a, b, c) = (cell("a"), cell("b"), cell("c"));
    let f0 = written("fn(x: 'a) -> 'a", &[("a", &a)]);
    let shared = written("fn(x: 'a) -> 'b", &[("a", &a), ("b", &b)]);
    let apart = written("fn(x: 'c) -> 'b", &[("c", &c), ("b", &b)]);
    let renamed = written("fn(x: 'c) -> 'c", &[("c", &c)]);
    let verdicts = [
        ("fn('a) -> 'a vs fn('a) -> 'b", identical(&f0, &shared), false),
        ("fn('a) -> 'b vs fn('a) -> 'a", identical(&shared, &f0), false),
        ("fn('a) -> 'a vs fn('c) -> 'b", identical(&f0, &apart), false),
        ("fn('a) -> 'a vs fn('c) -> 'c", identical(&f0, &renamed), true),
    ];
    for (what, got, want) in verdicts {
        println!("{what}: identical = {got}, expected {want}");
    }
    for (what, got, want) in verdicts {
        assert_eq!(got, want, "{what}");
    }
}

// `Identity::same_inner`'s arm `(Type::TVar(a), t) | (t, Type::TVar(a))`
// calls `self.same(&binding, t)` for both alternatives: with the bound cell
// on the right the sides are swapped, the cells under it are recorded as
// (right, left), and the consistency check, which reads each pair as
// (left, right), misses a left cell paired twice.
#[test]
fn a_bound_cell_on_the_right_keeps_the_sides() {
    let (a, b, c, e) = (cell("a"), cell("b"), cell("c"), cell("e"));
    let f0 = written("fn(x: Array<'a>, y: 'a) -> null", &[("a", &a)]);
    let Type::TVar(cv) = &c else { unreachable!() };
    cv.bind(written("Array<'b>", &[("b", &b)]));
    let bound = written("fn(x: 'c, y: 'e) -> null", &[("c", &c), ("e", &e)]);
    let plain = written("fn(x: Array<'b>, y: 'e) -> null", &[("b", &b), ("e", &e)]);
    let verdicts = [
        (
            "fn(Array<'a>, 'a) vs fn('c := Array<'b>, 'e)",
            identical(&f0, &bound),
            false,
        ),
        (
            "fn('c := Array<'b>, 'e) vs fn(Array<'a>, 'a)",
            identical(&bound, &f0),
            false,
        ),
        ("fn(Array<'a>, 'a) vs fn(Array<'b>, 'e)", identical(&f0, &plain), false),
    ];
    for (what, got, want) in verdicts {
        println!("{what}: identical = {got}, expected {want}");
    }
    for (what, got, want) in verdicts {
        assert_eq!(got, want, "{what}");
    }
}

/// An env holding the typedefs `defs` at the root (as the crate's own
/// `Env::typed_at_root`, which is `cfg(test)`).
fn env_with(defs: &[(&str, &str)]) -> Env {
    let root = ModPath::root();
    let mut env = Env::default();
    for (name, body) in defs {
        let (name, params) = name.split_once('<').unwrap_or((name, ""));
        let params = params.trim_end_matches('>').split(',').map(str::trim);
        let params = Arc::from_iter(params.filter(|p| !p.is_empty()).map(|p| {
            (TVar::empty_named(ArcStr::from(p.trim_start_matches('\''))), None)
        }));
        env.deftype(
            &root,
            name,
            params,
            &TypeDefBody::Alias(parse_type(body).unwrap()),
            true,
            None,
            Default::default(),
            Default::default(),
            Arc::new(Default::default()),
        )
        .unwrap();
    }
    env.seed_typedef_refs();
    env
}

// The rule `union_tagged` states: "members of one alternative collapse
// only where identical as they stand", pinned for bare function members by
// normalize.rs `a_join_of_alternatives_keeps_the_union_one_of_them_holds`.
// `union_ref_params` compares the parameters of two applications with a
// `Welds` of its own that holds no alternatives, so two applications of one
// alternative whose parameters differ by a renaming collapse, and their
// cells weld. And the variant arm counts its differing positions with a
// `Welds` that holds none either, so a position that differs as it stands
// is not counted, and two positions are unioned at once.
#[test]
fn members_of_one_alternative_collapse_only_as_they_stand() {
    let env = env_with(&[("R<'f>", "{f: 'f}")]);
    let cases = [
        ("bare", "([fn(q: 'c) -> i64, fn(q: 'd) -> i64], 'c, 'd)"),
        ("applied", "([R<fn(q: 'c) -> i64>, R<fn(q: 'd) -> i64>], 'c, 'd)"),
        (
            "variant",
            "([`P(fn(q: 'c) -> i64, i64), `P(fn(q: 'd) -> i64, string)], 'c, 'd)",
        ),
    ];
    let mut verdicts = vec![];
    for (what, written) in cases {
        let t = parse_type(written).unwrap().scope_refs(&ModPath::root());
        t.seed_refs(&env);
        t.alias_tvars(&mut AHashMap::default());
        let Type::Tuple(fs) = &t else { unreachable!() };
        let (Type::TVar(c), Type::TVar(d)) = (&fs[1], &fs[2]) else { unreachable!() };
        let Type::Set(ms) = &fs[0] else { unreachable!("{}", fs[0]) };
        let (p, q) = (&ms[0], &ms[1]);
        let read = Type::union_exact_of_members(&env, &[p, q]).unwrap();
        let two = matches!(&read, Type::Set(ms) if ms.len() == 2);
        let apart = c.stands_for() != d.stands_for();
        println!("{what}: {p} | {q} = {read}; two members: {two}; 'c, 'd apart: {apart}");
        verdicts.push((what, two, apart));
    }
    for (what, two, apart) in verdicts {
        assert!(two && apart, "{what}");
    }
}

// `Type::matching_type` keeps each pair's answer by the pair's
// `RefHist::ref_id`s, which key two function types alike where
// `union_identical_in` pairs their cells: the second function part of a
// tuple is answered with the first's type.
#[test]
fn matching_type_types_each_function_part_as_its_own() {
    let env = Env::default();
    let (a, b) = (cell("a"), cell("b"));
    let test = parse_type("(Any, Any, i64)").unwrap();
    let reaching = written(
        "(fn(x: 'a) -> 'a, fn(x: 'b) -> 'b, [i64, null])",
        &[("a", &a), ("b", &b)],
    );
    let m = test.matching_type(&env, &reaching).unwrap();
    let Type::Tuple(parts) = &m else { unreachable!("{m}") };
    let (Type::Fn(f), Type::Fn(g)) = (&parts[0], &parts[1]) else { unreachable!("{m}") };
    let (Type::TVar(fa), Type::TVar(gb)) = (&f.rtype, &g.rtype) else { unreachable!() };
    let (Type::TVar(a), Type::TVar(b)) = (&a, &b) else { unreachable!() };
    println!(
        "{m}: part 0 returns 'a: {}, part 1 returns 'b: {}, part 1 returns 'a: {}",
        fa.same_cell(a),
        gb.same_cell(b),
        gb.same_cell(a)
    );
    assert!(fa.same_cell(a) && gb.same_cell(b), "{m}");
}
