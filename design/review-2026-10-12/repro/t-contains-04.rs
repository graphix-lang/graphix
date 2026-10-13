use ahash::AHashMap;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use graphix_types::{
    env::Env,
    expr::parser::parse_type,
    typ::{TVar, Type},
};

fn fn_args(s: &str, known: &mut AHashMap<ArcStr, TVar>) -> Vec<Type> {
    let t = parse_type(s).unwrap();
    t.alias_tvars(known);
    match &t {
        Type::Fn(f) => f.args.iter().map(|a| a.typ.clone()).collect(),
        t => panic!("not a fn: {t}"),
    }
}

fn parsed(s: &str, known: &mut AHashMap<ArcStr, TVar>) -> Type {
    let t = parse_type(s).unwrap();
    t.alias_tvars(known);
    t
}

// t-contains-04: a probe of `'a ⊇ 'b` with 'b bound ignores 'a's bound,
// the commit honours it: set_commit admits a member whose commit then
// fails, after binding 'q.
#[test]
fn probe_ignores_bound_of_open_cell_against_bound_cell() {
    let env = Env::default();
    let mut known = AHashMap::default();
    let b = TVar::empty_named(arcstr::literal!("b"));
    b.bind(parse_type("string").unwrap());
    known.insert(arcstr::literal!("b"), b.clone());
    let args = fn_args(
        "fn<'a: i64>(x: [(Any, 'q, Array<'a>), (string, string, Array<[string, null]>)], y: 'q) -> i64",
        &mut AHashMap::default(),
    );
    let (x, y) = (&args[0], &args[1]);
    eprintln!("x = {x}, y = {y}");
    let member1 = match x {
        Type::Set(s) => s.iter().find(|m| format!("{m}").contains("Any")).unwrap().clone(),
        t => panic!("not a set {t}"),
    };
    let arg = parsed("(string, string, Array<'b>)", &mut known);
    let arg_direct = parse_type("(string, string, Array<string>)").unwrap();
    let probe_bound = member1.contains_with_flags(BitFlags::empty(), &env, &arg).unwrap();
    let probe_direct =
        member1.contains_with_flags(BitFlags::empty(), &env, &arg_direct).unwrap();
    eprintln!("probe via bound cell = {probe_bound}, probe via its binding = {probe_direct}");
    let member2 = match x {
        Type::Set(s) => s.iter().find(|m| !format!("{m}").contains("Any")).unwrap().clone(),
        _ => unreachable!(),
    };
    let x = &Type::Set(triomphe::Arc::from_iter([member1.clone(), member2]));
    let again = member1.contains_with_flags(BitFlags::empty(), &env, &arg).unwrap();
    let xprobe = x.contains_with_flags(BitFlags::empty(), &env, &arg).unwrap();
    eprintln!("probe again = {again}; x probe = {xprobe}");
    let ok = x.contains(&env, &arg).unwrap();
    eprintln!("commit x >= arg = {ok}; x after = {x}; y after = {:?}", y.deref_cloned());
    let ok1 = member1.contains(&env, &arg).unwrap();
    eprintln!("commit member1 >= arg = {ok1}; member1 after = {member1}; y after = {:?}", y.deref_cloned());
    assert_eq!(probe_bound, probe_direct, "probe depends on whether the rhs is a bound cell");
    assert!(y.deref_cloned().is_none(), "a failed alternative left 'q bound: {y}");
}
