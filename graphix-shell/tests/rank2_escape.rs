//! A REPL input holds a value to a written quantified function type as a
//! script does: a lambda that writes a binding outside it is refused.

mod common;
use common::repl;
#[cfg(target_os = "linux")]
use common::{Instructions, graphix};

#[test]
fn a_repl_input_holds_a_rank2_annotation_generally() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input = "{ let y = never(); let p: fn<'b: Any>(x: 'b) -> 'b = |x| { y <- x; x }; \
                 (p(1), p(\"s\"), y + 1) }\n\
                 sys::exit(3)\n";
    let ran = repl(dir.path(), &["--no-cache"], input);
    assert_eq!(ran.status.code(), Some(3), "{}\n{}", ran.stdout, ran.stderr);
    let all = format!("{}\n{}", ran.stdout, ran.stderr);
    assert!(all.contains("the value is not polymorphic in 'b"), "{all}");
}

#[test]
fn a_repl_write_to_a_generic_binding_holds_its_copy_generally() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input = "type W<'t> = Abstract<'t>\n\
                 let f = |x| x\n\
                 let yy = { let y = never(); f <- |v| { y <- array::map([v], |e| W(e)); v }; y }\n\
                 sys::exit(3)\n";
    let ran = repl(dir.path(), &["--no-cache"], input);
    assert_eq!(ran.status.code(), Some(3), "{}\n{}", ran.stdout, ran.stderr);
    let all = format!("{}\n{}", ran.stdout, ran.stderr);
    assert!(all.contains("made generic by an earlier compile"), "{all}");
}

/// A call passing `n` rank-2 formals each a lambda general in it.
#[cfg(target_os = "linux")]
fn many_rank2_formals(n: usize) -> String {
    let formals: String =
        (0..n).map(|i| format!("#k{i}: fn<'a: Any>(x: 'a) -> 'a, ")).collect();
    let args: String = (0..n).map(|i| format!("#k{i}: |x| x, ")).collect();
    format!("let f = |{formals}n: i64| n;\nprintln(f({args}1))\n")
}

#[cfg(target_os = "linux")]
#[test]
fn a_call_judges_its_signature_once() {
    const BILLION: f64 = 1e9;
    let cost = |n| {
        let dir = tempfile::tempdir().expect("tempdir");
        let work = Instructions::count()
            .expect("count instructions (kernel.perf_event_paranoid must be 2 or lower)");
        let ran =
            graphix(dir.path(), &many_rank2_formals(n), &["--no-cache", "--check"], &[]);
        let done = work.read();
        assert!(ran.status.success(), "{}\n{}", ran.stdout, ran.stderr);
        done
    };
    let (small, big) = (cost(8), cost(512));
    let extra = big.saturating_sub(small) as f64 / BILLION;
    eprintln!(
        "8 formals: {:.2}G, 512: {:.2}G",
        small as f64 / BILLION,
        big as f64 / BILLION
    );
    assert!(extra < 1.0, "512 rank-2 formals cost {extra:.2}G more than 8");
}

/// A write of a lambda whose tuple pattern takes `n` cells to a rank-2
/// holder, the lambda reading `n` bindings outside it: its copy's `n`
/// cells beside `n` bindings the check walks.
#[cfg(target_os = "linux")]
fn copied_beside_bindings(n: usize) -> String {
    let list = |f: &dyn Fn(usize) -> String| -> String {
        (0..n).map(f).collect::<Vec<_>>().join(", ")
    };
    let (ys, zeros, ints, xs) = (
        list(&|i| format!("y{i}")),
        list(&|_| "0".into()),
        list(&|_| "i64".into()),
        list(&|i| format!("a{i}")),
    );
    format!(
        "let ({ys}) = ({zeros});\n\
         let f: fn<'a: Any>(x: 'a, p: ({ints})) -> 'a = |x, p| x;\n\
         f <- |x, ({xs})| {{ let s = [{ys}]; x }};\n\
         f(1, ({zeros}))\n"
    )
}

#[cfg(target_os = "linux")]
#[test]
fn a_copy_is_asked_once_for_every_binding() {
    const BILLION: f64 = 1e9;
    let cost = |n| {
        let dir = tempfile::tempdir().expect("tempdir");
        let work = Instructions::count()
            .expect("count instructions (kernel.perf_event_paranoid must be 2 or lower)");
        let ran = graphix(
            dir.path(),
            &copied_beside_bindings(n),
            &["--no-cache", "--check"],
            &[],
        );
        let done = work.read();
        assert!(ran.status.success(), "{}\n{}", ran.stdout, ran.stderr);
        done
    };
    let (small, big) = (cost(8), cost(4000));
    let extra = big.saturating_sub(small) as f64 / BILLION;
    eprintln!(
        "8 cells and bindings: {:.2}G, 4000: {:.2}G",
        small as f64 / BILLION,
        big as f64 / BILLION
    );
    assert!(
        extra < 4.5,
        "4000 copied cells beside 4000 bindings cost {extra:.2}G more than 8"
    );
}

/// A write to a rank-2 holder of a lambda whose tuple pattern takes `k`
/// cells and nests them `k` deep through lets, which `b` bindings outside
/// it each take through a collection callback: every binding's walk meets
/// the copied cells' whole nest.
#[cfg(target_os = "linux")]
fn nested_copies_beside_bindings(b: usize, k: usize) -> String {
    let list = |n: usize, f: &dyn Fn(usize) -> String, sep: &str| -> String {
        (0..n).map(f).collect::<Vec<_>>().join(sep)
    };
    let (ints, zeros, xs) = (
        list(k, &|_| "i64".into(), ", "),
        list(k, &|_| "0".into(), ", "),
        list(k, &|i| format!("a{i}"), ", "),
    );
    let ys = list(b, &|i| format!("let y{i} = never();"), "\n");
    let nest = list(
        k,
        &|i| match i {
            0 => "let t0 = (a0, a0);".into(),
            i => format!("let t{i} = (t{}, a{i});", i - 1),
        },
        " ",
    );
    let writes =
        list(b, &|i| format!("y{i} <- array::map([t{}], |e| W(e));", k - 1), " ");
    format!(
        "type W<'t> = Abstract<'t>;\n\
         {ys}\n\
         let f: fn<'a: Any>(x: 'a, p: ({ints})) -> 'a = |x, p| x;\n\
         f <- |x, ({xs})| {{ {nest} {writes} x }};\n\
         f(1, ({zeros}))\n"
    )
}

#[cfg(target_os = "linux")]
#[test]
fn a_binding_meets_a_nest_of_copies_once() {
    const BILLION: f64 = 1e9;
    let cost = |b, k| {
        let dir = tempfile::tempdir().expect("tempdir");
        let work = Instructions::count()
            .expect("count instructions (kernel.perf_event_paranoid must be 2 or lower)");
        let ran = graphix(
            dir.path(),
            &nested_copies_beside_bindings(b, k),
            &["--no-cache", "--check"],
            &[],
        );
        let done = work.read();
        assert!(ran.status.success(), "{}\n{}", ran.stdout, ran.stderr);
        done
    };
    let (small, big) = (cost(8, 8), cost(100, 500));
    let extra = big.saturating_sub(small) as f64 / BILLION;
    eprintln!(
        "8 bindings over 8 cells: {:.2}G, 100 over 500: {:.2}G",
        small as f64 / BILLION,
        big as f64 / BILLION
    );
    assert!(
        extra < 5.0,
        "100 bindings over a nest of 500 copied cells cost {extra:.2}G more than 8 over 8"
    );
}
