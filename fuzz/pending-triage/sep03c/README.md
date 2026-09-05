# sep03c pull (2026-09-04) — 6 divergences

Campaign tree: 28e42b43 (A1 refinements: the write rule settled,
tracker deleted, `~!`, min/max stateless). Pulled at the sep03d deploy.

- aieka/000000, hz0/000000, katana/000000, mazikeen/000000,
  ryouko/000000: ONE CLASS = the sep03a class (see
  `../sep03a/README.md`): the interp over-fires a tail loop's base-case
  select whose scrutinee is a block holding a `let` (here an inner
  `let rec f` shadowing the outer) on every delivery of the guard's
  input; the JIT fires once, correctly. Minimized:

      {let x = array::iter([true, true, true, i64:0]); let m = x;
       let rec f = |n| select n {i64:0 => select {let rec f = |n: i64|
         select n {i64:0 => `A, _ => `B}; select f(i64:5) {`A => true,
         `B => i64:2}} {i64:0 if m == i64:0 => true, _ => true},
         _ => f(i64:0)}; f(i64:3)}
      interp=Trace([0:true 1:true 2:true 3:true 4:true]) jit=Trace([0:true])

  Still diverges on the A2 tree. NOT a regression from the
  refinements — the pre-A1 class, unfixed; sep03d will re-find it.
- ryouko/000001: interp emits (0, I64(0)), the JIT TIMES OUT. Program:
  `{let rec f = |v| f(v + 1); (|n| select n {0 => 0, _ => {let hit =
  never(); catch(e) hit <- e ~ n; error(\`Boom)?; hit + f(n - 1)}})(0)}`
  — an untaken arm holding an INFINITE tail recursion. A1's rule makes
  an arm that calls a recursive lambda lazy; at the arm's first update
  in FUSED mode the call `f(n - 1)` sat inside a kernel, which the
  purity walk treats as opaque, so the arm's only visible impurity —
  the sample `e ~ n` on the catch's bind — made it always-updated and
  the native loop ran. Two fixes (2026-09-04): a catch's bind counts
  as bound in the arm (`Catch::refs`), and per-arm purity is an
  ANALYSIS fact stamped over the unfused graph
  (`analysis::mark_select_purity`), the first-update computation kept
  as the fallback. Both modes emit 0.
