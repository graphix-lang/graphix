# sep03a pull (2026-09-03) — 5 divergences, one class

Pulled at the sep03b deploy. Campaign tree: a9e7ac9d + the never
intrinsic + the parser reporter (pre step A1).

- aieka/000000, hz0/000000, ryouko/000000, ryouko/000001: ONE CLASS —
  the interp OVER-FIRES a tail loop's base-case select on every
  delivery of an input read only by a structure-failed arm's guard.
  Shape: `let rec f = |n| select n { 0 => select { <block: a let
  over a constant, a nested select over it> } { 0 if m == 0 => …,
  _ => … }, _ => f(n - 1) }` with `m` a level iterating; `count(f(3))`
  is 5 on the interp and 1 on the JIT. Under the shell: `--no-fusion`
  prints the value five times, fused once. The JIT is RIGHT by the
  consulted-guard rule (the `0 if m == 0` arm fails structure, so its
  guard is not consulted and `m` is not a consumed input) and organic
  firing. Minimized witness (graphix-fuzz minimize, 1067 checks):

      {let x = array::iter([true, true, true, i64:4]); let m = x;
       let rec f = |n| select n {i64:0 => select {type Panel = [`D, `Q, `R];
         type Screen = [`Connect, `Panel(Panel)]; let s: Screen = `Panel(`D);
         select s {`Connect => true, `Panel(`Q) => true, `Panel(`D) => true,
         `Panel(`R) => i64:3}} {i64:0 if m == i64:0 => true, _ => true},
         _ => f(i64:0)}; let c = count(f(i64:3)); select x {i64:4 => c, _ => true}}
      interp=Trace([1:true 2:true 3:true 4:i64:5]) jit=Trace([… 4:i64:1])

  Suspected mechanism: the base case's scrutinee is a BLOCK with a
  `let` over a constant; in the tail loop's framed re-derivation each
  pass re-publishes the block-local let as a fresh first production
  (the frame overlay is private per pass), so the nested select's
  scrutinee arrives FIRED, the block's value fires, and the base
  select emits. Family: `findings/tailloop-overfire-jul2026`. Still
  diverges on the A1 tree (fef5d46c). NOT FIXED — off-topic for step
  A; raised with Eric before touching the tail-loop frames.
- katana/000000: AGREEs on the A1 tree (not reproduced).
