# Shell static-resolution flap (P4, 2026-07-10)

Repro (release shell, identical runs):

```
{
  let n = 10;
  let result = array::fold(array::init(n, |i| i), 0, |a, x| a + x);
  println("r=[result]")
}
```

`GXDBG_P4=1 graphix <file>` across 6 runs printed instance-fusion
lines on runs {2,3,5,6} and NOTHING on runs {1,4} — on the losing
runs the fold/init call sites never statically resolve, so no
instances fuse and the whole program node-walks (bench/fold_sum:
40s vs the fused loop's microseconds). The debug shell resolves
consistently; the graphix-fuzz harness path is deterministic
(detcheck-one: 6/6 stable shapes). So the race is in the SHELL/RT
compile pipeline, not the compiler proper — but it decides fusion,
which violates predictable performance.

Not yet root-caused: candidates are an ahash iteration order feeding
an order-sensitive resolution step, or a compile-vs-first-cycle race
in GXHandle::load. detcheck can't see it (harness-side); a shell-side
determinism check would.
