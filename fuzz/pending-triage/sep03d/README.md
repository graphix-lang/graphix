# sep03d pull (2026-09-04) — 1 divergence

Campaign tree: 9d69d2cd (A2: sleep deleted). Pulled at the sep03e deploy.

- katana/000000: interp (0, I64(1)), JIT Timeout — the untaken-arm
  infinite-recursion hang class of sep03c ryouko/000001 (an arm judged
  impure because its recursive call sat inside a kernel). AGREEs on
  d8d3de1e (select purity stamped at analysis over the unfused graph,
  `untaken_infinite_recursion_is_lazy`). Closed.
