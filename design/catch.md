# `catch`: an error handler is an install, not control flow

Status: built 2026-08-06 (ascription 2026-09-04)
Pins: `stdlib/graphix-tests/src/lang/errors.rs` (`catch1`, `catch4`,
`catch_positional`, `catch_block_scope`, `catch_in_lambda_throws`,
`catch_repl_cross_input`, `catch_per_activation`,
`catch_in_callee_stays_in_callee`, `catch_through_call`,
`catch_ascription_rethrow`, `catch_ascription_rethrow_no_throw`,
`catch_ascription_too_narrow_is_an_error`,
`catch_connect_union_mismatch_is_an_error`).

## The rule

`catch(e) expr` and `catch(e: T) expr` install an error handler
covering the REST of the enclosing block. The expression has type
Bottom and never produces; it is legal only in statement position (a
direct child of a block or module body, or a REPL input — a file is
wrapped in one synthetic block, so file top level is block position).
An error thrown with `?` reaches the handler iff no nearer handler
along the dynamic (call) chain takes it; a handler may rethrow with
`?`, which resolves to the handler installed before it. A second
`catch` in the same block shadows the first for the statements below
it.

What `?` raises is its error wrapped in an `ErrChain` (the error, its
position and origin, and a `cause`), once: an error that is a chain
already is extended, the old chain becoming the new link's cause,
never nested. Chain-ness is the TYPE's, one test
(`Type::chain_error_type`): core's `ErrChain<E>`, a struct of its four
field names that `ErrChain<E>` contains (`E` its `error` field), or a
union of chains. A site extends only when everything it may raise is a
chain, decided once from its raised type (`SiteValues::wrap_error`),
never from a value's shape: a struct with the names but other types, an
array of a chain's runtime form, and a union of a chain and anything
else are wrapped whole. The types follow the values:
`ErrChain<ErrChain<X>>` is `ErrChain<X>` (`Type::lookup_ref` collapses
it by the same test), so `?` over a generic `Result<T, 'e>` raises
`ErrChain<'e>`, and a call that makes `'e` a chain re-raises that chain
with its history. Pins: `generic_error_raises`,
`generic_reraise_keeps_the_chain`, `raise_wraps_a_chain_lookalike`,
`raise_extends_a_struct_that_fits_a_chain`,
`generic_raise_wraps_a_chain_lookalike`,
`raise_extends_a_union_of_chains`, `mixed_chain_payload_is_one_error`,
`raise_wraps_an_array_of_a_chains_form`.

```graphix
let error_display: [null, string] = null;
let x = {
  catch(e) error_display <- "[e]";
  calc(in0)
};
widget([x, text(error_display)])
```

The handler covers `calc(in0)` and nothing outside the block; a
toplevel `catch(e) on_toplevel_err(e);` between `expr0;` and `expr1;`
covers `expr1` onward, not `expr0`.

### Why not `try e0 catch(e) e1`

The earlier spelling read like control flow when it was installing an
error monitor at a scope and changing no control flow at all. The
install is made explicit by giving it a bottom-returning expression:
the reader sees that catching errors does not produce a value and does
not skip anything. `try` stays reserved and errors with a pointer to
the form (it is now the lead of seq's `try … with`, which IS control
flow — `design/seq_blocks.md` §7).

## Mechanics

- A catch opens an IMPLICIT NESTED SCOPE: the siblings after it compile
  with the dynamic scope extended by one node carrying the handler
  `(BindId, ExprId)`; the lexical path is unchanged, so post-catch
  exports stay visible. All three lookup clocks (Qop compile, callsite
  typecheck, late instance binds) see the same chain.
- The dynamic scope is `DynScope` (`lib.rs`): a parent-linked chain
  with ONE NODE PER HANDLER INSTALL and nothing else. `Scope::append`
  (blocks, arms, lambdas, modules) extends the lexical path only;
  `Scope::with_catch` extends the dynamic one. An instantiated body
  starts from its CALL SITE's dynamic scope, so a body that installs no
  handler shares its caller's chain outright and one that does adds one
  node per activation. The chain replaced a flattened path string that
  every block level re-spelled per activation — 2GB and 78% of the
  interpreter's cycles at 20k recursion depth.
- `DynScope::catch()` IS the lookup; there is no handler registry. The
  lambda def gate compiles its body under a faux-catch CHILD of the def
  scope.
- Blocks and module bodies run catches LAST, INNERMOST FIRST, in update
  and in both typecheck passes, so same-cycle delivery (`deliver_error`'s
  Vacant-insert, including an inner handler's rethrow) lands in the
  cycle the error was raised. Cross-top deliveries (a REPL catch in an
  earlier input) take the `set_var` next-cycle path; the shell threads
  a session scope so a toplevel catch covers later inputs.
- `catch(e: T)` ascribes `T` to `e` in the handler. Coverage is
  unchanged: `T` must contain the accumulated throw union, snapshotted
  before ascription and checked with the check's settle (a
  `PendingSettle::Contains`), never by elaboration; an unannotated `catch(e)`
  infers `e` from that union.
- A handler in a callee stays in the callee; a handler's `?` rethrows
  to the previous install or the next one out.
