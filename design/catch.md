# try catch is broken

> Status: BUILT — `catch(e) expr` installs a handler for the rest of its block and is not control flow; this is the design note that motivated it.

try exp0 catch(err) exp1 is extremely misleading, it reads like control flow,
when in fact is it is installing an error monitor at a specific scope and
does not change the control flow at all. What we want is instead to install
the monitor with a bottom returning expression to explicitly state to the
user that catching errors is not control flow

## e1

catch(e) expr is always of type bottom
it is valid in or below it's declared scope
an error thrown with ? within calc will end up
at the catch IFF no other catch exists in calc
that handles it. catch may rethrow using ?.

the catch(e) will not handle errors in widget, since
it's scope is the do block let x = { ...

````
let error_display: [null, string] = null;
let x = {
	catch(e) error_display <- "[e]";
	calc(in0)
};
widget([x, text(error_display)])
````

## e2

same rule, the top level catch handles the error IFF
no expression has a catch that handles it

catch(e) receives any uncaught error in expr1 or expr2, but
does not cover expr0

````
expr0;
catch(e) on_toplevel_err(e);
expr1;
expr2;
...
````

This is semantically exactly what we already have, but syntactically it is
no longer misleading, the challange is that we must track catch installations
by scope, we can't just depend on all relevant expressions being contained
within them. I think we can probably do this in the ExecCtx, it will be more
complex than the current implementation, but the payoff is worth it.

## BUILT (2026-08-06)

Implemented as designed, with the ordering/scoping mechanics settled during
planning (see the commit for details):

- `catch(e) expr` / `catch(e: T) expr`, no arrow; type Bottom; statement
  position only (direct child of a block/module body, or a REPL input —
  files are wrapped in one synthetic Do so file toplevel is block position).
  `try` stays reserved and errors with a pointer to the new form.
- A catch opens an IMPLICIT NESTED SCOPE: subsequent siblings compile with
  the dynamic scope extended by a node carrying the handler (lexical path
  unchanged — post-catch exports stay visible), reproducing the old
  nested-try path discipline for all three lookup clocks (Qop compile,
  callsite typecheck, late instance binds). Same-block second catch =
  shadowing; handler rethrow resolves to the predecessor (handlers compile
  before their own registration). AMENDED 2026-08-25: the dynamic scope is
  `DynScope`, a parent-linked chain with ONE NODE PER HANDLER INSTALL and
  nothing else — `Scope::append` (blocks, arms, lambdas, modules) extends
  the lexical path only. It was a flattened `Path` string that every
  block/arm level extended alongside the lexical path, and since an
  instantiated body starts from its CALL SITE's dynamic scope, a recursion
  re-spelled its whole ancestry once per activation: 2GB and 78% of the
  interpreter's cycles at 20k deep (`design/recursive_activations.md`,
  "As built — P1c"). A body that installs no handler now shares its
  caller's scope outright; one that does adds one node per activation,
  which is the chain's legitimate length.
- Blocks/module bodies run catches LAST, INNERMOST FIRST, in update and
  both typecheck passes — the try-era handler-after-body order that makes
  same-cycle Vacant-insert delivery (including inner-handler rethrow) land.
- `catch(e: T)` ascribes `T` to `e` in the handler (2026-09-04). Coverage
  is unchanged: `T` must contain the accumulated throw union (snapshotted
  before ascription, checked at typecheck1). Unannotated `catch(e)` still
  infers `e` from that union.
- The covering node carries (BindId, top) — there is no registry:
  `DynScope::catch()` IS the lookup (`Env.catch`/`lookup_catch` died
  2026-08-25 with the chain; the lambda def gate compiles its body under a
  faux-catch CHILD of the def scope instead of overriding a key and
  restoring it). Cross-top deliveries (REPL: catch in an earlier input)
  take the `set_var` next-cycle path. The shell threads a session scope so
  a toplevel catch covers later inputs.
