# try catch is broken

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
