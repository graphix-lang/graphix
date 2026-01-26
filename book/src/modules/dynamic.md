# Dynamic Modules

Graphix programs can dynamically load modules at runtime. The loaded code will
be compiled, type checked, and the loader will return an error indicating any
failure in that process. Because Graphix is a statically typed language we must
know ahead of time what interface the dynamically loaded module will have. We do
this by defining a module signature. We can also define what the dynamically
loaded module is allowed to reference, in order to prevent it from just calling
any function it likes (aka it's sandboxed). Lets dive right in with an example,

```graphix
// the module source, which we will publish in netidx
let path = "/local/foo";
let source = "
    let add = |x| x + 1;
    let sub = |x| x - 1;
    let cfg = \[1, 2, 3, 4, 5\];
    let hidden = 42
";
net::publish(path, source)$;

// now load the module
let status = mod foo dynamic {
    sandbox whitelist [core];
    sig {
        val add: fn(i64) -> i64 throws Error<ErrChain<`ArithError(string)>>;
        val sub: fn(i64) -> i64 throws Error<ErrChain<`ArithError(string)>>;
        val cfg: Array<i64>
    };
    source cast<string>(net::subscribe(path)$)$
};
select status {
    error as e => never(dbg(e)),
    null as _ => foo::add(foo::cfg[0]$)
}
```

running this we get,

```
$ graphix test.gx
2
```

In the first part of this program we just publish a string containing the source
code of the module we want to ultimately load. The second part is where it gets
interesting, lets break it down.

`mod foo dynamic` declares a dynamically loaded module named `foo`. In the rest
of our code we can refer (statically) to `foo` as if it was a normal module that
we loaded at compile time. There are three sections required to define a dynamic
module, they are required to be defined in order, sandbox, sig, and source,

- a `sandbox` statement, of which there are three types
  - `sandbox unrestricted;` no sandboxing, the dynamic module can access
    anything in it's scope
  - `sandbox whitelist [item0, item1, ...]` the dynamic module may access ONLY
    the names explicitly listed. e.g. `sandbox whitelist [core::array];` would
    allow the dynamic module to access only `core::array` and nothing else.
  - `sandbox blacklist [item0, item1, ...]` the dynamic module may access
    anything except the names listed. `sandbox blacklist
    [super::secret::module];` everything except super secret module would be
    accessible
- a `sig` statement is the type signature of the module. This is a special
  syntax for writing module type signatures. There are four possible statements,
  - a val statement defines a value and it's type, `val add: fn(i64) -> i64` is
    an example of a val statement, it need not be a function it can be any type
  - a type statement defines a type in the loaded module, e.g. `type T = { foo:
    string, bar: string }` val statements that come after a type statement may
    use the defined type. The type statement is identical to the normal type
    statement in Graphix (so it can be polymorphic, recursive, etc).
  - an abstract type statement declares a type without defining it, e.g. `type T;`
    The loaded module must provide a concrete definition, but that definition is
    hidden from the loading code. This provides encapsulation - see the
    [Abstract Types](./interfaces.md#abstract-types) section for details.
  - a mod statement defines a sub module of the dynamically loaded module. A sub
    module must have a sig. `mod m: sig { ... }` defines a sub module.
- a `source` statement defines where the source code for the dynamic module will
  come from. It's type must be a string.

The `mod foo dynamic ...` expression returns a value of type,

```[null, Error<`DynamicLoadError(string)>]```

The runtime will try to load the module every time the source updates. If it
succeeds it will update with `null`, if it fails it will update with an error
indicating what went wrong. Regardless of the outcome the previous loaded
module, if any, will be deleted. If compilation succeeded the new module will be
initialized, possibly causing values it exports to update.

Obviously the loaded module must match the type signature defined in the dynamic
mod statement. However, the signature checking only cares that every item
mentioned in the signature is present in the dynamic module and that the types
match. If extra items are present in the dynamic module they will simply be
ignored, and will be inaccessible to the loading program.
