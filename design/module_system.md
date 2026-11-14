# The module system: Rust-2018-style names

Status: built 2026-08-22
Pins: `stdlib/graphix-tests/src/lang/modules.rs` (`sig_type_through_use_alias`,
`private_type_in_public_body`, `imported_type_in_body`,
`private_type_union_member`, `use_in_value_position_is_compile_error`,
`declaration_in_value_position_is_compile_error`),
`graphix-types/src/expr/parser/test.rs` (`use_groups`, `use_new_grammar`,
`keyword_rooted_typath`), `graphix-types/src/env.rs` (`mod_root_strips_marked`).

## The invariant

> Each module's namespace is an explicit table — name → resolved
> target, private items included — and that table is the only thing
> ANY resolution consults: immediate, deferred, instance-time, or
> consumer-side.

Resolution is a pure function of `(module, name)`. The table is never
stripped or truncated for the module's own use; the interface (`.gxi`)
gates only lookups that CROSS into the module from outside.

### Why

Before this, name resolution was a function of ambient context:
`use` was OCaml `open` (a search-path injection), and a bare name from
scope `S` walked every enclosing scope to the root, at each level also
searching every module that level had opened. Three consequences, all
bugs the admin-TUI port hit:

1. **Resolution was time- and place-sensitive.** Anything resolving a
   name LATER (per-callsite instance elaboration, `TypeRef::lookup_ref`
   at consumer touch) or from a DIFFERENT scope (interface types read
   by consumers) had to reconstruct the def-site's implicit
   environment, and each reconstruction site was a place to get it
   wrong: a gxi type field spelled through a use-alias broke at
   consumer touch; a module-private type in a public lambda's body
   broke at instance elaboration; a use-imported bare type name in a
   body annotation broke the same way. The def site accepted spellings
   the use site could not resolve.
2. **Position-dependent visibility.** A submodule saw its parent's
   bindings only if the `mod` declaration came after them —
   declaration ORDER was a visibility rule.
3. **Ambient ambiguity.** From inside `netidx_admin::tui`, the path
   `tui::overlay::Layer` meant the root package only because no
   ancestor happened to have a nested `tui`.

The change was made while there was no installed base.

## Surface language

### Paths

Rust 2018 rules, in expression AND type positions (`super::helper(x)`,
`let p: package::tui::Pump = …`). A path's leading segment is one of:

- a **package name** (the package prelude): `tui::line`, `array::len`;
- **`package`** — the current package's root (Rust's `crate`);
- **`self`** — the current module (also names the module itself in a
  group: `use tui::overlay::{self, layer}`);
- **`super`** — the parent module, chainable.

`self`, `super`, `package` and `pub` are fully reserved: full
reservation keeps the path grammar LL-simple and the diagnostics honest
("`package` is a reserved word"), and the no-installed-base window made
the breakage free.

### `use`

`use` imports NAMES into the declaring scope; it is not an open.

```graphix
use tui::line;                       // one name
use tui::{line, span, style};        // group
use tui::overlay::{self, layer};     // the module and an item
use sys::{net, time};                // nesting
use tui::line as tline;              // rename
use tui::*;                          // glob
use super::{answer, identicon};      // relative
use package::tui::pump;              // package-relative
```

An imported name covers every declaration kind that shares it (value,
type, module). Globs are legal and style-discouraged; the one blessed
glob in exemplar code is the widget-module idiom `use gui::text::{self,
*}` — a module and its main function sharing a name is what the
spelling is FOR. `use` is legal inside any block or lambda body,
importing into that lexical scope only. It compiles to a static env
mutation plus a `Nop` node; there is no reactive `Use` node that
un-imports on delete. A `use` in a `.gxi` is a private import shared
with the impl, not a re-export. `pub use` parses and refuses
("re-exports (`pub use`) are not yet supported") so the grammar does
not move twice when re-exports arrive.

The parser retains comments on use-tree entries in `UseComments`, outside
syntax equality and hashing. Each comment records its prefix depth,
whether it stands above an entry, ends its line, or sits below a group,
and its line (`Comment`, which says whether a blank line stands above it
in its block).
A comment on a single path follows its imported name; a group comment
follows its prefix. `UseItem::sorted` moves this metadata to the first
canonical leaf of that prefix. If grouping joins several ending comments
at one prefix, they become leading lines there. The formatter retains
braces around a commented group even when it contains only one name, and
its ornament check verifies the normalized comment records on reparse.
Pins: `comments_in_use_trees` in `expr/format.rs`.

### `mod`

`mod foo;` declares the submodule and introduces the name `foo` — a
declaration, not an import. A `mod` may appear anywhere in the file and
the submodule sees NOTHING of the parent implicitly; it writes `use
super::…` for what it needs. A module's BODY still executes in graph
order, and only module and type names are visible whatever their order:
values are not pre-registered, so a value must be bound before the
`mod` whose body imports it (`mod c; let top = 1;` with `c.gx` doing
`use super::top` is refused, "top not defined").

### Preludes — the only implicit names

1. **The core prelude**: the root items of `core`'s interface plus the
   primitive type names, an implicit glob of `/core` in every module,
   shadowable by declarations and imports.
2. **The package prelude**: every registered package's NAME is a path
   root anywhere (`Env.package_roots`, populated by `defpackage!`'s
   generated `register`). Only the name is implicit, never the items.

### Shadowing and collisions

Precedence, first hit wins, kind-filtered: own declaration → explicit
import → glob → package prelude → core prelude.

- A `use` colliding with an existing same-scope declaration errors at
  the import; a declaration AFTER an import shadows it silently (`let`
  re-binding is idiomatic Graphix and own-first precedence makes it
  well-defined).
- Two explicit imports of one name: error (rename one). The REPL
  compiles with `CFlag::ReplaceImports` so a re-`use` shadows, like a
  `let` re-binding; file modules keep the error.
- Two globs providing one name: allowed at import, **error at first
  use** naming both sources, unless both reach the same item (Rust's rule — glob pairs are common and
  mostly disjoint). This immediately caught two real latent collisions
  the old walk resolved silently by search order: `window` (the gui
  widget vs `array::window`) and `Table` (`gui::data_table` vs
  `sys::net`).
- An explicit import covers only the kinds its target has; a kind miss
  falls through to globs. Load-bearing for `{self, *}`: the module name
  is the explicit entry, the same-named val arrives by glob.
- Path roots consult module-kind entries only, so `let array = …` never
  breaks `array::len` (Rust's locals-don't-block-paths rule). The
  earlier ruling that an item named like a package is refused was
  dropped for this: `tui::list` is a stdlib module named like the
  `list` package, and examples `use tui::list`. Importing a package
  under its own name is a no-op.

Expression-level lexical scoping is untouched: blocks, lambda params,
select-arm binds and local `let`s chain lexically, and the chain stops
at the module root instead of continuing into ancestor modules.

### Declarations are statement-position-only

`use`, static `mod`, `type`, `trait` and `impl` refuse in value
position (a `let` RHS, a call argument, a block's value slot, a select
arm body). They are ⊥-typed with a phantom value channel, and a
value-position one let a connect route runtime values through a ⊥
binding. A `let` refuses in value position too, a block's value slot
excepted: it has no value, and its name would bind in the enclosing
block, past the expression that holds it. A dynamic `mod` stays an expression with a real
`[error, null]` value.

### Interfaces

Unchanged in role: `use`, `type`, `trait` and `mod` in the `.gxi` apply
to the `.gx` automatically (requiring duplication is ceremony), spliced
into its body (`resolver::add_interface_modules`): each after the body's
last declaration of every interface item listed before it, whatever
order the body declares them in, ahead of the body when it declares
none of them, and in interface order among themselves; a type, or a
trait with no default body, reads no value and goes ahead of the body.
One listed after a `val` the body does not declare goes after the body,
which the interface's check refuses for the missing item, unless the
compile fails first: the spliced item, or a body statement reading it,
that reads the missing name is refused as not defined.
The `.gx` may not define a type or trait its interface
defines: only a hidden type (`type T;`) takes a definition there, an
abstract with the declared parameters (`module.rs::check_redefinition`).
The reverse does not hold: a name the `.gxi` writes resolves through
what the `.gxi` imports and declares, the prelude and other modules'
paths, never through the `.gx`'s imports or its own types, so an
interface means one thing whatever its implementation imports. The
interface check resolves each item's names in that view
(`module.rs::interface_env`, `check_interface_names`) and refuses one it
does not give; a script's interface alike. A dynamic module's `sig` is
held to it at the program's check as at each load
(`module.rs::check_sig_decls`), both in the env its statement compiled
in (`Body::Dynamic::sig_env`): a type the script or the module's body
around it declares below the dynamic module is not in that env, and a
sig naming one, by an import or by its path, is refused, "declare it
before its first use". A private
item is visible to the defining module and its descendants, uniformly
for values and types — a private type in a public lambda's body is the
module's own business when the body is instance-elaborated elsewhere.

At run time a `val` and the binding implementing it are one variable,
the val's, from the module's check on (`module.rs::Proxy`, joined in
`StaticCallees`). The two stay two bindings to the checker, each with
its own type, and the runtime reads the implementing binding's id as
the val's (`branch::VarAliases`, given to the runtime and the cycle's
event at `ExecCtx::apply_deferred`; the runtime's store, reader and
notify calls take the `branch::Var` an id names, resolved by each table
a read or delivery consults, `VarAliases::var_of`): the body's `let` publishes to the
val, its readers read it, a `<-` or a write through `&mut` to either
waits in the val's one queue, in the order the writes were made, each
landing on the value the one before it left, and a level written
through either replaces the one waiting before it. A wake's re-raise
of a seq machine's own name in the body (`design/sleep_and_wake.md`) is
taken back by the block around the module, as a block's own is. A `<-` marks the binding it names, and its
typing is that binding's; whether the variable is written reads every
name, so a call through either binds statically only where neither is
written (`CompileCtx::rebinds`) and a woken `let` keeps what was
written through the `val` (`CompileCtx::var_written`); a seq's plan
counts the pair one variable (`design/dependency_summaries.md` §3).

The joins are one table (`StaticCallees`), shared by the runtime and
every branch of a forked cycle, and counted at each change. A cycle
makes one at a dynamic module's load and at each instance of a function
whose body declares the module, so forked siblings can each join. A table
read from it carries that count (`VarAliases::read_at`), and each view
of the runtime, the runtime's own and each forked branch's, takes the
joins again whenever the count has passed its table's
(`ExecCtx::sync_var_aliases`, `StaticCallees::aliases_after`): a branch
that joins reads its own join whatever its siblings read first, and one
that joins nothing may read a sibling's early, which names bindings it
never uses. A branch's table grows (`VarAliases::absorb`), except by
the bindings that left their variables in it or in a branch it merged
(`ForkRt::left`, below), which it stops naming at once. The tables it
read replay into the runtime in the branch's order at the merge, so a
retired body's readers leave before its join does; the runtime keeps the
later reading of two (`VarAliases::older_than`), so a table a sibling
took while a reload compiled, replayed after the reload's own, does not
bring the retired binding back; and once the branches have merged the
runtime reads the joins as they stand and each view gives the cycle's
event its table (`branch::merge_in_order`). Pins: `lang::modules::
a_write_through_a_binding_retired_beside_syncing_siblings_misses_the_val`
and `a_reload_in_a_forked_branch_retires_its_binding_in_the_event`.

A join costs its change, not the table. The table is a persistent map,
and each reading is made from the one before it by the bindings whose
joins changed since (`StaticCallees::aliases_after`,
`VarAliases::updated`), so two readings, and the tables branches make
from them, share every subtree no change between them reached. A table
made from another keeps the pairs it lacks of it, so the runtime's walk
of the bindings that left and a branch's absorb visit those alone where
one table was made from the other. Otherwise, as where a sibling's sync
came between a branch's fork and its own, they visit the pairs outside
the subtrees the two maps share that the other map's node in their place
does not hold (`VarAliases::maybe_lacked`, `env::each_unshared`): the
pairs the changes between the two tables reached, at most the leaf and
the inner nodes on each change's path. An absorb that walks builds on
the larger of the two tables and visits the smaller's
(`VarAliases::with_kept`), so a branch whose reading lost pairs to
retirements does not put each back one at a time. Pins:
`branch::tests::a_table_made_from_another_is_compared_by_what_changed`,
`sibling_tables_are_compared_by_what_changed`,
`a_table_that_lacks_pairs_of_the_kept_one_is_built_on_it`,
`static_callees_tests::a_view_takes_in_only_what_changed_since_its_table`
and `env::test::unshared_pairs_hold_every_difference_and_little_else`.

A reload joins its new body's binding before the body it replaces
retires; while the new source compiles, the replaced body's bindings
are no names of the variable (`StaticCallees::set_aside`), so a write
it made marks nothing the reload sees. The retirement drops the writes
the replaced body made through its binding and had not landed, a
reference to that binding alive or not, while a write made through the
val waiting across the reload lands on the new body.

**A binding that leaves its join is a variable of its own again.** It
leaves at its retirement by a reload, and at its body's deletion (a slot
of a function whose body declares the module) unless values name both
it and its val. Where a function value or a reference still names it
(`Lives::pinned`), it keeps what the val's variable held at that moment
as its own, that cycle's fire included (`RtView::store_own`), and the
readers registered through it go with it, with the fires they are owed
(`GXRt::set_var_aliases`, `alias_reads`). This is the rule of a deleted
slot (`design/activation_state.md`, Closure lifetimes): the capture is
kept alive, its initializer no longer runs.

At a reload (`ExecCtx::forget_binding`) the val goes on as the new
body's. So a function saved from a module is a closure over the
bindings of the body it was made in, whenever it is first called, and
an instance of it alive at the reload is too. After a reload it reads
what that body's bindings last held; a write through the val reaches
the new body alone, and a write through the saved function the retired
binding alone. Code that wants the new body calls through the module
again. A forked branch that retires a binding stops naming it the val's
there and then, and so does each branch that merges it, so a first call
later in the reload's own cycle reads the retired binding too.

**A load keeps what it registered.** The registries hold one record of
a trait, one impl of a trait for a type and one import table a scope, and
the two bodies name them alike (a trait and an abstract type are their
paths), so a reload gives each to the new source (`Trait::unregister`,
`Impl::unregister`, `Env::clear_names_under`). A function made in the
retired body compiles its body in that body's lexical view whenever it is
first called, so the view carries the load (`Lexical::body`, one
`LoadedBody` shared by every definition made in the load): the traits it
declares, with their own dispatchers and defaults; the impls it writes,
with the bindings of the methods they write; and the import tables at
the module's scope and below as its compile left them
(`LoadedBody::keep_names`, at the end of `Module::compile_source`). The
tables are the load's from its own end: every instance of a function
whose body declares the module names the module's scope alike, so another
instance's load scrubs the registry's tables as a reload does. A reload
that fails changes none of them: the body that runs on reads its own.
They are whole at that end: a load's blocks are named by their place in
its source, so a function of the load compiled later imports into the
scopes its definition's check did. The registry's tables serve the
compile in progress. A module declared in a loaded body starts from the
load around it (`LoadedBody::under`), and a scope's table is the
innermost load's around it, the registry's while that load compiles.
`Env::trait_def`, `impls_of`, `find_impl` and name resolution read the
load's own first and the registries after. A call in a loaded body
therefore resolves to the body's own method binding from the start, a
method the impl leaves out to the body's own default, never to the
declared entry's binding (the val's side, which the next body joins): an
instance alive across the reload runs on over the methods it was made
with, and one made later finds them. Their bindings are kept as captures
are: every function made in the load pins the load's dispatchers,
defaults and impl methods (`LoadedBody::dispatch_bindings`, at
`Lambda::update`), a dispatcher deleted while pinned is orphaned as a
binding is (`ExecCtx::delete_dispatcher`), and they go with the last such
function. Outside the body a call resolves to the declared entry as
before, and the core traits, which the runtime consults by a value's
type, read the registries alone: a value prints and compares by the impl
registered now (CR `retired-body-outside-dispatch-01`). Pins:
`lang::modules::
a_function_saved_across_a_reload_dispatches_to_its_own_bodys_methods`,
`.._dispatches_past_a_declared_impl`,
`.._dispatches_an_outside_trait_to_its_own_impl`, `.._keeps_its_imports`,
`a_function_saved_past_another_instances_load_keeps_its_imports`,
`.._reads_its_own_import`, `.._keeps_a_blocks_imports`,
`.._keeps_its_imports_past_a_failed_reload`,
`.._keeps_a_nested_modules_imports`,
`a_function_made_before_another_instances_load_keeps_a_blocks_imports`,
`a_function_saved_across_a_nested_reload_dispatches_to_its_own_bodys_methods`,
`a_dynamic_module_in_a_loaded_body_reads_its_own_imports` and
`lib_tests::leaks::
a_dispatching_function_saved_across_reloads_keeps_nothing_once_dropped`,
`a_function_saved_past_a_declared_impl_keeps_nothing_once_dropped`.

**An instance is made in its call's load.** A function defined outside
the module that a loaded body calls dispatches on that body's types (a
trait-bounded parameter, a trait's default calling its sibling), so its
instance for the call is inside the caller's load: `Lexical::called_in`
gives the definition's view the load of the call. A definition made in a
load keeps its own unless the call's load is inside it (an inner module's
body calling a function of the body around it), since a view holds one
chain of loads and the definition's imports and impls are in its own; a
call from a load beside it still reads the registries (CR
`cross-load-dispatch-01`). A call is often made long after its text
compiled, in a cycle, when the env's view is the root's: a run-time bind,
a collection's slot, a builtin calling a function it was handed. So the
point of the call carries the load: `Scope::load` is the `Lexical::body`
of the view the scope compiled under. `Module::compile_inner` puts it on
the scope of a body's own statements and `lambda::make_init` on an
instance's, every scope derived from one keeps it, and a labeled default
compiles in its definition's view and load. A lambda literal in such an
instance is a definition made in the load, so it pins the load's
dispatch bindings as the body's own functions do: a closure an outside
function returns to a body dispatches to that body's impl when no
function of the body's own is left. Run-time bound instances share
kernels by definition, type and the callees their bodies resolved
(`fusion::fuse_instance`, `lowering::body_fingerprint`), so two loads'
instances of one outside function at one type share none. An instance of
a retired load's function built after the reload binds that load's own
functions at run time and fuses none of those calls (CR
`orphan-fn-late-bind-defuse-01`). An image holds no scope with a load
(`image::scope_encode`). Pins: `lang::modules::
a_function_saved_across_a_reload_dispatches_an_outside_function_to_its_own_impl`,
`.._an_outside_default_to_its_own_impl`,
`.._an_outside_function_to_its_own_i64_impl`,
`.._a_function_it_binds_at_run_time_to_its_own_impl`,
`.._a_collections_callback_to_its_own_impl`,
`.._a_builtins_callback_to_its_own_impl`,
`.._a_defaults_call_to_its_own_impl`,
`.._an_outside_union_call_to_its_own_impls`,
`.._an_outside_impls_method_to_its_own_impl`,
`.._a_seq_steps_call_to_its_own_impl`,
`a_function_saved_across_a_reload_binds_an_outside_function_the_next_body_implements_nothing_for`,
`a_function_saved_across_a_reload_shares_no_run_time_bind_with_the_next_body`,
`a_closure_an_outside_function_made_for_a_reloaded_body_dispatches_to_that_bodys_impl`,
`a_function_saved_across_a_nested_reload_dispatches_the_outer_loads_function_to_its_own_impl`
and `lib_tests::leaks::
a_saved_function_calling_an_outside_function_keeps_nothing_once_dropped`.

At a deletion the val's binding is deleted with the body's, as a
`let`'s is (`ExecCtx::delete_binding`), and the pair is what one plain
`let` would be. Where values name each of the two (a function the body
made and one made outside it over `m::v`), they stay one variable
(`StaticCallees::keep`, `ExecCtx::part_deleted_pair`): a write through
either reaches the readers of both. When the last value naming one of
them goes, the other keeps the variable's value
(`ExecCtx::release_kept`); until then the orphan collection walks that
value once, as the val's. Where values name the body's binding alone it
leaves with the value, and a write made through it that has not landed
lands on it (`Waiting::move_from`); a reload drops such a write, above.

The kept value goes with the last function value or reference that
names the binding (`ExecCtx::release_references`): nothing of a module
outlives it but what a value still names. Pins: `lang::modules::
a_function_saved_across_a_reload_reads_the_retired_binding`,
`a_function_first_called_in_its_reloads_cycle_reads_the_retired_binding`,
`a_function_escaping_a_deleted_interface_module_keeps_its_binding`,
`both_sides_of_a_deleted_interface_module_stay_one_variable` and
`a_write_waiting_at_an_interface_modules_deletion_lands` (each against
the same program over plain lets), `lib_tests::leaks::
a_function_saved_across_reloads_keeps_nothing_once_dropped`,
`a_function_escaping_deleted_interface_modules_keeps_nothing_once_dropped`,
`both_sides_of_deleted_interface_modules_keep_nothing_once_dropped`,
`a_held_function_outlives_a_collected_reader_of_its_val`
and graphix-shell's `retired_binding_image` (cold and from an image).

What the interface declared goes with the module too (`SigDecls`,
`Module::delete`). A declared impl's method bindings are deleted as the
vals are and its entry leaves the registry; a declared trait's
dispatchers are deleted as a body's are (`ExecCtx::delete_dispatcher`)
and its record is given back; and every name the interface made at the
module's scope or below leaves the lexical maps
(`Env::unbind_lexical_under`). So a module an embedder drops is declared
again by the next compile, its core-trait impls included. A reload of a
source that declares the module gives the record and the entries back
before the new source compiles (`Module::unregister`), as a body's own
`trait` and `impl` do, and the module's delete, which follows, leaves the
new load's: a declared impl is its load's (`ImplDef::same_definition`
compares the origin, since every load names the declaration's block
alike). The load that reload replaces keeps what the registries gave
back (`LoadedBody::give`: the record with its defaults, the declared
impl), so a function made in it still dispatches through the module's
trait wherever it is first called; its functions pin the interface's
dispatchers and impl methods from the load's compile
(`LoadedBody::declare`), and a reload that fails gives nothing
(`LoadedBody::revive`). The import tables at scopes that cross a body
go with their input ("A body's table goes with its input", below); the
one at the module's own path stays (CR `root-module-table-01`). Pins:
`lib_tests::leaks::a_dropped_module_takes_its_declared_core_impl`,
`a_dropped_module_takes_its_declared_trait_and_impl`,
`a_dropped_interface_module_takes_what_its_interface_declared`,
`a_dropped_root_module_is_declared_again`,
`a_function_saved_across_reloads_around_an_interface_module_keeps_nothing_once_dropped`,
`lang::image::a_restored_module_dropped_is_declared_again` and
`lang::modules::
a_reload_around_an_interface_module_declares_its_impl_again`,
`a_reload_around_a_declared_core_impl_declares_it_again`,
`a_function_saved_across_a_reload_around_an_interface_module_dispatches_to_its_own`,
`a_failed_reload_around_an_interface_module_leaves_its_interface_registered`.

**A function keeps the trait it dispatches through.** A function value
may outlive the declaration of a trait its body calls: an embedder drops
the expression that declared it and keeps the function in a root
binding. The function builds its body in its definition's view whenever
it is next called, so what that view names must still mean what it did.
A function names a dispatcher as it names any capture, so a dispatcher
deleted while a value names it is orphaned
(`ExecCtx::delete_dispatcher`), and those dispatchers keep the rest
(`Env::kept_traits`, a `KeptTrait` per deleted declaration, under each
of its dispatchers a value names). `ExecCtx::keep_trait` opens one
before a declaration is deleted, a `trait` statement's at
`Trait::delete` and an interface's at `Module::delete`. It takes the
record of record with its defaults, the impl entries the registry holds
for the record at that moment, and the typedefs those signatures and
heads name, which a resolution cell holds only weakly. An impl still
registered then is kept whenever it is dropped later: one in another
input, or one a module's interface declares of a trait outside the
module. A declaration that is not the record opens none: a retired
load's is kept by the load (`LoadedBody`), and an interface's splice
into its module's body is kept with the interface's. Each such dispatcher holds the bindings a call through it
reaches, the defaults and the impls' methods, as a function value holds
its captures (`Lives::hold`), so they are orphaned when their own
declarations go, whichever function was made first. The collection of
orphans counts what a holder holds as pinned by the holder
(`Lives::collect`), so a default that calls its own trait's dispatcher
is collected with it. A dispatcher deleted while no value names it
releases its share and what it held at once, one orphaned releases them
when the last value goes (`Env::release_dispatcher`), and the
`KeptTrait` goes with the last share. A view reads a kept declaration
only where it names it: its binding at the trait's path is one of the
declaration's dispatchers, the interface's or the ones the splice into
the module's body minted (`Env::kept_trait`). There `Env::trait_def`
gives the kept record and `impls_of` and `find_impl` the kept entries
ahead of the registered ones, after a load's own. So a trait and an impl
declared again at the same path serve the functions made under them and
leave the functions of the dropped ones alone, whether the head is an
abstract type or not. Two cases are not kept: an impl entry given back
while its trait's declaration still stands (CR
`escaped-fn-dropped-impl-01`), and a declaration none of whose
dispatchers a value names, which a function that writes the trait only
as a bound still needs (CR `escaped-fn-dropped-bound-01`). Pins:
`lib_tests::leaks::
a_function_escaping_a_dropped_block_dispatches_to_its_impl`,
`.._to_its_defaults`,
`a_function_escaping_a_dropped_interface_module_dispatches_to_its_impl`,
`a_function_over_a_dropped_interface_modules_trait_dispatches_to_its_impl`,
`a_function_escaping_a_dropped_dynamic_module_dispatches_to_its_impl`,
`a_function_over_a_dropped_dynamic_modules_trait_dispatches_to_its_impl`,
`a_function_escaping_a_dropped_load_around_an_interface_module_dispatches`,
`a_function_escaping_a_dropped_trait_dispatches_past_its_redeclaration`,
`a_function_escaping_dropped_impls_of_plain_heads_dispatches_past_their_redeclaration`,
`a_function_outliving_another_inputs_trait_dispatches_to_its_impl`,
`a_function_made_before_an_impl_dispatches_to_it_once_both_are_dropped`,
`a_function_over_an_interfaces_impl_of_a_dropped_outside_trait_dispatches`
and `reference::test::a_holder_pins_what_it_holds_until_it_goes`.

### A `val`'s type variables

A `val` is a scheme its uses copy: every val whose type holds a variable
is generalized (`Env::poly_binds`), and a read of a function-typed val
and a call through a function type a val holds instantiate the
variables standing inside function types. A variable outside every
function type (`val v: Array<'a>`) no use copies, so a
write at one type and a read at another would share one cell: the
module's check refuses such a val, "declare the val at one type"
(`module.rs::check_val_variables` over the val's binding, a variable
counted whatever a use has bound it to, so a sibling's check decides
nothing here; `Type::variable_outside_functions`: a typedef as its
expansion, an abstract type's parameters and an unresolved name's
arguments outside, a typedef met inside itself over grown parameters
counting its arguments outside). A dynamic module's `sig` is held to it
at the program's check, a script's interface at `check_script_sig`.

`Type::sig_matches` pairs each open implementation cell with the
declaration's type at its place (`SigMatch::paired`). A cell no use of
the implementation copies (a written `let`'s, an outer binding's) is one
type: paired with a type holding a variable a use picks it is refused,
naming the way out (the outer binding's type, the val at one type, a
quantified annotation on the `let`, or, for an implementation that is no
lambda, a lambda, or a literal of lambdas for a val that is no function;
the binding named is one the lambdas the implementation holds reach
through the lambdas they capture: one whose own type is the cell, else
one that holds it in data (a typedef by its expansion, as a file's module
keeps `let s0: H<'b>` written), else one that holds a function and that
something writes, else one that holds it only in functions no lambda of
which captures what holds it, else one that holds it in lambdas it holds
as data; of several alike the nearest, then the one the program writes
first, so its annotation lifts the refusal, `module::Holder::of`). A
binding that holds the cell only in such functions holds one instance (a
call's result: `let k = mk()`, `{h: mk()}`, a generic read of either),
whose annotation lifts nothing where the implementation returns the
cell; it is named with the way out that always works, a lambda in that
function's place (`h: |x| x`), never its type. Pins: `lang::interfaces::
val_capturing_a_struct_holding_a_call_result_names_a_lambda` and its
siblings, `lang::traits::
method_over_a_struct_holding_a_call_result_names_a_lambda`, and through a
typedef `graphix-shell/tests/shell/typedef_holder_remedy.rs`. A written binding
that holds a function (`let k = |x| x; k <- |x| x`, a written read `let
j = s.g; j <- ..`, a written call's result) is no scheme: every read
shares its type, so it is named as written and its lambdas are not
followed, since what they capture lifts nothing while it shares. Its
annotation lifts the refusal only where the implementation hands it the
cell's values and takes none back (`{ k(x); x }` over `k: fn(x: Any) ->
Any`, not `k(x)`). The
lambdas a binding holds are its own, those its data's function types
carry, and those behind a generic binding's read of a function (`let f =
s.g`); a binding whose value is any other function (a call's result,
`let g = mk()`) is one instance whatever its lambdas capture, so it is
not followed. An implementation that is no lambda and
whose lambdas capture nothing holding the cell (a call's result in a
struct update, `{base with h: mk()}`) is told the lambda or the literal
of lambdas, never a binding to annotate; paired with
a type holding none, a
concrete type or a whole quantified function type, the declaration
decides it (`module.rs::hold_paired`), so `val e: B<i64>` over `let e =
B(1)` builds `e` at `B<i64>`. A trait method's body, an impl's and a
default, is held the same way (`traits.rs::implements`). A method that
is a lambda has the cells its definition owns copied by each call, but
an impl's `let` is annotated with the method's signature, which claims
them at the top level; so its cell is one type only where a binding it
captures reaches it. A method whose value is no lambda has its cells
judged by level, as a val's implementation's are. One whose value reads
a binding (a name, or a field or tuple of one) takes no annotation from
the declaration (`traits::signature_of`): its `let` is typed as any
`let` of that read, so a read of a generalized binding (`let put = s.g`,
a default `= s.g`) forwards the scheme (`bind::forwards`), which each
call copies, and the declaration must be the binding's type and no
wider, as a val's must (`let m = f` over `f: fn(s: i64) -> i64` under
`fn(self) -> [i64, null]` is refused; `|s| f(s)`, or the annotation
written on the `let`, takes the declared type). Any other (`(f, 1).0`, a
select, a call's result, a read under a written annotation) is
annotated, so the declaration may be wider than it, and keeps its
value's types, which no call copies.

The implementation's bounds are judged once the pairing is done
(`FnType::sig_bounds`). A conjunct a signature bound matches pairs the
variables inside it, as a position does; one of another shape pairs
nothing, and neither does a union of the bound's length whose members
pair with the bound's in no order (the implementation's bound may be the
wider: ``'q: [`A(Number), 'p]`` under ``'b: [`A(i64), 'a]``). Each
conjunct is then read in
the signature's terms, a paired cell as the signature's type at its
place and any other open cell as a copy whose conjuncts are read so,
with every signature variable rigid, one only its bounds name too, since
each is the caller's to pick: it must follow from a bound of the
signature variable its cell stands for (`Type::implies`), or hold the
signature's type at its place. An open cell that meets a signature
variable takes on nothing of it (`ContainsFlags::Sealed`): it stands for
the variable only where its own conjuncts admit it, so ``val f: fn<'a:
[i64, string], 'b: ['a, `X]>(x: 'b) -> 'b`` over ``'p: [i64, f64], 'q:
['p, `X, `Y]`` is refused and over ``'p: [i64, string]`` holds. A
predicate conjunct (`Singleton`, `NoError`, `OneNumber`, ..) admits the
variable only where one of the variable's bounds implies it, as for a
paired cell: ``'p: Singleton`` stands for ``'a: [i64, string] +
Singleton``, never for ``'a: [i64, string]`` (pins: `lang::interfaces`,
`an_unpaired_variable_*`, `a_variable_inside_a_positions_bound_*`,
`a_paired_variable_*`). Where the cell stands for a union the signature
writes around a variable, the variable is still any type its bounds
admit: ``'q: OneNumber`` holds ``x: ['a, `X]`` under ``'a: Number +
OneNumber``, never under ``'a: Number``, and ``'q: NoError`` holds it
only where a bound of `'a` implies `NoError`; the refusal names the bound
to declare where declaring it is enough, so not beside a number or an
error (`design/tvar_constraints.md`, the two predicates' sections;
pins: `a_one_number_bound_*`, `a_no_error_bound_*`,
`a_one_number_variable_*`, `a_variable_bounded_by_*`,
`a_singleton_variable_*`).

A variable only bounds name has no place of its own at an instance, which
decides it where a bound naming it is held, by what stands there, or as
what its own bounds settle it to (`FnType::held_at`,
`TVar::settle_bounds_first`). The judgment decides it the same way: judged
apart in each bound, it stood for two types that no one instance holds
(``'p: [i64, string] + Singleton, 'q: (Number, 'p), 'r: (Number, 'p)``
under ``'b: (i64, 'a), 'd: (i64, 'c)``, each of `'a` and `'c` a
`Singleton`). Each bound, read in the signature's terms, is walked beside
the part of the signature its holder stands for (`fntyp::Meetings`), which
gives every place the variable is named at and whether an instance holds
exactly that type there. A position's type is exact. What a holder
variable's bound holds is not, since a caller's pick of the holder may be
narrower, and neither is what a union's bare member takes of the other
side's members, which it takes as one type. A place the walk cannot follow
(a function argument built another way, a name it does not expand) counts
as two.

- Named at one place, the variable is what stands there.
- Named at more, it is that type where every place is exact and holds the
  same type.
- Otherwise it is the witness of its own bounds (`TVar::bounds_witness`:
  the bound every other bound of it contains), read into every bound that
  names it, each of which must then hold what its holder stands for: ``'p:
  [i64, string]`` above is `[i64, string]`, and ``'p: Any`` is `Any`.
- With no witness (no bound, a predicate alone, a union under `Singleton`
  or `OneNumber`) the implementation is refused, "is one type wherever a
  bound names it", naming two of the places: the variable is to be named
  at a parameter, as the signature does, or given a bound that is one
  type.

A bound of the signature's own shape pairs its variable and is no place of
its own, so an implementation that restates the signature holds. The
pairing is a place, one under each holder variable whose bound paired it,
where a bound of another shape names the variable too, or where two
holders paired it and a position of the signature reaches the paired
variable through no bound of theirs: a caller's pick elsewhere then
decides that variable, and the holders' picks may differ beneath it. A
variable its own bound does not admit where it is named stands nowhere,
and stays a copy each bound judges apart, which refuses the bound.

An instance that takes one variable as its witness takes every variable
only bounds name that has a witness as its own: `FnType::held_at` settles
them together, once holding each at its place has failed. So beside a
variable that is its witness, each other one with a witness is judged
twice, at its place and as its witness, and every bound naming it must
hold both ways. A bound that names it in a function's argument holds it at
its place and not as the wider witness (``'k: [i64, string], 's: fn(x: 'k,
y: i64) -> i64`` under ``'e: [i64, string], 'g: fn(x: 'e, y: Number) ->
i64``, beside the `'p` above), and is refused, "an instance that takes 'p
as its own bound takes 'k as its own too": one of the two is to be named
at a parameter, or `'k` given no bound that is one type. A method's cells
carry its trait's bounds, so there the second variable may be the trait's
own, which the impl cannot unbound: the impl gives each of the trait's
variables a variable of its own. An instance could hold such an
implementation, `'k` at its place and `'p` as its witness
(`inst-settle-all-01`).

The order a call decides its variables in is not modelled, so some
implementations every instance of which is held are refused: a variable
with no witness at two places of one signature variable that the first of
them decides (`design/tvar_constraints.md`, "refusal does not model which
place of a signature variable a call decides first"). Pins: `lang::interfaces`
`an_unpaired_variable_*_is_one_type` with their twins
(`*_is_its_own_bound`, `*_holds`), `a_variable_paired_by_*`,
`an_unpaired_variable_in_an_argument_must_be_admitted`,
`*_beside_one_at_two_places_*`;
`lang::traits::method_variable_in_two_bounds_*`,
`method_variables_each_in_one_bound_*`.

What is written through a val lands on the implementation, the pair
being one variable, and every use takes the implementation's generic
variables at its own types; so a val something writes must fit its
implementation: a copy of the val's type contained in the
implementation's, the implementation's variables rigid, none taking a
bound its own bounds do not imply (`CompileCtx::check_val_written`).
The copy's variables are solved by every position that meets them: in
an argument one takes the implementation's part as a lower bound rather
than binding to it (`ContainsFlags::LowerInArgs`), so `val f: fn(x: 'a)
-> B<'a>` over `let f = |x| B(x)` (`fn(x: 'x) -> B<'c>`, `'c` above
`'x`) fits, `'a` being `'c`.
Within one compile a `<-` or `&mut` among a block's statements compiles
before the module checks, so the module's check asks it of a val that
is a `<-` target; a later compile's write or `&mut` through a val
already joined asks it as it marks the target
(`CompileCtx::mark_connect_target`). One in a lambda body compiles at
its definition's check, and in a sibling module that check may run
beside the val's module's own: a write there through a sibling's val
(`Env::in_sibling`) is not judged as it marks the target, and once a
block's statements are checked each of its static modules, and the
static modules in them, asks it again of every val a write reached
(`Module::check_written_vals`), so the verdict and its message do not
depend on which check ran first. The val as a
whole is held, not each value written: `m::f <- |x| x` over `val f:
fn(x: i64) -> i64` and a generic implementation is refused too.

A val generic in a variable, wherever its function types stand (a
struct, an array, a typedef's arguments) and whether it writes its own
quantifiers (`fn<'a: ..>`) or not, is read as a copy
(`Type::scheme_read`, which copies a typedef application whose arguments
hold a cell a copy takes, `Type::copied_outside_functions`, as it copies
a function type), and holds a write through it as any generalized
binding does: the value written must fit a copy of the val's type with
its variables rigid, held by cell (two function types the copy took
apart name their variables alike), a variable that stands only in a
quantifier's bound included, and carry none of them out
(`Connect::typecheck0_with`). `CompileCtx::monomorphic`, which makes that
copy, instantiates a typedef's arguments as one, since they stand for
what its body holds. A writable reference to a val is refused where the
val is generic (`CompileCtx::generalized_earlier`, which counts a
declared signature's own quantifiers, its scheme's), and one to a place of it where
the place's type holds such a cell (`crate::holds_generic`, at the
reference's check). A name that aliases a generalized binding and is
written (`let g = m::c; g <- ..`) holds an instance of its own, the
value restriction (`Bind::forwards`).

## Resolution

### The table

`Env.names: Map<ModPath, ScopeNames>`, where a `ScopeNames` is
`imports: name → ImportEntry { scope, name, keyword_anchored }` plus the scope's
glob source list in declaration order. Scope paths are globally
unique, so `names` is a per-context registry of every module's and
block's import table, and it is EXEMPT from `Env::swap_lexical` (the
context's `with_restored`) — never swapped out by the module privacy
swap. That exemption IS the
invariant: `ctx.env.names[def_scope]` equals a captured
`f.env.names[def_scope]` by construction, which is what lets instance
elaboration, `TypeRef::lookup_ref` and the interface bridging walks all
resolve through the DEFINING module's table whenever they run. The
places that used to rely on `Use::delete` scrub explicitly: a dynamic
module recompile clears `names` under its scope; `unbind_scope_subtree`
scrubs it for the LSP.

**A body's table goes with its input.** A scope that crosses a body
(a `#` component) is named by one top-level input's compile and no
other, and the only code that compiles there afterwards runs under a
lexical view made in that input: a definition's (`LambdaDef::env`, at
each new instance) or a module's (`Module::env`, at each load). So
each top-level compile (`compile_top`) makes an `InputHold`, which
every view made while it compiles carries (`Lexical::input`), and a
table made at such a scope is tagged with it, weakly
(`ScopeNames::input`, `Env::names_mut`). The next top-level compile
drops the tables whose input no view holds (`Env::sweep_names`): a
dropped expression's go once the last function that left it is gone,
and not before, since that function builds its body against them at
every new call site. A table at a module's own path or at the root is
no input's and stays: later inputs write it. A view decoded from an
image holds no input, and the tables the image carries are untagged.
A live expression that keeps no view (`{ use str::len; len("ab") }`)
loses its tables at the next top-level compile too: nothing compiles
there again, a type its block imported was resolved while it compiled
(the cell is filled), the unused-import scan runs before the next
compile, and a tool reads the copy of the env its check returned. A
lookup asserts (debug) that a view made in one input reads no other
input's table. Pins: `lib_tests::leaks::
a_dropped_block_takes_its_import_table`,
`a_function_escaping_a_dropped_block_keeps_its_imports_while_held`,
`a_blocks_imported_type_outlives_its_table` and the
`a_dropped_*module*` tests there, whose count includes `env.names`.

### Marked block components

Generated block scope components carry a `#` prefix (`#do123`, `#fn7`,
`#sel9`); identifiers cannot start with `#`, so no module can collide.
`mod_root(scope)` strips trailing `#`-components. This beats testing
membership in `env.modules` because it works in EVERY env with zero
lookups — the modules set is env-relative and a private-env snapshot
need not contain the module's own path.

### The algorithm

A bare name from scope `S`, walking `S`, `dirname(S)`, … to
`mod_root(S)`: own declarations (kind-specific) → explicit imports (one
redirect, kind-checked at the target) → globs (all sources checked,
two distinct hits is the ambiguity error). Past the module root: the
package prelude (module-kind lookups only), then the core prelude.

Qualified paths resolve the leading segment module-kind-only through
the same chain, then descend `modules` with privacy by presence in the
consulted env's view; interior imports of the modules walked are not
consulted (plain `use` is private), except through `self::`/`super::`
roots, which are inside the privacy boundary. The package prelude gates
at the DESCENT: `package_roots` membership alone answers the leading
segment, so a sandboxed dynamic-module env may keep `/sys/net` without
`/sys`.

Keyword roots resolve structurally. `self::x` looks up at
`mod_root(S)`. `package::x` looks up at the package root — `/pkg` when
`S`'s first component is a registered package, else `/` (the program
is the package). A script's top level is the root in every mode (the
check, the LSP, the REPL and the run all compile it there), so
`package::m` reaches a script's file-top `mod m` from any depth, and a
script's `mod` named like a package is a duplicate.
`super` is SCOPE-relative, not module-relative: one `super` from module
`M` anchors at `dirname(M)` and resolves along that anchor's chain (an
entry carries `keyword_anchored: true`; a `super::*` glob expands to one source
per chain level at use time). This is what makes script files work: a
submodule's `use super::x` must reach the file's top-level lets, which
live at the root. Climbing above `/` or a registered package's root is
an error.

### Headers

Before compiling a block's children, `compile_block_children`
pre-registers the block's `mod` NAMES (refusing a duplicate there; each
`mod` statement then compiles with `predeclared` set, so its own
duplicate guard does not trip; a dynamic module written where a value is,
`let st = mod foo dynamic { .. }`, a call's argument or a dynamic module's
source, is pre-registered the same way, found by `declared_modules` without
entering a nested scope, and compiles `predeclared` by
`Env::list_declares_module`), and `bind_sig` pre-registers sig `mod`
items the same way, so declaration order carries no visibility meaning
and a sibling submodule wins over a same-named package (`list::List`
inside tui is the submodule). Values are NOT pre-registered: forward
value references remain errors, because body evaluation order is
semantics in a dataflow language.

### Import compilation

Every MODULE segment of a use path validates eagerly (headers make
forward `mod`s visible). The terminal name may not exist yet
(`use self::sub::x` may legitimately precede `mod sub;` in body order):
the entry is stored and re-checked when the enclosing top-level
statement finishes compiling, so a typo'd import still errors at the
right position. Glob items validate the module path only. Renames key
the entry under the `as` name; `use m` alone stores a module-kind entry.
Use segments accept type names (an uppercase interior segment refuses
at resolution), which `use super::{Client, Response}` needs.

A dynamic module's `source` expression compiles in the ENCLOSING scope:
it is loader-side code, and compiling it under the module's own scope
made `let src = …; mod foo dynamic { … source src }` inexpressible
(inside a block no spelling reaches the block-local; `super::` names
module items only). The sig still binds under the module.

### Diagnostics

The table knows exactly what is in scope, so an unresolved name gets a
real error, an ambiguous glob names both sources, and `resolve_pure`
logs structural resolution errors before mapping them to `None` so an
ambiguity cannot masquerade as "undefined type".

## What was deleted

The ancestor-module visibility walk; `use`-as-open; the `used`
search-path map and its deferred re-resolution of written use names;
positional `mod` visibility and the gxi ordering rule; the reactive
`Use` node; the "use paths are absolute" fiction (root was just the
last ancestor tried).

Not touched: the node graph, the JIT, module SOURCE loading
(`ModuleResolver`/GRAPHIX_MODPATH — file resolution is orthogonal to
name resolution), the dynamic catch scope.

Not built: `pub use` re-exports.

## The rules in brief

**Module loading** is the `ModuleResolver` trait (`expr/resolver.rs`);
`VfsResolver`/`FilesResolver` are in-core, `NetidxResolver` is in
`graphix-package-sys`. `sys::net` owns its netidx through `NetState` in
`ctx.libstate`; `NetHandles` shares the raw publisher/subscriber between
the loader and `NetState`; unseeded contexts get a process-internal
netidx on demand, so programs that never touch `sys::net` have zero
network. The shell library is netidx-agnostic; the CLI is the
netidx-aware embedder (`ShellBuilder::setup_context`,
`resolver_factories`, `GXHandle::with_ctx`).

- **Module system** (`design/module_system.md`): Rust-2018-style
  `use`; every name arrives by declaration, `use`, or prelude;
  `self`/`super`/`package` roots (`package::` is the registered
  package, else `/`, where a script's top level is in every mode);
  declarations, `let` included, are statement-position only. A module
  with an interface compiles its body with its `mod` statement and
  checks it with the statement (`Module::typecheck0`), in a compile task
  of its own; a run of them checks in parallel, after the statements
  before it. Siblings reach each other only through interfaces: an impl
  a body adds undeclared is hidden from its siblings' checks
  (`Env::hidden_impls`) and seen after them, a write through a sibling's
  `val` is judged against its implementation once all are checked
  (`Module::check_written_vals`), and a module's check that
  would write a cell created outside the module is refused and the
  write not made (`tvar::decided`; annotate the binding it would
  decide; `design/parallel_compile.md`); a raise of nothing joins no
  handler (`error::join_raised`), and an error the body raises to the
  function around the module, whose throws are a cell only its check
  writes, is refused: catch it inside the module. A job a compile puts on a shared pool runs
  under its spawner's `tvar::TaskContext` (task, level; the thread's
  foreign-write record set aside), never the thread's: a runtime
  compiles on its own pool (`Control::eval_pool`), and a worker blocked
  in a join runs jobs stolen from any of its compiles (`branch::Live`,
  `check_in_tasks`, `fusion::fuse_each`). Every cell of an interface
  `val`'s function type is generic, a constructor trait's element
  included: a call copies it. A `val`'s type variable stands only
  inside function types (a typedef as its expansion; an abstract
  type's parameter is outside), else it is refused: declare the val at
  one type (`Type::variable_outside_functions`). An implementation cell
  no use copies (a written `let`'s, an outer binding's, a call's
  result's) may not stand where the val, or a trait method's
  declaration, has a variable a use picks; else the declaration decides
  it, as an annotation on the `let` would (`module::hold_paired`; a
  method that is a lambda is judged by the bindings it captures, any
  other method and a val's implementation by level). A val whose type
  holds a variable, its own quantifiers and a typedef's arguments
  included, is generic (`Env::poly_binds`): each read copies it, a
  typedef's arguments as its function types (`Type::scheme_read`), and
  it is held as an earlier compile's generalized binding is above. A val
  something writes
  (`<-`, `&mut`, in any compile) must fit its implementation, whose
  variables are held rigid with their bounds
  (`CompileCtx::check_val_written`).
