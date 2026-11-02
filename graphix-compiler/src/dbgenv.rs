//! Process-lifetime caches for the GRAPHIX_DBG_* / GXDBG_* debug env
//! flags. Each flag is read once per process because several gate
//! prints on hot paths; set them at launch.

use graphix_types::dbg_flag;

dbg_flag!(graphix_dbg_freeze, "GRAPHIX_DBG_FREEZE");
dbg_flag!(graphix_dbg_invoke, "GRAPHIX_DBG_INVOKE");
dbg_flag!(graphix_dbg_kernels, "GRAPHIX_DBG_KERNELS");
dbg_flag!(graphix_dbg_perf, "GRAPHIX_DBG_PERF");
dbg_flag!(graphix_dbg_region, "GRAPHIX_DBG_REGION");
dbg_flag!(graphix_dbg_select, "GRAPHIX_DBG_SELECT");
dbg_flag!(graphix_dump_clif, "GRAPHIX_DUMP_CLIF");
dbg_flag!(graphix_elab_audit, "GRAPHIX_ELAB_AUDIT");
// XCR claude for claude: [test-gap/low] GRAPHIX_NO_SUBST is the A/B for instances by
// substitution, but an instance it checks again at a call's narrower type refuses a
// select arm the definition's check kept: `let g = 'b: [`A(i64), `B(i64)] |x: 'b|
// select x { `A(z) => "ga [z]", _ => "gn" }; (g(`A(3)), g(`B(1)))` prints ("ga 3",
// "gn") by default and is refused "unreachable arm" under the flag (base 31519602c
// and main a084f9887 alike), so the pins
// lang::select::literal_pool_over_a_bounded_variable_counts_its_bound and
// lang::fusion::native_interpolates_a_bind_over_a_bounded_variable_at_one_member fail
// under it in every mode. The re-check should skip the coverage verdicts the
// definition's check owns. probe: ~/ptmp/cr29/scratch/r6-E/r6F-fix/ns.gx
// (no-subst-dead-arm-01)
// 2026-11-01 claude: The second pin named above,
// native_interpolates_a_bind_over_a_bounded_variable_at_one_member, is on the parked
// branch cr35-E (5dbc36a34), not on main;
// literal_pool_over_a_bounded_variable_counts_its_bound shows the failure here.
// 2026-11-01 claude: Partly fixed (lane E, r6G): an instance whose body
// GRAPHIX_NO_SUBST checks again sets CompileCtx::instance_recheck around that check
// (lambda.rs, the Typing::Substituted branch with no types), and Select::typecheck0
// then judges no coverage, dead arms or guards (checking false, as an instance's
// typecheck0_instance already does); the default path never sets it. The probe ns.gx
// prints ("ga 3", "gn") under the flag (main a084f9887 refuses it 'unreachable arm'),
// and under the flag lang::select + lang::types go from 14 failing tests (x4 modes)
// to 7. Still failing, so this stays a CR:
// literal_pool_over_a_bounded_variable_counts_its_bound. In h's instance at (bool,
// i64) the arm 'i64 as n => n * 10' is unreachable; it is no longer refused, but its
// bind n is narrowed to what reaches it, [] (narrow_pattern of nothing), and 'n * 10'
// is refused 'cannot compute [] * i64'. It needs the select's arm loop
// (typecheck0_with) to bind an arm nothing reaches at ⊥, or to leave its body the
// definition's types, which is select narrowing this round leaves alone; the same []
// would meet a default-path instance whose select has no aux row (a shared id), which
// checks with checking false too. The other six under the flag are the instance's own
// check judging the caller's types (design/tvar_constraints.md, Settling):
// generic_select_telling_quantifier_bound_told (string does not contain [string,
// &i64]), generic_slice_binds_beside_keep_check (`Keep(i64) does not contain [i64,
// `Keep(i64)]), partial_struct_empty_cells_a_definition_returns_are_one_member ({ a,
// .. } matches more than one member), cast_of_a_reference_beside_a_filled_application
// and cast_of_a_reference_in_an_instance (can't cast a reference),
// union_bound_open_cell_accepts ($ on i64). Lists:
// ~/ptmp/cr29/scratch/r6-E/r6G/nosubst_before.txt, nosubst_after.txt.
// 2026-11-01 claude: The remaining pin passes: under a NO_SUBST re-check an arm
// nothing reaches at the instance's types (an earlier unguarded wildcard, nothing
// left, or a test that cannot match what is left: Reach::passes_nothing) binds its
// names at ⊥ in Select::typecheck0_with, not at the [] narrowing gives, so its body
// checks ('n * 10' over ⊥ takes i64). The default path never asks (the condition is
// ctx.instance_recheck first).
// lang::select::literal_pool_over_a_bounded_variable_counts_its_bound passes under
// GRAPHIX_NO_SUBST=1 in all four modes and fails there without the change ('cannot
// compute [] * i64'); lang::select + lang::types under the flag: 24 failures (6 tests
// x 4 modes), the six listed in the note above, all the instance's own check judging
// the caller's types, left as they are (list
// ~/ptmp/cr29/scratch/r6-E/r6G2/nosubst_after2.txt). The same class on the default
// path, a labeled default compiled at a bind, is default-bind-coverage-01.
dbg_flag!(graphix_no_subst, "GRAPHIX_NO_SUBST");
dbg_flag!(graphix_fuse_serial, "GRAPHIX_FUSE_SERIAL");
dbg_flag!(graphix_no_outline, "GRAPHIX_NO_OUTLINE");
dbg_flag!(graphix_par_audit, "GRAPHIX_PAR_AUDIT");
dbg_flag!(graphix_abort_on_no_match, "GRAPHIX_ABORT_ON_NO_MATCH");
dbg_flag!(graphix_dbg_par, "GRAPHIX_DBG_PAR");
dbg_flag!(
    #[cfg(debug_assertions)]
    gxdbg_callret,
    "GXDBG_CALLRET"
);
dbg_flag!(gxdbg_cs, "GXDBG_CS");
dbg_flag!(gxdbg_dync, "GXDBG_DYNC");
dbg_flag!(gxdbg_ref, "GXDBG_REF");
dbg_flag!(gxdbg_effect, "GXDBG_EFFECT");
dbg_flag!(gxdbg_freeze_ret, "GXDBG_FREEZE_RET");
dbg_flag!(gxdbg_instance_fusion, "GXDBG_INSTANCE_FUSION");
dbg_flag!(gxdbg_kernel_sleep, "GXDBG_KERNEL_SLEEP");
dbg_flag!(gxdbg_letbind, "GXDBG_LETBIND");
dbg_flag!(gxdbg_slot, "GXDBG_SLOT");
dbg_flag!(gxdbg_kpoll, "GXDBG_KPOLL");
dbg_flag!(gxdbg_native_all, "GXDBG_NATIVE_ALL");
dbg_flag!(gxdbg_refmiss, "GXDBG_REFMISS");
dbg_flag!(gxdbg_resolve, "GXDBG_RESOLVE");
dbg_flag!(gxdbg_seqplan, "GXDBG_SEQPLAN");
dbg_flag!(gxdbg_shallow, "GXDBG_SHALLOW");
