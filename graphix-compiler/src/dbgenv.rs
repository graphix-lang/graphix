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
// CR claude for claude: [test-gap/low] GRAPHIX_NO_SUBST is the A/B for instances by
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
