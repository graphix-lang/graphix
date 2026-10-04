//! Process-lifetime caches for the GRAPHIX_DBG_* / GXDBG_* debug env
//! flags. Each flag is read once per process because several gate
//! prints on hot paths; set them at launch.

macro_rules! dbg_flag {
    ($(#[$m:meta])* $name:ident, $env:literal) => {
        $(#[$m])*
        pub(crate) fn $name() -> bool {
            static F: std::sync::LazyLock<bool> =
                std::sync::LazyLock::new(|| std::env::var_os($env).is_some());
            *F
        }
    };
}

dbg_flag!(graphix_dbg_freeze, "GRAPHIX_DBG_FREEZE");
dbg_flag!(graphix_dbg_invoke, "GRAPHIX_DBG_INVOKE");
dbg_flag!(graphix_dbg_kernels, "GRAPHIX_DBG_KERNELS");
dbg_flag!(graphix_dbg_perf, "GRAPHIX_DBG_PERF");
dbg_flag!(graphix_dbg_region, "GRAPHIX_DBG_REGION");
dbg_flag!(graphix_dbg_select, "GRAPHIX_DBG_SELECT");
dbg_flag!(graphix_dump_clif, "GRAPHIX_DUMP_CLIF");
dbg_flag!(graphix_elab_audit, "GRAPHIX_ELAB_AUDIT");
dbg_flag!(graphix_rigid_audit, "GRAPHIX_RIGID_AUDIT");
dbg_flag!(graphix_no_subst, "GRAPHIX_NO_SUBST");
dbg_flag!(graphix_fuse_serial, "GRAPHIX_FUSE_SERIAL");
dbg_flag!(graphix_par_audit, "GRAPHIX_PAR_AUDIT");
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
