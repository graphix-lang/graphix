//! Process-lifetime caches for the GRAPHIX_DBG_* / GXDBG_* debug
//! env flags. `std::env::var` takes the global env lock and scans
//! environ on every call, and several flags gate prints on
//! typecheck/dispatch hot paths (GRAPHIX_DBG_BIND runs per
//! unification act, GXDBG_CS per CallSite dispatch), so each flag
//! is read once per process. Set flags at launch — mid-process
//! changes are not observed, the same contract as the existing
//! cached flags (GRAPHIX_DBG_VARS in graphix-rt, GRAPHIX_DUMP_CLIF,
//! GRAPHIX_DBG_PERF).

macro_rules! dbg_flag {
    ($name:ident, $env:literal) => {
        pub(crate) fn $name() -> bool {
            static F: std::sync::LazyLock<bool> =
                std::sync::LazyLock::new(|| std::env::var_os($env).is_some());
            *F
        }
    };
}

dbg_flag!(graphix_dbg_bind, "GRAPHIX_DBG_BIND");
dbg_flag!(graphix_dbg_bind_bt, "GRAPHIX_DBG_BIND_BT");
dbg_flag!(graphix_dbg_cycle_bt, "GRAPHIX_DBG_CYCLE_BT");
dbg_flag!(graphix_dbg_depth, "GRAPHIX_DBG_DEPTH");
dbg_flag!(graphix_dbg_freeze, "GRAPHIX_DBG_FREEZE");
dbg_flag!(graphix_dbg_invoke, "GRAPHIX_DBG_INVOKE");
dbg_flag!(graphix_dbg_kernels, "GRAPHIX_DBG_KERNELS");
dbg_flag!(graphix_dbg_region, "GRAPHIX_DBG_REGION");
dbg_flag!(graphix_dbg_select, "GRAPHIX_DBG_SELECT");
dbg_flag!(graphix_dbg_tval, "GRAPHIX_DBG_TVAL");
dbg_flag!(graphix_rigid_audit, "GRAPHIX_RIGID_AUDIT");
dbg_flag!(gxdbg_cs, "GXDBG_CS");
dbg_flag!(gxdbg_effect, "GXDBG_EFFECT");
dbg_flag!(gxdbg_freeze_ret, "GXDBG_FREEZE_RET");
dbg_flag!(gxdbg_instance_fusion, "GXDBG_INSTANCE_FUSION");
dbg_flag!(gxdbg_instcheck, "GXDBG_INSTCHECK");
dbg_flag!(gxdbg_kernel_sleep, "GXDBG_KERNEL_SLEEP");
dbg_flag!(gxdbg_kpoll, "GXDBG_KPOLL");
dbg_flag!(gxdbg_native_all, "GXDBG_NATIVE_ALL");
dbg_flag!(gxdbg_refmiss, "GXDBG_REFMISS");
dbg_flag!(gxdbg_reset, "GXDBG_RESET");
dbg_flag!(gxdbg_resolve, "GXDBG_RESOLVE");
dbg_flag!(gxdbg_swallow, "GXDBG_SWALLOW");

/// GRAPHIX_DBG_BIND_BT doubles as a VALUE read (a target TVarId for
/// per-cell write backtraces) — cached like the flags.
pub(crate) fn graphix_dbg_bind_bt_id() -> Option<&'static str> {
    static V: std::sync::LazyLock<Option<String>> =
        std::sync::LazyLock::new(|| std::env::var("GRAPHIX_DBG_BIND_BT").ok());
    V.as_deref()
}
