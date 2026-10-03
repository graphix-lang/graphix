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

dbg_flag!(graphix_dbg_bind, "GRAPHIX_DBG_BIND");
dbg_flag!(graphix_dbg_cycle_bt, "GRAPHIX_DBG_CYCLE_BT");
dbg_flag!(graphix_dbg_tval, "GRAPHIX_DBG_TVAL");
dbg_flag!(graphix_profile, "GRAPHIX_PROFILE");
dbg_flag!(graphix_profile_instances, "GRAPHIX_PROFILE_INSTANCES");
dbg_flag!(graphix_task_audit, "GRAPHIX_TASK_AUDIT");
dbg_flag!(gxdbg_typeref, "GXDBG_TYPEREF");

/// The value of GRAPHIX_DBG_BIND_BT: a target TVarId for per-cell
/// write backtraces.
pub(crate) fn graphix_dbg_bind_bt_id() -> Option<&'static str> {
    static V: std::sync::LazyLock<Option<String>> =
        std::sync::LazyLock::new(|| std::env::var("GRAPHIX_DBG_BIND_BT").ok());
    V.as_deref()
}
