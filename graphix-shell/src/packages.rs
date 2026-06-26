//! Registration of additional (external / third-party) packages.
//!
//! This file is rewritten by the graphix package manager when external packages
//! are installed, and for standalone builds. The committed default registers
//! nothing: the stdlib is registered by `graphix_shell::deps`, gated by Cargo
//! features. `main.rs` wires these three functions into the `ShellBuilder`
//! hooks. Embedders should ignore this file and call the `ShellBuilder`
//! `register_packages` / `main_program` / `custom_display` hooks directly.

use ahash::AHashMap;
use anyhow::Result;
use arcstr::ArcStr;
use graphix_compiler::{expr::VfsEntry, ExecCtx};
use graphix_package::IndexSet;
use graphix_rt::{GXExt, GXRt};
use graphix_shell::{deps::CustomResult, CustomDisplayFn};
use netidx_core::path::Path;

/// Register additional packages, after the stdlib. The `ShellBuilder`'s
/// `register_packages` hook.
pub fn register<X: GXExt>(
    _ctx: &mut ExecCtx<GXRt<X>, X::UserEvent>,
    _modules: &mut AHashMap<Path, VfsEntry>,
    _root_mods: &mut IndexSet<ArcStr>,
) -> Result<()> {
    Ok(())
}

/// A default program to run (set for standalone builds). The `ShellBuilder`'s
/// `main_program` hook.
pub fn main_program() -> Option<&'static str> {
    None
}

/// Custom-display dispatch for additional packages. The `ShellBuilder`'s
/// `custom_display` hook.
pub fn custom_display<X: GXExt>() -> CustomDisplayFn<X> {
    Box::new(|_gx, _env, e, _run_on_main| {
        Box::pin(async move { Ok(CustomResult::NotCustom(e)) })
    })
}
