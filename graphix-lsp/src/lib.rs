//! Graphix language intelligence library.
//!
//! Provides completion, hover, go-to-definition, and diagnostics
//! for graphix source code. Used by both the LSP server (graphix-shell)
//! and the netidx-browser design mode.

mod diagnostics;
mod server;
mod state;
pub mod handlers;
pub mod workspace;

pub use server::serve;
pub use state::{Document, LspBackend, ServerState, TypecheckResult};
