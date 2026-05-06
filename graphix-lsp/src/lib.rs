//! Graphix language intelligence library.
//!
//! Provides completion, hover, go-to-definition, and diagnostics
//! for graphix source code. Used by both the LSP server (graphix-shell)
//! and the netidx-browser design mode.

mod diagnostics;
mod server;
mod state;
pub mod handlers;
pub mod position;
pub mod uri;
pub mod workspace;

pub use position::PositionEncoding;
pub use server::serve;
pub use state::{Document, LspBackend, ServerState, TypecheckResult};
