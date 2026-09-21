//! The Graphix language server: diagnostics, completion, hover,
//! go-to-definition, references, symbols and formatting over a
//! [`LspBackend`] that owns the compiler. The shell's `graphix lsp` is
//! the backend that ships.

mod complete;
mod diagnostics;
mod handlers;
pub mod position;
mod query;
mod server;
mod state;
mod symbols;
mod text;
pub mod uri;
pub mod workspace;

pub use lsp_server::Connection;
pub use position::PositionEncoding;
pub use server::{serve, workspace_roots};
pub use state::{Checked, LspBackend};
