//! The Graphix language server: diagnostics, completion, hover,
//! go-to-definition, references, symbols and formatting over a
//! [`LspBackend`] that owns the compiler. The shell's `graphix lsp` is
//! the backend that ships.

// CR claude for claude: [structure/low] This crate depends on graphix-compiler, yet
// everything it imports (expr, typ, env, ide, `BindId`, `PrintFlag`, `SourcePosition`,
// `format_with_flags`, `mod_root`) is graphix-types', re-exported by the compiler. The
// code that needs the compiler is graphix-shell's `lsp_backend`. Fix: depend on
// graphix-types and import from `graphix_types::`, so the protocol crate builds beside
// the compiler rather than after it, and `cargo test -p graphix-lsp` does not build
// cranelift. Found by reading; not run. (lsp-07)
mod complete;
mod diagnostics;
mod formatting;
pub mod position;
mod query;
mod server;
mod state;
pub mod stdio;
mod symbols;
pub mod text;
pub mod uri;
pub mod workspace;

pub use lsp_server::Connection;
pub use position::PositionEncoding;
pub use query::display_type;
pub use server::{serve, workspace_roots};
pub use state::{Checked, LspBackend};
