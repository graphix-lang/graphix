use crate::ServerState;
use lsp_types::{SymbolInformation, WorkspaceSymbolParams};

pub fn handle(
    state: &ServerState,
    params: WorkspaceSymbolParams,
) -> Option<Vec<SymbolInformation>> {
    let syms = state.workspace_symbols(&params.query);
    if syms.is_empty() { None } else { Some(syms) }
}
