use crate::ServerState;
use lsp_types::{Location, ReferenceParams};

pub fn handle(state: &ServerState, params: ReferenceParams) -> Option<Vec<Location>> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    let include_decl = params.context.include_declaration;
    let locs = state.references(uri, position, include_decl);
    if locs.is_empty() {
        None
    } else {
        Some(locs)
    }
}
