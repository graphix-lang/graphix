use crate::lsp::state::ServerState;
use lsp_types::{CompletionParams, CompletionResponse};

pub fn handle(state: &ServerState, params: CompletionParams) -> Option<CompletionResponse> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;

    let items = state.completions(uri, position);

    if items.is_empty() {
        None
    } else {
        Some(CompletionResponse::Array(items))
    }
}
