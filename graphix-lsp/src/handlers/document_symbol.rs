use crate::ServerState;
use lsp_types::{DocumentSymbolParams, DocumentSymbolResponse};

pub fn handle(
    state: &ServerState,
    params: DocumentSymbolParams,
) -> Option<DocumentSymbolResponse> {
    let symbols = state.document_symbols(&params.text_document.uri);
    if symbols.is_empty() { None } else { Some(DocumentSymbolResponse::Nested(symbols)) }
}
