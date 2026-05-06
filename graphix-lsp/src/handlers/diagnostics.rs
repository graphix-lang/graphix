use crate::state::Document;
use crate::ServerState;
use log::info;
use std::collections::HashMap;

/// Handle document open: track the document and return per-URI
/// diagnostics. The map always contains the active URI (possibly with
/// an empty list) plus any other file the project compile attributed
/// errors to.
pub fn did_open(
    state: &mut ServerState,
    uri: lsp_types::Uri,
    text: String,
    version: i32,
) -> HashMap<lsp_types::Uri, Vec<lsp_types::Diagnostic>> {
    info!("Document opened: {:?}", uri);
    state.documents.insert(uri.clone(), Document::new(text, version));
    state.check_document(&uri)
}

/// Handle document change: update text and return per-URI diagnostics.
pub fn did_change(
    state: &mut ServerState,
    uri: lsp_types::Uri,
    text: String,
    version: i32,
) -> HashMap<lsp_types::Uri, Vec<lsp_types::Diagnostic>> {
    info!("Document changed: {:?}", uri);
    state.update_document(&uri, text, version);
    state.check_document(&uri)
}

/// Handle document close: stop tracking.
pub fn did_close(state: &mut ServerState, uri: lsp_types::Uri) {
    info!("Document closed: {:?}", uri);
    state.close_document(&uri);
}
