use crate::state::{Document, ServerState};
use anyhow::Result;
use log::info;
use lsp_server::{Connection, Message, Notification};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    PublishDiagnosticsParams,
};

pub fn did_open(
    connection: &Connection,
    state: &mut ServerState,
    params: DidOpenTextDocumentParams,
) -> Result<()> {
    let uri = params.text_document.uri.clone();
    let version = params.text_document.version;
    let text = params.text_document.text;

    info!("Document opened: {:?}", uri);

    state.documents.insert(uri.clone(), Document::new(text, version));

    // Run diagnostics
    let diagnostics = state.check_document(&uri);

    // Publish diagnostics
    let notification = Notification::new(
        "textDocument/publishDiagnostics".to_string(),
        PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: Some(version),
        },
    );
    connection.sender.send(Message::Notification(notification))?;

    Ok(())
}

pub fn did_change(
    connection: &Connection,
    state: &mut ServerState,
    params: DidChangeTextDocumentParams,
) -> Result<()> {
    let uri = params.text_document.uri.clone();
    let version = params.text_document.version;

    // For full sync, there's only one change with the entire content
    if let Some(change) = params.content_changes.into_iter().next() {
        if let Some(doc) = state.documents.get_mut(&uri) {
            doc.text = change.text;
            doc.version = version;
        } else {
            state.documents.insert(uri.clone(), Document::new(change.text, version));
        }
    }

    info!("Document changed: {:?}", uri);

    // Run diagnostics
    let diagnostics = state.check_document(&uri);

    // Publish diagnostics
    let notification = Notification::new(
        "textDocument/publishDiagnostics".to_string(),
        PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: Some(version),
        },
    );
    connection.sender.send(Message::Notification(notification))?;

    Ok(())
}

pub fn did_close(state: &mut ServerState, params: DidCloseTextDocumentParams) {
    let uri = params.text_document.uri;
    info!("Document closed: {:?}", uri);
    state.documents.remove(&uri);
}
