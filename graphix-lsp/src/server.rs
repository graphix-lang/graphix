//! LSP server entrypoint built on top of `lsp_server::Connection`.
//!
//! Wraps the language intelligence in [`crate::ServerState`] with a
//! stdio-based JSON-RPC loop and routes textDocument/* messages to the
//! handlers in [`crate::handlers`].

use crate::{
    handlers,
    state::{LspBackend, ServerState},
};
use anyhow::Result;
use log::info;
use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{
    CompletionOptions, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, HoverProviderCapability,
    InitializeParams, OneOf, PublishDiagnosticsParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
};
use std::path::PathBuf;
use std::sync::Arc;

/// Run the LSP server, communicating over stdin/stdout. Blocks until
/// the client requests shutdown.
///
/// `make_backend` is invoked once after the `initialize` handshake so
/// it can use the client's `rootUri` / `workspaceFolders` to set up
/// project-aware module resolution before the first document arrives.
pub fn serve<F>(make_backend: F) -> Result<()>
where
    F: FnOnce(&InitializeParams) -> Result<Arc<dyn LspBackend>>,
{
    let (connection, io_threads) = Connection::stdio();
    run_server(connection, make_backend)?;
    io_threads.join()?;
    Ok(())
}

fn server_capabilities() -> ServerCapabilities {
    use lsp_types::{SaveOptions, TextDocumentSyncOptions};
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                save: Some(SaveOptions { include_text: Some(false) }.into()),
                ..Default::default()
            },
        )),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
            ..Default::default()
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        ..Default::default()
    }
}

fn run_server<F>(connection: Connection, make_backend: F) -> Result<()>
where
    F: FnOnce(&InitializeParams) -> Result<Arc<dyn LspBackend>>,
{
    let init_value =
        connection.initialize(serde_json::to_value(server_capabilities())?)?;
    let init_params: InitializeParams = serde_json::from_value(init_value)?;
    let backend = make_backend(&init_params)?;
    info!("graphix lsp server initialized");
    let workspace_roots = workspace_roots_from(&init_params);
    let mut state = ServerState::new(backend);
    state.set_workspace_roots(workspace_roots);
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                handle_request(&connection, &mut state, req)?;
            }
            Message::Notification(not) => {
                handle_notification(&connection, &mut state, not)?;
            }
            Message::Response(_) => {}
        }
    }
    Ok(())
}

fn handle_request(
    connection: &Connection,
    state: &mut ServerState,
    req: Request,
) -> Result<()> {
    let req_id = req.id.clone();
    let response = match req.method.as_str() {
        "textDocument/completion" => {
            let params: lsp_types::CompletionParams = serde_json::from_value(req.params)?;
            Response::new_ok(req_id, handlers::completion::handle(state, params))
        }
        "textDocument/hover" => {
            let params: lsp_types::HoverParams = serde_json::from_value(req.params)?;
            Response::new_ok(req_id, handlers::hover::handle(state, params))
        }
        "textDocument/definition" => {
            let params: lsp_types::GotoDefinitionParams =
                serde_json::from_value(req.params)?;
            Response::new_ok(req_id, handlers::definition::handle(state, params))
        }
        "textDocument/documentSymbol" => {
            let params: lsp_types::DocumentSymbolParams =
                serde_json::from_value(req.params)?;
            Response::new_ok(req_id, handlers::document_symbol::handle(state, params))
        }
        "textDocument/references" => {
            let params: lsp_types::ReferenceParams =
                serde_json::from_value(req.params)?;
            Response::new_ok(req_id, handlers::references::handle(state, params))
        }
        _ => {
            info!("unhandled request: {}", req.method);
            Response::new_err(
                req_id,
                lsp_server::ErrorCode::MethodNotFound as i32,
                format!("Method not found: {}", req.method),
            )
        }
    };
    connection.sender.send(Message::Response(response))?;
    Ok(())
}

fn handle_notification(
    connection: &Connection,
    state: &mut ServerState,
    not: Notification,
) -> Result<()> {
    match not.method.as_str() {
        "textDocument/didOpen" => {
            let params: DidOpenTextDocumentParams = serde_json::from_value(not.params)?;
            let uri = params.text_document.uri.clone();
            let version = params.text_document.version;
            let diagnostics = handlers::diagnostics::did_open(
                state,
                uri.clone(),
                params.text_document.text,
                version,
            );
            publish_diagnostics(connection, uri, diagnostics, version)?;
        }
        "textDocument/didChange" => {
            let params: DidChangeTextDocumentParams = serde_json::from_value(not.params)?;
            let uri = params.text_document.uri.clone();
            let version = params.text_document.version;
            if let Some(change) = params.content_changes.into_iter().next() {
                let diagnostics =
                    handlers::diagnostics::did_change(state, uri.clone(), change.text, version);
                publish_diagnostics(connection, uri, diagnostics, version)?;
            }
        }
        "textDocument/didClose" => {
            let params: DidCloseTextDocumentParams = serde_json::from_value(not.params)?;
            handlers::diagnostics::did_close(state, params.text_document.uri);
        }
        "textDocument/didSave" => {
            let _params: DidSaveTextDocumentParams =
                serde_json::from_value(not.params)?;
            // A save means disk now reflects the user's intent —
            // recompile every project so cross-file errors and
            // references update.
            state.recheck_workspace();
        }
        _ => {
            info!("unhandled notification: {}", not.method);
        }
    }
    Ok(())
}

/// Pull filesystem roots out of the editor's `initialize` params.
/// Prefers `workspaceFolders` (multi-root capable), falls back to the
/// deprecated `rootUri`/`rootPath`.
fn workspace_roots_from(init: &InitializeParams) -> Vec<PathBuf> {
    let mut out = Vec::new();
    if let Some(folders) = &init.workspace_folders {
        for folder in folders {
            if let Some(p) = file_uri_to_path(&folder.uri) {
                out.push(p);
            }
        }
    }
    if out.is_empty() {
        #[allow(deprecated)]
        if let Some(uri) = &init.root_uri {
            if let Some(p) = file_uri_to_path(uri) {
                out.push(p);
            }
        }
    }
    if out.is_empty() {
        #[allow(deprecated)]
        if let Some(p) = init.root_path.as_ref() {
            out.push(PathBuf::from(p));
        }
    }
    out
}

fn file_uri_to_path(uri: &Uri) -> Option<PathBuf> {
    let s = uri.as_str();
    s.strip_prefix("file://").map(PathBuf::from)
}

fn publish_diagnostics(
    connection: &Connection,
    uri: Uri,
    diagnostics: Vec<lsp_types::Diagnostic>,
    version: i32,
) -> Result<()> {
    let notification = Notification::new(
        "textDocument/publishDiagnostics".to_string(),
        PublishDiagnosticsParams { uri, diagnostics, version: Some(version) },
    );
    connection.sender.send(Message::Notification(notification))?;
    Ok(())
}
