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
    InitializeParams, OneOf, PositionEncodingKind, PublishDiagnosticsParams,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
};
use std::{path::PathBuf, sync::Arc};

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

/// Prefer UTF-32 when the client supports it: our cursor logic counts
/// `char`s, and UTF-16 (the LSP default) diverges on non-BMP characters.
fn select_position_encoding(init: &InitializeParams) -> Option<PositionEncodingKind> {
    let offered = init.capabilities.general.as_ref()?.position_encodings.as_ref()?;
    offered.iter().find(|e| **e == PositionEncodingKind::UTF32).cloned()
}

fn server_capabilities(
    position_encoding: Option<PositionEncodingKind>,
) -> ServerCapabilities {
    use lsp_types::{SaveOptions, TextDocumentSyncOptions};
    ServerCapabilities {
        position_encoding,
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                save: Some(SaveOptions { include_text: Some(false) }.into()),
                ..Default::default()
            },
        )),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![
                ".".to_string(),
                ":".to_string(),
                "#".to_string(),
            ]),
            ..Default::default()
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        ..Default::default()
    }
}

fn run_server<F>(connection: Connection, make_backend: F) -> Result<()>
where
    F: FnOnce(&InitializeParams) -> Result<Arc<dyn LspBackend>>,
{
    // Two-phase init: read the client's `positionEncodings` before
    // committing to one in our capabilities.
    let (req_id, init_value) = connection.initialize_start()?;
    let init_params: InitializeParams = serde_json::from_value(init_value)?;
    let encoding = select_position_encoding(&init_params);
    info!(
        "negotiated position encoding: {}",
        encoding.as_ref().map(|e| e.as_str()).unwrap_or("utf-16 (default)"),
    );
    connection.initialize_finish(
        req_id,
        serde_json::to_value(lsp_types::InitializeResult {
            capabilities: server_capabilities(encoding.clone()),
            server_info: None,
        })?,
    )?;
    let backend = make_backend(&init_params)?;
    info!("graphix lsp server initialized");
    let workspace_roots = workspace_roots_from(&init_params);
    let snippet_support = init_params
        .capabilities
        .text_document
        .as_ref()
        .and_then(|td| td.completion.as_ref())
        .and_then(|c| c.completion_item.as_ref())
        .and_then(|ci| ci.snippet_support)
        .unwrap_or(false);
    let position_encoding =
        crate::position::PositionEncoding::from_kind(encoding.as_ref());
    let mut state = ServerState::new(backend, snippet_support, position_encoding);
    let initial = state.set_workspace_roots(workspace_roots);
    for (uri, diags) in initial {
        // Project diagnostics for files the user may not have open have no
        // editor-tracked version.
        publish_diagnostics(&connection, uri, diags, None)?;
    }
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
        "workspace/symbol" => {
            let params: lsp_types::WorkspaceSymbolParams =
                serde_json::from_value(req.params)?;
            Response::new_ok(req_id, handlers::workspace_symbol::handle(state, params))
        }
        "textDocument/references" => {
            let params: lsp_types::ReferenceParams = serde_json::from_value(req.params)?;
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
            let updates = handlers::diagnostics::did_open(
                state,
                uri.clone(),
                params.text_document.text,
                version,
            );
            for (target_uri, diags) in updates {
                let v = if target_uri == uri {
                    Some(version)
                } else {
                    state.documents.get(&target_uri).map(|d| d.version)
                };
                publish_diagnostics(connection, target_uri, diags, v)?;
            }
        }
        "textDocument/didChange" => {
            let params: DidChangeTextDocumentParams = serde_json::from_value(not.params)?;
            let uri = params.text_document.uri.clone();
            let version = params.text_document.version;
            if let Some(change) = params.content_changes.into_iter().next() {
                let updates = handlers::diagnostics::did_change(
                    state,
                    uri.clone(),
                    change.text,
                    version,
                );
                for (target_uri, diags) in updates {
                    let v = if target_uri == uri {
                        Some(version)
                    } else {
                        state.documents.get(&target_uri).map(|d| d.version)
                    };
                    publish_diagnostics(connection, target_uri, diags, v)?;
                }
            }
        }
        "textDocument/didClose" => {
            let params: DidCloseTextDocumentParams = serde_json::from_value(not.params)?;
            handlers::diagnostics::did_close(state, params.text_document.uri);
        }
        "textDocument/didSave" => {
            let _params: DidSaveTextDocumentParams = serde_json::from_value(not.params)?;
            // Disk now reflects the user's intent: recompile every project.
            let updates = state.recheck_workspace();
            // Open files get the tracked document version; closed files are
            // sent unversioned.
            for (uri, diags) in updates {
                let version = state.documents.get(&uri).map(|d| d.version);
                publish_diagnostics(connection, uri, diags, version)?;
            }
        }
        _ => {
            info!("unhandled notification: {}", not.method);
        }
    }
    Ok(())
}

/// Filesystem roots from the editor's `initialize` params:
/// `workspaceFolders`, else the deprecated `rootUri`/`rootPath`.
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
    crate::uri::uri_to_path(uri)
}

fn publish_diagnostics(
    connection: &Connection,
    uri: Uri,
    diagnostics: Vec<lsp_types::Diagnostic>,
    version: Option<i32>,
) -> Result<()> {
    let notification = Notification::new(
        "textDocument/publishDiagnostics".to_string(),
        PublishDiagnosticsParams { uri, diagnostics, version },
    );
    connection.sender.send(Message::Notification(notification))?;
    Ok(())
}
