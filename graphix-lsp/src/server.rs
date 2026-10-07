//! The JSON-RPC loop: routes messages to [`ServerState`] and checks
//! what changed whenever the client has nothing more queued.

use crate::{
    formatting,
    position::PositionEncoding,
    state::{Diagnostics, LspBackend, Publish, ServerState},
    uri::uri_to_path,
};
use anyhow::Result;
use log::info;
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, Response};
use lsp_types::{
    CompletionOptions, CompletionParams, CompletionResponse, DidChangeTextDocumentParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, DocumentSymbolParams,
    DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, OneOf,
    PositionEncodingKind, PublishDiagnosticsParams, ReferenceParams, SaveOptions,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, WorkspaceSymbolParams,
};
use serde::{Serialize, de::DeserializeOwned};
use std::{path::PathBuf, sync::Arc};

/// Prefer UTF-32 when the client supports it: our cursor logic counts
/// `char`s, and UTF-16 (the LSP default) diverges on non-BMP characters.
fn select_position_encoding(init: &InitializeParams) -> Option<PositionEncodingKind> {
    let offered = init.capabilities.general.as_ref()?.position_encodings.as_ref()?;
    offered.iter().find(|e| **e == PositionEncodingKind::UTF32).cloned()
}

fn server_capabilities(
    position_encoding: Option<PositionEncodingKind>,
) -> ServerCapabilities {
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
            trigger_characters: Some(vec![".".into(), ":".into(), "#".into()]),
            ..Default::default()
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        ..Default::default()
    }
}

/// The filesystem roots the editor named at `initialize`:
/// `workspaceFolders`, else the deprecated `rootUri`, else `rootPath`.
pub fn workspace_roots(init: &InitializeParams) -> Vec<PathBuf> {
    let folders = init.workspace_folders.iter().flatten();
    let roots: Vec<PathBuf> = folders.filter_map(|f| uri_to_path(&f.uri)).collect();
    if !roots.is_empty() {
        return roots;
    }
    #[allow(deprecated)]
    let root = match (&init.root_uri, &init.root_path) {
        (Some(uri), _) => uri_to_path(uri),
        (None, path) => path.as_ref().map(PathBuf::from),
    };
    root.into_iter().collect()
}

/// Run the LSP server over `connection` until the client requests
/// shutdown (`Connection::stdio()` in the shell, `Connection::memory()`
/// in tests). `make_backend` runs once the `initialize` handshake has
/// named the workspace.
pub fn serve<F>(connection: Connection, make_backend: F) -> Result<()>
where
    F: FnOnce(&InitializeParams) -> Result<Arc<dyn LspBackend>>,
{
    // Two-phase init: read the client's `positionEncodings` before
    // committing to one in our capabilities.
    let (req_id, init_value) = connection.initialize_start()?;
    let init: InitializeParams = serde_json::from_value(init_value)?;
    let encoding = select_position_encoding(&init);
    connection.initialize_finish(
        req_id,
        serde_json::to_value(InitializeResult {
            capabilities: server_capabilities(encoding.clone()),
            server_info: None,
        })?,
    )?;
    let completion =
        init.capabilities.text_document.as_ref().and_then(|td| {
            td.completion.as_ref()?.completion_item.as_ref()?.snippet_support
        });
    let mut state = ServerState::new(
        make_backend(&init)?,
        workspace_roots(&init),
        completion.unwrap_or(false),
        PositionEncoding::from_kind(encoding.as_ref()),
    );
    info!("graphix lsp server initialized");
    for msg in &connection.receiver {
        // Checking waits for a quiet moment, so a burst of edits costs
        // one check; a request the client is waiting on alone sees the
        // text as it stands.
        let idle = connection.receiver.is_empty();
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                if idle {
                    let checked = guarded("a check", || state.flush());
                    publish(&connection, checked.unwrap_or_default())?;
                }
                let response = handle_request(&state, req);
                connection.sender.send(Message::Response(response))?;
            }
            Message::Notification(not) => {
                let what = not.method.clone();
                let cleared = guarded(&what, || handle_notification(&mut state, not));
                publish(&connection, cleared.unwrap_or_default())?;
                if idle {
                    let checked = guarded("a check", || state.flush());
                    publish(&connection, checked.unwrap_or_default())?;
                }
            }
            Message::Response(_) => {}
        }
    }
    Ok(())
}

/// `f`, or `None` when it panics: one bad input never ends the server.
fn guarded<T>(what: &str, f: impl FnOnce() -> T) -> Option<T> {
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(t) => Some(t),
        Err(_) => {
            log::error!("{what} panicked");
            None
        }
    }
}

fn publish(connection: &Connection, diags: Diagnostics) -> Result<()> {
    for Publish { uri, version, diagnostics } in diags {
        let params = PublishDiagnosticsParams { uri, diagnostics, version };
        let not = Notification::new("textDocument/publishDiagnostics".into(), params);
        connection.sender.send(Message::Notification(not))?;
    }
    Ok(())
}

/// Answer `req` with `f` over its params; params that do not parse and
/// an `Err` are error responses, never the end of the server.
fn respond<P: DeserializeOwned, R: Serialize>(
    req: Request,
    f: impl FnOnce(P) -> Result<R>,
) -> Response {
    match serde_json::from_value(req.params) {
        Err(e) => {
            Response::new_err(req.id, ErrorCode::InvalidParams as i32, e.to_string())
        }
        Ok(params) => match guarded(&req.method, || f(params)) {
            Some(Ok(result)) => Response::new_ok(req.id, result),
            Some(Err(e)) => Response::new_err(
                req.id,
                ErrorCode::RequestFailed as i32,
                format!("{e:#}"),
            ),
            None => Response::new_err(
                req.id,
                ErrorCode::InternalError as i32,
                format!("{} panicked", req.method),
            ),
        },
    }
}

fn handle_request(state: &ServerState, req: Request) -> Response {
    // an empty answer is no answer
    fn some<T>(v: Vec<T>) -> Option<Vec<T>> {
        (!v.is_empty()).then_some(v)
    }
    match req.method.as_str() {
        "textDocument/completion" => respond(req, |p: CompletionParams| {
            let at = p.text_document_position;
            let items = state.completions(&at.text_document.uri, at.position);
            Ok(some(items).map(CompletionResponse::Array))
        }),
        "textDocument/hover" => respond(req, |p: HoverParams| {
            let at = p.text_document_position_params;
            Ok(state.hover(&at.text_document.uri, at.position))
        }),
        "textDocument/definition" => respond(req, |p: GotoDefinitionParams| {
            let at = p.text_document_position_params;
            let def = state.definition(&at.text_document.uri, at.position);
            Ok(def.map(GotoDefinitionResponse::Scalar))
        }),
        "textDocument/references" => respond(req, |p: ReferenceParams| {
            let at = p.text_document_position;
            let decl = p.context.include_declaration;
            Ok(some(state.references(&at.text_document.uri, at.position, decl)))
        }),
        "textDocument/documentSymbol" => respond(req, |p: DocumentSymbolParams| {
            let symbols = state.document_symbols(&p.text_document.uri);
            Ok(some(symbols).map(DocumentSymbolResponse::Nested))
        }),
        "workspace/symbol" => respond(req, |p: WorkspaceSymbolParams| {
            Ok(some(state.workspace_symbols(&p.query)))
        }),
        "textDocument/formatting" => respond(req, |p| formatting::handle(state, p)),
        method => {
            info!("unhandled request: {method}");
            let msg = format!("Method not found: {method}");
            Response::new_err(req.id, ErrorCode::MethodNotFound as i32, msg)
        }
    }
}

/// Apply a document notification; what it returns is cleared at once.
fn handle_notification(state: &mut ServerState, not: Notification) -> Diagnostics {
    fn params<P: DeserializeOwned>(not: Notification) -> Option<P> {
        let method = not.method;
        serde_json::from_value(not.params)
            .inspect_err(|e| info!("ignoring malformed {method}: {e}"))
            .ok()
    }
    match not.method.as_str() {
        "textDocument/didOpen" => {
            if let Some(DidOpenTextDocumentParams { text_document: d }) = params(not) {
                state.set_document(d.uri, d.text, d.version);
            }
        }
        "textDocument/didChange" => {
            if let Some(p) = params::<DidChangeTextDocumentParams>(not)
                && let Some(change) = p.content_changes.into_iter().next_back()
            {
                state.set_document(
                    p.text_document.uri,
                    change.text,
                    p.text_document.version,
                );
            }
        }
        "textDocument/didClose" => {
            if let Some(p) = params::<DidCloseTextDocumentParams>(not) {
                return state.close_document(&p.text_document.uri);
            }
        }
        // the disk changed, through the editor or anything else
        "textDocument/didSave" | "workspace/didChangeWatchedFiles" => {
            return state.saved();
        }
        method => info!("unhandled notification: {method}"),
    }
    vec![]
}
