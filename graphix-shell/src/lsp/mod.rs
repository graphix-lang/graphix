mod handlers;
mod state;

use anyhow::Result;
use log::info;
use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{
    CompletionOptions, InitializeParams, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind,
};
use state::ServerState;

pub fn run() -> Result<()> {
    let (connection, io_threads) = Connection::stdio();
    run_server(connection)?;
    io_threads.join()?;
    Ok(())
}

fn run_server(connection: Connection) -> Result<()> {
    let server_capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::FULL,
        )),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
            ..Default::default()
        }),
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
        definition_provider: Some(lsp_types::OneOf::Left(true)),
        ..Default::default()
    };

    let init_params = connection.initialize(serde_json::to_value(server_capabilities)?)?;
    let _init_params: InitializeParams = serde_json::from_value(init_params)?;

    info!("graphix lsp server initialized");

    let mut state = ServerState::new();

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
            let items = handlers::completion::handle(state, params);
            Response::new_ok(req_id, items)
        }
        "textDocument/hover" => {
            let params: lsp_types::HoverParams = serde_json::from_value(req.params)?;
            let hover = handlers::hover::handle(state, params);
            Response::new_ok(req_id, hover)
        }
        "textDocument/definition" => {
            let params: lsp_types::GotoDefinitionParams =
                serde_json::from_value(req.params)?;
            let location = handlers::definition::handle(state, params);
            Response::new_ok(req_id, location)
        }
        _ => {
            info!("Unhandled request: {}", req.method);
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
            let params: lsp_types::DidOpenTextDocumentParams =
                serde_json::from_value(not.params)?;
            handlers::diagnostics::did_open(connection, state, params)?;
        }
        "textDocument/didChange" => {
            let params: lsp_types::DidChangeTextDocumentParams =
                serde_json::from_value(not.params)?;
            handlers::diagnostics::did_change(connection, state, params)?;
        }
        "textDocument/didClose" => {
            let params: lsp_types::DidCloseTextDocumentParams =
                serde_json::from_value(not.params)?;
            handlers::diagnostics::did_close(state, params);
        }
        _ => {
            info!("Unhandled notification: {}", not.method);
        }
    }

    Ok(())
}
