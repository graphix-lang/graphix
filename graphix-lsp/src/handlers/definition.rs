use crate::state::ServerState;
use lsp_types::{GotoDefinitionParams, GotoDefinitionResponse};

pub fn handle(state: &ServerState, params: GotoDefinitionParams) -> Option<GotoDefinitionResponse> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let location = state.definition(uri, position)?;

    Some(GotoDefinitionResponse::Scalar(location))
}
