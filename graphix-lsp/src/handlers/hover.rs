use crate::state::ServerState;
use lsp_types::{Hover, HoverParams};

pub fn handle(state: &ServerState, params: HoverParams) -> Option<Hover> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    state.hover(uri, position)
}
