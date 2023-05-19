use crate::{
    manager::erlang,
    messaging::{PlaygroundEnvironment, PlaygroundMessage, PlaygroundRequest, ServerState},
};

use axum::{extract::State, Json};
use tracing::debug;

pub async fn handle(
    State(state): State<ServerState>,
    Json(payload): Json<PlaygroundRequest>,
) -> super::ResponseType {
    let PlaygroundRequest { id, message } = payload;
    debug!("Incoming request with ID = {id}");

    match message {
        PlaygroundMessage::Create(name, language) => handle_create(name, language).await,
        _ => todo!(),
    }
}

async fn handle_create(name: String, language: PlaygroundEnvironment) -> super::ResponseType {
    match language {
        PlaygroundEnvironment::Erlang => match erlang::create(name).await {
            Ok(response) => response,
            Err(error) => error.as_response(),
        },
        _ => todo!(),
    }
}
