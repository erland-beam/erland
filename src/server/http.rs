use crate::{
    manager::{erlang, find_environment},
    messaging::{PlaygroundEnvironment, PlaygroundMessage, PlaygroundRequest, ServerState},
};

use axum::{extract::State, http::StatusCode, Json};
use std::collections::HashMap;
use tracing::debug;

pub async fn handle(
    State(state): State<ServerState>,
    Json(payload): Json<PlaygroundRequest>,
) -> super::ResponseType {
    let PlaygroundRequest { id, message } = payload;
    debug!("Incoming request with ID = {id}");

    match message {
        PlaygroundMessage::Create(name, language) => handle_create(name, language).await,
        PlaygroundMessage::Update {
            name,
            content,
            dependencies,
        } => handle_update(name, content, dependencies).await,
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

async fn handle_update(
    name: String,
    content: String,
    dependencies: HashMap<String, String>,
) -> super::ResponseType {
    match find_environment(&name).await {
        None => {
            let error = crate::err!("Playground not found");
            (StatusCode::NOT_FOUND, error)
        }
        Some(PlaygroundEnvironment::Erlang) => {
            match erlang::update(name, content, dependencies).await {
                Ok(response) => response,
                Err(error) => error.as_response(),
            }
        }
        _ => todo!(),
    }
}
