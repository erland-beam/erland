//! Request handlers for Erland Server.

use std::{collections::HashMap, sync::Arc};

use crate::{
    messaging::{PlaygroundEnvironment, PlaygroundRequest},
    result, send_err, send_ok,
};

use axum::extract::ws::{Message, WebSocket};
use futures::{stream::SplitSink, SinkExt};
use tokio::{fs, sync::RwLock};

mod elixir;
mod erlang;

type WebSocketSender = Arc<RwLock<SplitSink<WebSocket, Message>>>;

/// WebSocket Sender with Request ID for handling multiple concurrent requests.
pub struct WebSocketPack {
    pub id: String,
    pub sender: WebSocketSender,
}

/// Matches request and calls other handlers.
pub async fn handle(PlaygroundRequest { id, message }: PlaygroundRequest, sender: WebSocketSender) {
    let pack = WebSocketPack { id, sender };

    match message {
        crate::messaging::PlaygroundMessage::Create { name, env } => {
            handle_create(pack, name, env).await
        }
        crate::messaging::PlaygroundMessage::Update {
            name,
            content,
            dependencies,
        } => handle_update(pack, name, content, dependencies).await,
        crate::messaging::PlaygroundMessage::Run(name) => handle_run(pack, name).await,
    }
}

/// Handle `create` request.
async fn handle_create(pack: WebSocketPack, name: String, env: PlaygroundEnvironment) {
    // Check if already exists
    if find_environment(&name).await.is_none() {
        // Handle for environment
        let result = match env {
            PlaygroundEnvironment::Erlang => erlang::create(name).await,
            PlaygroundEnvironment::Elixir => elixir::create(name).await,
        };

        match result {
            Ok(()) => send_ok!(pack),
            Err(error) => send_err!(pack, error),
        };
    } else {
        // Send already exists error
        send_err!(pack, result::Error::Exist);
    }
}

// Handle `update` request.
async fn handle_update(
    pack: WebSocketPack,
    name: String,
    content: String,
    dependencies: HashMap<String, String>,
) {
    // Check if playground exists
    if let Some(env) = find_environment(&name).await {
        // Handle for environment
        let result = match env {
            PlaygroundEnvironment::Erlang => erlang::update(name, content, dependencies).await,
            PlaygroundEnvironment::Elixir => elixir::update(name, content, dependencies).await,
        };

        match result {
            Ok(()) => send_ok!(pack),
            Err(error) => send_err!(pack, error),
        };
    } else {
        // Send not exist error
        send_err!(pack, result::Error::NotExist);
    }
}

/// Handle `run` request.
async fn handle_run(pack: WebSocketPack, name: String) {
    // Check if playground exists
    if let Some(env) = find_environment(&name).await {
        // Handle for environment
        let result = match env {
            PlaygroundEnvironment::Erlang => erlang::run(&pack, name).await,
            PlaygroundEnvironment::Elixir => elixir::run(&pack, name).await,
        };

        if let Err(error) = result {
            send_err!(pack, error);
        }
    } else {
        // Send not exist error
        send_err!(pack, result::Error::NotExist);
    }
}

/// Get playground environment from name.
async fn find_environment(name: &str) -> Option<PlaygroundEnvironment> {
    let path = format!("/tmp/erland/{name}");

    if let Ok(true) = fs::try_exists(&path).await {
        let rebar_config_path = format!("{path}/rebar.config");

        if let Ok(true) = fs::try_exists(&rebar_config_path).await {
            Some(PlaygroundEnvironment::Erlang)
        } else {
            Some(PlaygroundEnvironment::Elixir)
        }
    } else {
        None
    }
}
