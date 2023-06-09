//! Request handlers for Erland Server.

use std::{collections::HashMap, sync::Arc};

use axum::extract::ws::{Message, WebSocket};
use futures::{stream::SplitSink, SinkExt};
use tokio::{fs, sync::Mutex};

use crate::{
    messaging::{PlaygroundEnvironment, PlaygroundMessage, PlaygroundRequest},
    playground_path, result, send_err, send_ok,
};

mod elixir;
mod erlang;
mod gleam;

type WebSocketSender = Arc<Mutex<SplitSink<WebSocket, Message>>>;

/// WebSocket Sender with Request ID for handling multiple concurrent requests.
pub struct WebSocketPack {
    pub id: String,
    pub sender: WebSocketSender,
}

/// Matches request and calls other handlers.
pub async fn handle(PlaygroundRequest { id, message }: PlaygroundRequest, sender: WebSocketSender) {
    let pack = WebSocketPack { id, sender };

    match message {
        PlaygroundMessage::Create { name, env } => handle_create(pack, name, env).await,
        PlaygroundMessage::Update {
            name,
            content,
            dependencies,
        } => handle_update(pack, name, content, dependencies).await,
        PlaygroundMessage::Run(name) => handle_run(pack, name).await,
        PlaygroundMessage::Remove(name) => handle_remove(pack, name).await,
    }
}

/// Handle `create` request.
async fn handle_create(pack: WebSocketPack, name: String, env: PlaygroundEnvironment) {
    // Check name format
    if !check_playground_name(&name) {
        // Send format error
        send_err!(pack, result::Error::Format);
        return;
    }

    // Check if already exists
    if find_environment(&name).await.is_none() {
        // Handle for environment
        let result = match env {
            PlaygroundEnvironment::Erlang => erlang::create(name).await,
            PlaygroundEnvironment::Elixir => elixir::create(name).await,
            PlaygroundEnvironment::Gleam => gleam::create(name).await,
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
    dependencies: Option<HashMap<String, String>>,
) {
    // Check if playground exists
    if let Some(env) = find_environment(&name).await {
        // Handle for environment
        let result = match env {
            PlaygroundEnvironment::Erlang => erlang::update(name, content, dependencies).await,
            PlaygroundEnvironment::Elixir => elixir::update(name, content, dependencies).await,
            PlaygroundEnvironment::Gleam => gleam::update(name, content, dependencies).await,
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
            PlaygroundEnvironment::Gleam => gleam::run(&pack, name).await,
        };

        if let Err(error) = result {
            send_err!(pack, error);
        }
    } else {
        // Send not exist error
        send_err!(pack, result::Error::NotExist);
    }
}

/// Handle `remove` request.
async fn handle_remove(pack: WebSocketPack, name: String) {
    // Check if playground exists
    if find_environment(&name).await.is_none() {
        // Send not exist error
        send_err!(pack, result::Error::NotExist);
    } else {
        // Remove directory
        let path = playground_path!(name);
        let result = fs::remove_dir_all(path)
            .await
            .map_err(|_| result::Error::Filesystem);

        match result {
            Ok(()) => send_ok!(pack),
            Err(error) => send_err!(pack, error),
        };
    }
}

/// Get playground environment from name.
async fn find_environment(name: &str) -> Option<PlaygroundEnvironment> {
    let path = playground_path!(name);

    let Ok(true) = fs::try_exists(&path).await else {
        return None;
    };

    if let Ok(true) = fs::try_exists(format!("{path}/rebar.config")).await {
        return Some(PlaygroundEnvironment::Erlang);
    };

    if let Ok(true) = fs::try_exists(format!("{path}/gleam.toml")).await {
        return Some(PlaygroundEnvironment::Gleam);
    };

    Some(PlaygroundEnvironment::Elixir)
}

/// Check playground name format
fn check_playground_name(name: &str) -> bool {
    let length = name.len();
    if !(3..=16).contains(&length) {
        return false;
    }

    for character in name.chars() {
        if !(character.is_ascii_alphanumeric() || character == '_') {
            return false;
        }
    }

    true
}
