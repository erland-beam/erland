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

pub type WebSocketSender = Arc<RwLock<SplitSink<WebSocket, Message>>>;

pub struct WebSocketPack {
    pub id: String,
    pub sender: WebSocketSender,
}

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

async fn handle_create(pack: WebSocketPack, name: String, env: PlaygroundEnvironment) {
    if find_environment(&name).await.is_none() {
        let result = match env {
            PlaygroundEnvironment::Erlang => erlang::create(name).await,
            PlaygroundEnvironment::Elixir => elixir::create(name).await,
        };

        match result {
            Ok(()) => send_ok!(pack),
            Err(error) => send_err!(pack, error),
        };
    } else {
        send_err!(pack, result::Error::Exist);
    }
}

async fn handle_update(
    pack: WebSocketPack,
    name: String,
    content: String,
    dependencies: HashMap<String, String>,
) {
    if let Some(env) = find_environment(&name).await {
        let result = match env {
            PlaygroundEnvironment::Erlang => erlang::update(name, content, dependencies).await,
            PlaygroundEnvironment::Elixir => elixir::update(name, content, dependencies).await,
        };

        match result {
            Ok(()) => send_ok!(pack),
            Err(error) => send_err!(pack, error),
        };
    } else {
        send_err!(pack, result::Error::NotExist);
    }
}

async fn handle_run(pack: WebSocketPack, name: String) {
    if let Some(env) = find_environment(&name).await {
        let result = match env {
            PlaygroundEnvironment::Erlang => erlang::run(&pack, name).await,
            PlaygroundEnvironment::Elixir => elixir::run(&pack, name).await,
        };

        if let Err(error) = result {
            send_err!(pack, error);
        }
    } else {
        send_err!(pack, result::Error::NotExist);
    }
}

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
