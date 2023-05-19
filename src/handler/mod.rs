use std::{collections::HashMap, sync::Arc};

use crate::{
    messaging::{PlaygroundEnvironment, PlaygroundRequest, PlaygroundResponse},
    result,
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

async fn handle_create(
    WebSocketPack { id, sender }: WebSocketPack,
    name: String,
    env: PlaygroundEnvironment,
) {
    let mut guard = sender.write().await;

    if let Some(_) = find_environment(&name).await {
        let error = result::Error::Exist.to_response(id).to_message();
        guard.send(error).await.ok();

        return;
    }

    let result = match env {
        PlaygroundEnvironment::Erlang => erlang::create(name).await,
        PlaygroundEnvironment::Elixir => elixir::create(name).await,
    };

    match result {
        Ok(()) => guard
            .send(PlaygroundResponse::ok(id).to_message())
            .await
            .ok(),
        Err(error) => guard.send(error.to_response(id).to_message()).await.ok(),
    };
}

async fn handle_update(
    WebSocketPack { id, sender }: WebSocketPack,
    name: String,
    content: String,
    dependencies: HashMap<String, String>,
) {
    let mut guard = sender.write().await;

    if let Some(env) = find_environment(&name).await {
        let result = match env {
            PlaygroundEnvironment::Erlang => erlang::update(name, content, dependencies).await,
            PlaygroundEnvironment::Elixir => elixir::update(name, content, dependencies).await,
        };

        match result {
            Ok(()) => guard
                .send(PlaygroundResponse::ok(id).to_message())
                .await
                .ok(),
            Err(error) => guard.send(error.to_response(id).to_message()).await.ok(),
        };
    } else {
        let error = result::Error::NotExist.to_response(id).to_message();
        guard.send(error).await.ok();
    }
}

async fn handle_run(pack: WebSocketPack, name: String) {
    if let Some(env) = find_environment(&name).await {
        let result = match env {
            PlaygroundEnvironment::Erlang => erlang::run(&pack, name).await,
            PlaygroundEnvironment::Elixir => elixir::run(&pack, name).await,
        };

        if let Err(error) = result {
            pack.sender
                .write()
                .await
                .send(error.to_response(pack.id).to_message())
                .await
                .ok();
        }
    } else {
        let error = result::Error::NotExist.to_response(pack.id).to_message();
        pack.sender.write().await.send(error).await.ok();
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
