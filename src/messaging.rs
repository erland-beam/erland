use serde::{Deserialize, Serialize};
use std::{collections::HashMap, sync::Arc};
use tokio::sync::{
    mpsc::{UnboundedReceiver, UnboundedSender},
    Mutex,
};

#[derive(Clone)]
pub struct ServerState {
    pub sender: UnboundedSender<PlaygroundMessage>,
    pub receiver: Arc<Mutex<UnboundedReceiver<PlaygroundMessage>>>,
}

#[derive(Serialize, Deserialize)]
pub enum PlaygroundEnvironment {
    Erlang,
    Elixir,
}

#[derive(Serialize, Deserialize)]
pub struct PlaygroundRequest {
    pub id: String,
    pub message: PlaygroundMessage,
}

#[derive(Serialize, Deserialize)]
pub enum PlaygroundMessage {
    Create(String, PlaygroundEnvironment),
    Update {
        name: String,
        content: String,
        dependencies: HashMap<String, String>,
    },
    Run(String),
}

#[derive(Serialize, Deserialize)]
pub enum PlaygroundResponse {
    Ok,
    Data { id: String, data: String },
    Error(String),
}
