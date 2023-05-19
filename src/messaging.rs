use axum::extract::ws::Message;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

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
    Create {
        name: String,
        env: PlaygroundEnvironment,
    },
    Update {
        name: String,
        content: String,
        dependencies: HashMap<String, String>,
    },
    Run(String),
}

#[derive(Serialize, Deserialize)]
pub struct PlaygroundResponse {
    pub id: String,
    pub r#type: usize,
    pub data: Option<String>,
}

impl PlaygroundResponse {
    pub fn to_message(&self) -> Message {
        Message::Text(serde_json::to_string(self).unwrap())
    }

    pub fn ok(id: String) -> PlaygroundResponse {
        Self {
            id,
            r#type: 0,
            data: None,
        }
    }
}
