//! Messaging types for Erland Server.

use axum::extract::ws::Message;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlaygroundEnvironment {
    Erlang,
    Elixir,
    Gleam,
}

#[derive(Deserialize)]
/// PlaygroundMessage with ID.
pub struct PlaygroundRequest {
    pub id: String,
    pub message: PlaygroundMessage,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlaygroundMessage {
    Create {
        name: String,
        env: PlaygroundEnvironment,
    },
    Update {
        name: String,
        content: String,
        dependencies: Option<HashMap<String, String>>,
    },
    Run(String),
    Remove(String),
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlaygroundResponseType {
    Ok,
    Error,
    Data,
}

#[derive(Serialize)]
pub struct PlaygroundResponse {
    pub id: String,
    pub r#type: PlaygroundResponseType,
    pub data: Option<String>,
}

impl PlaygroundResponse {
    pub fn to_message(&self) -> Message {
        Message::Text(serde_json::to_string(self).unwrap())
    }

    pub fn ok(id: String) -> PlaygroundResponse {
        Self {
            id,
            r#type: PlaygroundResponseType::Ok,
            data: None,
        }
    }
}
