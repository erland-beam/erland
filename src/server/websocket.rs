//! WebSocket for Erland Server.

use crate::{handler, messaging::PlaygroundRequest};

use axum::{
    extract::{
        ws::{Message, WebSocket},
        WebSocketUpgrade,
    },
    response::IntoResponse,
};
use futures::stream::StreamExt;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, info};

/// WebSocket connection handler.
pub async fn handle(socket: WebSocketUpgrade) -> impl IntoResponse {
    info!("New connection established");
    socket.on_upgrade(execute_loop)
}

/// Waits for messages and calls [`handler::handle`].
async fn execute_loop(socket: WebSocket) {
    let (sender, mut receiver) = socket.split();
    let sender = Arc::new(RwLock::new(sender));

    // Get messages
    while let Some(Ok(Message::Text(message))) = receiver.next().await {
        debug!("Received message ->\n    {message}");

        // Ignore if not valid struct
        if let Ok(request) = serde_json::from_str::<PlaygroundRequest>(&message) {
            // Spawn new task for request
            let sender = Arc::clone(&sender);
            tokio::spawn(async { handler::handle(request, sender).await });
        };
    }
}
