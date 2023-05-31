//! WebSocket for Erland Server.

use std::sync::Arc;

use axum::{
    extract::{
        ws::{Message, WebSocket},
        WebSocketUpgrade,
    },
    response::IntoResponse,
};
use futures::{stream::StreamExt, SinkExt};
use tokio::sync::Mutex;
use tracing::{debug, info};

use crate::{handler, messaging::PlaygroundRequest, send_raw_packet};

/// WebSocket connection handler.
pub async fn handle(socket: WebSocketUpgrade) -> impl IntoResponse {
    info!("New connection established");
    socket.on_upgrade(execute_loop)
}

/// Waits for messages and calls [`handler::handle`].
async fn execute_loop(socket: WebSocket) {
    let (sender, mut receiver) = socket.split();
    let sender = Arc::new(Mutex::new(sender));

    // Get messages
    while let Some(Ok(message)) = receiver.next().await {
        debug!("Received message:\n{message:?}");

        match message {
            Message::Text(content) => {
                // Ignore if not valid struct
                if let Ok(request) = serde_json::from_str::<PlaygroundRequest>(&content) {
                    // Spawn new task for request
                    let sender = Arc::clone(&sender);
                    tokio::spawn(async { handler::handle(request, sender).await });
                };
            }
            Message::Ping(data) => {
                // Send pong with same data
                send_raw_packet!(sender, Message::Pong(data));
            }
            _ => (),
        }
    }
}
