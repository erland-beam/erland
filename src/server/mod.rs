use crate::messaging::{PlaygroundResponse, ServerState};

use axum::{
    http::StatusCode,
    routing::{get, post},
    Json, Router,
};
use std::{net::SocketAddr, sync::Arc};
use tokio::sync::{mpsc, Mutex};
use tracing::info;

mod http;

pub type ResponseType = (StatusCode, Json<PlaygroundResponse>);

pub async fn start_server(addr: SocketAddr) {
    let state = init_state();

    let app = Router::new()
        .route("/run", post(http::handle))
        .with_state(state);

    info!("Starting server on {addr:?}");

    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .ok();
}

fn init_state() -> ServerState {
    let (sender, receiver) = mpsc::unbounded_channel();

    ServerState {
        sender,
        receiver: Arc::new(Mutex::new(receiver)),
    }
}
