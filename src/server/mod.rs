//! Erland WebServer.

use axum::{routing::get, Router};
use std::net::SocketAddr;
use tracing::info;

mod websocket;

/// Start server with given address.
pub async fn start_server(addr: SocketAddr) {
    // Build router
    let app = Router::new().route("/", get(websocket::handle));

    info!("Starting server on {addr:?}");

    // Start server
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .ok();
}
