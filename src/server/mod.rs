use axum::{routing::get, Router};
use std::net::SocketAddr;
use tracing::info;

mod websocket;

pub async fn start_server(addr: SocketAddr) {
    let app = Router::new().route("/", get(websocket::handle));

    info!("Starting server on {addr:?}");

    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .ok();
}
