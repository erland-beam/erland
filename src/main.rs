use std::net::SocketAddr;
use tokio::fs;

mod handler;
mod macros;
mod messaging;
mod result;
mod server;
mod stream;

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt::try_init().ok();

    fs::create_dir("/tmp/erland").await.ok();
    server::start_server(SocketAddr::from(([0, 0, 0, 0], 8080))).await;
}
