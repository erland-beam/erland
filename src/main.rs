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
    // Initialize tracing subscriber
    tracing_subscriber::fmt::try_init().ok();

    // Try creating /tmp/erland
    fs::create_dir("/tmp/erland").await.ok();

    // Start server
    server::start_server(SocketAddr::from(([0, 0, 0, 0], 8080))).await;
}
