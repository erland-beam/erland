use std::net::{Ipv4Addr, SocketAddr};

use clap::{value_parser, Parser};
use tokio::fs;

mod handler;
mod macros;
mod messaging;
mod result;
mod server;
mod stream;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub(crate) struct Args {
    /// Port to expose.
    #[arg(short, long, default_value_t = 8080)]
    pub port: u16,

    /// IPv4 address to listen.
    #[arg(short, long, default_value_t = Ipv4Addr::new(0, 0, 0, 0), value_parser = value_parser ! (Ipv4Addr))]
    pub bind: Ipv4Addr,

    /// Keeps output clean.
    #[arg(short, long)]
    pub silent: bool,
}

#[tokio::main]
async fn main() {
    // Get args
    let args = Args::parse();

    // Get socket address
    let addr = SocketAddr::from((args.bind.octets(), args.port));

    if !args.silent {
        // Initialize tracing subscriber
        tracing_subscriber::fmt::try_init().ok();
    }

    // Try reset /tmp/erland
    fs::remove_dir_all("/tmp/erland").await.ok();
    fs::create_dir("/tmp/erland").await.ok();

    // Start server
    server::start_server(addr).await;
}
