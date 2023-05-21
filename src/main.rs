use clap::{value_parser, Parser};
use std::net::{Ipv4Addr, SocketAddr};
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
    #[arg(short, long, default_value_t = Ipv4Addr::new(0, 0, 0, 0), value_parser = value_parser!(Ipv4Addr))]
    pub bind: Ipv4Addr,

    /// Clean erland directory before startup.
    #[arg(short, long)]
    pub clean: bool,

    /// Print tracing outputs
    #[arg(short, long)]
    pub tracing: bool,
}

#[tokio::main]
async fn main() {
    // Get args
    let args = Args::parse();
    let addr = SocketAddr::from((args.bind.octets(), args.port));

    if args.tracing {
        // Initialize tracing subscriber
        tracing_subscriber::fmt::try_init().ok();
    }

    if args.clean {
        // Try removing /tmp/erland
        fs::remove_dir_all("/tmp/erland").await.ok();
    }

    // Try creating /tmp/erland
    fs::create_dir("/tmp/erland").await.ok();

    // Start server
    server::start_server(addr).await;
}
