//! This module stream a command output to WebSocket.

use std::process::{ExitStatus, Stdio};

use futures::SinkExt;
use tokio::{
    io::{AsyncBufReadExt, BufReader},
    process::Command,
};

use crate::{
    handler::WebSocketPack,
    messaging::{PlaygroundResponse, PlaygroundResponseType},
    result, send_packet,
};

pub async fn run(pack: &WebSocketPack, command: String) -> result::Result<()> {
    // Spawn new child
    let mut child = Command::new("/bin/bash")
        .arg("-c")
        .arg(command)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|_| result::Error::Command)?;

    // Should panic if fails
    let stdout = child.stdout.take().unwrap();
    let stderr = child.stderr.take().unwrap();

    let mut stdout_reader = BufReader::new(stdout).lines();
    let mut stderr_reader = BufReader::new(stderr).lines();

    // Stream child
    loop {
        tokio::select! {
            // Handle stdout
            result = stdout_reader.next_line() => {
                match result {
                    Ok(Some(line)) => output_reader(pack, line).await,
                    Err(_) => break,
                    _ => (),
                }
            }
            // Handle stderr
            result = stderr_reader.next_line() => {
                match result {
                    Ok(Some(line)) => output_reader(pack, line).await,
                    Err(_) => break,
                    _ => (),
                }
            }
            // Handle exit
            result = child.wait() => {
                if let Ok(code) = result {
                    status_reader(pack, code).await;
                }

                break
            }
        }
    }

    Ok(())
}

/// Send output to WebSocket.
async fn output_reader(WebSocketPack { id, sender }: &WebSocketPack, data: String) {
    let packet = PlaygroundResponse {
        id: id.to_owned(),
        r#type: PlaygroundResponseType::Data,
        data: Some(data),
    };

    send_packet!(sender, packet);
}

/// Send status to WebSocket.
async fn status_reader(WebSocketPack { id, sender }: &WebSocketPack, status: ExitStatus) {
    let packet = if status.success() {
        PlaygroundResponse {
            id: id.to_owned(),
            r#type: PlaygroundResponseType::Ok,
            data: None,
        }
    } else {
        PlaygroundResponse {
            id: id.to_owned(),
            r#type: PlaygroundResponseType::Error,
            data: None,
        }
    };

    send_packet!(sender, packet);
}
