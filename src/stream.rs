use crate::{
    handler::WebSocketPack,
    messaging::{PlaygroundResponse, PlaygroundResponseType},
    result,
};

use futures::SinkExt;
use std::process::{ExitStatus, Stdio};
use tokio::{
    io::{AsyncBufReadExt, BufReader},
    process::Command,
};

pub async fn run(pack: &WebSocketPack, command: String) -> result::Result<()> {
    let mut child = Command::new("/bin/bash")
        .arg("-c")
        .arg(command)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|_| crate::result::Error::CmdError)?;

    // Should panic if fails
    let stdout = child.stdout.take().unwrap();
    let stderr = child.stderr.take().unwrap();

    let mut stdout_reader = BufReader::new(stdout).lines();
    let mut stderr_reader = BufReader::new(stderr).lines();

    loop {
        tokio::select! {
            result = stdout_reader.next_line() => {
                match result {
                    Ok(Some(line)) => output_reader(pack, line).await,
                    Err(_) => break,
                    _ => (),
                }
            }
            result = stderr_reader.next_line() => {
                match result {
                    Ok(Some(line)) => output_reader(pack, line).await,
                    Err(_) => break,
                    _ => (),
                }
            }
            result = child.wait() => {
                if let Ok(code) = result {
                    status_reader(&pack, code).await;
                }

                break
            }
        };
    }

    Ok(())
}

async fn output_reader(WebSocketPack { id, sender }: &WebSocketPack, data: String) {
    let packet = PlaygroundResponse {
        id: id.to_owned(),
        r#type: PlaygroundResponseType::Data,
        data: Some(data),
    };

    sender.write().await.send(packet.to_message()).await.ok();
}

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

    sender.write().await.send(packet.to_message()).await.ok();
}
