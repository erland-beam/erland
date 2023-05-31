//! Macros for Crate.

#[macro_export]
/// Run a shell command and get status.
macro_rules! shell {
    ($command:expr) => {
        tokio::process::Command::new("/bin/bash")
            .arg("-c")
            .arg($command)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .await
            .map_err(|_| $crate::result::Error::Command)
    };
}

#[macro_export]
/// Send an error to WebSocket.
macro_rules! send_err {
    ($pack:expr, $error:expr) => {
        $pack
            .sender
            .lock()
            .await
            .send($error.to_response($pack.id).to_message())
            .await
            .ok()
    };
}

#[macro_export]
/// Send an ok message with No data to WebSocket.
macro_rules! send_ok {
    ($pack:expr) => {
        $pack
            .sender
            .lock()
            .await
            .send($crate::messaging::PlaygroundResponse::ok($pack.id).to_message())
            .await
            .ok()
    };
}

#[macro_export]
/// Send packet to WebSocket
macro_rules! send_packet {
    ($sender:expr, $packet:expr) => {
        $sender.lock().await.send($packet.to_message()).await.ok()
    };
}

#[macro_export]
/// Send a raw packet to WebSocket
macro_rules! send_raw_packet {
    ($sender:expr, $packet:expr) => {
        $sender.lock().await.send($packet).await.ok()
    };
}

#[macro_export]
macro_rules! playground_path {
    ($path:expr) => {
        format!("/tmp/erland/{}", $path)
    };
}
