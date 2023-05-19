#[macro_export]
macro_rules! shell {
    ($command:expr) => {
        tokio::process::Command::new("/bin/bash")
            .arg("-c")
            .arg($command)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .await
            .map_err(|_| crate::result::Error::CmdError)
    };
}

#[macro_export]
macro_rules! send_err {
    ($pack:expr, $error:expr) => {
        $pack
            .sender
            .write()
            .await
            .send($error.to_response($pack.id).to_message())
            .await
            .ok()
    };
}

#[macro_export]
macro_rules! send_ok {
    ($pack:expr) => {
        $pack
            .sender
            .write()
            .await
            .send(PlaygroundResponse::ok($pack.id).to_message())
            .await
            .ok()
    };
}
