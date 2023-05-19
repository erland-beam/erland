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
macro_rules! err {
    ($($fmt:expr),*) => {
        axum::Json(crate::messaging::PlaygroundResponse::Error(format!(
            $($fmt),*
        )))
    };
}
