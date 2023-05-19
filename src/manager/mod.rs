pub mod erlang;

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

macro_rules! err {
    ($($fmt:expr),*) => {
        axum::Json(crate::messaging::PlaygroundResponse::Error(format!(
            $($fmt),*
        )))
    };
}

pub(super) use err;
pub(super) use shell;
