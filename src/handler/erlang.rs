use super::WebSocketPack;
use crate::{result, stream};

use std::{collections::HashMap, process::Stdio};
use tokio::fs;

macro_rules! format_command {
    ($path:expr) => {
        format!(
            include_str!("../../include/erlang/init.sh"),
            include_str!("../../include/erlang/run.sh"),
            $path
        )
    };
}

macro_rules! format_rebar_config {
    ($deps:expr) => {
        format!(
            include_str!("../../include/erlang/rebar.config"),
            $deps
                .iter()
                .map(|(key, value)| format!("{{{key}, \"{value}\"}}"))
                .collect::<Vec<_>>()
                .join(", "),
        )
    };
}

macro_rules! format_app_src {
    ($deps:expr) => {
        format!(include_str!("../../include/erlang/testing.app.src"), {
            let mut temp = $deps.keys().cloned().collect::<Vec<_>>();
            temp.extend_from_slice(&["kernel".to_string(), "stdlib".to_string()]);
            temp.join(", ")
        })
    };
}

macro_rules! format_script {
    ($content:expr) => {
        format!(include_str!("../../include/erlang/testing.erl"), $content)
    };
}

pub async fn create(name: String) -> result::Result<()> {
    let path = format!("/tmp/erland/{name}");
    let command = format_command!(&path);

    if crate::shell!(command)?.success() {
        Ok(())
    } else {
        Err(result::Error::CmdError)
    }
}

pub async fn update(
    name: String,
    content: String,
    dependencies: HashMap<String, String>,
) -> result::Result<()> {
    let path = format!("/tmp/erland/{name}");

    let rebar_config_path = format!("{path}/rebar.config");
    let app_src_path = format!("{path}/src/testing.app.src");
    let script_path = format!("{path}/src/testing.erl");

    let rebar_config_content = format_rebar_config!(dependencies);
    let app_src_content = format_app_src!(dependencies);
    let script_content = format_script!(content);

    fs::write(rebar_config_path, rebar_config_content)
        .await
        .map_err(|_| result::Error::FsError)?;

    fs::write(app_src_path, app_src_content)
        .await
        .map_err(|_| result::Error::FsError)?;

    fs::write(script_path, script_content)
        .await
        .map_err(|_| result::Error::FsError)?;

    Ok(())
}

pub async fn run(pack: &WebSocketPack, name: String) -> result::Result<()> {
    let command = format!("cd /tmp/erland/{name} && TERM=dumb ./run.sh");
    stream::run(pack, command).await
}
