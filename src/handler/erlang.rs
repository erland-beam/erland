//! Erlang handler for Erland Server.

use super::WebSocketPack;
use crate::{playground_path, result, stream};

use std::{collections::HashMap, process::Stdio};
use tokio::fs;

/// Macro for creating initialize script.
macro_rules! format_command {
    ($path:expr) => {
        format!(
            include_str!("../../include/erlang/init.sh"),
            include_str!("../../include/erlang/run.sh"),
            $path
        )
    };
}

/// Macro for creating `rebar.config`.
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

/// Macro for creating `testing.app.src`.
macro_rules! format_app_src {
    ($deps:expr) => {
        format!(include_str!("../../include/erlang/testing.app.src"), {
            let mut temp = $deps.keys().cloned().collect::<Vec<_>>();
            temp.extend_from_slice(&["kernel".to_string(), "stdlib".to_string()]);
            temp.join(", ")
        })
    };
}

/// Macro for creating `testing.erl`.
macro_rules! format_script {
    ($content:expr) => {
        format!(include_str!("../../include/erlang/testing.erl"), $content)
    };
}

/// Create new erlang playground.
pub async fn create(name: String) -> result::Result<()> {
    let path = playground_path!(name);
    let command = format_command!(&path);

    if crate::shell!(command)?.success() {
        Ok(())
    } else {
        Err(result::Error::Command)
    }
}

/// Update erlang playground.
pub async fn update(
    name: String,
    content: String,
    dependencies: Option<HashMap<String, String>>,
) -> result::Result<()> {
    let path = playground_path!(name);

    // File paths
    let rebar_config_path = format!("{path}/rebar.config");
    let app_src_path = format!("{path}/src/testing.app.src");
    let script_path = format!("{path}/src/testing.erl");

    // File contents
    let _dependencies = dependencies.unwrap_or(HashMap::new());
    let rebar_config_content = format_rebar_config!(_dependencies);
    let app_src_content = format_app_src!(_dependencies);
    let script_content = format_script!(content);

    // Write contents
    fs::write(rebar_config_path, rebar_config_content)
        .await
        .map_err(|_| result::Error::Filesystem)?;

    fs::write(app_src_path, app_src_content)
        .await
        .map_err(|_| result::Error::Filesystem)?;

    fs::write(script_path, script_content)
        .await
        .map_err(|_| result::Error::Filesystem)?;

    Ok(())
}

/// Run erlang playground.
pub async fn run(pack: &WebSocketPack, name: String) -> result::Result<()> {
    let path = playground_path!(name);
    let command = format!("cd {path} && TERM=dumb ./run.sh");

    stream::run(pack, command).await
}
