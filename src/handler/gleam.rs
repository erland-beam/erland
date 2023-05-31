//! Gleam handler for Erland Server.

use std::{collections::HashMap, process::Stdio};

use tokio::fs;

use crate::{playground_path, result, stream};

use super::WebSocketPack;

/// Macro for creating initialize script.
macro_rules! format_command {
    ($path:expr) => {
        format!(include_str!("../../include/gleam/init.sh"), $path)
    };
}

/// Macro for creating `gleam.toml`.
macro_rules! format_gleam_toml {
    ($deps:expr) => {
        format!(
            include_str!("../../include/gleam/gleam.toml"),
            $deps
                .iter()
                .map(|(key, value)| format!("{key} = \"~> {value}\""))
                .collect::<Vec<_>>()
                .join("\n"),
        )
    };
}

/// Create new gleam playground.
pub async fn create(name: String) -> result::Result<()> {
    let path = playground_path!(name);
    let command = format_command!(&path);

    if crate::shell!(command)?.success() {
        Ok(())
    } else {
        Err(result::Error::Command)
    }
}

/// Update gleam playground.
pub async fn update(
    name: String,
    content: String,
    dependencies: Option<HashMap<String, String>>,
) -> result::Result<()> {
    let path = playground_path!(name);

    // File paths
    let gleam_toml_path = format!("{path}/gleam.toml");
    let script_path = format!("{path}/src/testing.gleam");

    // File contents
    let gleam_toml_content = format_gleam_toml!(dependencies.unwrap_or_default());

    // Write contents
    fs::write(gleam_toml_path, gleam_toml_content)
        .await
        .map_err(|_| result::Error::Filesystem)?;

    fs::write(script_path, content)
        .await
        .map_err(|_| result::Error::Filesystem)?;

    Ok(())
}

/// Run gleam playground.
pub async fn run(pack: &WebSocketPack, name: String) -> result::Result<()> {
    let path = playground_path!(name);
    let command = format!("cd {path} && TERM=dumb gleam run");

    stream::run(pack, command).await
}
