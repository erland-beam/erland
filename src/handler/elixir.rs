//! Elixir handler for Erland Server.

use super::WebSocketPack;
use crate::{playground_path, result, stream};

use std::{collections::HashMap, process::Stdio};
use tokio::fs;

/// Macro for creating initialize script.
macro_rules! format_command {
    ($path:expr) => {
        format!(include_str!("../../include/elixir/init.sh"), $path)
    };
}

/// Macro for creating `testing.exs`.
macro_rules! format_script {
    ($content:expr, $deps:expr) => {
        format!(
            include_str!("../../include/elixir/testing.exs"),
            $deps
                .iter()
                .map(|(key, value)| format!("{{:{key}, \"~> {value}\"}}"))
                .collect::<Vec<_>>()
                .join(", "),
            $content
        )
    };
}

/// Create new elixir playground.
pub async fn create(name: String) -> result::Result<()> {
    let path = playground_path!(name);
    let command = format_command!(&path);

    if crate::shell!(command)?.success() {
        Ok(())
    } else {
        Err(result::Error::Command)
    }
}

/// Update elixir playground.
pub async fn update(
    name: String,
    content: String,
    dependencies: Option<HashMap<String, String>>,
) -> result::Result<()> {
    let path = playground_path!(name);

    let script_path = format!("{path}/testing.exs");
    let script_content = format_script!(content, dependencies.unwrap_or_default());

    fs::write(script_path, script_content)
        .await
        .map_err(|_| result::Error::Filesystem)?;

    Ok(())
}

/// Run elixir playground.
pub async fn run(pack: &WebSocketPack, name: String) -> result::Result<()> {
    let path = playground_path!(name);
    let command = format!("cd {path} && TERM=dumb elixir testing.exs");

    stream::run(pack, command).await
}
