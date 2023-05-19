use super::WebSocketPack;
use crate::{result, stream};

use std::{collections::HashMap, process::Stdio};
use tokio::fs;

macro_rules! format_command {
    ($path:expr) => {
        format!(
            r#"mkdir testing && 
echo "IO.puts :ok" > ./testing/testing.exs && 
mv ./testing {}"#,
            $path
        )
    };
}

macro_rules! format_script {
    ($content:expr, $deps:expr) => {
        format!(
            "Mix.install([{}])\n\n{}",
            $deps
                .iter()
                .map(|(key, value)| format!("{{:{key}, \"~> {value}\"}}"))
                .collect::<Vec<_>>()
                .join(", "),
            $content
        )
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

    let script_path = format!("{path}/testing.exs");
    let script_content = format_script!(content, dependencies);

    fs::write(script_path, script_content)
        .await
        .map_err(|_| result::Error::FsError)?;

    Ok(())
}

pub async fn run(pack: &WebSocketPack, name: String) -> result::Result<()> {
    let command = format!("cd /tmp/erland/{name} && TERM=dumb elixir testing.exs");
    stream::run(pack, command).await
}
