use std::{io::stdin, process::Stdio};

use axum::{http::StatusCode, Json};
use tokio::{fs, process};

use crate::{messaging::PlaygroundResponse, server::ResponseType};

const REBAR_CONFIG: &'static str = r#"{erl_opts, [no_debug_info]}.
{escript_incl_apps, [testing]}.
{escript_main_app, testing}.
{escript_name, testing}.
{profiles, [{test, [{erl_opts, [debug_info]}]}]}.
"#;

const FILE_HEADER: &'static str = r#"-module(testing).
"-export([main/1]).
"#;

const APP_SRC: &'static str = r#"{application, testing, [
  {description, "Erland playground template for rebar3"},
  {vsn, "0.0.0"},
  {registered, []},
  {applications, [{}]},
  {env, []},
  {modules, []},
  {licenses, []},
  {links, []}
]}.
"#;

const RUN_SCRIPT: &'static str = r#"export COMPILED_BINARY=./_build/default/bin/testing;
rm \$COMPILED_BINARY >/dev/null 2>&1;
rebar3 do compile, escriptize || { exit 1; };
\$COMPILED_BINARY;"#;

macro_rules! format_command {
    ($path:expr) => {
        format!(
            r#"TERM=dumb rebar3 new escript testing && 
echo "{}" > ./testing/run.sh && 
chmod +x ./testing/run.sh && 
mv ./testing {}"#,
            RUN_SCRIPT, $path
        )
    };
}

pub async fn create(name: String) -> crate::result::Result<ResponseType> {
    let path = format!("/tmp/erland/{name}");

    if fs::try_exists(&path)
        .await
        .map_err(|_| crate::result::Error::FsError)?
    {
        let error = super::err!("A playground with name \"{}\" already exists", name);
        return Ok((StatusCode::CONFLICT, error));
    }

    let command = format_command!(&path);
    let status = super::shell!(command)?;

    if status.success() {
        Ok((StatusCode::NO_CONTENT, Json(PlaygroundResponse::Ok)))
    } else {
        let error = super::err!("Unexpected error while running command");
        Ok((StatusCode::INTERNAL_SERVER_ERROR, error))
    }
}
