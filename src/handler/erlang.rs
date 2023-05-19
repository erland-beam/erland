use crate::result;

use std::{collections::HashMap, process::Stdio};
use tokio::fs;

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

macro_rules! format_rebar_config {
    ($deps:expr) => {
        format!(
            r#"{{erl_opts, [no_debug_info]}}.
{{escript_incl_apps, [testing]}}.
{{escript_main_app, testing}}.
{{escript_name, testing}}.
{{profiles, [{{test, [{{erl_opts, [debug_info]}}]}}]}}.
{{deps, [{}]}}.
"#,
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
        format!(
            r#"{{application, testing, [
    {{description, "Erland playground template for rebar3"}},
    {{vsn, "0.0.0"}},
    {{registered, []}},
    {{applications, [{}]}},
    {{env, []}},
    {{modules, []}},
    {{licenses, []}},
    {{links, []}}
]}}.
"#,
            {
                let mut temp = $deps.keys().cloned().collect::<Vec<_>>();
                temp.extend_from_slice(&["kernel".to_string(), "stdlib".to_string()]);
                temp.join(", ")
            },
        )
    };
}

macro_rules! format_script {
    ($content:expr) => {
        format!("-module(testing).\n-export([main/1]).\n\n{}", $content)
    };
}

pub async fn create(name: String) -> crate::result::Result<()> {
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
) -> crate::result::Result<()> {
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

pub async fn run(name: String) -> crate::result::Result<()> {
    todo!()
}
