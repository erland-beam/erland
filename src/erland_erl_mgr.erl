-module(erland_erl_mgr).
-behaviour(erland_mgr).

-define(MODULE_REBAR_CONFIG,
    "{erl_opts, [no_debug_info]}.\n"
    "{escript_incl_apps, [testing]}.\n"
    "{escript_main_app, testing}.\n"
    "{escript_name, testing}.\n"
    "{profiles, [{test, [{erl_opts, [debug_info]}]}]}.\n"
).

-define(MODULE_FILE_HEADER,
    "-module(testing).\n"
    "-export([main/1]).\n"
    "\n"
).

-define(MODULE_APP_SRC,
    "{application, testing, [\n"
    "  {description, \"Erland playground template for rebar3\"},\n"
    "  {vsn, \"0.0.0\"},\n"
    "  {registered, []},\n"
    "  {applications, [~s]},\n"
    "  {env, []},\n"
    "  {modules, []},\n"
    "  {licenses, []},\n"
    "  {links, []}\n"
    "]}."
).

-define(RUN_SH,
    "export COMPILED_BINARY=./_build/default/bin/testing;\n"
    "rm \\$COMPILED_BINARY >/dev/null 2>&1;\n"
    "rebar3 do compile, escriptize || { exit 1; };\n"
    "\\$COMPILED_BINARY;"
).

-export([
    create/3,
    set/5,
    run/3
]).

create(<<"testing">>, Id, Listener) ->
    Listener ! {{command, create}, Id, {error, unique}};
create(Name, Id, Listener) ->
    case filelib:is_dir(Name) of
        true ->
            Listener ! {{command, create}, Id, {error, exists}};
        false ->
            Command = io_lib:format(
                "rebar3 new escript testing && "
                "echo \"~s\" > testing/run.sh && "
                "chmod +x testing/run.sh && "
                "mv testing ~s",
                [?RUN_SH, Name]
            ),
            erland_cmd:run(".", Command, Id, create, Listener)
    end.

set(Name, Deps, Content, Id, Listener) ->
    DepsFormat = lists:join(
        ", ",
        lists:map(
            fun({Key, Value}) -> io_lib:format("{~s, \"~s\"}", [Key, Value]) end, maps:to_list(Deps)
        )
    ),

    RebarConfigFile = io_lib:format("./~s/rebar.config", [Name]),
    RebarConfigContent = io_lib:format("~s{deps, [~s]}.\n", [?MODULE_REBAR_CONFIG, DepsFormat]),

    case file:write_file(RebarConfigFile, RebarConfigContent) of
        ok ->
            ModuleFile = io_lib:format("./~s/src/testing.erl", [Name]),
            ModuleContent = [?MODULE_FILE_HEADER | Content],

            file:write_file(ModuleFile, ModuleContent),

            AppsFormat = lists:flatten(
                lists:join(
                    ", ", lists:map(fun(Key) -> io_lib:format("~s", [Key]) end, maps:keys(Deps))
                )
            ),

            AppFile = io_lib:format("./~s/src/testing.app.src", [Name]),
            AppContent = io_lib:format(?MODULE_APP_SRC, [AppsFormat]),

            Result = file:write_file(AppFile, AppContent),
            Listener ! {{command, set}, Id, Result};
        {error, _Reason} ->
            Listener ! {{command, set}, Id, {error, perm}}
    end.

run(Name, Id, Listener) ->
    erland_cmd:run(Name, "./run.sh", Id, run, Listener).
