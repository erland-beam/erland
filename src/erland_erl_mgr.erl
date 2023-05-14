-module(erland_erl_mgr).

-behaviour(erland_mgr).

-define(MODULE_REBAR_CONFIG,
    "{erl_opts, [no_debug_info]}.\n"
    "{escript_incl_apps, [testing]}.\n"
    "{escript_main_app, testing}.\n"
    "{escript_name, testing}.\n"
    "{escript_emu_args, \"%%! +sbtu +A1~n\"}.\n"
    "{profiles, [{test, [{erl_opts, [debug_info]}]}]}.\n"
).

-define(MODULE_FILE_HEADER,
    "-module(testing).\n"
    "-export([main/1]).\n"
    "\n"
).

-export([
    create/3,
    content/5,
    run/3
]).

create(Name, Id, Listener) ->
    case filelib:is_dir(Name) of
        true ->
            Listener ! {{command, create}, Id, {error, already_exists}};
        false ->
            Command = io_lib:format(
                "git clone https://github.com/erland-beam/rebar3-template.git ~s", [Name]
            ),

            erland_cmd:run(".", Command, Id, create, Listener)
    end,
    ok.

content(Name, Deps, Content, Id, Listener) ->
    RebarConfigFile = io_lib:format("./~s/rebar.config", [Name]),
    RebarConfigContent = io_lib:format("~s{deps, ~p}.\n", [?MODULE_REBAR_CONFIG, Deps]),

    case file:write_file(RebarConfigFile, RebarConfigContent) of
        {error, _Reason} ->
            Listener ! {{command, content}, Id, {error, not_exists}};
        ok ->
            ModuleFile = io_lib:format("./~s/src/testing.erl", [Name]),
            ModuleContent = [?MODULE_FILE_HEADER | Content],

            Result = file:write_file(ModuleFile, ModuleContent),
            Listener ! {{command, content}, Id, Result}
    end,
    ok.

run(Name, Id, Listener) ->
    case filelib:is_dir(Name) of
        true ->
            erland_cmd:run(Name, "./run.sh", Id, run, Listener);
        false ->
            Listener ! {{command, run}, Id, {error, not_exists}}
    end,
    ok.
