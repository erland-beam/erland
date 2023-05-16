%%% @doc Erlang manager for Erland.
-module(erland_erl_mgr).
-behaviour(erland_mgr).

-include("./erland.hrl").

-export([
    create/3,
    set/5,
    run/3
]).

create(<<"testing">>, Id, Listener) ->
    Listener ! {{command, create}, Id, {error, unique}};
create(Name, Id, Listener) ->
    FolderPath = ?PLAYGROUND_PATH(Name),
    case filelib:is_dir(FolderPath) of
        true ->
            Listener ! {{command, create}, Id, {error, exists}};
        false ->
            Command = io_lib:format(
                "TERM=dumb rebar3 new escript testing && "
                "echo \"~s\" > ./testing/run.sh && "
                "chmod +x ./testing/run.sh && "
                "mv ./testing ~s",
                [?FILE_RUN_SH, FolderPath]
            ),
            erland_cmd:run(".", Command, Id, create, Listener)
    end.

set(Name, Deps, Content, Id, Listener) ->
    FolderPath = ?PLAYGROUND_PATH(Name),

    % Create format for rebar.config
    DepsFormat = lists:join(
        ", ",
        lists:map(
            fun({Key, Value}) -> io_lib:format("{~s, \"~s\"}", [Key, Value]) end, maps:to_list(Deps)
        )
    ),

    RebarConfigFile = [FolderPath | "/rebar.config"],
    RebarConfigContent = io_lib:format("~s{deps, [~s]}.\n", [?MODULE_REBAR_CONFIG, DepsFormat]),

    % Try to write contents (if success all other files must also exists too)
    case file:write_file(RebarConfigFile, RebarConfigContent) of
        ok ->
            % Update main file
            ModuleFile = [FolderPath | "/src/testing.erl"],
            ModuleContent = [?MODULE_FILE_HEADER | Content],

            file:write_file(ModuleFile, ModuleContent),

            % Update .app.src file
            AppsFormat = lists:flatten(
                lists:join(
                    ", ",
                    lists:map(fun(Key) -> binary_to_list(Key) end, [
                        <<"kernel">>, <<"stdlib">> | maps:keys(Deps)
                    ])
                )
            ),

            AppFile = [FolderPath | "/src/testing.app.src"],
            AppContent = io_lib:format(?MODULE_APP_SRC, [AppsFormat]),

            Result = file:write_file(AppFile, AppContent),
            Listener ! {{command, set}, Id, Result};
        {error, _Reason} ->
            Listener ! {{command, set}, Id, {error, perm}}
    end.

run(Name, Id, Listener) ->
    erland_cmd:run(?PLAYGROUND_PATH(Name), "./run.sh", Id, run, Listener).
