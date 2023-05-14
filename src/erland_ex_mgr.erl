-module(erland_ex_mgr).
-behaviour(erland_mgr).

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
                "mkdir testing && "
                "echo \"IO.puts :ok\" > testing/testing.exs &&"
                "mv testing ~s",
                [Name]
            ),
            erland_cmd:run(".", Command, Id, create, Listener)
    end.

set(Name, Deps, Content, Id, Listener) ->
    DepsFormat = lists:join(
        ", ",
        lists:map(
            fun({Key, Value}) -> io_lib:format("{:~s, \"~~> ~s\"}", [Key, Value]) end,
            maps:to_list(Deps)
        )
    ),

    FileName = io_lib:format("~s/testing.exs", [Name]),
    FileContent = io_lib:format("Mix.install([~s])\n\n~s", [DepsFormat, Content]),

    Result = file:write_file(FileName, FileContent),
    Listener ! {{command, set}, Id, Result}.

run(Name, Id, Listener) ->
    erland_cmd:run(Name, "elixir testing.exs", Id, run, Listener).
