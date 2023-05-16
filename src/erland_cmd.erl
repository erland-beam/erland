%%% @doc Shell runner which gives us real-time information about state.
-module(erland_cmd).

-export([run/5]).

run(Directory, Command, Id, Type, Listener) ->
    Args = io_lib:format("cd ~s && ~s", [Directory, Command]),

    spawn(fun() ->
        Port = open_port({spawn_executable, "/bin/bash"}, [
            stderr_to_stdout,
            in,
            exit_status,
            binary,
            stream,
            {args, ["-c", Args]}
        ]),

        handle_output(Port, Id, Type, Listener)
    end).

%% -------------------------------------------
%% Following functions are for internal usage.
%% -------------------------------------------

handle_output(Port, Id, Type, Listener) ->
    receive
        {Port, {data, _Data} = Data} ->
            Listener ! {{command, Type}, Id, Data},
            handle_output(Port, Id, Type, Listener);
        {Port, {exit_status, 0}} ->
            Listener ! {{command, Type}, Id, ok};
        {Port, {exit_status, Error}} ->
            Listener ! {{command, Type}, Id, {error, Error}};
        _Other ->
            handle_output(Port, Id, Type, Listener)
    end.
