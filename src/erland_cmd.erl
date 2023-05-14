-module(erland_cmd).

-export([run/5]).

run(Directory, Command, Id, Type, Listener) ->
    Args = io_lib:format("cd ~s && TERM=dumb ~s", [Directory, Command]),

    spawn(fun() ->
        Port = open_port({spawn_executable, "/bin/sh"}, [
            stderr_to_stdout,
            binary,
            exit_status,
            {args, ["-c", Args]}
        ]),

        handle_output(Port, Id, Type, Listener)
    end).

handle_output(Port, Id, Type, Listener) ->
    receive
        {Port, {data, Data}} ->
            Listener ! {{command, Type}, Id, Data},
            handle_output(Port, Id, Type, Listener);
        {Port, {exit_status, 0}} ->
            Listener ! {{command, Type}, Id, ok};
        {Port, {exit_status, Error}} ->
            Listener ! {{command, Type}, Id, {error, Error}};
        _Other ->
            handle_output(Port, Id, Type, Listener)
    end.
