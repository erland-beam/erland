-module(erland_cmd).

-export([run/4]).

run(Directory, Command, Id, Listener) ->
    Args = io_lib:format("cd ~s && TERM=dumb ~s", [Directory, Command]),

    spawn(fun() ->
        Port = open_port({spawn_executable, "/bin/sh"}, [
            stderr_to_stdout,
            binary,
            exit_status,
            {args, ["-c", Args]}
        ]),

        handle_output(Port, Id, Listener)
    end).

handle_output(Port, Id, Listener) ->
    receive
        {Port, {data, Data}} ->
            Listener ! {command, Id, Data},
            handle_output(Port, Id, Listener);
        {Port, {exit_status, 0}} ->
            Listener ! {command, ok};
        {Port, {exit_status, Error}} ->
            Listener ! {command, {error, Error}}
    end.
