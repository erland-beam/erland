%%% @doc Request handler for WebSocket.
-module(erland_request).

-export([
    handle/2
]).

%% ------------------------------------------------------
%% Following functions are for handling "create" command.
%% ------------------------------------------------------
handle(
    #{<<"id">> := Id, <<"method">> := <<"create">>, <<"params">> := [<<"erlang">>, Name]}, Listener
) when is_binary(Name) ->
    erland_erl_mgr:create(Name, Id, Listener);
handle(
    #{<<"id">> := Id, <<"method">> := <<"create">>, <<"params">> := [<<"elixir">>, Name]}, Listener
) when is_binary(Name) ->
    erland_ex_mgr:create(Name, Id, Listener);
%% ------------------------------------------------------------
%% Following functions are for handling "set" (update) command.
%% ------------------------------------------------------------
handle(
    #{<<"id">> := Id, <<"method">> := <<"set">>, <<"params">> := [Deps, Content, Name]}, Listener
) when is_map(Deps), is_binary(Content), is_binary(Name) ->
    case find_language(Name) of
        erlang ->
            erland_erl_mgr:set(Name, Deps, Content, Id, Listener);
        elixir ->
            erland_ex_mgr:set(Name, Deps, Content, Id, Listener);
        null ->
            handle({fallback, Id}, Listener)
    end;
%% ---------------------------------------------------
%% Following functions are for handling "run" command.
%% ---------------------------------------------------
handle(
    #{<<"id">> := Id, <<"method">> := <<"run">>, <<"params">> := [Name]}, Listener
) when is_binary(Name) ->
    case find_language(Name) of
        erlang ->
            erland_erl_mgr:run(Name, Id, Listener);
        elixir ->
            erland_ex_mgr:run(Name, Id, Listener);
        null ->
            handle({fallback, Id}, Listener)
    end;
%% ------------------------------------------------------
%% Following functions are for handling "delete" command.
%% ------------------------------------------------------
handle(
    #{<<"id">> := Id, <<"method">> := <<"delete">>, <<"params">> := [Name]}, Listener
) when is_binary(Name) ->
    case find_language(Name) of
        null ->
            handle({fallback, Id}, Listener);
        _Other ->
            Result = file:del_dir_r(["/tmp/erland/" | binary_to_list(Name)]),
            Listener ! {{command, delete}, Id, Result}
    end;
%% ------------------------------------------------
%% Following functions are for handling edge cases.
%% ------------------------------------------------
handle({fallback, Id}, Listener) ->
    Listener ! {fallback, Id};
handle(_Request, _Listener) ->
    ok.

%% -------------------------------------------
%% Following functions are for internal usage.
%% -------------------------------------------

find_language(Name) ->
    FolderPath = ["/tmp/erland/" | binary_to_list(Name)],

    case filelib:is_dir(FolderPath) of
        true ->
            RebarConfig = [FolderPath | "/rebar.config"],

            case filelib:is_file(RebarConfig) of
                true ->
                    erlang;
                false ->
                    elixir
            end;
        false ->
            null
    end.
