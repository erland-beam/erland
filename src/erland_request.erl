-module(erland_request).

-export([
    handle/2
]).

handle(
    #{<<"id">> := Id, <<"method">> := <<"create">>, <<"params">> := [<<"erlang">>, Name]}, Listener
) when is_binary(Name) ->
    erland_erl_mgr:create(Name, Id, Listener);
handle(
    #{<<"id">> := Id, <<"method">> := <<"set">>, <<"params">> := [Deps, Content, Name]}, Listener
) when is_map(Deps), is_binary(Content), is_binary(Name) ->
    case find_language(Name) of
        erlang ->
            erland_erl_mgr:set(Name, Deps, Content, Id, Listener);
        null ->
            handle({fallback, Id}, Listener)
    end;
handle(
    #{<<"id">> := Id, <<"method">> := <<"run">>, <<"params">> := [Name]}, Listener
) when is_binary(Name) ->
    case find_language(Name) of
        erlang ->
            erland_erl_mgr:run(Name, Id, Listener);
        null ->
            handle({fallback, Id}, Listener)
    end;
handle({fallback, Id}, Listener) ->
    Listener ! {fallback, Id};
handle(_Request, _Listener) ->
    ok.

find_language(Name) ->
    case filelib:is_dir(Name) of
        true ->
            RebarConfig = io_lib:format("./~s/rebar.config", [Name]),

            case filelib:is_file(RebarConfig) of
                true ->
                    erlang;
                false ->
                    elixir
            end;
        false ->
            null
    end.
